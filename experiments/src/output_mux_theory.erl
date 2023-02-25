-module(output_mux_theory).

-export([run/0]).

% This is a theory of how IOB local interconnects are muxed into the
% IOC outputs.
%
% The theory is based on the data produced by `output_mux_playground`.
%
% ASIDE: Muxes seen so far in the MAX II architecture seem to be all
%   one-hot (or one-cold) selecting multiplexers (rather than
%   binary encoded).
%
% Each IOC output has a two dimentional mux.
%
% Side IOCs have the two coordinates as:
%  * a 6-to-1 mux `output6`, and
%  * a 3-to-1 mux `output3`.
% Combined the form either a 18-to-1 mux.
%
% Top/bottom IOCs have the two coordinates as:
%  * a 4-to-1 mux `output4`, and
%  * a 3-to-1 mux `output3`.
% Combined the form either a 12-to-1 mux.
%
%  Checking
% ==========
%
% This theory is checked against all cached experiments
% as a non-exhaustive proof.
%
% It scans the fuses in each experiment looking
% for output6, output4 & output3 one-cold muxes whilst:
%
% * confirming that they are actually _one-cold_, and
% * building up a local_interconnect model.
%
% Then match the model up with the RCF file.

%%====================================================================
%% run
%%====================================================================

run() ->
    experiments(experiment_cache:iterate()).

%%--------------------------------------------------------------------

experiments(false) ->
    ok;
experiments({Device, Experiment, Iterator}) ->
    case skip(Experiment) of
        true ->
            experiments(experiment_cache:iterate(Iterator));

        false ->
            Density = device:density(Device),
            case experiment(Density, Experiment) of
                ok ->
                    experiments(experiment_cache:iterate(Iterator));

                {error, Error, Fuses, Signals} ->
                    {cached, Dir} = Experiment,
                    contradiction(Density, Dir, Fuses, Signals, Error)
            end
    end.

%%--------------------------------------------------------------------

%skip({cached, "cache/W6/E3eRH3uQ3KvgCQhdAb1vcf8obs1gTvlu_5OiYk0A0"}) ->
%    % mux3 are missing!
%    % i.e.
%    % #{data_a6 => mux6_5,
%        data_b6 => mux6_4,
%        data_c6 => mux6_1,
%        data_d6 => mux6_0}
%    true;
%skip({cached, "cache/nb/D_5XwElyalw_RYNZlvLPDxwL_QfcLoiyREXtoeWnQ"}) ->
%    % same as above
%    true;
skip(_) -> false.

%%--------------------------------------------------------------------

experiment(Density, Experiment) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    {ok, #{signals := Signals}} = experiment:rcf(Experiment),
    try
        Model = fuses(Density, Fuses),
        signals(Signals, Model),
        ok
    catch
        throw:Throw ->
            {error, Throw, Fuses, Signals}
    end.

%%--------------------------------------------------------------------

contradiction(Density, Dir, Fuses0, Signals, Error) ->
    io:format("~n => ~s~n", [Dir]),
    io:format("~nFUSES:~n", []),
    Fuses = fuses:subtract(Fuses0, density:minimal_fuses(Density)),
    lists:foreach(fun (Fuse) -> contradiction(Density, Fuse) end, Fuses),
    io:format("~nSIGNALS:~n  ~p~n", [Signals]),
    io:format("~nERROR:~n  ~p~n", [Error]),
    ok.

%%--------------------------------------------------------------------

contradiction(Density, Fuse) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Name} ->
            io:format("  ~w~n", [Name]);

        {error, Error} ->
            io:format("  ~w~n", [Error])
    end.

%%====================================================================
%% fuses
%%====================================================================

fuses(Density, Fuses) ->
    lists:foldl(fun (Fuse, LCs) -> fuse(Density, Fuse, LCs) end, #{}, Fuses).

%%--------------------------------------------------------------------

-define(SIDE(Sector, Index, Key, Value),
        {X, Y, N, Index, side, Sector} ->
            fuse_mux({ioc, X, Y, N - 2}, Key, Value, LCs)
).
-define(HEAD(Sector, Index, N, Key, Value),
        {X, head, Index, cell, Sector} ->
            fuse_mux(fuse_head(X, N, Density), Key, Value, LCs)
).
-define(TAIL(Sector, Index, N, Key, Value),
        {X, tail, Index, cell, Sector} ->
            fuse_mux(fuse_tail(X, N, Density), Key, Value, LCs)
).

fuse(Density, Fuse, LCs) ->
    case fuse_map:to_location(Fuse, Density) of
        ?SIDE( 1, 1, bypass, on);
        ?SIDE( 2, 0, output3, mux1);
        ?SIDE( 3, 0, output3, mux0);
        ?SIDE( 3, 1, output3, mux2);
        ?SIDE( 4, 0, output6, mux0);
        ?SIDE( 4, 1, output6, mux1);
        ?SIDE( 5, 0, output6, mux2);
        ?SIDE( 5, 1, output6, mux3);
        ?SIDE( 6, 0, output6, mux4);
        ?SIDE( 6, 1, output6, mux5);
        ?HEAD( 6, 1, 2, output4, mux2);
        ?HEAD( 6, 2, 2, output4, mux3);
        ?HEAD( 6, 3, 3, output4, mux2);
        ?HEAD( 6, 4, 3, output4, mux3);
        ?HEAD( 7, 1, 2, output4, mux0);
        ?HEAD( 7, 2, 2, output4, mux1);
        ?HEAD( 7, 3, 3, output4, mux0);
        ?HEAD( 7, 4, 3, output4, mux1);
        ?HEAD( 8, 1, 2, output3, mux0);
        ?HEAD( 8, 2, 2, output3, mux2);
        ?HEAD( 8, 3, 3, output3, mux0);
        ?HEAD( 8, 4, 3, output3, mux2);
        ?HEAD( 9, 1, 2, output3, mux1);
        ?HEAD( 9, 3, 3, output3, mux1);
        ?HEAD(10, 2, 2, bypass, on);
        ?HEAD(10, 4, 3, bypass, on);
        ?HEAD(12, 2, 0, bypass, on);
        ?HEAD(12, 4, 1, bypass, on);
        ?HEAD(13, 1, 0, output3, mux1);
        ?HEAD(13, 3, 1, output3, mux1);
        ?HEAD(15, 1, 0, output3, mux0);
        ?HEAD(15, 2, 0, output3, mux2);
        ?HEAD(15, 3, 1, output3, mux0);
        ?HEAD(15, 4, 1, output3, mux2);
        ?HEAD(16, 1, 0, output4, mux0);
        ?HEAD(16, 2, 0, output4, mux1);
        ?HEAD(16, 3, 1, output4, mux0);
        ?HEAD(16, 4, 1, output4, mux1);
        ?HEAD(17, 1, 0, output4, mux2);
        ?HEAD(17, 2, 0, output4, mux3);
        ?HEAD(17, 3, 1, output4, mux2);
        ?HEAD(17, 4, 1, output4, mux3);
        ?TAIL( 6, 1, 2, output4, mux2);
        ?TAIL( 6, 2, 2, output4, mux3);
        ?TAIL( 6, 3, 3, output4, mux2);
        ?TAIL( 6, 4, 3, output4, mux3);
        ?TAIL( 7, 1, 2, output4, mux0);
        ?TAIL( 7, 2, 2, output4, mux1);
        ?TAIL( 7, 3, 3, output4, mux0);
        ?TAIL( 7, 4, 3, output4, mux1);
        ?TAIL( 8, 1, 2, output3, mux0);
        ?TAIL( 8, 2, 2, output3, mux2);
        ?TAIL( 8, 3, 3, output3, mux0);
        ?TAIL( 8, 4, 3, output3, mux2);
        ?TAIL( 9, 1, 2, output3, mux1);
        ?TAIL( 9, 3, 3, output3, mux1);
        ?TAIL(10, 2, 2, bypass, on);
        ?TAIL(10, 4, 3, bypass, on);
        ?TAIL(12, 2, 0, bypass, on);
        ?TAIL(12, 4, 1, bypass, on);
        ?TAIL(13, 1, 0, output3, mux1);
        ?TAIL(13, 3, 1, output3, mux1);
        ?TAIL(15, 1, 0, output3, mux0);
        ?TAIL(15, 2, 0, output3, mux2);
        ?TAIL(15, 3, 1, output3, mux0);
        ?TAIL(15, 4, 1, output3, mux2);
        ?TAIL(16, 1, 0, output4, mux0);
        ?TAIL(16, 2, 0, output4, mux1);
        ?TAIL(16, 3, 1, output4, mux0);
        ?TAIL(16, 4, 1, output4, mux1);
        ?TAIL(17, 1, 0, output4, mux2);
        ?TAIL(17, 2, 0, output4, mux3);
        ?TAIL(17, 3, 1, output4, mux2);
        ?TAIL(17, 4, 1, output4, mux3);

        _ ->
            LCs
    end.

%%--------------------------------------------------------------------

fuse_head(X, N, epm240) ->
    {ioc, X, 5, N};
fuse_head(X, N, epm570) ->
    {ioc, X, 8, N};
fuse_head(X, N, epm1270) ->
    {ioc, X, 11, N};
fuse_head(X, N, epm2210) ->
    {ioc, X, 14, N}.

%%--------------------------------------------------------------------

fuse_tail(X, N, epm240) ->
    {ioc, X, 0, N};
fuse_tail(X, N, epm570) when X < 10 ->
    {ioc, X, 3, N};
fuse_tail(X, N, epm570) ->
    {ioc, X, 0, N};
fuse_tail(X, N, epm1270) when X < 12 ->
    {ioc, X, 3, N};
fuse_tail(X, N, epm1270) ->
    {ioc, X, 0, N};
fuse_tail(X, N, epm2210) when X < 14 ->
    {ioc, X, 3, N};
fuse_tail(X, N, epm2210) ->
    {ioc, X, 0, N}.

%%--------------------------------------------------------------------

fuse_mux(LC, Key, Value, LCs) ->
    case LCs of
        #{LC := #{Key := Existing}} when Existing =:= Value ->
            LCs;

        #{LC := #{Key := Existing}} ->
            throw({LC, Key, Value, existing, Existing});

        #{LC := Muxes} ->
            LCs#{LC => Muxes#{Key => Value}};

        _ ->
            LCs#{LC => #{Key => Value}}
    end.

%%====================================================================
%% signals
%%====================================================================

signals(Signals, Model) ->
    maps:foreach(fun (_, Signal) -> signal(Signal, Model) end, Signals).

%%--------------------------------------------------------------------

signal(#{dests := Dests}, Model) ->
    lists:foreach(fun (Dest) -> signal_dest(Dest, Model) end, Dests).

%%--------------------------------------------------------------------

signal_dest(#{ioc := IOC, port := data_in, route := Route0}, Model) ->
    Route = signal_route(Route0),
    case theory(IOC, Model) of
        Theory when Theory =:= Route ->
            ok;

        Theory ->
            throw({IOC, Route, theory, Theory})
    end;
signal_dest(_, _) ->
    ok.

%%--------------------------------------------------------------------

signal_route([Bypass = {io_bypass_out, _, _, _, _} | _]) ->
    Bypass;
signal_route([{io_data_out, _, _, _, _}, Interconnect | _]) ->
    Interconnect.

%%====================================================================
%% theory
%%====================================================================

theory(IOC = {ioc, X, Y, N}, Model) ->
    case Model of
        #{IOC := #{output4 := Mux4, output3 := Mux3, bypass := on}} ->
            theory_cell(X, Y, N, Mux4, Mux3);

        #{IOC := #{output4 := Mux4, output3 := Mux3}} ->
            theory_cell(X, Y, x, Mux4, Mux3);

        #{IOC := #{output6 := Mux6, output3 := Mux3}} ->
            theory_side(X, Y, Mux6, Mux3);

        #{IOC := #{bypass := on}} ->
            {io_bypass_out, X, Y, N, 0};

        #{IOC := Muxes} ->
            Muxes;

        _ ->
            IOC
    end.

%%--------------------------------------------------------------------

theory_cell(X, Y, Bypass, Mux4, Mux3) ->
    case output_mux_map:to_interconnect4(Mux4, Mux3) of
        {interconnect, N} when Bypass =:= x ->
            {local_interconnect, X, Y, 0, N};

        bypass when Bypass =/= x ->
            {io_bypass_out, X, Y, Bypass, 0}
    end.

%%--------------------------------------------------------------------

theory_side(X, Y, Mux6, Mux3) ->
    {interconnect, N} = output_mux_map:to_interconnect7(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

