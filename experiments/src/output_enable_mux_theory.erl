-module(output_enable_mux_theory).

-export([run/0]).

% This is a theory of how IOB local interconnects are muxed into the
% IOC output enables.
%
% The theory is based on the data produced by `output_enable_mux_playground`.
%
% The output enable MUX is equivilent to the output MUX with the same
% number of mux coordinates and shareing the same `output_mux_map`.
%
% See `output_mux_theory` for a description.
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
    Density = device:density(Device),
    case experiment(Density, Experiment) of
        ok ->
            experiments(experiment_cache:iterate(Iterator));

        {error, Error, Fuses, Signals} ->
            {cached, Dir} = Experiment,
            contradiction(Density, Dir, Fuses, Signals, Error)
    end.

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

fuse(Density, Fuse, LCs) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {IOC = {ioc, _, _, _}, Key = enable6, Value}} ->
            fuse_mux(IOC, Key, Value, LCs);

        {ok, {IOC = {ioc, _, _, _}, Key = enable4, Value}} ->
            fuse_mux(IOC, Key, Value, LCs);

        {ok, {IOC = {ioc, _, _, _}, Key = enable3, Value}} ->
            fuse_mux(IOC, Key, Value, LCs);

        _ ->
            LCs
    end.

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

signal_dest(#{ioc := IOC, port := oe, route := Route0}, Model) ->
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

signal_route([{io_oe, _, _, _, _}, Interconnect | _]) ->
    Interconnect.

%%====================================================================
%% theory
%%====================================================================

theory(IOC = {ioc, X, Y, _}, Model) ->
    case Model of
        #{IOC := #{enable4 := Mux4, enable3 := Mux3}} ->
            theory_col(X, Y, Mux4, Mux3);

        #{IOC := #{enable6 := Mux6, enable3 := Mux3}} ->
            theory_row(X, Y, Mux6, Mux3);

        #{IOC := Muxes} ->
            Muxes;

        _ ->
            IOC
    end.

%%--------------------------------------------------------------------

theory_col(X, Y, Mux4, Mux3) ->
    {interconnect, N} = output_mux_map:to_col_interconnect(Mux4, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_row(X, Y, Mux6, Mux3) ->
    {interconnect, N} = output_mux_map:to_row_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

