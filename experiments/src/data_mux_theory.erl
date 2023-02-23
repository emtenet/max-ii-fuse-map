-module(data_mux_theory).

-export([run/0]).

% This is a theory of how inputs are muxed into the four LUT data
% inputs: data_a, data_b, data_c & data_d.
%
% The theory is based on the data produced by `data_mux_playground`.
%
% ASIDE: Muxes seen so far in the MAX II architecture seem to be all
%   one-hot (or one-cold) selecting multiplexers (rather than
%   binary encoded).
%
% Each LUT input has a two dimentional mux, with the two coordinates as:
%  * a 6-to-1 mux `data_#6`, and
%  * a 3-to-1 mux `data_#3`.
% Combined the form a 18-to-1 mux.
%
% There are 36 local interconnects to select from:
%  * 10 local lines from the output of LCs in the same LAB
%  * 26 fed from direct links, C4s, R4, ...
%
% Each LUT can only select from a subset (18) of the 36.
% But with overlapping subsets and input re-ordering,
% a LUT can select every combination (1..4) of the 36 interconnects.
%
%  Fuses
% =======
%
% For an example LUT in {lc,X,Y,N}.
%
% The 6-to-1 muxes have the following fuses, mux6_[0-5]:
%
%   data_a6            data_b6            data_c6            data_d6
%   -------            -------            -------            -------
%   {X,Y,N,0,cell,3}   {X,Y,N,0,cell,5}   {X,Y,N,2,cell,3}   {X,Y,N,2,cell,6}
%   {X,Y,N,1,cell,3}   {X,Y,N,1,cell,5}   {X,Y,N,3,cell,3}   {X,Y,N,3,cell,6}
%   {X,Y,N,0,cell,4}   {X,Y,N,0,cell,6}   {X,Y,N,2,cell,4}   {X,Y,N,2,cell,7}
%   {X,Y,N,1,cell,4}   {X,Y,N,1,cell,6}   {X,Y,N,3,cell,4}   {X,Y,N,3,cell,7}
%   {X,Y,N,0,cell,8}   {X,Y,N,0,cell,7}   {X,Y,N,2,cell,5}   {X,Y,N,2,cell,8}
%   {X,Y,N,1,cell,8}   {X,Y,N,1,cell,7}   {X,Y,N,3,cell,5}   {X,Y,N,3,cell,8}
%
% The 3-to-1 muxes have the following fuses, mux3_[0-2]:
%
%   data_a3            data_b3            data_c3            data_d3
%   -------            -------            -------            -------
%   {X,Y,N,0,cell, 9}  {X,Y,N,1,cell, 9}  {X,Y,N,2,cell, 9}  {X,Y,N,3,cell, 9}
%   {X,Y,N,0,cell,10}  {X,Y,N,1,cell,10}  {X,Y,N,2,cell,10}  {X,Y,N,3,cell,10}
%   {X,Y,N,0,cell,11}  {X,Y,N,1,cell,11}  {X,Y,N,2,cell,11}  {X,Y,N,3,cell,11}
%
% For example, the combined data_a mux is:
%
%           mux3_0              mux3_1              mux3_2
%           ------              ------              ------
%   mux6_0: interconnect,0      interconnect,3      interconnect,8
%   mux6_1: interconnect,9      interconnect,11     interconnect,14
%   mux6_2: interconnect,18     interconnect,22     interconnect,25
%   mux6_3: local_line,4        local_line,5        local_line,6
%   mux6_4: interconnect,1      interconnect,6      interconnect,15
%   mux6_5: interconnect,19     local_line,3        local_line,8
%
%  Checking
% ==========
%
% This theory is checked against all cached experiments
% as a non-exhaustive proof.
%
% It scans the fuses in each experiment looking
% for data_#6 & data_#3 one-hot muxes whilst:
%
% * confirming that they are actually _one-cold_, and
% * building up a local_interconnect model.
%
% Then match the model up with the RCF file.
%
%  Outliers
% =========
%
% As of this writting, two outlier experiments where found that
% contained data_#6 fuses _without_ a data_#3 fuse! (zero-hot!)
% Assuming a mux3_0 fuse as default is enough to pass this theoryi
% check for those two experiments.

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

fuse(Density, Fuse, LCs) ->
    case fuse_map:to_name(Fuse, Density) of
        {error, {X, Y, N, 0, cell, 3}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_0, LCs);

        {error, {X, Y, N, 1, cell, 3}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_1, LCs);

        {error, {X, Y, N, 0, cell, 4}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_2, LCs);

        {error, {X, Y, N, 1, cell, 4}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_3, LCs);

        {error, {X, Y, N, 0, cell, 8}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_4, LCs);

        {error, {X, Y, N, 1, cell, 8}} ->
            fuse_mux({lc, X, Y, N}, data_a6, mux6_5, LCs);

        {error, {X, Y, N, 0, cell, 5}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_0, LCs);

        {error, {X, Y, N, 1, cell, 5}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_1, LCs);

        {error, {X, Y, N, 0, cell, 6}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_2, LCs);

        {error, {X, Y, N, 1, cell, 6}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_3, LCs);

        {error, {X, Y, N, 0, cell, 7}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_4, LCs);

        {error, {X, Y, N, 1, cell, 7}} ->
            fuse_mux({lc, X, Y, N}, data_b6, mux6_5, LCs);

        {error, {X, Y, N, 2, cell, 3}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_0, LCs);

        {error, {X, Y, N, 3, cell, 3}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_1, LCs);

        {error, {X, Y, N, 2, cell, 4}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_2, LCs);

        {error, {X, Y, N, 3, cell, 4}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_3, LCs);

        {error, {X, Y, N, 2, cell, 5}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_4, LCs);

        {error, {X, Y, N, 3, cell, 5}} ->
            fuse_mux({lc, X, Y, N}, data_c6, mux6_5, LCs);

        {error, {X, Y, N, 2, cell, 6}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_0, LCs);

        {error, {X, Y, N, 3, cell, 6}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_1, LCs);

        {error, {X, Y, N, 2, cell, 7}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_2, LCs);

        {error, {X, Y, N, 3, cell, 7}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_3, LCs);

        {error, {X, Y, N, 2, cell, 8}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_4, LCs);

        {error, {X, Y, N, 3, cell, 8}} ->
            fuse_mux({lc, X, Y, N}, data_d6, mux6_5, LCs);

        {error, {X, Y, N, 0, cell, 9}} ->
            fuse_mux({lc, X, Y, N}, data_a3, mux3_0, LCs);

        {error, {X, Y, N, 1, cell, 9}} ->
            fuse_mux({lc, X, Y, N}, data_b3, mux3_0, LCs);

        {error, {X, Y, N, 2, cell, 9}} ->
            fuse_mux({lc, X, Y, N}, data_c3, mux3_0, LCs);

        {error, {X, Y, N, 3, cell, 9}} ->
            fuse_mux({lc, X, Y, N}, data_d3, mux3_0, LCs);

        {error, {X, Y, N, 0, cell, 10}} ->
            fuse_mux({lc, X, Y, N}, data_a3, mux3_1, LCs);

        {error, {X, Y, N, 1, cell, 10}} ->
            fuse_mux({lc, X, Y, N}, data_b3, mux3_1, LCs);

        {error, {X, Y, N, 2, cell, 10}} ->
            fuse_mux({lc, X, Y, N}, data_c3, mux3_1, LCs);

        {error, {X, Y, N, 3, cell, 10}} ->
            fuse_mux({lc, X, Y, N}, data_d3, mux3_1, LCs);

        {error, {X, Y, N, 0, cell, 11}} ->
            fuse_mux({lc, X, Y, N}, data_a3, mux3_2, LCs);

        {error, {X, Y, N, 1, cell, 11}} ->
            fuse_mux({lc, X, Y, N}, data_b3, mux3_2, LCs);

        {error, {X, Y, N, 2, cell, 11}} ->
            fuse_mux({lc, X, Y, N}, data_c3, mux3_2, LCs);

        {error, {X, Y, N, 3, cell, 11}} ->
            fuse_mux({lc, X, Y, N}, data_d3, mux3_2, LCs);

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

signal_dest(#{route := [{lut_chain, _, _, _, _} | _] }, _) ->
    ok;
signal_dest(#{lc := LC, route_port := Port, route := [Route | _] }, Model) ->
    case theory(LC, Port, Model) of
        Theory when Theory =:= Route ->
            ok;

        Theory ->
            throw({LC, Port, Route, theory, Theory})
    end;
signal_dest(_, _) ->
    ok.

%%====================================================================
%% theory
%%====================================================================

theory(LC = {lc, X, Y, _}, data_a, Model) ->
    case Model of
        #{LC := #{data_a3 := Mux3, data_a6 := Mux6}}  ->
            theory(X, Y, data_a, Mux3, Mux6);

        #{LC := #{data_a6 := Mux6}}  ->
            theory(X, Y, data_a, mux3_0, Mux6);

        #{LC := Muxes} ->
            Muxes;

        _ ->
            LC
    end;
theory(LC = {lc, X, Y, _}, data_b, Model) ->
    case Model of
        #{LC := #{data_b3 := Mux3, data_b6 := Mux6}} ->
            theory(X, Y, data_b, Mux3, Mux6);

        #{LC := #{data_b6 := Mux6}}  ->
            theory(X, Y, data_b, mux3_0, Mux6);

        #{LC := Muxes} ->
            Muxes;

        _ ->
            LC
    end;
theory(LC = {lc, X, Y, _}, data_c, Model) ->
    case Model of
        #{LC := #{data_c3 := Mux3, data_c6 := Mux6}} ->
            theory(X, Y, data_c, Mux3, Mux6);

        #{LC := #{data_c6 := Mux6}}  ->
            theory(X, Y, data_c, mux3_0, Mux6);

        #{LC := Muxes} ->
            Muxes;

        _ ->
            LC
    end;
theory(LC = {lc, X, Y, _}, data_d, Model) ->
    case Model of
        #{LC := #{data_d3 := Mux3, data_d6 := Mux6}} ->
            theory(X, Y, data_d, Mux3, Mux6);

        #{LC := #{data_d6 := Mux6}}  ->
            theory(X, Y, data_d, mux3_0, Mux6);

        #{LC := Muxes} ->
            Muxes;

        _ ->
            LC
    end.

%%--------------------------------------------------------------------

theory(X, Y, data_a, mux3_0, mux6_0) -> {local_interconnect, X, Y, 0, 0};
theory(X, Y, data_a, mux3_0, mux6_1) -> {local_interconnect, X, Y, 0, 9};
theory(X, Y, data_a, mux3_0, mux6_2) -> {local_interconnect, X, Y, 0, 18};
theory(X, Y, data_a, mux3_0, mux6_3) -> {local_line, X, Y, 0, 4};
theory(X, Y, data_a, mux3_0, mux6_4) -> {local_interconnect, X, Y, 0, 1};
theory(X, Y, data_a, mux3_0, mux6_5) -> {local_interconnect, X, Y, 0, 19};
theory(X, Y, data_a, mux3_1, mux6_0) -> {local_interconnect, X, Y, 0, 3};
theory(X, Y, data_a, mux3_1, mux6_1) -> {local_interconnect, X, Y, 0, 11};
theory(X, Y, data_a, mux3_1, mux6_2) -> {local_interconnect, X, Y, 0, 22};
theory(X, Y, data_a, mux3_1, mux6_3) -> {local_line, X, Y, 0, 5};
theory(X, Y, data_a, mux3_1, mux6_4) -> {local_interconnect, X, Y, 0, 6};
theory(X, Y, data_a, mux3_1, mux6_5) -> {local_line, X, Y, 0, 3};
theory(X, Y, data_a, mux3_2, mux6_0) -> {local_interconnect, X, Y, 0, 8};
theory(X, Y, data_a, mux3_2, mux6_1) -> {local_interconnect, X, Y, 0, 14};
theory(X, Y, data_a, mux3_2, mux6_2) -> {local_interconnect, X, Y, 0, 25};
theory(X, Y, data_a, mux3_2, mux6_3) -> {local_line, X, Y, 0, 6};
theory(X, Y, data_a, mux3_2, mux6_4) -> {local_interconnect, X, Y, 0, 15};
theory(X, Y, data_a, mux3_2, mux6_5) -> {local_line, X, Y, 0, 8};
theory(X, Y, data_b, mux3_0, mux6_0) -> {local_interconnect, X, Y, 0, 17};
theory(X, Y, data_b, mux3_0, mux6_1) -> {local_line, X, Y, 0, 7};
theory(X, Y, data_b, mux3_0, mux6_2) -> {local_interconnect, X, Y, 0, 10};
theory(X, Y, data_b, mux3_0, mux6_3) -> {local_interconnect, X, Y, 0, 16};
theory(X, Y, data_b, mux3_0, mux6_4) -> {local_interconnect, X, Y, 0, 24};
theory(X, Y, data_b, mux3_0, mux6_5) -> {local_line, X, Y, 0, 9};
theory(X, Y, data_b, mux3_1, mux6_0) -> {local_interconnect, X, Y, 0, 7};
theory(X, Y, data_b, mux3_1, mux6_1) -> {local_line, X, Y, 0, 0};
theory(X, Y, data_b, mux3_1, mux6_2) -> {local_interconnect, X, Y, 0, 5};
theory(X, Y, data_b, mux3_1, mux6_3) -> {local_interconnect, X, Y, 0, 13};
theory(X, Y, data_b, mux3_1, mux6_4) -> {local_interconnect, X, Y, 0, 23};
theory(X, Y, data_b, mux3_1, mux6_5) -> {local_line, X, Y, 0, 2};
theory(X, Y, data_b, mux3_2, mux6_0) -> {local_interconnect, X, Y, 0, 2};
theory(X, Y, data_b, mux3_2, mux6_1) -> {local_interconnect, X, Y, 0, 21};
theory(X, Y, data_b, mux3_2, mux6_2) -> {local_interconnect, X, Y, 0, 4};
theory(X, Y, data_b, mux3_2, mux6_3) -> {local_interconnect, X, Y, 0, 12};
theory(X, Y, data_b, mux3_2, mux6_4) -> {local_interconnect, X, Y, 0, 20};
theory(X, Y, data_b, mux3_2, mux6_5) -> {local_line, X, Y, 0, 1};
theory(X, Y, data_c, mux3_0, mux6_0) -> {local_interconnect, X, Y, 0, 0};
theory(X, Y, data_c, mux3_0, mux6_1) -> {local_interconnect, X, Y, 0, 9};
theory(X, Y, data_c, mux3_0, mux6_2) -> {local_interconnect, X, Y, 0, 18};
theory(X, Y, data_c, mux3_0, mux6_3) -> {local_line, X, Y, 0, 4};
theory(X, Y, data_c, mux3_0, mux6_4) -> {local_interconnect, X, Y, 0, 2};
theory(X, Y, data_c, mux3_0, mux6_5) -> {local_interconnect, X, Y, 0, 21};
theory(X, Y, data_c, mux3_1, mux6_0) -> {local_interconnect, X, Y, 0, 3};
theory(X, Y, data_c, mux3_1, mux6_1) -> {local_interconnect, X, Y, 0, 11};
theory(X, Y, data_c, mux3_1, mux6_2) -> {local_interconnect, X, Y, 0, 22};
theory(X, Y, data_c, mux3_1, mux6_3) -> {local_line, X, Y, 0, 5};
theory(X, Y, data_c, mux3_1, mux6_4) -> {local_interconnect, X, Y, 0, 7};
theory(X, Y, data_c, mux3_1, mux6_5) -> {local_line, X, Y, 0, 0};
theory(X, Y, data_c, mux3_2, mux6_0) -> {local_interconnect, X, Y, 0, 8};
theory(X, Y, data_c, mux3_2, mux6_1) -> {local_interconnect, X, Y, 0, 14};
theory(X, Y, data_c, mux3_2, mux6_2) -> {local_interconnect, X, Y, 0, 25};
theory(X, Y, data_c, mux3_2, mux6_3) -> {local_line, X, Y, 0, 6};
theory(X, Y, data_c, mux3_2, mux6_4) -> {local_interconnect, X, Y, 0, 17};
theory(X, Y, data_c, mux3_2, mux6_5) -> {local_line, X, Y, 0, 7};
theory(X, Y, data_d, mux3_0, mux6_0) -> {local_interconnect, X, Y, 0, 10};
theory(X, Y, data_d, mux3_0, mux6_1) -> {local_interconnect, X, Y, 0, 16};
theory(X, Y, data_d, mux3_0, mux6_2) -> {local_interconnect, X, Y, 0, 24};
theory(X, Y, data_d, mux3_0, mux6_3) -> {local_line, X, Y, 0, 9};
theory(X, Y, data_d, mux3_0, mux6_4) -> {local_interconnect, X, Y, 0, 15};
theory(X, Y, data_d, mux3_0, mux6_5) -> {local_line, X, Y, 0, 8};
theory(X, Y, data_d, mux3_1, mux6_0) -> {local_interconnect, X, Y, 0, 5};
theory(X, Y, data_d, mux3_1, mux6_1) -> {local_interconnect, X, Y, 0, 13};
theory(X, Y, data_d, mux3_1, mux6_2) -> {local_interconnect, X, Y, 0, 23};
theory(X, Y, data_d, mux3_1, mux6_3) -> {local_line, X, Y, 0, 2};
theory(X, Y, data_d, mux3_1, mux6_4) -> {local_interconnect, X, Y, 0, 6};
theory(X, Y, data_d, mux3_1, mux6_5) -> {local_line, X, Y, 0, 3};
theory(X, Y, data_d, mux3_2, mux6_0) -> {local_interconnect, X, Y, 0, 4};
theory(X, Y, data_d, mux3_2, mux6_1) -> {local_interconnect, X, Y, 0, 12};
theory(X, Y, data_d, mux3_2, mux6_2) -> {local_interconnect, X, Y, 0, 20};
theory(X, Y, data_d, mux3_2, mux6_3) -> {local_line, X, Y, 0, 1};
theory(X, Y, data_d, mux3_2, mux6_4) -> {local_interconnect, X, Y, 0, 1};
theory(X, Y, data_d, mux3_2, mux6_5) -> {local_interconnect, X, Y, 0, 19};
theory(X, Y, Port, Mux3, Mux6) ->
    {X, Y, Port, Mux3, Mux6}.

