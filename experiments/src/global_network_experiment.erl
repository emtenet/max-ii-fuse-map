-module(global_network_experiment).

-export([run/0]).

% This experiment checks the global network fuses for all cached
% experiments. The fuses checked are:
%
%  * {{global, #}, row, off}
%  * {{global, #}, interconnect}
%  * {{global, #}, {column #}, off}
%
% This experiment needs to check for "absense" of the two "off" fuses.

-include("max_ii.hrl").

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    % check that all columns are covered by the cache
    lists:foreach(fun columns/1, density:list()),
    % check global fuses
    Fuses = maps:from_list(lists:map(fun fuses/1, density:list())),
    iterate(experiment_cache:iterate(), Fuses).

%%--------------------------------------------------------------------

iterate(false, _) ->
    ok;
iterate({Device, Experiment, Iterator}, Fuses) ->
    Density = device:density(Device),
    experiment(Density, Experiment, Fuses),
    iterate(experiment_cache:iterate(Iterator), Fuses).

%%--------------------------------------------------------------------

experiment(Density, Experiment, Fuses0) ->
    #{Density := Fuses} = Fuses0,
    {ok, POF} = experiment:pof(Experiment),
    {ok, RCF} = experiment:rcf(Experiment),
    Network = network_rcf(RCF),
    Expect = expect(Network, Fuses),
    case check(Expect, POF) of
        true ->
            ok;

        false ->
            discrepancy(Density, Experiment, POF, Expect)
    end.

%%====================================================================
%% columns
%%====================================================================

columns(Density) ->
    io:format(" ==> ~p~n", [Density]),
    {ok, Cache} = route_cache:open(Density),
    Metric = density:metric(Density),
    column(Cache, Metric#metric.left_io,
                  Metric#metric.indent_bottom_io + 1),
    column(Cache, Metric#metric.left_lab,
                  Metric#metric.indent_right_lab,
                  Metric#metric.indent_bottom_io),
    column(Cache, Metric#metric.indent_left_io,
                  Metric#metric.indent_bottom_io - 1),
    column(Cache, Metric#metric.indent_left_lab,
                  Metric#metric.right_lab,
                  Metric#metric.bottom_io),
    column(Cache, Metric#metric.right_io,
                  Metric#metric.bottom_io + 1),
    ok.

%%--------------------------------------------------------------------

column(Cache, X, Stop, Y) when X =< Stop ->
    column(Cache, X, Y),
    column(Cache, X + 1, Stop, Y);
column(_, _, _, _) ->
    ok.

%%--------------------------------------------------------------------

column(_, 1, -1) ->
    ok;
column(Cache, X, Y) ->
    Block = {lab_clk, X, Y},
    %io:format("  ~p~n", [Block]),
    {ok, [_ | _]} = route_cache:froms(Block, 0, Cache),
    {ok, [_ | _]} = route_cache:froms(Block, 1, Cache),
    {ok, [_ | _]} = route_cache:froms(Block, 2, Cache),
    {ok, [_ | _]} = route_cache:froms(Block, 3, Cache),
    ok.

%%====================================================================
%% fuses
%%====================================================================

fuses(Density) ->
    {Density, lists:sort(lists:flatten([
         fuses_global(Density, G)
         ||
         G <- [0, 1, 2, 3]
    ]))}.

%%--------------------------------------------------------------------

fuses_global(Density, G) ->
    {ok, Row} = fuse_map:from_name({{global, G}, row, off}, Density),
    {ok, Int} = fuse_map:from_name({{global, G}, interconnect}, Density),
    L = density:left_io(4, Density),
    R = density:right_io(Density),
    [
        {{G, row}, Row},
        {{G, interconnect}, Int},
        fuses_column(Density, G, L),
        fuses_column(Density, G, R)
        |
        [
            fuses_column(Density, G, X)
            ||
            X <- density:columns(Density)
        ]
    ].

%%--------------------------------------------------------------------

fuses_column(Density, G, X) ->
    {ok, Column} = fuse_map:from_name({{global, G}, {column, X}, off}, Density),
    {{G, {column, X}}, Column}.

%%====================================================================
%% network
%%====================================================================

network_rcf(#{signals := Signals}) ->
    maps:fold(fun (_, Signal, Network) ->
        network_signal(Signal, Network)
    end, #{}, Signals).

%%--------------------------------------------------------------------

network_signal(#{dests := Dests}, Network0) ->
    lists:foldl(fun (Dest, Network) ->
        network_dest(Dest, Network)
    end, Network0, Dests).

%%--------------------------------------------------------------------

network_dest(#{route := Route}, Network0) ->
    network_route(Route, Network0).

%%--------------------------------------------------------------------

network_route([], Network) ->
    Network;
network_route([_], Network) ->
    Network;
network_route([{global_clk_h,_,_,_,G}, {clk_buffer,_,_,_,_}], Network) ->
    Network#{{G, interconnect} => false, {G, row} => true};
network_route([{global_clk_h,_,_,_,G}, {global_clk_mux,_,_,_,G} | _], Network) ->
    Network#{{G, interconnect} => true, {G, row} => true};
network_route(Route = [{global_clk_h,_,_,_,_} | _], Network) ->
    throw(Route),
    network_route(Route, Network);
network_route(Route = [{global_clk_mux,_,_,_,_} | _], Network) ->
    throw(Route),
    network_route(Route, Network);
network_route([{lab_clk,X,_,_,G} | Route], Network) ->
    [{global_clk_h,_,_,_,G} | _] = Route,
    network_route(Route, Network#{{G, {column, X}} => true, {G, row} => true});
network_route([_ | Route], Network) ->
    network_route(Route, Network).

%%====================================================================
%% expect
%%====================================================================

expect(Network, Fuses) ->
    lists:map(fun (Fuse) ->
        expect_fuse(Network, Fuse)
    end, Fuses).

%%--------------------------------------------------------------------

expect_fuse(Network, {Key = {_, interconnect}, Fuse}) ->
    case Network of
        #{Key := true} ->
            {Key, Fuse, true};

        _ ->
            {Key, Fuse, false}
    end;
expect_fuse(Network, {Key, Fuse}) ->
    case Network of
        #{Key := true} ->
            {Key, Fuse, false};

        _ ->
            {Key, Fuse, true}
    end.

%%====================================================================
%% check
%%====================================================================

check([], _) ->
    true;
check([{_, Fuse, Expect} | Checks], POF) ->
    case pof_file:has_fuse(Fuse, POF) of
        Expect ->
            check(Checks, POF);

        _ ->
            false
    end.

%%====================================================================
%% discrepancy
%%====================================================================

discrepancy(Density, Experiment, POF, Expect) ->
    io:format(" ==> ~p~n", [Density]),
    io:format("  ~p~n", [Experiment]),
    lists:foreach(fun (Fuse) ->
        discrepancy_fuse(Fuse, POF)
    end, Expect),
    throw(failure).

%%--------------------------------------------------------------------

discrepancy_fuse({Name, Fuse, Expect}, POF) ->
    case pof_file:has_fuse(Fuse, POF) of
        Expect ->
            io:format("  ~6b ~p OK~n", [Fuse, Name]);

        _ ->
            io:format("  ~6b ~p FAIL expect ~p~n", [Fuse, Name, Expect])
    end.

