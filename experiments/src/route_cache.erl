-module(route_cache).

% Cache a list of experiments (cache dirs) that contain a route segment.
%
% For example, route segment:
%   {le_buffer, 3, 10, 0, 18} -> {r4, 0, 10, 0, 53}
% would be cached as:
%   {route_cache,
%    epm240,
%    #{0 => "cache/D3/--V_pLoF0A47cHkSUqXNWubGTYrRCaI264C8u6HWI"},
%    #{{r4,0,10} => #{53 => #{{le_buffer,3,10,0,18} => 0}}}
%   }

-export([run/0]).

-export([open/1]).
-export([blocks/2]).
-export([index_max/2]).
-export([froms/2]).
-export([froms/3]).
-export([experiments/3]).
-export([experiments/4]).

-type density() :: density:density().

-type cache() :: {route_cache, density(), dirs(), blocks()}.

-type dirs() :: #{dir_index() => file:name()}.
-type dir_index() :: non_neg_integer().

-type blocks() :: #{block() => indexes()}.
-type block() :: {atom(), max_ii:x(), max_ii:y()}.

-type indexes() :: #{index() => froms()}.
-type index() :: non_neg_integer().

-type froms() :: #{from() => [dir_index()]}.
-type from() :: {atom(), max_ii:x(), max_ii:y(), non_neg_integer(), non_neg_integer()}.

-type experiment() :: {experiment:title(), experiment:fuses(), rcf_file:rcf()}.

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    Collectors = maps:from_list([
        {Density, {ready, collect_start(Density)}}
        ||
        Density <- density:list()
    ]),
    iterate(experiment_cache:iterate(), Collectors).

%%--------------------------------------------------------------------

iterate(false, Collectors) ->
    iterate_finish(Collectors);
iterate({Device, Experiment, Iterator}, Collectors0) ->
    Density = device:density(Device),
    {Collector, Collectors} = iterate_collector(Density, Collectors0),
    Collector ! {collect, self(), Experiment},
    iterate(experiment_cache:iterate(Iterator), Collectors).

%%--------------------------------------------------------------------

iterate_collector(Density, Collectors) ->
    case Collectors of
        #{Density := {ready, Pid}} ->
            {Pid, Collectors#{Density => {collecting, Pid}}};

        #{Density := {collecting, _}} ->
            iterate_collector_wait(Density, Collectors)
    end.

%%--------------------------------------------------------------------

iterate_collector_wait(WaitFor, Collectors) ->
    receive
        {collected, Density} when WaitFor =:= Density ->
            #{Density := {collecting, Pid}} = Collectors,
            {Pid, Collectors};

        {collected, Density} ->
            #{Density := {collecting, Pid}} = Collectors,
            iterate_collector_wait(WaitFor, Collectors#{
                Density => {ready, Pid}
            })
    end.

%%--------------------------------------------------------------------

iterate_finish(Collectors0) ->
    Collecting = maps:fold(fun
            (_, {ready, _}, Count) -> Count;
            (_, {collecting, _}, Count) -> Count + 1
    end, 0, Collectors0),
    Collectors = iterate_finish_wait(Collecting, Collectors0),
    maps:foreach(fun iterate_finish_collector/2, Collectors).

%%--------------------------------------------------------------------

iterate_finish_wait(0, Collectors) ->
    Collectors;
iterate_finish_wait(Collecting, Collectors) when Collecting > 0 ->
    receive
        {collected, Density} ->
            #{Density := {collecting, Pid}} = Collectors,
            iterate_finish_wait(Collecting - 1, Collectors#{
                Density => {ready, Pid}
            })
    end.

%%--------------------------------------------------------------------

iterate_finish_collector(_Density, {ready, Pid}) ->
    Pid ! save.

%%====================================================================
%% worker
%%====================================================================

collect_start(Density) ->
    erlang:spawn_link(fun () -> collect_init(Density) end).

%%--------------------------------------------------------------------

collect_init(Density) ->
    collect_loop(Density, 0, #{}, #{}).

%%--------------------------------------------------------------------

collect_loop(Density, DirCount, Dirs, Blocks) ->
    receive
        {collect, From, Experiment} ->
            collect(Experiment, From, Density, DirCount, Dirs, Blocks);

        save ->
            io:format(" ==> ~s SAVE~n", [Density]),
            File = density_file(Density),
            Binary = erlang:term_to_binary({Dirs, Blocks}, [compressed]),
            ok = file:write_file(File, Binary)
    end.

%%--------------------------------------------------------------------

collect(Experiment, From, Density, DirCount0, Dirs0, Blocks0) ->
    {cached, Dir} = Experiment,
    DirCount = DirCount0 + 1,
    Dirs = Dirs0#{DirCount => Dir},
    {ok, #{signals := Signals}} = experiment:rcf(Experiment),
    Blocks = collect_signals(Signals, DirCount, Blocks0),
    From ! {collected, Density},
    collect_loop(Density, DirCount, Dirs, Blocks).

%%--------------------------------------------------------------------

collect_signals(Signals, Dir, Blocks0) ->
    maps:fold(fun (_, Signal, Blocks) ->
        collect_signal(Signal, Dir, Blocks)
    end, Blocks0, Signals).

%%--------------------------------------------------------------------

collect_signal(#{dests := Dests}, Dir, Blocks0) ->
    lists:foldl(fun (Dest, Blocks) ->
        collect_dest(Dest, Dir, Blocks)
    end, Blocks0, Dests).

%%--------------------------------------------------------------------

collect_dest(#{route := [{io_bypass_out, _, _, _, _} | Route]}, Dir, Blocks) ->
    collect_route(Route, Dir, Blocks);
collect_dest(#{route := [{io_data_out, _, _, _, _} | Route]}, Dir, Blocks) ->
    collect_route(Route, Dir, Blocks);
collect_dest(#{route := [{io_oe, _, _, _, _} | Route]}, Dir, Blocks) ->
    collect_route(Route, Dir, Blocks);
collect_dest(#{route := Route}, Dir, Blocks) ->
    collect_route(Route, Dir, Blocks).

%%--------------------------------------------------------------------

collect_route([], _, Blocks) ->
    Blocks;
collect_route([_], _, Blocks) ->
    Blocks;
collect_route([Thru | Route = [From | _]], Dir, Blocks0) ->
    {Type, X, Y, 0, Index} = Thru,
    Block = {Type, X, Y},
    Blocks = collect_block(Block, Index, From, Dir, Blocks0),
    collect_route(Route, Dir, Blocks).

%%--------------------------------------------------------------------

collect_block(Block, Index, From, Dir, Blocks) ->
    case Blocks of
        #{Block := Indexes = #{Index := Froms = #{From := Dirs}}} ->
            Blocks#{Block => Indexes#{Index => Froms#{From => [Dir | Dirs]}}};

        #{Block := Indexes = #{Index := Froms}} ->
            Blocks#{Block => Indexes#{Index => Froms#{From => [Dir]}}};

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => #{From => [Dir]}}};

        _ ->
            Blocks#{Block => #{Index => #{From => [Dir]}}}
    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, cache()}.

open(Density) ->
    File = density_file(Density),
    {ok, Binary} = file:read_file(File),
    {Dirs, Blocks} = erlang:binary_to_term(Binary), %, [safe]),
    {ok, {route_cache, Density, Dirs, Blocks}}.

%%====================================================================
%% blocks
%%====================================================================

-spec blocks(atom(), cache()) -> [block()].

blocks(Type, {route_cache, _, _, Blocks}) ->
    lists:sort(
        lists:filter(fun ({T, _, _}) -> T =:= Type end,
            maps:keys(Blocks)
        )
    ).

%%====================================================================
%% index_max
%%====================================================================

-spec index_max(atom(), cache()) -> [block()].

index_max(Type, {route_cache, _, _, Blocks}) ->
    maps:fold(fun (Block, Indexes, Max) ->
        index_max(Type, Block, Indexes, Max)
    end, 0, Blocks).

%%--------------------------------------------------------------------

index_max(Type, {Type, _, _}, Indexes, Max) ->
    max(Max, lists:max(maps:keys(Indexes)));
index_max(_, _, _, Max) ->
    Max.

%%====================================================================
%% froms
%%====================================================================

-spec froms(from(), cache())
    -> {ok, [from()]} | false.

froms({Type, X, Y, 0, Index}, Cache = {route_cache, _, _, _}) ->
    froms({Type, X, Y}, Index, Cache).

%%--------------------------------------------------------------------

-spec froms(block(), index(), cache())
    -> {ok, [from()]} | false.

froms(Block, Index, {route_cache, _, _, Blocks}) ->
    case Blocks of
        #{Block := #{Index := Froms}} ->
            {ok, lists:sort(maps:keys(Froms))};

        _ ->
            false
    end.

%%====================================================================
%% experiments
%%====================================================================

-spec experiments(from(), from(), cache())
    -> {ok, [experiment()]} | false.

experiments({Type, X, Y, 0, Index}, From, Cache = {route_cache, _, _, _}) ->
    experiments({Type, X, Y}, Index, From, Cache).

%%--------------------------------------------------------------------

-spec experiments(block(), index(), from(), cache())
    -> {ok, [experiment()]} | false.

experiments(Block, Index, From, {route_cache, _, Dirs, Blocks}) ->
    case Blocks of
        #{Block := #{Index := #{From := DirIndexes}}} ->
            {ok, [
                experiment(DirIndex, Dirs)
                ||
                DirIndex <- DirIndexes
            ]};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

experiment(Index, Dirs) ->
    #{Index := Dir} = Dirs,
    Result = {cached, Dir},
    {ok, Fuses} = experiment:fuses(Result),
    {ok, RCF} = experiment:rcf(Result),
    {Index, Fuses, RCF}.

%%====================================================================
%% utility
%%====================================================================

density_file(Density) ->
    Name = lists:flatten(io_lib:format("~s.routes", [Density])),
    filename:join("cache", Name).

