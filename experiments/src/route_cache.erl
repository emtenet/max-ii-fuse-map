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
-export([block_types/1]).
-export([blocks/2]).
-export([index_max/2]).
-export([froms/2]).
-export([froms/3]).
-export([experiments/3]).
-export([experiments/4]).
-export([cached/2]).

-export([fold_blocks/4]).
-export([fold_indexes/3]).
-export([fold_froms/3]).
-export([fold_cached/3]).
-export([fold_cached/4]).

-type density() :: density:density().

-type cache() :: {route_cache, density(), dirs(), blocks()}.

-type fold_indexes() :: {fold_indexes, indexes(), dirs()}.
-type fold_froms() :: {fold_froms, froms(), dirs()}.
-type fold_cached() :: {fold_cached, [dir_index()], dirs()}.

-type dirs() :: #{dir_index() => file:name()}.
-type dir_index() :: non_neg_integer().

-type blocks() :: #{block() => indexes()}.
-type block() :: {atom(), max_ii:x(), max_ii:y()}.

-type indexes() :: #{index() => froms()}.
-type index() :: non_neg_integer().

-type froms() :: #{from() => [dir_index()]}.
-type from() :: {atom(), max_ii:x(), max_ii:y(), non_neg_integer(), non_neg_integer()}.

-type experiment() :: {experiment:title(), experiment:fuses(), rcf_file:rcf()}.

-define(INCREMENTAL, true).

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

-ifdef(INCREMENTAL).

collect_init(Density) ->
    {ok, {route_cache, Density, Dirs, Blocks}} = open(Density),
    {Seen, DirCount} = maps:fold(fun collect_init/3, {#{}, 0}, Dirs),
    io:format(" ==> ~p INCREMENTAL~n", [Density]),
    collect_loop(Density, Seen, DirCount, Dirs, Blocks).

%%--------------------------------------------------------------------

collect_init(DirIndex, Dir, {Seen, DirCount}) ->
    {Seen#{Dir => seen}, max(DirIndex, DirCount)}.

-else.

collect_init(Density) ->
    io:format(" ==> ~p EMPTY~n", [Density]),
    Seen = #{},
    DirCount = 0,
    Dirs = #{},
    Blocks = #{},
    collect_loop(Density, Seen, DirCount, Dirs, Blocks).

-endif.

%%--------------------------------------------------------------------

collect_loop(Density, Seen, DirCount, Dirs, Blocks) ->
    receive
        {collect, From, Experiment} ->
            collect(Experiment, From, Density, Seen, DirCount, Dirs, Blocks);

        save ->
            save(Density, Dirs, Blocks)
    end.

%%--------------------------------------------------------------------

collect(Experiment, From, Density, Seen, DirCount0, Dirs0, Blocks0) ->
    {cached, Dir} = Experiment,
    case Seen of
        #{Dir := seen} ->
            From ! {collected, Density},
            collect_loop(Density, Seen, DirCount0, Dirs0, Blocks0);

        _ ->
            DirCount = DirCount0 + 1,
            Dirs = Dirs0#{DirCount => Dir},
            {ok, #{signals := Signals}} = experiment:rcf(Experiment),
            Blocks = collect_signals(Signals, DirCount, Blocks0),
            From ! {collected, Density},
            collect_loop(Density, Seen, DirCount, Dirs, Blocks)
    end.

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

collect_dest(#{route := Route}, Dir, Blocks) ->
    collect_route(Route, Dir, Blocks).

%%--------------------------------------------------------------------

collect_route([], _, Blocks) ->
    Blocks;
collect_route([_], _, Blocks) ->
    Blocks;
collect_route([Thru | Route = [From | _]], Dir, Blocks0) ->
    {Block, Index} = collect_thru(Thru),
    Blocks = collect_block(Block, Index, From, Dir, Blocks0),
    collect_route(Route, Dir, Blocks).

%%--------------------------------------------------------------------

collect_thru({Type = io_bypass_out, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type = io_data_out, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type = io_oe, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type, X, Y, 0, Index}) ->
    {{Type, X, Y}, Index}.

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
%% save
%%====================================================================

save(Density, Dirs, Blocks) ->
    io:format(" ==> ~s SAVE~n", [Density]),
    File = density_file(Density),
    Binary = erlang:term_to_binary({Dirs, Blocks}, [compressed]),
    ok = file:write_file(File, Binary).

%%====================================================================
%% block_types
%%====================================================================

-spec block_types(cache()) -> [atom()].

block_types({route_cache, _, _, Blocks}) ->
    maps:keys(maps:fold(fun block_types/3, #{}, Blocks)).

%%--------------------------------------------------------------------

block_types({Type, _X, _Y}, _Indexes, Types) ->
    case Types of
        #{Type := _} ->
            Types;

        _ ->
            Types#{Type => true}
    end.

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

-spec index_max(atom(), cache()) -> index().

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

experiment(DirIndex, Dirs) ->
    Result = cached(DirIndex, Dirs),
    {ok, Fuses} = experiment:fuses(Result),
    {ok, RCF} = experiment:rcf(Result),
    {DirIndex, Fuses, RCF}.

%%====================================================================
%% cached
%%====================================================================

-spec cached(dir_index(), cache() | dirs()) -> experiment:result().

cached(DirIndex, {route_cache, _, Dirs, _}) ->
    #{DirIndex := Dir} = Dirs,
    {cached, <<"cache/", Dir/binary>>};
cached(DirIndex, Dirs) ->
    #{DirIndex := Dir} = Dirs,
    {cached, <<"cache/", Dir/binary>>}.

%%====================================================================
%% fold_blocks
%%====================================================================

-spec fold_blocks(Type, Fold, Acc, cache()) -> Acc when
    Type :: atom(),
    Fold :: fun((block(), fold_indexes(), Acc) -> Acc),
    Acc :: term().

fold_blocks(Type, Fold, Init, Cache) ->
    Blocks = route_cache:blocks(Type, Cache),
    lists:foldl(
        fun (Block, Acc) ->
            fold_block(Fold, Block, Cache, Acc)
        end,
        Init,
        Blocks
    ).

%%--------------------------------------------------------------------

fold_block(Fold, Block, {route_cache, _, Dirs, Blocks}, Acc) ->
    #{Block := Indexes} = Blocks,
    Fold(Block, {fold_indexes, Indexes, Dirs}, Acc).

%%====================================================================
%% fold_indexes
%%====================================================================

-spec fold_indexes(Fold, Acc, fold_indexes()) -> Acc when
    Fold :: fun((index(), fold_froms(), Acc) -> Acc),
    Acc :: term().

fold_indexes(Fold, Init, Cache) ->
    Indexes = fold_indexes(Cache),
    lists:foldl(
        fun (Index, Acc) ->
            fold_index(Fold, Index, Cache, Acc)
        end,
        Init,
        Indexes
    ).

%%--------------------------------------------------------------------

fold_indexes({fold_indexes, Indexes, _}) ->
    lists:sort(maps:keys(Indexes)).

%%--------------------------------------------------------------------

fold_index(Fold, Index, {fold_indexes, Indexes, Dirs}, Acc) ->
    #{Index := Froms} = Indexes,
    Fold(Index, {fold_froms, Froms, Dirs}, Acc).

%%====================================================================
%% fold_froms
%%====================================================================

-spec fold_froms(Fold, Acc, fold_froms()) -> Acc when
    Fold :: fun((from(), fold_cached(), Acc) -> Acc),
    Acc :: term().

fold_froms(Fold, Init, {fold_froms, Froms, Dirs}) ->
    maps:fold(
        fun (From, DirIndexes, Acc) ->
            Fold(From, {fold_cached, DirIndexes, Dirs}, Acc)
        end,
        Init,
        Froms
    ).

%%====================================================================
%% fold_cached
%%====================================================================

-spec fold_cached(Fold, Acc, fold_cached()) -> Acc when
    Fold :: fun((experiment:result(), Acc) -> Acc),
    Acc :: term().

-spec fold_cached(Fold, Acc, fold_cached(), {limit, Limit}) -> Acc when
    Fold :: fun((experiment:result(), Acc) -> Acc),
    Acc :: term(),
    Limit :: pos_integer().

fold_cached(Fold, Init, {fold_cached, DirIndexes, Dirs}) ->
    lists:foldl(
        fun (DirIndex, Acc) ->
            fold_cached_fold(Fold, DirIndex, Dirs, Acc)
        end,
        Init,
        DirIndexes
    ).

%%--------------------------------------------------------------------

fold_cached(Fold, Init, {fold_cached, DirIndexes0, Dirs}, {limit, Limit}) ->
    DirIndexes = limit(DirIndexes0, Limit),
    lists:foldl(
        fun (DirIndex, Acc) ->
            fold_cached_fold(Fold, DirIndex, Dirs, Acc)
        end,
        Init,
        DirIndexes
    ).

%%--------------------------------------------------------------------

fold_cached_fold(Fold, DirIndex, Dirs, Acc) ->
    Cached = cached(DirIndex, Dirs),
    Fold(Cached, Acc).

%%--------------------------------------------------------------------

limit(List, Limit) ->
    case length(List) of
        Total when Total < Limit ->
            List;

        Total ->
            lists:nthtail(Total - Limit, List)
    end.

%%====================================================================
%% utility
%%====================================================================

density_file(Density) ->
    Name = lists:flatten(io_lib:format("~s.routes", [Density])),
    filename:join("cache", Name).

