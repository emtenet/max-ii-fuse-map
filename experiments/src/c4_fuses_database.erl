-module(c4_fuses_database).

% Collect pairs of fuses that represent a route segment.
%
% For example, a route segment:
%   {le_buffer,5,2,0,12} -> {c4,4,3,0,4}
% may be represented by the pair of fuses:
%   * {4,2,5,2,cell,26} and
%   * {5,2,5,2,cell,0}.
%
% That example would be stored in the database as:
%   #{{c4,4,3} =>
%       #{4 =>
%           #{{{4,2,5,2,cell,26},{5,2,5,2,cell,0}} =>
%               {le_buffer,5,2,0,12}
%           }
%       }
%   }

-export([run/0]).

-export([open/1]).
-export([save/2]).
-export([add/5]).

-export_type([block/0]).
-export_type([blocks/0]).
-export_type([index/0]).
-export_type([indexes/0]).
-export_type([mux/0]).
-export_type([entry/0]).
-export_type([from/0]).

-type density() :: density:density().

-type blocks() :: #{block() => indexes()}.
-type block() :: {c4, max_ii:x(), max_ii:y()}.

-type indexes() :: #{index() => mux()}.
-type index() :: non_neg_integer().

-type mux() :: #{entry() => from()}.
-type entry() :: fuse_map:location() | {fuse_map:location(), fuse_map:location()}.
-type from() ::
    {c4, max_ii:x(), max_ii:y(), 0, index()} |
    {le_buffer, max_ii:x(), max_ii:y(), 0, index()} |
    {r4, max_ii:x(), max_ii:y(), 0, index()}.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run_density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

run_density(Density) ->
    {ok, Db0} = open(Density),
    {ok, Routes} = route_cache:open(Density),
    Blocks = route_cache:blocks(c4, Routes),
    Db1 = lists:foldl(
        fun (Block, Db) ->
            run_block(Routes, Block, Db)
        end,
        Db0,
        lists:sort(Blocks)
    ),
    save(Density, Db1),
    ok.

%%--------------------------------------------------------------------

run_block(Routes, Block = {Type, _, _}, Db0) ->
    io:format(" ==> ~p:~n", [Block]),
    Max = route_cache:index_max(Type, Routes),
    lists:foldl(
        fun (Index, Db) ->
            run_index(Routes, Block, Index, Db)
        end,
        Db0,
        lists:seq(0, Max)
    ).

%%--------------------------------------------------------------------

run_index({route_cache, Density, Dirs, Blocks}, Block, Index, Db0) ->
    #{Block := Indexes} = Blocks,
    case Indexes of
        #{Index := Froms} ->
            maps:fold(fun (From, DirIndexes, Db) ->
                run_entry(Density, Dirs, Block, Index, From, DirIndexes, Db)
            end, Db0, Froms);

        _ ->
            Db0
    end.

%%--------------------------------------------------------------------

run_entry(Density, Dirs, Block, Index, From, DirIndexes, Db) ->
    case run_common(Density, DirIndexes, Dirs) of
        [] ->
            Db;

        [Entry] ->
            io:format("~2b: ~-40w -> ~p~n", [Index, Entry, From]),
            add(Block, Index, Entry, From, Db);

        [A, B] ->
            Entry = {A, B},
            io:format("~2b: ~-40w -> ~p~n", [Index, Entry, From]),
            add(Block, Index, Entry, From, Db)
    end.

%%--------------------------------------------------------------------

run_common(Density, [DirIndex | DirIndexes], Dirs) ->
    #{DirIndex := Dir} = Dirs,
    Experiment = {cached, Dir},
    {ok, Fuses0} = experiment:fuses(Experiment),
    Fuses = fuses:subtract(Fuses0, density:minimal_fuses(Density)),
    run_common(Density, DirIndexes, Dirs, Fuses).

%%--------------------------------------------------------------------

run_common(Density, [], _, [Fuse]) ->
    Location = fuse_map:to_location(Fuse, Density),
    [Location];
run_common(Density, [], _, [Fuse0, Fuse1]) ->
    Location0 = fuse_map:to_location(Fuse0, Density),
    Location1 = fuse_map:to_location(Fuse1, Density),
    case Location0 < Location1 of
        true ->
            [Location0, Location1];

        false ->
            [Location1, Location0]
    end;
run_common(_, [], _, _) ->
    [];
run_common(Density, [DirIndex | DirIndexes], Dirs, Fuses0) ->
    #{DirIndex := Dir} = Dirs,
    Experiment = {cached, Dir},
    {ok, Fuses} = experiment:fuses(Experiment),
    run_common(Density, DirIndexes, Dirs, fuses:intersect(Fuses0, Fuses)).

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, blocks()}.

open(Density) ->
    File = database_file(Density),
    case file:consult(File) of
        {ok, [{c4, X, Y, 0, I} | Lines]} ->
            Blocks = open(Lines, #{}, I, {c4, X, Y}, #{}),
            {ok, Blocks};

        {error, enoent} ->
            {ok, #{}}
    end.

%%--------------------------------------------------------------------

open([], Mux, Index, Block, Blocks) ->
    case Blocks of
        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => Mux}};

        _ ->
            Blocks#{Block => #{Index => Mux}}
    end;
open([{c4, X, Y, 0, I} | Lines], Mux, Index, Block, Blocks) ->
    case Blocks of
        #{Block := Indexes} ->
            open(Lines, #{}, I, {c4, X, Y},
                 Blocks#{Block => Indexes#{Index => Mux}});

        _ ->
            open(Lines, #{}, I, {c4, X, Y},
                 Blocks#{Block => #{Index => Mux}})
    end;
open([{Entry, From} | Lines], Mux, Index, Block, Blocks) ->
    open(Lines, Mux#{Entry => From}, Index, Block, Blocks).

%%====================================================================
%% save
%%====================================================================

-spec save(density(), blocks()) -> ok.

save(Density, Blocks) ->
    File = database_file(Density),
    ok = file:write_file(File, [
        save_block(Block, Blocks)
        ||
        Block <- lists:sort(maps:keys(Blocks))
    ]).

%%--------------------------------------------------------------------

save_block(Block, Blocks) ->
    #{Block := Indexes} = Blocks,
    [
        save_index(Block, Index, Indexes)
        ||
        Index <- lists:sort(maps:keys(Indexes))
    ].
%%--------------------------------------------------------------------

save_index({c4, X, Y}, Index, Indexes) ->
    #{Index := Mux} = Indexes,
    [
        iolist_to_binary(io_lib:format("{c4,~p,~p,0,~p}.~n", [X, Y, Index]))
        |
        [
            iolist_to_binary(io_lib:format("~w.~n", [Entry]))
            ||
            Entry <- lists:sort(maps:to_list(Mux))
        ]
    ].

%%====================================================================
%% add
%%====================================================================

-spec add(block(), index(), entry(), from(), blocks()) -> blocks().

add(Block, Index, Entry, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := #{Entry := From}}} ->
            Blocks;

        #{Block := #{Index := #{Entry := Existing}}} ->
            throw({c4_database, Block, Index, Entry, add, From, existing, Existing});

        #{Block := Indexes = #{Index := Mux}} ->
            Blocks#{Block => Indexes#{Index => Mux#{Entry => From}}};

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => #{Entry => From}}};

        _ ->
            Blocks#{Block => #{Index => #{Entry => From}}}
    end.

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.c4-fuses", [Density])).

