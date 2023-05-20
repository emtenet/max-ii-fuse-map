-module(lab_interconnect_database).

-export([run/0]).

-export([open/1]).
-export([save/2]).
-export([find_key/4]).
-export([add/5]).

-export_type([block/0]).
-export_type([blocks/0]).
-export_type([indexes/0]).
-export_type([index/0]).
-export_type([mux/0]).
-export_type([mux_key/0]).
-export_type([from/0]).

-type density() :: density:density().

-type blocks() :: #{block() => indexes()}.
-type block() :: lab:lab().

-type indexes() :: #{index() => mux()}.
-type index() :: 0..25.

-type mux() :: #{mux_key() => from()}.
-type mux_key() :: direct_link | {mux4(), mux3()}.
-type mux4() :: max_ii:mux4().
-type mux3() :: max_ii:mux3().
-type from() :: max_ii:c4() | max_ii:le_buffer() | max_ii:r4().

%%====================================================================
%% run
%%====================================================================

run() ->
    %build(epm240),
    %build(epm570),
    %build(epm1270),
    %build(epm2210),
    lists:foreach(fun build/1, density:list()),
    ok.

%%--------------------------------------------------------------------

build(Density) ->
    %Db0 = #{},
    {ok, Db0} = open(Density),
    {ok, Cache} = route_cache:open(Density),
    Db1 = route_cache:fold_blocks(
        local_interconnect,
        fun (Block, Indexes, Acc) ->
            build_block(Density, Block, Indexes, Acc)
        end,
        Db0,
        Cache
    ),
    save(Density, Db1),
    ok.

%%--------------------------------------------------------------------

build_block(Density, {local_interconnect, X, Y}, Indexes, Db0) ->
    case density:is_lab(X, Y, Density) of
        true ->
            Block = {lab, X, Y},
            io:format(" ==> ~p~n", [Block]),
            route_cache:fold_indexes(
                fun (Index, Froms, Acc) ->
                    build_index(Density, Block, Index, Froms, Acc)
                end,
                Db0,
                Indexes
            );

        false ->
            Db0
    end.

%%--------------------------------------------------------------------

build_index(Density, Block, Index, Froms, Db0) ->
    route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            build_from_check(Density, Block, Index, From, Cached, Acc)
        end,
        Db0,
        Froms
    ).

%%--------------------------------------------------------------------

build_from_check(Density, Block, Index, From, Cached, Db) ->
    case find_key(Block, Index, From, Db) of
        {ok, _} ->
            Db;

        false ->
            build_from(From),
            build_from(Density, Block, Index, From, Cached, Db)
    end.

%%--------------------------------------------------------------------

build_from({c4, _, _, 0, _}) -> ok;
build_from({lab_clk, _, _, 0, _}) -> ok;
build_from({le_buffer, _, _, 0, _}) -> ok;
build_from({io_data_in, _, _, _, 0}) -> ok;
build_from({r4, _, _, 0, _}) -> ok.

%%--------------------------------------------------------------------

build_from(Density, Block, Index, From, Cached, Db) ->
    case build_reduce(Density, Cached) of
        false ->
            Db;

        {ok, Fuses} ->
            build_keys(Density, Block, Index, From, Fuses, Db)
    end.

%%--------------------------------------------------------------------

build_reduce(Density, Cached) ->
    route_cache:fold_cached(
        fun build_reduce_zero/2,
        Density,
        fun build_reduce_rest/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

build_reduce_zero(Experiment, Density) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:subtract(Fuses, density:minimal_fuses(Density)).

%%--------------------------------------------------------------------

build_reduce_rest(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

%%--------------------------------------------------------------------

build_keys(Density, Block, Index, From, Fuses, Db) ->
    Keys = build_keys(Density, Fuses, #{}, []),
    case build_key(Keys, Block, Index) of
        {ok, Key} ->
            build_add(Block, Index, Key, From, Db);

        false ->
            build_maybe(Density, Block, Index, From, Fuses),
            Db
    end.

%%--------------------------------------------------------------------

build_keys(_, [], _, Keys) ->
    Keys;
build_keys(Density, [Fuse | Fuses], Muxes0, Keys) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {Block = {lab, _, _}, {interconnect, Index}, direct_link}} ->
            Key = {Block, Index, direct_link},
            build_keys(Density, Fuses, Muxes0, [Key | Keys]);

        {ok, {Block = {lab, _, _}, {interconnect, Index}, from3, Mux3}} ->
            Mux = {Block, Index},
            case maps:take(Mux, Muxes0) of
                {{from4, Mux4}, Muxes} ->
                    Key = {Block, Index, {Mux4, Mux3}},
                    build_keys(Density, Fuses, Muxes, [Key | Keys]);

                error ->
                    From = {from3, Mux3},
                    build_keys(Density, Fuses, Muxes0#{Mux => From}, Keys)
            end;

        {ok, {Block = {lab, _, _}, {interconnect, Index}, from4, Mux4}} ->
            Mux = {Block, Index},
            case maps:take(Mux, Muxes0) of
                {{from3, Mux3}, Muxes} ->
                    Key = {Block, Index, {Mux4, Mux3}},
                    build_keys(Density, Fuses, Muxes, [Key | Keys]);

                error ->
                    From = {from4, Mux4},
                    build_keys(Density, Fuses, Muxes0#{Mux => From}, Keys)
            end;

        _ ->
            build_keys(Density, Fuses, Muxes0, Keys)
    end.

%%--------------------------------------------------------------------

build_key([], _, _) ->
    false;
build_key([{Block, Index, Key} | _], Block, Index) ->
    {ok, Key};
build_key([_ | Keys], Block, Index) ->
    build_key(Keys, Block, Index).

%%--------------------------------------------------------------------

build_add(Block, Index, Key, From, Db) ->
    io:format("~11w ~2b: ~11w <- ~w~n", [
        Block, Index, Key, From
    ]),
    add(Block, Index, Key, From, Db).

%%--------------------------------------------------------------------

%build_maybe(_, _, _, {lab_clk, _, _, _, _}, _) -> ok;
%build_maybe(epm240, {lab, 2, 1}, 12, {lab_clk, 2, 0, 0, 2}, _) -> ok;
%build_maybe(epm240, {lab, 2, 3}, 12, {lab_clk, 2, 0, 0, 0}, _) -> ok;
%build_maybe(epm240, {lab, 2, 3}, 12, {lab_clk, 2, 0, 0, 1}, _) -> ok;
%build_maybe(epm240, {lab, 2, 3}, 12, {lab_clk, 2, 0, 0, 2}, _) -> ok;
%build_maybe(epm240, {lab, 3, 1}, 12, {lab_clk, 3, 0, 0, 2}, _) -> ok;
%build_maybe(epm240, {lab, 7, 2}, 12, {lab_clk, 7, 0, 0, 0}, _) -> ok;
%build_maybe(epm240, {lab, 7, 2}, 12, {lab_clk, 7, 0, 0, 1}, _) -> ok;
build_maybe(Density, Block = {lab, X, Y}, Index, From, Fuses) ->
%build_maybe(_, Block, Index, From, _) ->
    io:format("~11w ~2b: ??????????? <- ~w~n", [
        Block, Index, From
    ]),
    Locations = lists:filtermap(fun (Fuse) ->
        build_location(Density, X, Y, Fuse)
    end, Fuses),
    %case Locations of
    %    [A] ->
    %        io:format("  ~p~n", [A]);

    %    [A, B] ->
    %        io:format("  ~p ~p~n", [A, B]);

    %    [_, _, _ | _] ->
    %        ok
    %end,
    %lists:foreach(fun (Fuse) ->
    %    build_maybe(Density, X, Y, Fuse)
    %end, Fuses),
    throw(Locations),
    ok.

%%--------------------------------------------------------------------

build_location(Density, X, Y, Fuse) ->
    case fuse_map:to_location(Fuse, Density) of
        {XX, YY, N, I, cell, S} when XX =:= X orelse YY =:= Y ->
            {true, {S, N, I}};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

%build_maybe(Density, X, Y, Fuse) ->
%    case fuse_map:to_location(Fuse, Density) of
%        %{ok, {{c4, _, _}, _, _}} ->
%        %    ok;
%
%        %{ok, {{c4, _, _}, _, _, _}} ->
%        %    ok;
%
%        %{ok, {{iob, _, _}, _, _}} ->
%        %    ok;
%
%        %{ok, {{iob, _, _}, _, _, _}} ->
%        %    ok;
%
%        %{ok, {{ioc, _, _, _}, _}} ->
%        %    ok;
%
%        %{ok, {{ioc, _, _, _}, _, _}} ->
%        %    ok;
%
%        %{ok, {{ioc, _, _, _}, _, _, _}} ->
%        %    ok;
%
%        %{ok, {{lc, _, _, _}, _}} ->
%        %    ok;
%
%        %{ok, {{lc, _, _, _}, _, _}} ->
%        %    ok;
%
%        %{ok, {{lc, _, _, _}, _, _, _}} ->
%        %    ok;
%
%        %{ok, {{r4, _, _}, _, _}} ->
%        %    ok;
%
%        %{ok, {{r4, _, _}, _, _, _}} ->
%        %    ok;
%
%        %{ok, {{lab, XX, YY}, _}} when XX =/= X orelse YY =/= Y ->
%        %    ok;
%
%        %{ok, {{lab, XX, YY}, _, _}} when XX =/= X orelse YY =/= Y ->
%        %    ok;
%
%        %{ok, {{lab, XX, YY}, _, _, _}} when XX =/= X orelse YY =/= Y ->
%            ok;
%
%        {_, _, _, _, side, _} ->
%            ok;
%
%        {XX, YY, _, _, cell, _} when XX =/= X orelse YY =/= Y ->
%            ok;
%
%        %{error, {_, _, N, _, cell, _}} when N =/= line ->
%        %    ok;
%
%        %{ok, Name} ->
%        %    io:format("    ~p~n", [Name]);
%
%        Location ->
%            io:format("    ~p~n", [Location])
%    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, blocks()}.

open(Density) ->
    File = database_file(Density),
    case file:consult(File) of
        {ok, []} ->
            {ok, #{}};

        {ok, [{Block = {lab, _, _}, Index} | Lines]} ->
            Blocks = open(Lines, #{}, Block, Index, #{}),
            {ok, Blocks};

        {error, enoent} ->
            {ok, #{}}
    end.

%%--------------------------------------------------------------------

open([], Blocks, Block, Index, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => Mux}};

        _ ->
            Blocks#{Block => #{Index => Mux}}
    end;
open([{NextBlock = {lab, _, _}, NextIndex} | Lines],
     Blocks, Block, Index, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            open(
                Lines, Blocks#{Block => Indexes#{Index => Mux}},
                NextBlock, NextIndex, #{}
            );

        _ ->
            open(
                Lines, Blocks#{Block => #{Index => Mux}},
                NextBlock, NextIndex, #{}
            )
    end;
open([{Key, From} | Lines], Blocks, Block, Index, Mux) ->
    open(Lines, Blocks, Block, Index, Mux#{Key => From}).

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

save_index(Block, Index, Indexes) ->
    #{Index := Mux} = Indexes,
    [
        iolist_to_binary(io_lib:format(
            "{~p,~p}.~n",
            [Block, Index]
        ))
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

-spec add(block(), index(), mux_key(), from(), blocks())
    -> blocks().

add(Block, Index, Key, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := #{Key := From}}} ->
            Blocks;

        #{Block := #{Index := #{Key := Existing}}} ->
            throw({
                lab_interconnect, Block, Index, Key,
                add, From, existing, Existing
            });

        #{Block := Indexes = #{Index := Mux}} ->
            Blocks#{Block => Indexes#{Index => Mux#{Key => From}}};

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => #{Key => From}}};

        _ ->
            Blocks#{Block => #{Index => #{Key => From}}}
    end.

%%====================================================================
%% find_key
%%====================================================================

-spec find_key(block(), index(), from(), blocks())
    -> {ok, mux_key()} | false.

find_key(Block, Index, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := Keys}} ->
            find_key(From, maps:next(maps:iterator(Keys)));

        _ ->
            false
    end.

%%--------------------------------------------------------------------

find_key(_, none) ->
    false;
find_key(From, {Key, From, _}) ->
    {ok, Key};
find_key(From, {_, _, Iterator}) ->
    find_key(From, maps:next(Iterator)).

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.lab-interconnect", [Density])).

