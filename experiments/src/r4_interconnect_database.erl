-module(r4_interconnect_database).

-export([run/0]).

-export([open/1]).
-export([save/2]).
-export([add/6]).

-export_type([block/0]).
-export_type([blocks/0]).
-export_type([mux_index/0]).
-export_type([muxes/0]).
-export_type([mux/0]).
-export_type([mux_key/0]).
-export_type([mux4/0]).
-export_type([mux3/0]).
-export_type([from/0]).

-type density() :: density:density().

-type blocks() :: #{block() => muxes()}.
-type block() :: {r4, max_ii:x(), max_ii:y()}.

-type muxes() :: #{mux_index() => {max_ii:r4(), mux()}}.
-type mux_index() :: 0..15.

-type mux() :: #{mux_key() => from()}.
-type mux_key() :: direct_link | {mux4(), mux3()}.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.
-type from() :: max_ii:c4() | max_ii:le_buffer() | max_ii:r4().

%%====================================================================
%% run
%%====================================================================

run() ->
    build(epm240),
    %build(epm570),
    %build(epm1270),
    %build(epm2210),
    %lists:foreach(fun build/1, density:list()),
    ok.

%%--------------------------------------------------------------------

build(Density) ->
    Db0 = #{},
    {ok, Routes} = route_cache:open(Density),
    Blocks = route_cache:blocks(r4, Routes),
    Db1 = lists:foldl(
        fun (Block, Db) ->
            build_block(Routes, Block, Db)
        end,
        Db0,
        lists:sort(Blocks)
    ),
    save(Density, Db1),
    ok.

%%--------------------------------------------------------------------

build_block(Routes, Block = {Type, _, _}, Db0) ->
    io:format(" ==> ~p:~n", [Block]),
    Max = route_cache:index_max(Type, Routes),
    lists:foldl(
        fun (Index, Db) ->
            build_index(Routes, Block, Index, Db)
        end,
        Db0,
        lists:seq(0, Max)
    ).

%%--------------------------------------------------------------------

build_index({route_cache, Density, Dirs, Blocks}, Block, Index, Db0) ->
    #{Block := Indexes} = Blocks,
    case Indexes of
        #{Index := Froms} ->
            {r4, X, Y} = Block,
            Interconnect = {r4, X, Y, 0, Index},
            maps:fold(fun (From, DirIndexes, Db) ->
                build_from(Density, Dirs, Interconnect, From, DirIndexes, Db)
            end, Db0, Froms);

        _ ->
            Db0
    end.

%%--------------------------------------------------------------------

build_from(_, _, _, {io_data_in, _, _, _, _}, _, Db) ->
    Db;
build_from(Density, Dirs, Interconnect, From, DirIndexes, Db) ->
    case build_dirs(Density, DirIndexes, Dirs) of
        false ->
            Db;

        {Block, Index, Key} ->
            io:format("~10w ~8w ~15w: ~w -> ~w~n", [Block, Index, Interconnect, Key, From]),
            add(Block, Index, Interconnect, Key, From, Db)
    end.

%%--------------------------------------------------------------------

build_dirs(Density, [DirIndex | DirIndexes], Dirs) ->
    #{DirIndex := Dir} = Dirs,
    Experiment = {cached, Dir},
    {ok, Fuses0} = experiment:fuses(Experiment),
    Fuses = fuses:subtract(Fuses0, density:minimal_fuses(Density)),
    build_dirs(Density, DirIndexes, Dirs, Fuses).

%%--------------------------------------------------------------------

build_dirs(Density, [], _, Fuses) ->
    build_port(Density, Fuses, #{}, []);
build_dirs(Density, [DirIndex | DirIndexes], Dirs, Fuses0) ->
    #{DirIndex := Dir} = Dirs,
    Experiment = {cached, Dir},
    {ok, Fuses} = experiment:fuses(Experiment),
    build_dirs(Density, DirIndexes, Dirs, fuses:intersect(Fuses0, Fuses)).

%%--------------------------------------------------------------------

build_port(_, [], _, []) ->
    false;
build_port(_, [], _, [Port]) ->
    Port;
build_port(_, [], _, _Ports) ->
    %throw(_Ports),
    false;
build_port(Density, [Fuse | Fuses], Muxes0, Ports) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {Block = {r4, _, _}, Index = {mux, _}, direct_link}} ->
            Port = {Block, Index, direct_link},
            build_port(Density, Fuses, Muxes0, [Port | Ports]);

        {ok, {Block = {r4, _, _}, Index = {mux, _}, from3, Mux3}} ->
            Key = {Block, Index},
            case maps:take(Key, Muxes0) of
                {{from4, Mux4}, Muxes} ->
                    Port = {Block, Index, {Mux4, Mux3}},
                    build_port(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {from3, Mux3},
                    build_port(Density, Fuses, Muxes0#{Key => Mux}, Ports)
            end;

        {ok, {Block = {r4, _, _}, Index = {mux, _}, from4, Mux4}} ->
            Key = {Block, Index},
            case maps:take(Key, Muxes0) of
                {{from3, Mux3}, Muxes} ->
                    Port = {Block, Index, {Mux4, Mux3}},
                    build_port(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {from4, Mux4},
                    build_port(Density, Fuses, Muxes0#{Key => Mux}, Ports)
            end;

        _ ->
            build_port(Density, Fuses, Muxes0, Ports)
    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> blocks().

open(Density) ->
    File = database_file(Density),
    case file:consult(File) of
        {ok, [{Block = {r4, _, _}, Index = {mux, _}, Interconnect} | Lines]} ->
            {r4, _, _, 0, _} = Interconnect,
            Blocks = open(Lines, #{}, Block, Index, Interconnect, #{}),
            {ok, Blocks};

        {error, enoent} ->
            {ok, #{}}
    end.

%%--------------------------------------------------------------------

open([], Blocks, Block, Index, Interconnect, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => {Interconnect, Mux}}};

        _ ->
            Blocks#{Block => #{Index => {Interconnect, Mux}}}
    end;
open([{NextBlock = {r4, _, _}, NextIndex = {mux, _}, NextInterconnect} | Lines],
     Blocks, Block, Index, Interconnect, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            open(
                Lines, Blocks#{Block => Indexes#{Index => {Interconnect, Mux}}},
                NextBlock, NextIndex, NextInterconnect, #{}
            );

        _ ->
            open(
                Lines, Blocks#{Block => #{Index => {Interconnect, Mux}}},
                NextBlock, NextIndex, NextInterconnect, #{}
            )
    end;
open([{Key, From} | Lines], Blocks, Block, Index, Interconnect, Mux) ->
    open(Lines, Blocks, Block, Index, Interconnect, Mux#{Key => From}).

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
    #{Block := Ports} = Blocks,
    [
        save_index(Block, Index, Ports)
        ||
        Index <- lists:sort(maps:keys(Ports))
    ].

%%--------------------------------------------------------------------

save_index(Block, Index, Ports) ->
    #{Index := {Interconnect, Mux}} = Ports,
    [
        iolist_to_binary(io_lib:format(
            "{~p,~p,~p}.~n",
            [Block, Index, Interconnect]
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

-spec add(block(), mux_index(), max_ii:r4(), mux_key(), from(), blocks())
    -> blocks().

add(Block, Index, Interconnect, Key, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := {Interconnect, #{Key := From}}}} ->
            Blocks;

        #{Block := #{Index := {Interconnect, #{Key := Existing}}}} ->
            throw({
                r4_database, Block, Index, Interconnect, Key,
                add, From, existing, Existing
            });

        #{Block := Indexes = #{Index := {Interconnect, Mux}}} ->
            Blocks#{
                Block => Indexes#{
                    Index => {
                        Interconnect,
                        Mux#{Key => From}
                    }
                }
            };

        #{Block := #{Index := {Existing, _}}} ->
            throw({
                r4_database, Block, Index,
                add, Interconnect, existing, Existing
            });

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => {Interconnect, #{Key => From}}}};


        _ ->
            Blocks#{Block => #{Index => {Interconnect, #{Key => From}}}}
    end.

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.r4-interconnect", [Density])).

