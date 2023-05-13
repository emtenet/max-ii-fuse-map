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
-type mux_index() :: {mux, 0..15}.

-type mux() :: #{mux_key() => from()}.
-type mux_key() :: direct_link | {mux4(), mux3()}.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.
-type from() :: max_ii:c4() | max_ii:le_buffer() | max_ii:r4().

%%====================================================================
%% run
%%====================================================================

run() ->
    %build(epm240),
    %build(epm570),
    %build(epm1270),
    build(epm2210),
    %lists:foreach(fun build/1, density:list()),
    ok.

%%--------------------------------------------------------------------

build(Density) ->
    %Db0 = #{},
    {ok, Db0} = open(Density),
    {ok, Cache} = route_cache:open(Density),
    Db1 = route_cache:fold_blocks(
        r4,
        fun (Block, Indexes, Acc) ->
            build_block(Density, Block, Indexes, Acc)
        end,
        Db0,
        Cache
    ),
    save(Density, Db1),
    ok.

%%--------------------------------------------------------------------

build_block(Density, Block, Indexes, Db0) ->
    io:format(" ==> ~p:~n", [Block]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            build_index(Density, Block, Index, Froms, Acc)
        end,
        Db0,
        Indexes
    ).

%%--------------------------------------------------------------------

build_index(Density, {r4, X, Y}, Index, Froms, Db0) ->
    Interconnect = {r4, X, Y, 0, Index},
    route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            build_from_check(Density, Interconnect, From, Cached, Acc)
        end,
        Db0,
        Froms
    ).

%%--------------------------------------------------------------------

build_from_check(Density, Interconnect, From, Cached, Db) ->
    case r4_interconnect_map:to_mux(Interconnect, Density) of
        {ok, Block, Index} ->
            case find_key(Block, Index, Interconnect, From, Db) of
                {ok, _} ->
                    Db;

                false ->
                    build_from(Density, Block, Index, Interconnect, From, Cached, Db)
            end;

        Error ->
            io:format("-----------------------------------------------~n", []),
            io:format("TO MUX ~p ~p ~p~n", [Density, Interconnect, Error]),
            Fuses = build_fuses(Density, Cached),
            fuse_locations(Fuses, Density),
            io:format("-----------------------------------------------~n", []),
            Db
    end.

%%--------------------------------------------------------------------

build_from(Density, Block, Index, Interconnect, From, Cached, Db) ->
    case build_fuses(Density, Cached) of
        false ->
            Db;

        {ok, Fuses} ->
            build_fuses(Density, Block, Index, Interconnect, From, Fuses, Db)
    end.

%%--------------------------------------------------------------------

build_fuses(Density, Block, Index, Interconnect, From, Fuses, Db) ->
    case build_ports(Density, Fuses, #{}, []) of
        [] ->
            io:format("~10w ~8w ~15w: ?????? <- ~w~n", [
                Block, Index, Interconnect, From
            ]),
            fuse_locations(Fuses, Density),
            throw(fuse_not_found),
            Db;

        Ports ->
            build_from_pick(Ports, Block, Index, Interconnect, From, Db)
    end.

%%--------------------------------------------------------------------

fuse_locations(Fuses, Density) ->
    lists:foreach(fun (Fuse) -> fuse_location(Fuse, Density) end, Fuses).

%%--------------------------------------------------------------------

fuse_location(Fuse, Density) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, _} ->
            ok;

        {error, Location} ->
            io:format("  ~w~n", [Location])
    end.

%%--------------------------------------------------------------------

build_from_pick([], _, _, _, _, Db) ->
    Db;
build_from_pick(Ports, Block, Index, Interconnect, From, Db) ->
    case Ports of
        [{Block, Index, Key} | _] ->
            build_add(Block, Index, Interconnect, Key, From, Db);

        [_ | Rest] ->
            build_from_pick(Rest, Block, Index, Interconnect, From, Db)
    end.

%%--------------------------------------------------------------------

build_add(Block, Index, Interconnect, Key, From, Db) ->
    io:format("~10w ~8w ~15w: ~w <- ~w~n", [
        Block, Index, Interconnect, Key, From
    ]),
    add(Block, Index, Interconnect, Key, From, Db).

%%--------------------------------------------------------------------

build_fuses(Density, Cached) ->
    route_cache:fold_cached(
        fun build_fuses_reduce0/2,
        Density,
        fun build_fuses_reduce/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

build_fuses_reduce0(Experiment, Density) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:subtract(Fuses, density:minimal_fuses(Density)).

%%--------------------------------------------------------------------

build_fuses_reduce(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

%%--------------------------------------------------------------------

build_ports(_, [], _, Ports) ->
    Ports;
build_ports(Density, [Fuse | Fuses], Muxes0, Ports) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {Block = {r4, _, _}, Index = {mux, _}, Direct}}
                when Direct =:= direct_link orelse
                     Direct =:= io_data_in0 orelse
                     Direct =:= io_data_in1 ->
            Port = {Block, Index, Direct},
            build_ports(Density, Fuses, Muxes0, [Port | Ports]);

        {ok, {Block = {r4, _, _}, Index = {mux, _}, from3, Mux3}} ->
            Key = {Block, Index},
            case maps:take(Key, Muxes0) of
                {{from4, Mux4}, Muxes} ->
                    Port = {Block, Index, {Mux4, Mux3}},
                    build_ports(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {from3, Mux3},
                    build_ports(Density, Fuses, Muxes0#{Key => Mux}, Ports)
            end;

        {ok, {Block = {r4, _, _}, Index = {mux, _}, from4, Mux4}} ->
            Key = {Block, Index},
            case maps:take(Key, Muxes0) of
                {{from3, Mux3}, Muxes} ->
                    Port = {Block, Index, {Mux4, Mux3}},
                    build_ports(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {from4, Mux4},
                    build_ports(Density, Fuses, Muxes0#{Key => Mux}, Ports)
            end;

        _ ->
            build_ports(Density, Fuses, Muxes0, Ports)
    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, blocks()}.

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
%% find_key
%%====================================================================

-spec find_key(block(), mux_index(), max_ii:r4(), from(), blocks())
    -> {ok, mux_key()} | false.

find_key(Block, Index, Interconnect, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := {Interconnect, Keys}}} ->
            find_key(From, maps:next(maps:iterator(Keys)));

        #{Block := #{Index := {Existing, _}}} ->
            throw({
                r4_database, Block, Index,
                find_key, Interconnect, existing, Existing
            });

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
    lists:flatten(io_lib:format("../database/~s.r4-interconnect", [Density])).

