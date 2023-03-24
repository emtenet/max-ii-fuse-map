-module(c4_ports_database).

-export([run/0]).

-export([open/1]).
-export([save/2]).
-export([add/5]).

-export_type([block/0]).
-export_type([blocks/0]).
-export_type([port/0]).
-export_type([ports/0]).
-export_type([mux/0]).
-export_type([entry/0]).
-export_type([mux4/0]).
-export_type([mux3/0]).
-export_type([from/0]).
-export_type([index/0]).

-type density() :: density:density().

-type blocks() :: #{block() => ports()}.
-type block() :: {c4, max_ii:x(), max_ii:y()}.

-type ports() :: #{index() => mux()}.
-type index() :: non_neg_integer().

-type mux() :: #{entry() => {from(), c4()}}.
-type entry() :: direct_link | {mux4(), mux3()}.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.
-type from() ::
    {c4, max_ii:x(), max_ii:y(), 0, index()} |
    {le_buffer, max_ii:x(), max_ii:y(), 0, index()} |
    {r4, max_ii:x(), max_ii:y(), 0, index()}.
-type c4() :: {c4, max_ii:x(), max_ii:y(), 0, index()}.

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
    Db0 = #{},
    {ok, Routes} = route_cache:open(Density),
    Blocks = route_cache:blocks(c4, Routes),
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
            {c4, X, Y} = Block,
            Interconnect = {c4, X, Y, 0, Index},
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

        {{c4, X, Y, port, Port}, Entry} ->
            Block = {c4, X, Y},
            io:format("~10w ~2b ~w: ~25w -> ~w~n", [Block, Port, Entry, From, Interconnect]),
            add(Block, Port, Entry, {From, Interconnect}, Db)
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
        {ok, {R4 = {c4, _, _, port, _}, direct_link}} ->
            Port = {R4, direct_link},
            build_port(Density, Fuses, Muxes0, [Port | Ports]);

        {ok, {R4 = {c4, _, _, port, _}, port3, Mux3}} ->
            case maps:take(R4, Muxes0) of
                {{port4, Mux4}, Muxes} ->
                    Port = {R4, {Mux4, Mux3}},
                    build_port(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {port3, Mux3},
                    build_port(Density, Fuses, Muxes0#{R4 => Mux}, Ports)
            end;

        {ok, {R4 = {c4, _, _, port, _}, port4, Mux4}} ->
            case maps:take(R4, Muxes0) of
                {{port3, Mux3}, Muxes} ->
                    Port = {R4, {Mux4, Mux3}},
                    build_port(Density, Fuses, Muxes, [Port | Ports]);

                error ->
                    Mux = {port4, Mux4},
                    build_port(Density, Fuses, Muxes0#{R4 => Mux}, Ports)
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
        {ok, [{c4, X, Y, port, Port} | Lines]} ->
            Blocks = open(Lines, #{}, Port, {c4, X, Y}, #{}),
            {ok, Blocks};

        {error, enoent} ->
            {ok, #{}}
    end.

%%--------------------------------------------------------------------

open([], Mux, Port, Block, Blocks) ->
    case Blocks of
        #{Block := Ports} ->
            Blocks#{Block => Ports#{Port => Mux}};

        _ ->
            Blocks#{Block => #{Port => Mux}}
    end;
open([{c4, X, Y, port, I} | Lines], Mux, Port, Block, Blocks) ->
    case Blocks of
        #{Block := Ports} ->
            open(Lines, #{}, I, {c4, X, Y},
                 Blocks#{Block => Ports#{Port => Mux}});

        _ ->
            open(Lines, #{}, I, {c4, X, Y},
                 Blocks#{Block => #{Port => Mux}})
    end;
open([{Entry, From} | Lines], Mux, Port, Block, Blocks) ->
    open(Lines, Mux#{Entry => From}, Port, Block, Blocks).

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
        save_index(Block, Port, Ports)
        ||
        Port <- lists:sort(maps:keys(Ports))
    ].
%%--------------------------------------------------------------------

save_index({c4, X, Y}, Port, Ports) ->
    #{ Port := Mux} = Ports,
    [
        iolist_to_binary(io_lib:format("{c4,~p,~p,port,~p}.~n", [X, Y,  Port]))
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

add(Block, Port, Entry, From, Blocks) ->
    case Blocks of
        #{Block := #{Port := #{Entry := From}}} ->
            Blocks;

        #{Block := #{Port := #{Entry := Existing}}} ->
            throw({c4_database, Block, Port, Entry, add, From, existing, Existing});

        #{Block := Ports = #{Port := Mux}} ->
            Blocks#{Block => Ports#{Port => Mux#{Entry => From}}};

        #{Block := Ports} ->
            Blocks#{Block => Ports#{Port => #{Entry => From}}};

        _ ->
            Blocks#{Block => #{Port => #{Entry => From}}}
    end.

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.c4-ports", [Density])).

