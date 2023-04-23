-module(c4_tail_experiment).

-export([run/0]).

-include("max_ii.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    %density(epm240),
    %density(epm570),
    %density(epm1270),
    %density(epm2210),
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Columns = density:columns(Density),
    column(Density, Device, Columns, #{}).

%%--------------------------------------------------------------------

column(Density, _, [], Fuses0) ->
    Mappings = reduce(Fuses0),
    %io:format("~p~n", [Mappings]),
    check_mappings(Density, Mappings),
    ok;
column(Density, Device, [X | Xs], Fuses0) ->
    io:format(" ==> ~s ~p~n", [Density, X]),
    {Top, Outs, Bottom, Ins} = pins(X, Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source:ins_lut_outs(Device, {column, Y - Bottom + 1}, X, Y, Ins, Outs)
        ||
        Y <- lists:seq(Bottom, Top)
    ]),
    Minimal = {minimal, density:minimal_fuses(Density)},
    Matrix0 = matrix:build_with_location(Density, [Minimal | Experiments]),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_,tail,0,cell,25}) -> true;
        ({_,tail,_,cell, 0}) -> false;
        ({_,tail,_,cell,23}) -> false;
        ({_,tail,_,cell,25}) -> false;
        ({_,tail,_,cell,26}) -> false;
        ({_,tail,_,side, _}) -> false;
        (_) -> true
    end),
    %matrix:print(Matrix),
    Routes = routes(Experiments),
    %io:format("~p~n", [Routes]),
    MaxColumn = length(Experiments),
    Fuses = maps:fold(fun (Key, Columns, Acc) ->
        fuses(Key, Columns, MaxColumn, Matrix, Acc)
    end, Fuses0, Routes),
    column(Density, Device, Xs, Fuses).

%%--------------------------------------------------------------------

pins(X, Device) ->
    Top = device:top_lab(Device),
    Bottom = device:bottom_lab(X, Device),
    Tops = device:top_pins(X, Device),
    case device:metric(Device) of
        #metric{left_lab = X} ->
            Bottoms = device:bottom_pins(X, Device) ++
                device:bottom_pins(X + 1, Device) ++
                device:bottom_pins(X + 2, Device),
            {Top, Tops, Bottom, Bottoms};

        #metric{right_lab = X} ->
            Bottoms = device:bottom_pins(X - 2, Device) ++
                device:bottom_pins(X - 1, Device) ++
                device:bottom_pins(X, Device),
            {Top, Tops, Bottom, Bottoms};

        _ ->
            Bottoms = device:bottom_pins(X - 1, Device) ++
                device:bottom_pins(X, Device) ++
                device:bottom_pins(X + 1, Device),
            {Top, Tops, Bottom, Bottoms}
    end.

%%--------------------------------------------------------------------

routes(Experiments) ->
    lists:foldl(fun({{column, N}, _, #{signals := Signals}}, Routes0) ->
        maps:fold(fun(_, #{dests := Dests}, Routes1) ->
            lists:foldl(fun (#{route := Route}, Routes2) ->
                routes(Route, Routes2, N)
            end, Routes1, Dests)
        end, Routes0, Signals)
    end, #{}, Experiments).

%%--------------------------------------------------------------------

routes(Route, Routes, N) ->
    case lists:reverse(Route) of
        [In = {io_data_in, _, _, _, _}, C4 = {c4, _, _, _, _} | _] ->
            Key = {C4, In},
            case Routes of
                #{Key := Ns} ->
                    Routes#{Key => [N | Ns]};

                _ ->
                    Routes#{Key => [N]}
            end;

        _ ->
            Routes
    end.

%%--------------------------------------------------------------------

fuses(Key, Columns, MaxColumn, Matrix, Acc) ->
    Pattern = pattern(Columns, MaxColumn),
    Locations = lists:sort([
        Location
        ||
        {_, Location} <- matrix:pattern_is(Matrix, Pattern)
    ]),
    case Acc of
        #{Key := Existing} ->
            Acc#{Key => fuses:intersect(Existing, Locations)};

        _ ->
            Acc#{Key => Locations}
    end.

%%--------------------------------------------------------------------

pattern(Columns, MaxColumn) ->
    [
        case lists:member(Column, Columns) of
            true -> 0;
            false -> 1
        end
        ||
        Column <- lists:seq(0, MaxColumn)
    ].

%%--------------------------------------------------------------------

reduce(Routes0) ->
    {Known, Routes} = maps:fold(fun reduce_init/3, {[], #{}}, Routes0),
    reduce_next(Known, Routes, []).

%%--------------------------------------------------------------------

reduce_init(Key, [Location], {Known, Routes}) ->
    {[{Key, Location} | Known], Routes};
reduce_init(Key, Locations, {Known, Routes}) ->
    {Known, Routes#{Key => Locations}}.

%%--------------------------------------------------------------------

reduce_next([], _, Reduced) ->
    lists:sort(Reduced);
reduce_next([{{C4, In}, Location} | Queue0], Routes0, Reduced) ->
    {Queue, Routes} = maps:fold(fun (Key, Locations, Acc) ->
        reduce_take(Key, Location, Locations, Acc)
    end, {Queue0, #{}}, Routes0),
    reduce_next(Queue, Routes, [{C4, In, Location} | Reduced]).

%%--------------------------------------------------------------------

reduce_take(Key, Location, Locations, {Queue, Routes}) ->
    case lists:member(Location, Locations) of
        true ->
            case lists:delete(Location, Locations) of
                [Known] ->
                    {[{Key, Known} | Queue], Routes};

                Reduced = [_ | _] ->
                    {Queue, Routes#{Key => Reduced}}
            end;

        false ->
            {Queue, Routes#{Key => Locations}}
    end.

%%====================================================================
%% mappings
%%====================================================================

check_mappings(Density, Mappings) ->
    Metric = density:metric(Density),
    lists:foreach(fun (Mapping) ->
        check_mapping(Metric, Mapping)
    end, Mappings).

%%--------------------------------------------------------------------

check_mapping(Metric, {C4, IO, Fuse = {XX, tail, Index, Cell, Sector}}) ->
    {X, Mux, Sel} = mapping_mux(XX, Index, Cell, Sector),
    case mapping_c4(X, Mux, Metric) of
        C4 ->
            ok;

        OtherC4 ->
            %throw({mapping, X, Index, Sector, to, OtherC4, expecting, C4})
            io:format("mapping ~2b ~2b ~2b to ~p expecting ~p~n", [X, Index, Sector, OtherC4, C4])
    end,
    case mapping_io(X, Mux, Sel, Metric) of
        IO ->
            ok;

        OtherIO ->
            %throw({mapping, X, Index, Sector, to, OtherIO, expecting, IO})
            io:format("mapping ~2b ~2b ~2b to ~p expecting ~p~n", [X, Index, Sector, OtherIO, IO])
    end,
    Block = mapping_block(X, Metric),
    case c4_interconnect_map:from_mux(Block, {mux, Mux}, Metric#metric.density) of
        {ok, C4} ->
            ok;

        GotC4 ->
            io:format("from_mux(~10w, ~w) -> ~15w got ~p~n", [Block, {mux, Mux}, C4, GotC4])
    end,
    case c4_interconnect_map:to_mux(C4, Metric#metric.density) of
        {ok, Block, {mux, Mux}} ->
            ok;

        GotMux ->
            io:format("to_mux(~15w) -> ~10w ~w got ~p~n", [C4, Block, {mux, Mux}, GotMux])
    end,
    case fuse_map:to_name(Fuse, Metric#metric.density) of
        {ok, {Block, {mux, Mux}, Sel}} ->
            ok;

        GotFuse ->
            throw({fuse, Fuse, to, GotFuse, expecting, {Block, {mux, Mux}, Sel}})
    end,
    ok.

%%--------------------------------------------------------------------

mapping_mux(X,  2, side, 11) -> {X,     3, io_data_in1};
mapping_mux(X,  4, side, 11) -> {X,     4, io_data_in1};
mapping_mux(X,  6, side, 11) -> {X,     5, io_data_in1};
mapping_mux(X,  8, side, 11) -> {X,     6, io_data_in1};
mapping_mux(X, 10, side, 11) -> {X,     7, io_data_in1};
mapping_mux(X,  1, cell, 23) -> {X,     0, io_data_in0};
mapping_mux(X,  3, cell, 23) -> {X,     1, io_data_in0};
mapping_mux(X,  5, cell, 23) -> {X,     2, io_data_in0};
mapping_mux(X,  1, cell,  0) -> {X - 1, 3, io_data_in0};
mapping_mux(X,  3, cell,  0) -> {X - 1, 4, io_data_in0};
mapping_mux(X,  5, cell,  0) -> {X - 1, 5, io_data_in0};
mapping_mux(X,  7, cell,  0) -> {X - 1, 6, io_data_in0};
mapping_mux(X,  9, cell,  0) -> {X - 1, 7, io_data_in0};
mapping_mux(X,  7, cell, 23) -> {X,     8, io_data_in0};
mapping_mux(X,  9, cell, 23) -> {X,     9, io_data_in0};
mapping_mux(X,  2, cell, 25) -> {X,     0, io_data_in1};
mapping_mux(X,  4, cell, 25) -> {X,     1, io_data_in1};
mapping_mux(X,  6, cell, 25) -> {X,     2, io_data_in1};
mapping_mux(X,  2, cell, 26) -> {X,     3, io_data_in1};
mapping_mux(X,  4, cell, 26) -> {X,     4, io_data_in1};
mapping_mux(X,  6, cell, 26) -> {X,     5, io_data_in1};
mapping_mux(X,  8, cell, 26) -> {X,     6, io_data_in1};
mapping_mux(X, 10, cell, 26) -> {X,     7, io_data_in1};
mapping_mux(X,  8, cell, 25) -> {X,     8, io_data_in1};
mapping_mux(X, 10, cell, 25) -> {X,     9, io_data_in1}.

%%--------------------------------------------------------------------

mapping_block(X, #metric{indent_bottom_io = Bottom, indent_left_io = Left})
        when X < Left ->
    {c4, X, Bottom};
mapping_block(X, #metric{bottom_io = Bottom}) ->
    {c4, X, Bottom}.

%%--------------------------------------------------------------------

short({left, _, Base}, X, #metric{left_io = Left = X}) ->
    Base + element(1 + ((X + Left) rem 4), {0, b, 3, d});
short({left, Base, _}, X, #metric{left_io = Left}) ->
    Base + element(1 + ((X + Left) rem 4), {3, 1, 2, 0});
short({right, _, Base}, X, #metric{left_io = Left, indent_left_io = Indent})
        when X =:= Indent - 1 ->
    Base + element(1 + ((X + Left) rem 4), {0, b, 3, d});
short({right, Base, _}, X, #metric{left_io = Left}) ->
    Base + element(1 + ((X + Left) rem 4), {3, 1, 2, 0});
short(Base, X, #metric{left_io = Left}) ->
    Base + element(1 + ((X + Left) rem 4), {0, 2, 3, 1}).

%%--------------------------------------------------------------------

long({left, Base, _}, X, _) ->
    Base + element(1 + (X rem 4), {2, 0, 1, 3});
long({right, _, Base}, X, #metric{right_lab = X}) ->
    Base + element(1 + (X rem 4), {3, b, c, d});
long({right, Base, _}, X, _) ->
    Base + element(1 + (X rem 4), {2, 0, 1, 3});
long(Base, X, _) ->
    Base + element(1 + (X rem 4), {3, 1, 2, 0}).

%%--------------------------------------------------------------------

mapping_c4(X, Mux, Metric = #metric{indent_left_io = Indent}) ->
    Select = mapping_c4(Mux),
    case X of
        _ when X < Indent orelse Indent =:= 1 ->
            Y = Metric#metric.indent_bottom_lab,
            {c4, X, Y, 0, short(Select, X, Metric)};

        _ ->
            Y = Metric#metric.bottom_lab,
            {c4, X, Y, 0, long(Select, X, Metric)}
    end.

%%--------------------------------------------------------------------

mapping_c4(0) ->  0;
mapping_c4(1) ->  4;
mapping_c4(2) ->  8;
mapping_c4(3) -> 12;
mapping_c4(4) -> 16;
mapping_c4(5) -> 20;
mapping_c4(6) -> 24;
mapping_c4(7) -> {left, 12, 0};
mapping_c4(8) -> {right, 0, 12};
mapping_c4(9) -> {right, 4, 16}.

%%--------------------------------------------------------------------

mapping_io(X0, Mux, Sel, Metric = #metric{indent_left_lab = Indent}) ->
    case mapping_io(X0, Mux, Sel) of
        {X, I} when X < Indent ->
            {io_data_in, X, Metric#metric.indent_bottom_io, I, 0};

        {X, I} ->
            {io_data_in, X, Metric#metric.bottom_io, I, 0}
    end.

%%--------------------------------------------------------------------

mapping_io(X, 0, io_data_in0) -> {X,     0};
mapping_io(X, 1, io_data_in0) -> {X,     2};
mapping_io(X, 2, io_data_in0) -> {X,     0};
mapping_io(X, 3, io_data_in0) -> {X + 1, 0};
mapping_io(X, 4, io_data_in0) -> {X + 1, 2};
mapping_io(X, 5, io_data_in0) -> {X + 1, 0};
mapping_io(X, 6, io_data_in0) -> {X + 1, 1};
mapping_io(X, 7, io_data_in0) -> {X + 1, 0};
mapping_io(X, 8, io_data_in0) -> {X,     1};
mapping_io(X, 9, io_data_in0) -> {X,     0};
mapping_io(X, 0, io_data_in1) -> {X,     1};
mapping_io(X, 1, io_data_in1) -> {X,     3};
mapping_io(X, 2, io_data_in1) -> {X,     2};
mapping_io(X, 3, io_data_in1) -> {X + 1, 1};
mapping_io(X, 4, io_data_in1) -> {X + 1, 3};
mapping_io(X, 5, io_data_in1) -> {X + 1, 2};
mapping_io(X, 6, io_data_in1) -> {X + 1, 3};
mapping_io(X, 7, io_data_in1) -> {X + 1, 3};
mapping_io(X, 8, io_data_in1) -> {X,     3};
mapping_io(X, 9, io_data_in1) -> {X,     3}.

