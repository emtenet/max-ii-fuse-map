-module(c4_head_experiment).

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
    {Top, Ins, Bottom, Outs} = pins(X, Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source:ins_lut_outs(Device, {column, Top - Y + 1}, X, Y, Ins, Outs)
        ||
        Y <- lists:seq(Top, Bottom, -1)
    ]),
    Minimal = {minimal, density:minimal_fuses(Density)},
    Matrix0 = matrix:build_with_location(Density, [Minimal | Experiments]),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_,head,0,cell,25}) -> true;
        ({_,head,_,cell, 0}) -> false;
        ({_,head,_,cell,23}) -> false;
        ({_,head,_,cell,25}) -> false;
        ({_,head,_,cell,26}) -> false;
        ({_,head,_,side, _}) -> false;
        (_) -> true
    end),
    %matrix:print(Matrix),
    Routes = routes(Experiments),
    MaxColumn = length(Experiments),
    Fuses = maps:fold(fun (Key, Columns, Acc) ->
        fuses(Key, Columns, MaxColumn, Matrix, Acc)
    end, Fuses0, Routes),
    column(Density, Device, Xs, Fuses).

%%--------------------------------------------------------------------

pins(X, Device) ->
    Top = device:top_lab(Device),
    Bottom = device:bottom_lab(X, Device),
    case device:metric(Device) of
        #metric{left_lab = X} ->
            Tops = device:top_pins(X, Device) ++
                device:top_pins(X + 1, Device) ++
                device:top_pins(X + 2, Device),
            Bottoms = device:bottom_pins(X, Device),
            {Top, Tops, Bottom, Bottoms};

        #metric{right_lab = X} ->
            Tops = device:top_pins(X - 2, Device) ++
                device:top_pins(X - 1, Device) ++
                device:top_pins(X, Device),
            Bottoms = device:bottom_pins(X, Device),
            {Top, Tops, Bottom, Bottoms};

        #metric{indent_left_io = X} ->
            Tops = device:top_pins(X - 1, Device) ++
                device:top_pins(X, Device) ++
                device:top_pins(X + 1, Device),
            Bottoms = device:bottom_pins(X - 1, Device),
            {Top, Tops, Bottom, Bottoms};

        _ ->
            Tops = device:top_pins(X - 1, Device) ++
                device:top_pins(X, Device) ++
                device:top_pins(X + 1, Device),
            Bottoms =  device:bottom_pins(X, Device),
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

check_mapping(Metric, {C4, IO, Fuse = {XX, head, Index, Cell, Sector}}) ->
    {X, Mux, Sel} = mapping_mux(XX, Index, Cell, Sector),
    case mapping_c4(X, Mux, Metric) of
        C4 ->
            ok;

        OtherC4 ->
            throw({mapping, X, Index, Sector, to, OtherC4, expecting, C4})
    end,
    case mapping_io(X, Mux, Sel, Metric) of
        IO ->
            ok;

        OtherIO ->
            throw({mapping, X, Index, Sector, to, OtherIO, expecting, IO})
    end,
    Block = {c4, X, Metric#metric.top_io},
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

mapping_mux(X,  2, side, 11) -> {X + 1, 2, io_data_in1};
mapping_mux(X,  4, side, 11) -> {X + 1, 3, io_data_in1};
mapping_mux(X,  6, side, 11) -> {X + 1, 4, io_data_in1};
mapping_mux(X,  8, side, 11) -> {X + 1, 5, io_data_in1};
mapping_mux(X, 10, side, 11) -> {X + 1, 6, io_data_in1};
mapping_mux(X,  1, cell, 23) -> {X,     0, io_data_in0};
mapping_mux(X,  5, cell, 23) -> {X,     1, io_data_in0};
mapping_mux(X,  1, cell,  0) -> {X,     2, io_data_in0};
mapping_mux(X,  3, cell,  0) -> {X,     3, io_data_in0};
mapping_mux(X,  5, cell,  0) -> {X,     4, io_data_in0};
mapping_mux(X,  7, cell,  0) -> {X,     5, io_data_in0};
mapping_mux(X,  9, cell,  0) -> {X,     6, io_data_in0};
mapping_mux(X,  3, cell, 23) -> {X,     7, io_data_in0};
mapping_mux(X,  7, cell, 23) -> {X,     8, io_data_in0};
mapping_mux(X,  9, cell, 23) -> {X,     9, io_data_in0};
mapping_mux(X,  2, cell, 25) -> {X,     0, io_data_in1};
mapping_mux(X,  6, cell, 25) -> {X,     1, io_data_in1};
mapping_mux(X,  2, cell, 26) -> {X + 1, 2, io_data_in1};
mapping_mux(X,  4, cell, 26) -> {X + 1, 3, io_data_in1};
mapping_mux(X,  6, cell, 26) -> {X + 1, 4, io_data_in1};
mapping_mux(X,  8, cell, 26) -> {X + 1, 5, io_data_in1};
mapping_mux(X, 10, cell, 26) -> {X + 1, 6, io_data_in1};
mapping_mux(X,  4, cell, 25) -> {X,     7, io_data_in1};
mapping_mux(X,  8, cell, 25) -> {X,     8, io_data_in1};
mapping_mux(X, 10, cell, 25) -> {X,     9, io_data_in1}.

%%--------------------------------------------------------------------

mapping_c4(X0, Mux, Metric = #metric{top_io = Top}) ->
    case mapping_c4(Mux) of
        {indent, XX, Hi, _}
                when X0 + XX < Metric#metric.indent_left_io andalso
                     Top - 4 =< Metric#metric.indent_bottom_lab ->
            {c4, X0 + XX, Top - 4, 0, Hi};

        {indent, XX, Hi, _}
                when Top - 4 =< Metric#metric.bottom_lab ->
            {c4, X0 + XX, Top - 4, 0, Hi};

        {indent, XX, _, Lo} ->
            {c4, X0 + XX, Top - 4, 0, Lo};

        {right, XX, Hi, _}
                when X0 + XX =:= Metric#metric.right_lab ->
            {c4, X0 + XX, Top - 4, 0, Hi};

        {right, XX, _, Lo} ->
            {c4, X0 + XX, Top - 1, 0, Lo}
    end.

%%--------------------------------------------------------------------

mapping_c4(0) -> {indent,  0, 28,  7};
mapping_c4(1) -> {indent,  0, 29,  8};
mapping_c4(2) -> {indent, -1, 30,  9};
mapping_c4(3) -> {indent, -1, 31, 10};
mapping_c4(4) -> {indent, -1, 32, 11};
mapping_c4(5) -> {indent, -1, 33, 12};
mapping_c4(6) -> {indent, -1, 34, 13};
mapping_c4(7) -> {right,   0,  9,  7};
mapping_c4(8) -> {right,   0, 10,  8};
mapping_c4(9) -> {right,   0, 11,  9}.

%%--------------------------------------------------------------------

mapping_io(X, Mux, Sel, #metric{top_io = Y}) ->
    I = mapping_io(Mux, Sel),
    {io_data_in, X, Y, I, 0}.

%%--------------------------------------------------------------------

mapping_io(0, io_data_in0) -> 0;
mapping_io(1, io_data_in0) -> 0;
mapping_io(2, io_data_in0) -> 0;
mapping_io(3, io_data_in0) -> 2;
mapping_io(4, io_data_in0) -> 0;
mapping_io(5, io_data_in0) -> 1;
mapping_io(6, io_data_in0) -> 0;
mapping_io(7, io_data_in0) -> 2;
mapping_io(8, io_data_in0) -> 1;
mapping_io(9, io_data_in0) -> 0;
mapping_io(0, io_data_in1) -> 1;
mapping_io(1, io_data_in1) -> 2;
mapping_io(2, io_data_in1) -> 1;
mapping_io(3, io_data_in1) -> 3;
mapping_io(4, io_data_in1) -> 2;
mapping_io(5, io_data_in1) -> 3;
mapping_io(6, io_data_in1) -> 3;
mapping_io(7, io_data_in1) -> 3;
mapping_io(8, io_data_in1) -> 3;
mapping_io(9, io_data_in1) -> 3.

