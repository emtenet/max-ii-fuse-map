-module(generate_pins).

-export([run/0]).

% Generate files:
%
%   src/pin.erl
%   src/<device>.erl
%
% with pin information extracted from BSDL files.
%
% Pins are annotated by their IOC with experiments and reading
% the RCF file.

-include("max_ii.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    Devices = lists:map(fun bsdl/1, device:list()),
    Pins = merge(Devices),
    pin_file(Pins),
    lists:foreach(fun device_file/1, Devices).

%%--------------------------------------------------------------------

merge(Devices) ->
    lists:usort(lists:flatten([
        Pins
        ||
        {_, Pins} <- Devices
    ])).

%%====================================================================
%% bsdl
%%====================================================================

bsdl(Device) ->
    Name0 = device:name(Device),
    Size = byte_size(Name0) - 2,
    <<Name:Size/binary, "C5">> = Name0,
    File = <<"../bsdl/", Name/binary, ".bsdl">>,
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    {Device, bsdl(Lines, [])}.

%%--------------------------------------------------------------------

bsdl([], Pins) ->
    lists:sort(Pins);
bsdl([<<"  --BSC group ", Line/binary>> | Lines], Pins) ->
    %  --BSC group 4 for I/O pin G2
    %  --BSC group 144 for unused pad
    case binary:split(Line, <<" for ">>) of
        [_, <<"unused pad">>] ->
            bsdl(Lines, Pins);

        [_, <<"I/O pin ", Pin/binary>>] ->
            bsdl(Lines, [bsdl_pin(Pin) | Pins])
    end;
bsdl([_ | Lines], Pins) ->
    bsdl(Lines, Pins).

%%--------------------------------------------------------------------

bsdl_pin(Coord = <<U, Number0/binary>>) when U >= $A andalso U =< $Z ->
    Number = binary_to_integer(Number0),
    Sort = (1000 * (U + 1 - $A)) + Number,
    Name = <<"PIN_", Coord/binary>>,
    L = U + $a - $A,
    Enum = <<L, Number0/binary>>,
    {Sort, Enum, Name};
bsdl_pin(Number) ->
    Sort = binary_to_integer(Number),
    Name = <<"PIN_", Number/binary>>,
    Enum = <<"pin", Number/binary>>,
    {Sort, Enum, Name}.

%%====================================================================
%% pin_file
%%====================================================================

pin_file(Pins) ->
    Data = [<<
        "-module(pin).\n"
        "\n"
        "-export([name/1]).\n"
        "\n"
        "-export_type([pin/0]).\n"
        "\n"
        "-type pin() ::\n">>,
        pin_type(Pins, []), <<
        "\n"
        "-spec name(pin()) -> binary().\n"
        "\n">>,
        pin_func(Pins, []), <<
        "\n"
    >>],
    File = "src/pin.erl",
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

pin_type([], [[Line, <<" |\n">>] | Lines]) ->
    lists:reverse(Lines, [Line, <<".\n">>]);
pin_type([{_, Enum, _} | Pins], Lines) ->
    Line = [
        <<"    ", Enum/binary>>,
        <<" |\n">>
    ],
    pin_type(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

pin_func([], [[Line, <<";\n">>] | Lines]) ->
    lists:reverse(Lines, [Line, <<".\n">>]);
pin_func([{_, Enum, Name} | Pins], Lines) ->
    Line = [
        <<"name(", Enum/binary, ") -> <<\"", Name/binary, "\">>">>,
        <<";\n">>
    ],
    pin_func(Pins, [Line | Lines]).

%%====================================================================
%% device_file
%%====================================================================

device_file({Device, BSDLPins}) ->
    Density = device:density(Device),
    Metric = density:metric(Density),
    Pins = pins(Device, BSDLPins),
    Data = [<<
        "-module(">>, atom_to_binary(Device), <<").\n"
        "\n"
        "-export([iocs/0]).\n"
        "-export([pins/0]).\n"
        "-export([top_iocs/1]).\n"
        "-export([top_pins/1]).\n"
        "-export([left_iocs/1]).\n"
        "-export([left_pins/1]).\n"
        "-export([right_iocs/1]).\n"
        "-export([right_pins/1]).\n"
        "-export([bottom_iocs/1]).\n"
        "-export([bottom_pins/1]).\n"
        "\n"
        "-type ioc() :: ioc:ioc().\n"
        "-type pin() :: pin:pin().\n"
        "-type x() :: max_ii:x().\n"
        "-type y() :: max_ii:y().\n"
        "\n"
        "-spec iocs() -> [{pin(), ioc()}].\n"
        "\n"
        "iocs() ->\n">>,
        device_iocs(Pins, []), <<
        "\n"
        "-spec pins() -> [pin()].\n"
        "\n"
        "pins() ->\n">>,
        device_pins(Pins, []), <<
        "\n">>,
        side_iocs(Pins, Metric, <<"top">>, <<"x()">>,
                  fun device_columns/1, fun device_top/3),
        side_pins(Pins, Metric, <<"top">>, <<"x()">>,
                  fun device_columns/1, fun device_top/3),
        side_iocs(Pins, Metric, <<"left">>, <<"y()">>,
                  fun device_rows/1, fun device_left/3),
        side_pins(Pins, Metric, <<"left">>, <<"y()">>,
                  fun device_rows/1, fun device_left/3),
        side_iocs(Pins, Metric, <<"right">>, <<"y()">>,
                  fun device_rows/1, fun device_right/3),
        side_pins(Pins, Metric, <<"right">>, <<"y()">>,
                  fun device_rows/1, fun device_right/3),
        side_iocs(Pins, Metric, <<"bottom">>, <<"x()">>,
                  fun device_columns/1, fun device_bottom/3),
        side_pins(Pins, Metric, <<"bottom">>, <<"x()">>,
                  fun device_columns/1, fun device_bottom/3)
    ],
    Name = lists:flatten(io_lib:format("~s.erl", [Device])),
    File = filename:join("src", Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

device_iocs([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_iocs([Pin | Pins], Lines) ->
    Line = device_ioc(<<"     ">>, Pin, <<",\n">>),
    device_iocs(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

device_ioc(Head, {Enum, IOC}, Tail) ->
    [Head, io_lib:format("{~s,~w}", [Enum, IOC]), Tail].

%%--------------------------------------------------------------------

device_pins([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_pins([Pin | Pins], Lines) ->
    Line = device_pin(<<"     ">>, Pin, <<",\n">>),
    device_pins(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

device_pin(Head, {Enum, _}, Tail) ->
    [Head, io_lib:format("~s", [Enum]), Tail].

%%--------------------------------------------------------------------

device_columns(#metric{left_lab = Left, right_lab = Right}) ->
    {Left, Right}.

%%--------------------------------------------------------------------

device_rows(#metric{bottom_lab = Bottom, top_lab = Top}) ->
    {Bottom, Top}.

%%--------------------------------------------------------------------

device_top(Pins, #metric{top_io = Top}, At) ->
    lists:filter(fun ({_, {ioc, X, Y, _}}) ->
        X =:= At andalso Y =:= Top
    end, Pins).

%%--------------------------------------------------------------------

device_left(Pins, #metric{left_io = Left, indent_left_io = Indent}, At) ->
    lists:filter(fun ({_, {ioc, X, Y, _}}) ->
        (X =:= Left orelse X =:= Indent) andalso Y =:= At
    end, Pins).

%%--------------------------------------------------------------------

device_right(Pins, #metric{right_io = Right}, At) ->
    lists:filter(fun ({_, {ioc, X, Y, _}}) ->
        X =:= Right andalso Y =:= At
    end, Pins).

%%--------------------------------------------------------------------

device_bottom(Pins, #metric{bottom_io = Bottom, indent_bottom_io = Indent}, At) ->
    lists:filter(fun ({_, {ioc, X, Y, _}}) ->
        X =:= At andalso (Y =:= Bottom orelse Y =:= Indent)
    end, Pins).

%%--------------------------------------------------------------------

sort_by_ioc(Pins) ->
    lists:sort(fun ({_, A}, {_, B}) -> A =< B end, Pins).

%%--------------------------------------------------------------------

side_iocs(Pins, Metric, Name, Type, Range, Filter) ->
    {Min, Max} = Range(Metric),
    [<<
        "-spec ", Name/binary, "_iocs(", Type/binary, ") -> [{pin(), ioc()}].\n"
        "\n">>,
        side_iocs_clauses(Pins, Metric, Name, Min, Max, Filter, []), <<
        "\n"
    >>].

%%--------------------------------------------------------------------

side_iocs_clauses(Pins, Metric, Name, At, At, Filter, Clauses) ->
    Clause = side_iocs_clause(Pins, Metric, Name, At, Filter, <<".\n">>),
    lists:reverse(Clauses, [Clause]);
side_iocs_clauses(Pins, Metric, Name, At, Max, Filter, Clauses) ->
    Clause = side_iocs_clause(Pins, Metric, Name, At, Filter, <<";\n">>),
    side_iocs_clauses(Pins, Metric, Name, At + 1, Max, Filter, [Clause | Clauses]).

%%--------------------------------------------------------------------

side_iocs_clause(Pins0, Metric, Name, At, Filter, End) ->
    Pins = sort_by_ioc(Filter(Pins0, Metric, At)),
    Value = integer_to_binary(At),
    [
        <<Name/binary, "_iocs(", Value/binary, ") ->\n">>,
        side_iocs_lines(Pins),
        End
    ].

%%--------------------------------------------------------------------

side_iocs_lines([]) ->
    <<"    []">>;
side_iocs_lines([Pin]) ->
    device_ioc(<<"    [">>, Pin, <<"]">>);
side_iocs_lines(Pins) ->
    side_iocs_lines(Pins, <<"    [">>, []).

%%--------------------------------------------------------------------

side_iocs_lines([Pin], Head, Lines) ->
    Line = device_ioc(Head, Pin, <<"]">>),
    lists:reverse(Lines, [Line]);
side_iocs_lines([Pin | Pins], Head, Lines) ->
    Line = device_ioc(Head, Pin, <<",\n">>),
    side_iocs_lines(Pins, <<"     ">>, [Line | Lines]).

%%--------------------------------------------------------------------

side_pins(Pins, Metric, Name, Type, Range, Filter) ->
    {Min, Max} = Range(Metric),
    [<<
        "-spec ", Name/binary, "_pins(", Type/binary, ") -> [pin()].\n"
        "\n">>,
        side_pins_clauses(Pins, Metric, Name, Min, Max, Filter, []), <<
        "\n"
    >>].

%%--------------------------------------------------------------------

side_pins_clauses(Pins, Metric, Name, At, At, Filter, Clauses) ->
    Clause = side_pins_clause(Pins, Metric, Name, At, Filter, <<".\n">>),
    lists:reverse(Clauses, [Clause]);
side_pins_clauses(Pins, Metric, Name, At, Max, Filter, Clauses) ->
    Clause = side_pins_clause(Pins, Metric, Name, At, Filter, <<";\n">>),
    side_pins_clauses(Pins, Metric, Name, At + 1, Max, Filter, [Clause | Clauses]).

%%--------------------------------------------------------------------

side_pins_clause(Pins0, Metric, Name, At, Filter, End) ->
    Pins = Filter(Pins0, Metric, At),
    Value = integer_to_binary(At),
    [
        <<Name/binary, "_pins(", Value/binary, ") ->\n">>,
        side_pins_lines(Pins),
        End
    ].

%%--------------------------------------------------------------------

side_pins_lines([]) ->
    <<"    []">>;
side_pins_lines([Pin0 | Pins]) ->
    Line = device_pin(<<"    [">>, Pin0, <<>>),
    Lines = lists:map(fun (Pin) ->
        device_pin(<<", ">>, Pin, <<>>)
    end, Pins),
    [Line, Lines, <<"]">>].

%%====================================================================
%% pins
%%====================================================================

pins(Device, BSDLPins) ->
    io:format(" => ~s pins~n", [Device]),
    Sources = [
        pin_source(Device, Enum, Name)
        ||
        {_, Enum, Name} <- BSDLPins
    ],
    {ok, Experiments} = experiment:compile_to_rcf(Sources),
    lists:map(fun pin/1, Experiments).

%%--------------------------------------------------------------------

pin({Enum, RCF}) ->
    Pin = binary_to_atom(Enum),
    IOC = pin_ioc(RCF),
    {Pin, IOC}.

%%--------------------------------------------------------------------

pin_ioc(RCF) ->
    #{signals := #{lut := #{dests := [Dest]}}} = RCF,
    #{ioc := IOC, port := data_in} = Dest,
    IOC.

%%--------------------------------------------------------------------

pin_source(Device, Enum, Name) ->
    #{
        title => Enum,
        device => Device,
        settings => [
            {raw, <<"set_location_assignment -to q ", Name/binary, "\n">>}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => '0',\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

