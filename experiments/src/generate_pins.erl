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
    lists:reverse(Lines, [Line | <<".\n">>]);
pin_type([{_, Enum, _} | Pins], Lines) ->
    Line = [
        <<"    ", Enum/binary>>,
        <<" |\n">>
    ],
    pin_type(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

pin_func([], [[Line, <<";\n">>] | Lines]) ->
    lists:reverse(Lines, [Line | <<".\n">>]);
pin_func([{_, Enum, Name} | Pins], Lines) ->
    Line = [
        <<"name(", Enum/binary, ") -> <<\"", Name/binary, "\">>">>,
        <<";\n">>
    ],
    pin_func(Pins, [Line | Lines]).

%%====================================================================
%% device_file
%%====================================================================

device_file({Device, Pins0}) ->
    Pins = [ device_pin(Device, Pin) || Pin <- Pins0 ],
    Data = [<<
        "-module(">>, atom_to_binary(Device), <<").\n"
        "\n"
        "-export([pins/0]).\n"
        "\n"
        "-type lc() :: lc:lc().\n"
        "-type pin() :: pin:pin().\n"
        "\n"
        "-spec pins() -> [{pin(), lc()}].\n"
        "\n"
        "pins() ->\n">>,
        device_pins(Pins, []), <<
        "\n"
    >>],
    Name = lists:flatten(io_lib:format("~s.erl", [Device])),
    File = filename:join("src", Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

device_pins([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last | <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_pins([{Enum, IOC} | Pins], Lines) ->
    Line = [
        <<"     ">>,
        io_lib:format("{~s,~p}", [Enum, IOC]),
        <<",\n">>
    ],
    device_pins(Pins, [Line | Lines]).

%%====================================================================
%% device_pin
%%====================================================================

device_pin(Device, {_, Enum, Name}) ->
    io:format(" => ~s ~s~n", [Device, Enum]),
    {ok, Cache} = quartus:cache(#{
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
    }),
    {ok, RCF} = quartus:rcf(Cache),
    #{signals := #{lut := #{dests := [Dest]}}} = RCF,
    #{ioc := IOC, port := data_in} = Dest,
    {Enum, IOC}.

