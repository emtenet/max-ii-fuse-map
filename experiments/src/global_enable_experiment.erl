-module(global_enable_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    density(epm240),
    density(epm570),
    density(epm1270),
    density(epm2210),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" ==> ~p~n", [Density]),
    Device = density:largest_device(Density),
    [G0, G1, G2, G3 | _] = device:gclk_pins(Device),
    X = 2,
    Y = device:bottom_lab(X, Device),
    [P0 | _] = device:top_pins(X, Device),
    [D0, Q0, D1, Q1] = device:bottom_pins(X, Device),
    Settings = [
        {location, d0, D0},
        {location, d1, D1},
        {location, q0, Q0},
        {location, q1, Q1},
        {location, ff0, {lc, X, Y, 0}},
        {location, ff1, {lc, X, Y, 1}}
    ],
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source0(Device, G0, Settings),
        source1(Device, G0, Settings),
        source1(Device, G1, Settings),
        source1(Device, G2, Settings),
        source1(Device, G3, Settings),
        source1(Device, P0, Settings),
        source2(Device, G0, G1, Settings),
        source2(Device, G0, G2, Settings),
        source2(Device, G0, G3, Settings),
        source2(Device, G1, G2, Settings),
        source2(Device, G1, G3, Settings),
        source2(Device, G2, G3, Settings),
        source2(Device, G0, P0, Settings),
        source2(Device, G1, P0, Settings),
        source2(Device, G2, P0, Settings),
        source2(Device, G3, P0, Settings),
        source3(Device, G0, G1, G2, Settings),
        source3(Device, G0, G1, G3, Settings),
        source3(Device, G0, G2, G3, Settings),
        source3(Device, G1, G2, G3, Settings),
        source3(Device, G0, G1, P0, Settings),
        source3(Device, G0, G2, P0, Settings),
        source3(Device, G0, G3, P0, Settings),
        source3(Device, G1, G2, P0, Settings),
        source3(Device, G1, G3, P0, Settings),
        source3(Device, G2, G3, P0, Settings),
        source4(Device, G0, G1, G2, G3, Settings),
        source4(Device, P0, G1, G2, G3, Settings),
        source4(Device, G0, P0, G2, G3, Settings),
        source4(Device, G0, G1, P0, G3, Settings),
        source4(Device, G0, G1, G2, P0, Settings)
    ]),
    Matrix0 = matrix:build_with_map(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    Patterns = lists:foldr(fun patterns/2, #{
        {disable, 0} => [],
        {disable, 1} => [],
        {disable, 2} => [],
        {disable, 3} => [],
        {interconnect, 0} => [],
        {interconnect, 1} => [],
        {interconnect, 2} => [],
        {interconnect, 3} => []
    }, Experiments),
    maps:foreach(fun (Name, Pattern) ->
        fuse(Name, Pattern, Matrix)
    end, Patterns),
    ok.

%%--------------------------------------------------------------------

source0(Device, Clk, Settings) ->
    #{
        title => {local, Clk},
        device => Device,
        settings => [
            {location, clk, Clk}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    d0 : in STD_LOGIC;\n"
            "    d1 : in STD_LOGIC;\n"
            "    q0 : out STD_LOGIC;\n"
            "    q1 : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff0: DFF port map (\n"
            "    d => d0,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q0\n"
            "  );\n"
            "  ff1: DFF port map (\n"
            "    d => d1,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q1\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source1(Device, Clk, Settings) ->
    #{
        title => {global, Clk},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    d0 : in STD_LOGIC;\n"
            "    d1 : in STD_LOGIC;\n"
            "    q0 : out STD_LOGIC;\n"
            "    q1 : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff0: DFF port map (\n"
            "    d => d0,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q0\n"
            "  );\n"
            "  ff1: DFF port map (\n"
            "    d => d1,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q1\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source2(Device, Clk, Clr, Settings) ->
    #{
        title => {global, Clk, Clr},
        device => Device,
        settings => [
            {location, clk, Clk},
            {location, clr, Clr},
            {global_clock, clk, true},
            {global_clock, clr, true}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    clr : in STD_LOGIC;\n"
            "    d0 : in STD_LOGIC;\n"
            "    d1 : in STD_LOGIC;\n"
            "    q0 : out STD_LOGIC;\n"
            "    q1 : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff0: DFF port map (\n"
            "    d => d0,\n"
            "    clk => clk,\n"
            "    clrn => clr,\n"
            "    prn => '1',\n"
            "    q => q0\n"
            "  );\n"
            "  ff1: DFF port map (\n"
            "    d => d1,\n"
            "    clk => clk,\n"
            "    clrn => clr,\n"
            "    prn => '1',\n"
            "    q => q1\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source3(Device, Clk0, Clk1, Clr, Settings) ->
    #{
        title => {global, Clk0, Clk1, Clr},
        device => Device,
        settings => [
            {location, clk0, Clk0},
            {location, clk1, Clk1},
            {location, clr, Clr},
            {global_clock, clk0, true},
            {global_clock, clk1, true},
            {global_clock, clr, true}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk0 : in STD_LOGIC;\n"
            "    clk1 : in STD_LOGIC;\n"
            "    clr : in STD_LOGIC;\n"
            "    d0 : in STD_LOGIC;\n"
            "    d1 : in STD_LOGIC;\n"
            "    q0 : out STD_LOGIC;\n"
            "    q1 : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff0: DFF port map (\n"
            "    d => d0,\n"
            "    clk => clk0,\n"
            "    clrn => clr,\n"
            "    prn => '1',\n"
            "    q => q0\n"
            "  );\n"
            "  ff1: DFF port map (\n"
            "    d => d1,\n"
            "    clk => clk1,\n"
            "    clrn => clr,\n"
            "    prn => '1',\n"
            "    q => q1\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source4(Device, Clk0, Clk1, Clr0, Clr1, Settings) ->
    #{
        title => {global, Clk0, Clk1, Clr0, Clr1},
        device => Device,
        settings => [
            {location, clk0, Clk0},
            {location, clk1, Clk1},
            {location, clr0, Clr0},
            {location, clr1, Clr1},
            {global_clock, clk0, true},
            {global_clock, clk1, true},
            {global_clock, clr0, true},
            {global_clock, clr1, true}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk0 : in STD_LOGIC;\n"
            "    clk1 : in STD_LOGIC;\n"
            "    clr0 : in STD_LOGIC;\n"
            "    clr1 : in STD_LOGIC;\n"
            "    d0 : in STD_LOGIC;\n"
            "    d1 : in STD_LOGIC;\n"
            "    q0 : out STD_LOGIC;\n"
            "    q1 : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff0: DFF port map (\n"
            "    d => d0,\n"
            "    clk => clk0,\n"
            "    clrn => clr0,\n"
            "    prn => '1',\n"
            "    q => q0\n"
            "  );\n"
            "  ff1: DFF port map (\n"
            "    d => d1,\n"
            "    clk => clk1,\n"
            "    clrn => clr1,\n"
            "    prn => '1',\n"
            "    q => q1\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

patterns({_Name, _, #{signals := Signals}}, Patterns) ->
    Bits = maps:fold(fun patterns_signal/3, #{
        {disable, 0} => 0,
        {disable, 1} => 0,
        {disable, 2} => 0,
        {disable, 3} => 0,
        {interconnect, 0} => 1,
        {interconnect, 1} => 1,
        {interconnect, 2} => 1,
        {interconnect, 3} => 1
    }, Signals),
    maps:map(fun (Key, Pattern) ->
        #{Key := Bit} = Bits,
        [Bit | Pattern]
    end, Patterns).

%%--------------------------------------------------------------------

patterns_signal(_, #{dests := Dests}, Bits) ->
    lists:foldl(fun patterns_dest/2, Bits, Dests).

%%--------------------------------------------------------------------

patterns_dest(#{route := Route}, Bits) ->
    case Route of
        [{lab_clk,_,_,_,_},{global_clk_h,_,_,_,G},{clk_buffer,_,_,_,_}] ->
            Bits#{{disable, G} => 1};

        [_, _, {lab_clk,_,_,_,_},{global_clk_h,_,_,_,G},{clk_buffer,_,_,_,_}] ->
            Bits#{{disable, G} => 1};

        [{lab_clk,_,_,_,_},{global_clk_h,_,_,_,G},Mux | _] ->
            {global_clk_mux,_,_,_,G} = Mux,
            Bits#{{disable, G} => 1, {interconnect, G} => 0};

        [_, _, {lab_clk,_,_,_,_},{global_clk_h,_,_,_,G},Mux | _] ->
            {global_clk_mux,_,_,_,G} = Mux,
            Bits#{{disable, G} => 1, {interconnect, G} => 0};

        [{lab_control_mux,_,_,_,_} | _] ->
            Bits;

        [{local_interconnect,_,_,_,_} | _] ->
            Bits;

        [{io_data_out,_,_,_,_} | _] ->
            Bits
    end.

%%--------------------------------------------------------------------

fuse(Name, Pattern, Matrix) ->
    Fuses = matrix:pattern_is(Matrix, Pattern),
    io:format("~w: ~p~n", [Name, Fuses]).

