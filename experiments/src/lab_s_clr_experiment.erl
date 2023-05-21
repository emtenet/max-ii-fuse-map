-module(lab_s_clr_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's s-clr
% source was selected between the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% It appears that the s-clr line can only be enabled for all LCs in
% a LAB, not one at a time.
%
% The LAB's s-clr line is enabled with:
%
%  * {{lab, X, Y}, s_clr}
%
% The s-clr line is selected between {control, 4} and {control, 5}:
%
%  * {{lab, X, Y}, s_clr, control_5_not_4}
%
% The s-clr line can be inverted with:
%
%  * {{lab, X, Y}, s_clr, invert}

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    device(Density, Device).

%%--------------------------------------------------------------------

device(Density, Device) ->
    Gclks = device:gclk_pins(Device),
    Pins = lists:subtract(device:pins(Device), Gclks),
    [
        block(Density, Device, LAB, Gclks, Pins)
        ||
        LAB <- device:labs(Device)
    ],
    ok.

%%--------------------------------------------------------------------

block(Density, Device, LAB, Gclks, Pins) ->
    io:format(" ==> ~p ~p~n", [Density, LAB]),
    [Clk, Clr, _, _] = Gclks,
    [D, Q | _] = Pins,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_never(Device, LAB, Clk, D, Q, never),
        source_global(Device, LAB, Clk, D, Q, global, Clr, <<"NOT ">>),
        source_global(Device, LAB, Clk, D, Q, global_not, Clr, <<>>),
        source_local(Device, LAB, Clk, D, Q, local7, Clr, 7),
        source_local(Device, LAB, Clk, D, Q, local8, Clr, 8)
    ]),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _, _}) -> true;
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        ({{lab, _, _}, {control, _}, _, _}) -> true;
        ({{lab, _, _}, {interconnect, _}, _}) -> true;
        ({{lab, _, _}, {interconnect, _}, _, _}) -> true;
        ({{lab, _, _}, s_load, off}) -> true;
        ({{lc, _, _, _}, local_line}) -> true;
        ({{lc, _, _, _}, s_load}) -> true;
        ({_, lut, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %
    expect_fuse(Matrix, [1,0,0,0,0], {LAB, s_clr}),
    expect_fuse(Matrix, [1,0,1,1,1], {LAB, s_clr, invert}),
    %
    [_, _, _, Local7, Local8] = Experiments,
    expect_s_clr(Local7, cc, 4),
    expect_s_clr(Local8, cc, 5),
    %
    Control = control_pattern(Experiments),
    expect_fuse(Matrix, Control, {LAB, s_clr, control_5_not_4}),
    ok.

%%--------------------------------------------------------------------

expect_fuse(Matrix, Pattern, Fuse) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
    ok.

%%--------------------------------------------------------------------

expect_s_clr({_, _, #{signals := Signals}}, Signal, Control) ->
    #{Signal := #{dests := [#{port := s_clr, route := Route}]}} = Signals,
    [{lab_control_mux, _, _, 0, Control} | _] = Route.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        control_pattern_bit(Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(#{cc := #{dests := [#{port := s_clr, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 4} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 5} | _] ->
            0
    end;
control_pattern_bit(#{clr := #{dests := [#{port := s_clr, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 4} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 5} | _] ->
            0
    end;
control_pattern_bit(_) ->
    1.

%%--------------------------------------------------------------------

source_never(Device, LAB, Clk, D, Q, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, q, Q}
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
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, LAB, Clk, D, Q, Name, Clr, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, q, Q}
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
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFFEAS port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    ena => '1',\n"
            "    asdata => '1',\n"
            "    aload => '0',\n"
            "    sclr => ", Not/binary, "clr,\n"
            "    sload => '0',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, Clk, D, Q, Name, Clr, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, q, Q}
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
            "    clk : in STD_LOGIC;\n"
            "    clr : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "  signal c : STD_LOGIC;\n"
            "begin\n"
            "  cc: LCELL port map (\n"
            "    a_in => clr,\n"
            "    a_out => c\n"
            "  );\n"
            "  ff: DFFEAS port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    ena => '1',\n"
            "    asdata => '1',\n"
            "    aload => '0',\n"
            "    sclr => c,\n"
            "    sload => '0',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

