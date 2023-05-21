-module(lab_clr1_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's clr1
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% The clr1 line is turned off for the whole LAB with fuse:
%
%  * {{lab, X, Y}, clr1, off}
%
% The following fuse is for sourceing from global lines (or off):
%
%  * {{lab, X, Y}, clr1, global}
%
% And then the particular global is selected with:
%
%  * {{lab, X, Y}, clr1, global0}
%  * {{lab, X, Y}, clr1, global1}
%  * {{lab, X, Y}, clr1, global2}
%  * {{lab, X, Y}, clr1, global3}
%
% Otherwise a selection between control 4 or control 5 is made with:
%
%  * {{lab, X, Y}, clr1, control_5_not_4}
%
% The clr1 line can be inverted with:
%
%  * {{lab, X, Y}, clr1, invert}
%
% NOTE: Not sure why the clr1 lines is always inverted when not selecting
% from a global clock network?

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
    [Gclk0, Gclk1, Gclk2, Gclk3] = Gclks,
    [Clk, D, Q | _] = Pins,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_never(Device, LAB, Clk, D, Q, never),
        source_global(Device, LAB, Clk, D, Q, gclk0_not, Gclk0, <<"NOT ">>),
        source_global(Device, LAB, Clk, D, Q, gclk0, Gclk0, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk1, Gclk1, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk2, Gclk2, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk3, Gclk3, <<>>),
        source_local(Device, LAB, Clk, D, Q, local7, Gclk0, 7),
        source_local(Device, LAB, Clk, D, Q, local8, Gclk0, 8)
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
        ({{lab, _, _}, clk1, _}) -> true;
        ({{lab, _, _}, s_load, _}) -> true;
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
    %[
    %    io:format("  ~w ~w~n", [Name, Route])
    %    ||
    %    {Name,
    %     _,
    %     #{signals := #{clr := #{dests := [#{port := a_clr, route := Route}]}}}
    %    } <- Experiments
    %],
    %[
    %    io:format("  ~w ~w~n", [Name, Route])
    %    ||
    %    {Name,
    %     _,
    %     #{signals := #{cc := #{dests := [#{port := a_clr, route := Route}]}}}
    %    } <- Experiments
    %],
    %
    expect_fuse(Matrix, [0,1,1,1,1,1,1,1], {LAB, clr1, off}),
    expect_fuse(Matrix, [1,0,1,1,1,1,0,0], {LAB, clr1, invert}),
    expect_fuse(Matrix, [1,0,0,1,1,1,1,1], {LAB, clr1, global0}),
    expect_fuse(Matrix, [1,1,1,0,1,1,1,1], {LAB, clr1, global1}),
    expect_fuse(Matrix, [1,1,1,1,0,1,1,1], {LAB, clr1, global2}),
    expect_fuse(Matrix, [1,1,1,1,1,0,1,1], {LAB, clr1, global3}),
    expect_fuse(Matrix, [0,0,0,0,0,0,1,1], {LAB, clr1, global}),
    %
    [_, _, _, _, _, _, Local7, Local8] = Experiments,
    expect_a_clr(Local7, cc, 4),
    expect_a_clr(Local8, cc, 5),
    %
    Control = control_pattern(Experiments),
    expect_fuse(Matrix, Control, {LAB, clr1, control_5_not_4}),
    ok.

%%--------------------------------------------------------------------

expect_fuse(Matrix, Pattern, Fuse) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
    ok.

%%--------------------------------------------------------------------

expect_a_clr({_, _, #{signals := Signals}}, Signal, Control) ->
    #{Signal := #{dests := [#{port := a_clr, route := Route}]}} = Signals,
    [{lab_control_mux, _, _, 0, Control} | _] = Route.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        control_pattern_bit(Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(#{cc := #{dests := [#{port := a_clr, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 4} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 5} | _] ->
            0
    end;
control_pattern_bit(#{clr := #{dests := [#{port := a_clr, route := Route}]}}) ->
    case Route of
        [{lab_control_mux, _, _, 0, 4} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 5} | _] ->
            0;

        [{lab_clk, _, _, 0, _} | _] ->
            1
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
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => ", Not/binary, "clr,\n"
            "    prn => '1',\n"
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
            {not_gate_push_back, true},
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
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
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => c,\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

