-module(lab_clr1_global_experiment).

-export([run/0]).

% Determine LAB's selection & invert fuses for:
%
% * clr1
%
% focusing just on selection of global signals in this experiment.
%
% Two control LABs with two FF and two gclks per LAB so that all
% four global signals are always enabled.
%
% Experiment LAB with a FF (clk1 and clr1)
%
%  * clk1 = gclk3, clr1 = gclk0
%  * clk1 = gclk3, clr1 = ~gclk0
%  * clk1 = gclk3, clr1 = gclk1
%  * clk1 = gclk3, clr1 = gclk2
%  * clk1 = gclk0, clr1 = gclk3
%
% The known {lab(), clk1, global?} fuses can be excluded from the
% matrix to leave the clr1 fuses.

%%====================================================================
%% run
%%====================================================================

run() ->
    Sources = [
        {gclk0, run_vhdl(<<"gclk3">>, <<"gclk0">>)},
        {not_gclk0, run_vhdl(<<"gclk3">>, <<"NOT gclk0">>)},
        {gclk1, run_vhdl(<<"gclk3">>, <<"gclk1">>)},
        {gclk2, run_vhdl(<<"gclk3">>, <<"gclk2">>)},
        {gclk3, run_vhdl(<<"gclk0">>, <<"gclk3">>)}
    ],
    [
        run_density(Density, Sources)
        ||
        Density <- density:list()
    ],
    ok.

%%--------------------------------------------------------------------

run_density(Density, Sources) ->
    Device = density:largest_device(Density),
    [Gclk0, Gclk1, Gclk2, Gclk3] = Gclks = device:gclk_pins(Device),
    Pins = device:pins(Device),
    {[D0, Q0, D1, Q1, D2, Q2, D3, Q3, D, Q], _} = pins:choose(10, Pins, Gclks),
    Settings = [
        {global_clock, gclk0, true},
        {global_clock, gclk1, true},
        {global_clock, gclk2, true},
        {global_clock, gclk3, true},
        {location, gclk0, Gclk0},
        {location, gclk1, Gclk1},
        {location, gclk2, Gclk2},
        {location, gclk3, Gclk3},
        {location, d0, D0},
        {location, q0, Q0},
        {location, d1, D1},
        {location, q1, Q1},
        {location, d2, D2},
        {location, q2, Q2},
        {location, d3, D3},
        {location, q3, Q3},
        {location, d, D},
        {location, q, Q}
    ],
    [X, Y | LABs = [Z | _]] = device:labs(Device),
    run_lab(Density, Device, Sources, Settings, X, Y, Z),
    run_lab(Density, Device, Sources, Settings, Y, X, Z),
    run_labs(Density, Device, Sources, Settings, X, Y, LABs).

%%--------------------------------------------------------------------

run_labs(_, _, _, _, _, _, []) ->
    ok;
run_labs(Density, Device, Sources, Settings, X, Y, [LAB | LABs]) ->
    run_lab(Density, Device, Sources, Settings, LAB, X, Y),
    run_labs(Density, Device, Sources, Settings, X, LAB, LABs).

%%--------------------------------------------------------------------

run_lab(Density, Device, Sources, Settings0, LAB, X, Y) ->
    io:format(" => ~s ~p~n", [Device, LAB]),
    Settings = [
        {location, ff0, lab:lc(X, 0)},
        {location, ff1, lab:lc(X, 1)},
        {location, ff2, lab:lc(Y, 0)},
        {location, ff3, lab:lc(Y, 1)},
        {location, ff, lab:lc(LAB, 0)}
        |
        Settings0
    ],
    {ok, Experiments} = experiment:compile_to_fuses([
        #{
            title => Name,
            device => Device,
            settings => Settings,
            vhdl => VHDL
        }
        ||
        {Name, VHDL} <- Sources
    ]),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, [
        {LAB, clk1, global0},
        {LAB, clk1, global3}
    ]),
    %matrix:print(Matrix),
    [{Clr1Global0, _}] = matrix:pattern_is(Matrix, [0,0,1,1,1]),
    [{Clr1Global1, _}] = matrix:pattern_is(Matrix, [1,1,0,1,1]),
    [{Clr1Global2, _}] = matrix:pattern_is(Matrix, [1,1,1,0,1]),
    [{Clr1Global3, _}] = matrix:pattern_is(Matrix, [1,1,1,1,0]),
    [{Clr1Invert, _}]  = matrix:pattern_is(Matrix, [1,0,1,1,1]),
    fuse_database:update(Density, [
        {Clr1Global0, {LAB, clr, global0}},
        {Clr1Global1, {LAB, clr, global1}},
        {Clr1Global2, {LAB, clr, global2}},
        {Clr1Global3, {LAB, clr, global3}},
        {Clr1Invert, {LAB, clr1, invert}}
    ]).

%%--------------------------------------------------------------------

run_vhdl(Clk1, Clr1) ->
    <<
        "library IEEE;\n"
        "use IEEE.STD_LOGIC_1164.ALL;\n"
        "library altera;\n"
        "use altera.altera_primitives_components.all;\n"
        "\n"
        "entity experiment is\n"
        "  port (\n"
        "    gclk0 : in STD_LOGIC;\n"
        "    gclk1 : in STD_LOGIC;\n"
        "    gclk2 : in STD_LOGIC;\n"
        "    gclk3 : in STD_LOGIC;\n"
        "    d0 : in STD_LOGIC;\n"
        "    q0 : out STD_LOGIC;\n"
        "    d1 : in STD_LOGIC;\n"
        "    q1 : out STD_LOGIC;\n"
        "    d2 : in STD_LOGIC;\n"
        "    q2 : out STD_LOGIC;\n"
        "    d3 : in STD_LOGIC;\n"
        "    q3 : out STD_LOGIC;\n"
        "    d : in STD_LOGIC;\n"
        "    q : out STD_LOGIC\n"
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is\n"
        "begin\n"
        "  ff0: DFF port map (\n"
        "    d => d0,\n"
        "    clk => gclk0,\n"
        "    clrn => '1',\n"
        "    prn => '1',\n"
        "    q => q0\n"
        "  );\n"
        "  ff1: DFF port map (\n"
        "    d => d1,\n"
        "    clk => gclk1,\n"
        "    clrn => '1',\n"
        "    prn => '1',\n"
        "    q => q1\n"
        "  );\n"
        "  ff2: DFF port map (\n"
        "    d => d2,\n"
        "    clk => gclk2,\n"
        "    clrn => '1',\n"
        "    prn => '1',\n"
        "    q => q2\n"
        "  );\n"
        "  ff3: DFF port map (\n"
        "    d => d3,\n"
        "    clk => gclk3,\n"
        "    clrn => '1',\n"
        "    prn => '1',\n"
        "    q => q3\n"
        "  );\n"
        "  ff: DFF port map (\n"
        "    d => d,\n"
        "    clk => ", Clk1/binary, ",\n"
        "    clrn => ", Clr1/binary, ",\n"
        "    prn => '1',\n"
        "    q => q\n"
        "  );\n"
        "end behavioral;\n"
    >>.
