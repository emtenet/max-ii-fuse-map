-module(lc_clk_clr_experiment).

-export([run/0]).

% Find fuses per LC to select:
%
%  * a clock between lab-clk1 & lab-clk2,
%  * a clear between lab-clr1 & lab-clr2,
%
% {LC, clk} selects:
%
%   1 => lab-clk1
%   0 => lab-clk2
%
% {LC, clr} selects
%
%   1 => lab-clr1
%   0 => lab-clr2

%%====================================================================
%% run
%%====================================================================

run() ->
    Globals = [
        {gclk0, run_vhdl(<<"gclk0">>, <<"'1'">>)},
        {gclk0_gclr, run_vhdl(<<"gclk0">>, <<"gclr">>)},
        {gclk1, run_vhdl(<<"gclk1">>, <<"'1'">>)},
        {gclk1_gclr, run_vhdl(<<"gclk1">>, <<"gclr">>)}
    ],
    [
        run_density(Density, Globals)
        ||
        Density <- density:list()
    ],
    ok.

%%--------------------------------------------------------------------

run_density(Density, Globals) ->
    Device = density:largest_device(Density),
    [Gclk0, Gclk1, Gclk, _] = device:gclk_pins(Device),
    Pins = device:pins(Device),
    {[D0, Q0, D1, Q1, D, Q], _} = pins:choose(6, Pins, [Gclk0, Gclk1, Gclk]),
    Settings = [
        {global_clock, gclk0, true},
        {global_clock, gclk1, true},
        {global_clock, gclr, true},
        {location, gclk0, Gclk0},
        {location, gclk1, Gclk1},
        {location, gclr, Gclk},
        {location, d0, D0},
        {location, q0, Q0},
        {location, d1, D1},
        {location, q1, Q1},
        {location, d, D},
        {location, q, Q}
    ],
    [
        run_lc(LAB, N, Density, Device, Globals, Settings)
        ||
        LAB <- device:labs(Device),
        N <- lists:seq(0, 9)
    ],
    ok.

%%--------------------------------------------------------------------

run_control(0) -> {1, 2};
run_control(1) -> {0, 2};
run_control(_) -> {0, 1}.

%%--------------------------------------------------------------------

run_lc(LAB, N, Density, Device, Globals, Settings0) ->
    LC = lab:lc(LAB, N),
    io:format(" => ~s ~p~n", [Device, LC]),
    {N0, N1} = run_control(N),
    Settings = [
        {location, ff0, lab:lc(LAB, N0)},
        {location, ff1, lab:lc(LAB, N1)},
        {location, ff, LC}
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
        {Name, VHDL} <- Globals
    ]),
    %Matrix = matrix:build(Density, Experiments),
    %matrix:print(Matrix),
    Matrix = matrix:build(Experiments),
    % clk1 -> 1
    % clk2 -> 0
    % clr1 -> 1
    % clr2 -> 0
    [Clk] = matrix:pattern(Matrix, [1,1,0,0]),
    [Clr] = matrix:pattern(Matrix, [1,0,1,0]),
    fuse_database:update(Density, [
        {Clk, {LC, clk}},
        {Clr, {LC, clr}}
    ]).

%%--------------------------------------------------------------------

run_vhdl(Clk, Clr) ->
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
        "    gclr : in STD_LOGIC;\n"
        "    d0 : in STD_LOGIC;\n"
        "    d1 : in STD_LOGIC;\n"
        "    d : in STD_LOGIC;\n"
        "    q0 : out STD_LOGIC;\n"
        "    q1 : out STD_LOGIC;\n"
        "    q : out STD_LOGIC\n"
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is\n"
        "begin\n"
        "  ff0: DFF port map (\n"
        "    d => d0,\n"
        "    clk => gclk0,\n"
        "    clrn => gclr,\n"
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
        "  ff: DFF port map (\n"
        "    d => d,\n"
        "    clk => ", Clk/binary, ",\n"
        "    clrn => ", Clr/binary, ",\n"
        "    prn => '1',\n"
        "    q => q\n"
        "  );\n"
        "end behavioral;\n"
    >>.
