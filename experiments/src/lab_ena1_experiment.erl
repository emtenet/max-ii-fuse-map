-module(lab_ena1_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's ena1
% source was selected between the two (2) LAB control lines.
%
% The two control lines are specifically selected by:
%
% * routing the global clocks 2 and 3 into local interconnects.
%   Global 2 can only be selected into interconnect 12 and then control 3.
%   Global 3 can only be selected into interconnect 25 and then control 2.
%
% * routing the clock through LC's 7 and 8. LC 7 can only be selected
%   into even control lines, and LC 8 into odd control lines.
%
% The enable line is off by default:
%
%  * {{lab, X, Y}, ena1, off}
%
% The enable line is selected between {control, 2} and {control, 3}:
%
%  * {{lab, X, Y}, ena1, control_2_not_3}
%
% The emable line can be inverted with:
%
%  * {{lab, X, Y}, ena1, invert}

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
    [Gclk, _, Ena2, Ena3] = Gclks,
    [Clk, D, Q | _] = Pins,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_always(Device, LAB, Clk, D, Q, always),
        source_global(Device, LAB, Clk, D, Q, gclk2, Ena2, <<"NOT ">>),
        source_global(Device, LAB, Clk, D, Q, gclk2, Ena2, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk3, Ena3, <<>>),
        source_local(Device, LAB, Clk, D, Q, local7, Gclk, 7),
        source_local(Device, LAB, Clk, D, Q, local8, Gclk, 8)
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
        ({{lc, _, _, _}, local_line}) -> true;
        ({_, lut, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %[
    %    io:format("  ~w ~p~n", [Name, hd(Route)])
    %    ||
    %    {Name,
    %     _,
    %     #{signals := #{ena := #{dests := [#{port := ena, route := Route}]}}}
    %    } <- Experiments
    %],
    %[
    %    io:format("  ~w ~p~n", [Name, hd(Route)])
    %    ||
    %    {Name,
    %     _,
    %     #{signals := #{ee := #{dests := [#{port := ena, route := Route}]}}}
    %    } <- Experiments
    %],
    %
    lists:foreach(fun control_is/1,
                  lists:zip(Experiments, [x,3,3,2,2,3])),
    %
    expect(Matrix, [0,1,1,1,1,1], {LAB, ena1, off}),
    expect(Matrix, [1,0,0,1,1,0], {LAB, ena1, control_3_not_2}),
    expect(Matrix, [1,0,1,1,1,1], {LAB, ena1, invert}),
    ok.

%%--------------------------------------------------------------------

control_is({{_, _, #{signals := Signals}}, Expect}) ->
    case Signals of
        #{ena := #{dests := [#{port := ena, route := Route}]}} ->
            [{lab_control_mux, _, _, 0, Expect} | _] = Route;

        #{ee := #{dests := [#{port := ena, route := Route}]}} ->
            [{lab_control_mux, _, _, 0, Expect} | _] = Route;

        _ when not is_map_key(ena, Signals) ->
            ok
    end.

%%--------------------------------------------------------------------

expect(Matrix, Pattern, Fuse) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
    ok.

%%--------------------------------------------------------------------

source_always(Device, LAB, Clk, D, Q, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {location, ff, lab:lc(LAB, 0)},
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
            "  ff: DFFE port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    ena => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, LAB, Clk, D, Q, Name, Ena, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {location, ena, Ena},
            {global_clock, ena, true},
            {location, ff, lab:lc(LAB, 0)},
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
            "    ena : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFFE port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    ena => ", Not/binary, "ena,\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, Clk, D, Q, Name, Ena, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {location, ena, Ena},
            {global_clock, ena, false},
            {location, ee, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
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
            "    ena : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "  signal e : STD_LOGIC;\n"
            "begin\n"
            "  ee: LCELL port map (\n"
            "    a_in => ena,\n"
            "    a_out => e\n"
            "  );\n"
            "  ff: DFFE port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    ena => e,\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

