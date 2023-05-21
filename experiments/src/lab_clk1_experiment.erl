-module(lab_clk1_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's clk1
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% The first layer is a one-hot selection of:
%
%  * {{lab, X, Y}, clk1, global0}
%  * {{lab, X, Y}, clk1, global1}
%  * {{lab, X, Y}, clk1, global2}
%  * {{lab, X, Y}, clk1, global3}
%  * {{lab, X, Y}, clk1, control}
%
%  Then in control is selected above, a further fuse:
%
%  * {{lab, X, Y}, clk1, control_0_not_1}
%
%  selects between {control, 0} and {control, 1}.
%
% Sometimes the FF uses s-data instead of the LUT for its input, in
% those cases some s-load fuses can be matched when looking for the
% above fuses. Check for these s-load fuses while we are at it:
%
%  * {{lab, X, Y}, s_load, off}
%  * {{lc, X, Y, N}, s_load}

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
    [D, Q | _] = Pins,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_global(Device, LAB, D, Q, gclk0, Gclk0, <<"NOT ">>),
        source_global(Device, LAB, D, Q, gclk0, Gclk0, <<>>),
        source_global(Device, LAB, D, Q, gclk1, Gclk1, <<>>),
        source_global(Device, LAB, D, Q, gclk2, Gclk2, <<>>),
        source_global(Device, LAB, D, Q, gclk3, Gclk3, <<>>),
        source_local(Device, LAB, D, Q, local7, Gclk0, 7),
        source_local(Device, LAB, D, Q, local8, Gclk0, 8)
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
    %    io:format("  ~w ~w~n", [Name, Port])
    %    ||
    %    {Name,
    %     _,
    %     #{signals := #{d := #{dests := [#{port := Port}]}}}
    %    } <- Experiments
    %],
    %
    expect(Matrix, [0,0,1,1,1,1,1], {LAB, clk1, global0}),
    expect(Matrix, [1,1,0,1,1,1,1], {LAB, clk1, global1}),
    expect(Matrix, [1,1,1,0,1,1,1], {LAB, clk1, global2}),
    expect(Matrix, [1,1,1,1,0,1,1], {LAB, clk1, global3}),
    %
    lists:foreach(fun clk_not_s_data/1, Experiments),
    %
    Control = {LAB, clk1, control},
    Control0 = {LAB, clk1, control_0_not_1},
    Sload = {lab:lc(LAB, 0), s_load},
    SloadOff = {LAB, s_load, off},
    case s_load_patterns(Experiments) of
        {[0,0,0,0,0,0,0], _} ->
            expect(Matrix, [1,1,1,1,1,0,0], Control),
            expect(Matrix, [1,1,1,1,1,0,1], Control0);

        {[1,1,1,1,1,1,1], _} ->
            expect(Matrix, [1,1,1,1,1,0,0], Control),
            expect(Matrix, [1,1,1,1,1,0,1], Control0);

        {[0,0,0,0,0,1,1], _} ->
            expect(Matrix, [0,0,0,0,0,1,1], SloadOff),
            expect(Matrix, [1,1,1,1,1,0,0], Control, Sload),
            expect(Matrix, [1,1,1,1,1,0,1], Control0);

        {[1,1,1,1,1,0,0], _} ->
            expect(Matrix, [0,0,0,0,0,1,1], Sload),
            expect(Matrix, [1,1,1,1,1,0,0], Control, SloadOff),
            expect(Matrix, [1,1,1,1,1,0,1], Control0)
    end,
    ok.

%%--------------------------------------------------------------------

expect(Matrix, Pattern, Fuse) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
    ok.

%%--------------------------------------------------------------------

expect(Matrix, Pattern, Fuse1, Fuse2) ->
    case matrix:pattern_is(Matrix, Pattern) of
        [{_, Fuse1}, {_, Fuse2}] ->
            ok;

        [{_, Fuse2}, {_, Fuse1}] ->
            ok
    end.

%%--------------------------------------------------------------------

clk_not_s_data({_, _, #{signals := #{clk := #{dests := [#{port := Port}]}}}}) ->
    case Port of
        clk ->
            ok;

        data_a ->
            ok
    end.

%%--------------------------------------------------------------------

s_load_patterns(Experiments) ->
    LAB = lists:map(fun (Experiment) ->
        s_load_pattern_bit(Experiment, 1)
    end, Experiments),
    LC = lists:map(fun (Experiment) ->
        s_load_pattern_bit(Experiment, 0)
    end, Experiments),
    {LAB, LC}.

%%--------------------------------------------------------------------

s_load_pattern_bit(Experiment, Bit) ->
    {_, _, #{signals := #{d := #{dests := [#{port := Port}]}}}} = Experiment,
    case Port of
        s_data ->
            Bit;

        data_a ->
            1 - Bit
    end.

%%--------------------------------------------------------------------

source_global(Device, LAB, D, Q, Name, Clk, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
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
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => ", Not/binary, "clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, D, Q, Name, Clk, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, false},
            {location, cc, lab:lc(LAB, N)},
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
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "  signal c : STD_LOGIC;\n"
            "begin\n"
            "  cc: LCELL port map (\n"
            "    a_in => clk,\n"
            "    a_out => c\n"
            "  );\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => c,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

