-module(lab_clk1_playground).

-export([run/0]).

% This experiment was designed to look at how the LAB's clk1
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% Whilst still in playground status the following findings have
% been discovered:
%
% The fuse to select from the LAB control lines (rather than
% the global networks) seems to be:
%
%   {2,1,line,20,cell,15} -> {{lab,2,1},clk1,control}
%   {3,1,line,20,cell,15} -> {{lab,3,1},clk1,control}
%   {4,2,line,20,cell,15} -> {{lab,4,2},clk1,control}
%
% The LAB s-load is enabled/... with fuse:
%   {2,1,line,20,cell,20} -> {lab,2,1} s_load
%
% Each LC's s-load is enabled/... with fuse:
%   {2,1,0,0,cell,21}     -> {lc,2,1,0} s_load
%   {2,1,1,0,cell,21}     -> {lc,2,1,1} s_load
%
% The {{global,_},disable#} fuses are wrong!
% What appeared to be a pair (disable0 & disable1) of fuses
% per global network are probably:
%
%   * a row disable, per network)
%   * a column disable, per column, per network
%
% X == 2
%  1171 | | |*| | {{global,2},disable0} - row
%  2707 | | | |*| {{global,3},disable0} - row
%  3731 | |*| | | {{global,1},disable1} - row
%  4755 |*| | | | {{global,0},disable1} - row
%  3219 |*| | | | {{global,0},disable0} - column 2
%  3475 | |*| | | {{global,1},disable0} - column 2
%  3987 | | |*| | {{global,2},disable1} - column 2
%  4243 | | | |*| {{global,3},disable1} - column 2
%
% X == 3
%  1171 | | |*| | {{global,2},disable0} - row
%  2707 | | | |*| {{global,3},disable0} - row
%  3731 | |*| | | {{global,1},disable1} - row
%  4755 |*| | | | {{global,0},disable1} - row
% 10387 |*| | | | {2,skip,103,cell,27} - column 3
% 10643 | |*| | | {3,skip,103,cell, 0} - column 3
% 11155 | | |*| | {3,skip,103,cell, 2} - column 3
% 11411 | | | |*| {3,skip,103,cell, 3} - column 3
%
% X == 4
%  1171 | | |*| | {{global,2},disable0} - row
%  2707 | | | |*| {{global,3},disable0} - row
%  3731 | |*| | | {{global,1},disable1} - row
%  4755 |*| | | | {{global,0},disable1} - row
% 17555 |*| | | | {3,skip,103,cell,27} - column 4
% 17811 | |*| | | {4,skip,103,cell, 0} - column 4
% 18323 | | |*| | {4,skip,103,cell, 2} - column 4
% 18579 | | | |*| {4,skip,103,cell, 3} - column 4

%%====================================================================
%% run
%%====================================================================

run() ->
    density(epm240),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [Gclk0, Gclk1, Gclk2, Gclk3] = device:gclk_pins(Device),
    X = 7,
    Y = density:bottom_lab(X, Density) + 1,
    FF = {lc, X, Y, 0},
    [D, Q | _] = device:bottom_pins(X, Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, gclk0, Gclk0, true, FF, D, Q),
        source(Device, gclk1, Gclk1, true, FF, D, Q),
        source(Device, gclk2, Gclk2, true, FF, D, Q),
        source(Device, gclk3, Gclk3, true, FF, D, Q)
        |
        [
            source(Device, Pin, Pin, false, FF, D, Q)
            ||
            Column <- density:columns(Density),
            Pin <- device:top_pins(Column, Device)
        ]
    ]),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, skip, _, _, _}) -> false;
        ({XX, _, _, _, _, _}) -> XX =/= X;
        ({{global, _}, _}) -> false;
        ({{global, _}, _, _}) -> false;
        ({{lab, XX, YY}, _}) -> XX =/= X orelse YY =/= Y;
        ({{lab, XX, YY}, _, _}) -> XX =/= X orelse YY =/= Y;
        ({{lab, XX, YY}, _, _, _}) -> XX =/= X orelse YY =/= Y;
        ({{lc, XX, YY, _}, _}) -> XX =/= X orelse YY =/= Y;
        ({{lc, XX, YY, _}, _, _}) -> XX =/= X orelse YY =/= Y;
        ({{lc, XX, YY, _}, _, _, _}) -> XX =/= X orelse YY =/= Y;
        (_) -> true
    end),
    matrix:print(Matrix),
    %[
    %    io:format("~w: ~w~n", [Name, Signals])
    %    ||
    %    {Name, _, #{signals := Signals}} <- Experiments
    %],
    ok.

%%--------------------------------------------------------------------

source(Device, Name, Clk, Global, FF, D, Q) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, Global},
            {location, ff, FF},
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

