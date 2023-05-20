-module(lab_interconnect_global).

-export([run/0]).

% This experiment sends global networks in to the LAB interconnects.
%
% It appears to only be to interconnects 12 and 25 with the from4
% mux being suplimented by an additional one-hot entry.
%
% fuse: {X,Y,4,0,cell,4}
% name: {{lab,X,Y},{interconnect,12},from4,gclk}
%
% fuse: {X,Y,4,0,cell,4}
% name: {{lab,X,Y},{interconnect,25},from4,gclk}

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    lists:foreach(fun (LAB) -> block(Density, LAB) end, density:labs(Density)).

%%--------------------------------------------------------------------

block(Density, LAB) ->
    io:format(" ==> ~p ~p~n", [Density, LAB]),
    Device = density:largest_device(Density),
    Ds = device:gclk_pins(Device),
    [Q | _] = device:pins(Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        experiment(Device, LAB, Ds, Q, true, true, true, true),
        experiment(Device, LAB, Ds, Q, true, true, true, false),
        experiment(Device, LAB, Ds, Q, true, true, false, true),
        experiment(Device, LAB, Ds, Q, true, true, false, false),
        experiment(Device, LAB, Ds, Q, true, false, true, true),
        experiment(Device, LAB, Ds, Q, true, false, true, false),
        experiment(Device, LAB, Ds, Q, true, false, false, true),
        experiment(Device, LAB, Ds, Q, true, false, false, false),
        experiment(Device, LAB, Ds, Q, false, true, true, true),
        experiment(Device, LAB, Ds, Q, false, true, true, false),
        experiment(Device, LAB, Ds, Q, false, true, false, true),
        experiment(Device, LAB, Ds, Q, false, true, false, false),
        experiment(Device, LAB, Ds, Q, false, false, true, true),
        experiment(Device, LAB, Ds, Q, false, false, true, false),
        experiment(Device, LAB, Ds, Q, false, false, false, true),
        experiment(Device, LAB, Ds, Q, false, false, false, false)
    ]),
    _ = Experiments,
    %Minimal = {minimal, epm240_minimal:fuses()},
    %Matrix0 = matrix:build_with_map(epm240, [Minimal | Experiments]),
    %Matrix = matrix:remove_fuses(Matrix0, fun
    %    ({{lc, _, _, _}, lut, _}) -> true;
    %    ({{lab, 2, 3}, _}) -> false;
    %    ({{lab, 2, 3}, _, _}) -> false;
    %    ({{lab, 2, 3}, _, _, _}) -> false;
    %    ({{lc, 2, 3, _}, _}) -> false;
    %    ({{lc, 2, 3, _}, _, _}) -> false;
    %    ({{lc, 2, 3, _}, _, _, _}) -> false;
    %    ({2, 3, _, _, cell, _}) -> false;
    %    ({2, 3, line, _, cell, _}) -> false;
    %    (_) -> true
    %end),
    %matrix:print(Matrix),
    %lists:foreach(fun routes/1, Experiments),
    ok.

%%--------------------------------------------------------------------

%routes({Experiment, _, #{signals := Signals}}) ->
%    io:format(" ==> ~p~n", [Experiment]),
%    maps:foreach(fun (Signal, #{dests := Dests}) ->
%        lists:foreach(fun (#{port := Port, route := Route}) ->
%            io:format("  ~w -> ~w ~w~n", [Signal, Route, Port])
%        end, Dests)
%    end, Signals).

%%--------------------------------------------------------------------

experiment(Device, LAB, [PinA, PinB, PinC, PinD], Q, A, B, C, D) ->
    #{
        title => {LAB, A, B, C, D},
        device => Device,
        settings => [
            {global_clock, a, A},
            {global_clock, b, B},
            {global_clock, c, C},
            {global_clock, d, D},
            {location, a, PinA},
            {location, b, PinB},
            {location, c, PinC},
            {location, d, PinD},
            {location, lut, lab:lc(LAB, 0)},
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

