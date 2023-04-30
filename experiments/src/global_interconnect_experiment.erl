-module(global_interconnect_experiment).

-export([run/0]).

% This experiment explores the selection of local interconnects into
% the four global clock networks.
%
% Densities larger than the EPM240 appear to have have:
%
%  * a 4x3 selection mux per global network, and
%  * ten (10) local interconnects that select sources
%    via a 4x3 mux and a direct-link.
%
% These resources are located in row 3 of the "grow" column (9, 11, 13).
%
% The EPM240 is different re-using the 1,3 IO-block's:
%  * unused output/enable 6x3 selection muxes for the global networks, and
%  * sharing the IO-blocks 18 local interconnects.

%%====================================================================
%% run
%%====================================================================

run() ->
    density(epm240, left),
    density(epm240, right),
    density(epm570, left),
    density(epm570, right),
    density(epm1270, left),
    density(epm1270, right),
    density(epm2210, left),
    density(epm2210, right),
    ok.

%%--------------------------------------------------------------------

density(Density, left) ->
    Device = density:largest_device(Density),
    [D, Q | _] = device:gclk_pins(Device),
    density(Density, Device, D, Q);
density(Density, right) ->
    Device = density:largest_device(Density),
    [_, _, D, Q | _] = device:gclk_pins(Device),
    density(Density, Device, D, Q).

%%--------------------------------------------------------------------

density(Density, Device, D, Q) ->
    IOBs = device:iobs(Device),
    IOCs = device:iocs(Device),
    {D, IOC} = lists:keyfind(D, 1, IOCs),
    IOB = ioc:iob(IOC),
    {_, LAB} = lists:keyfind(IOB, 1, IOBs),
    LC = lab:lc(LAB, 0),
    Pins = [ Pin || {Pin, _} <- lists:keydelete(D, 1, lists:keydelete(Q, 1, IOCs)) ],
    Sources = sources(Device, D, Q, LC, Pins, []),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{global, _}, _, _}) -> false;
        ({Block = {iob, _, _}, {interconnect, _}, _, _}) ->
            remove_interconnect(Density, Block);
        ({Block = {iob, _, _}, {interconnect, _}, _, _, _}) ->
            remove_interconnect(Density, Block);
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        (_) -> false
    end),
    matrix:print(Matrix),
    [
        io:format(" ==> ~w~n  clk <- ~w~n  clr <- ~w~n  pr  <- ~w~n  ena <- ~w~n", [
            Name,
            route(clk, Signals),
            route(clr, Signals),
            route(pr, Signals),
            route(ena, Signals)
        ])
        ||
        {Name, _, #{signals := Signals}} <- Experiments
    ],
    ok.

%%--------------------------------------------------------------------

remove_interconnect(epm240, {iob, 1, 3}) -> false;
remove_interconnect(epm570, {iob, 9, 3}) -> false;
remove_interconnect(epm1270, {iob, 11, 3}) -> false;
remove_interconnect(epm2210, {iob, 13, 3}) -> false;
remove_interconnect(_, _) -> true.

%%--------------------------------------------------------------------

route(Key, Signals) ->
    #{Key := #{dests := [#{route := Route}]}} = Signals,
    route(Route).

%%--------------------------------------------------------------------

route([]) ->
    undefined;
route([{lab_clk,_,_,_,_},{global_clk_h,_,_,_,G} | Route]) ->
    case Route of
        [Buf={clk_buffer,_,_,_,_}] ->
            {{gclk, G}, Buf};

        [{global_clk_mux,_,_,_,G},Interconnect,From | _] ->
            {{gclk, G}, Interconnect, From}
    end;
route([_ | Route]) ->
    route(Route).

%%--------------------------------------------------------------------

sources(Device, D, Q, LC, [Clk, Clr, Pr, Ena | Pins], Sources) ->
    Source = source(Device, D, Clk, Clr, Pr, Ena, Q, LC),
    sources(Device, D, Q, LC, Pins, [Source | Sources]);
sources(_, _, _, _, _, Sources) ->
    lists:reverse(Sources).

%%--------------------------------------------------------------------

source(Device, D, Clk, Clr, Pr, Ena, Q, LC) ->
    #{
        title => {Clk, Clr, Pr, Ena},
        device => Device,
        settings => [
            {location, d, D},
            {location, clk, Clk},
            {location, clr, Clr},
            {location, pr, Pr},
            {location, ena, Ena},
            {location, q, Q},
            {location, ff, LC},
            {global_clock, clk, true},
            {global_clock, clr, true},
            {global_clock, pr, true},
            {global_clock, ena, true}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    d : in STD_LOGIC;\n"
            "    clk : in STD_LOGIC;\n"
            "    clr : in STD_LOGIC;\n"
            "    pr : in STD_LOGIC;\n"
            "    ena : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFFE port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => clr,\n"
            "    prn => pr,\n"
            "    ena => ena,\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

