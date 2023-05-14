-module(global_interconnect_mux_experiment).

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

%-define(PRINT_MATRIX, true).
%-define(PRINT_ROUTES, true).

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
    print_matrix(Matrix),
    print_routes(Experiments),
    Routes = routes(Experiments),
    fuses(Routes, Experiments, Matrix),
    ok.

%%--------------------------------------------------------------------

remove_interconnect(epm240, {iob, 1, 3}) -> false;
remove_interconnect(epm570, {iob, 9, 3}) -> false;
remove_interconnect(epm1270, {iob, 11, 3}) -> false;
remove_interconnect(epm2210, {iob, 13, 3}) -> false;
remove_interconnect(_, _) -> true.

%%--------------------------------------------------------------------

-ifdef(PRINT_MATRIX).
print_matrix(Matrix) ->
    matrix:print(Matrix).
-else.
print_matrix(_) ->
    ok.
-endif.

%%--------------------------------------------------------------------

-ifdef(PRINT_ROUTES).
print_routes(Experiments) ->
    [
        io:format(" ==> ~w~n  clk <- ~w~n  clr <- ~w~n  pr  <- ~w~n  ena <- ~w~n", [
            Name,
            route_of(clk, Signals),
            route_of(clr, Signals),
            route_of(pr, Signals),
            route_of(ena, Signals)
        ])
        ||
        {Name, _, #{signals := Signals}} <- Experiments
    ],
    ok.

%%--------------------------------------------------------------------

route_of(Key, Signals) ->
    #{Key := #{dests := [#{route := Route}]}} = Signals,
    route_of(Route).

%%--------------------------------------------------------------------

route_of([]) ->
    undefined;
route_of([{lab_clk,_,_,_,_},{global_clk_h,_,_,_,G} | Route]) ->
    case Route of
        [Buf={clk_buffer,_,_,_,_}] ->
            {{gclk, G}, Buf};

        [{global_clk_mux,_,_,_,G},Interconnect,From | _] ->
            {{gclk, G}, Interconnect, From}
    end;
route_of([_ | Route]) ->
    route_of(Route).
-else.
print_routes(_) ->
    ok.
-endif.

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

%%====================================================================
%% global fuses
%%====================================================================

routes(Experiments) ->
    lists:foldl(fun routes_experiment/2, #{}, Experiments).

%%--------------------------------------------------------------------

routes_experiment({Name, _, #{signals := Signals}}, Routes0) ->
    Routes1 = routes_signal(Name, clk, Signals, Routes0),
    Routes2 = routes_signal(Name, clr, Signals, Routes1),
    Routes3 = routes_signal(Name, pr, Signals, Routes2),
    Routes4 = routes_signal(Name, ena, Signals, Routes3),
    Routes4.

%%--------------------------------------------------------------------

routes_signal(Name, Key, Signals, Routes) ->
    #{Key := #{dests := [#{route := Route}]}} = Signals,
    routes_route(Name, Route, Routes).

%%--------------------------------------------------------------------

routes_route(_, [], Routes) ->
    Routes;
routes_route(Name, [{global_clk_mux, _, _, _, G} | Route], Routes) ->
    [{local_interconnect, _, 3, 0, N} | _] = Route,
    Key = {G, N},
    case Routes of
        #{Key := Names} ->
            Routes#{Key => Names#{Name => true}};

        _ ->
            Routes#{Key => #{Name => true}}
    end;
routes_route(Name, [_ | Route], Routes) ->
    routes_route(Name, Route, Routes).

%%--------------------------------------------------------------------

fuses(Routes, Experiments, Matrix) ->
    lists:foreach(fun ({{G, N}, Names}) ->
        fuses(G, N, Names, Experiments, Matrix)
    end, lists:sort(maps:to_list(Routes))),
    ok.

%%--------------------------------------------------------------------

fuses(Global, Interconnect, Names, Experiments, Matrix) ->
    %io:format("{global, ~p} <- {local_interconnect, _, _, 0, ~p}~n", [
    %    Global, Interconnect
    %]),
    Pattern = lists:map(fun (Experiment) ->
        pattern(Experiment, Names)
    end, Experiments),
    Fuses0 = matrix:pattern_match(Matrix, Pattern),
    Fuses = lists:filtermap(fun
        ({_, {{global, G}, From, Mux}}) when G =:= Global ->
            {true, {From, Mux}};
        (_) ->
            false
    end, Fuses0),
    fuses_map(Fuses, Interconnect).

%%--------------------------------------------------------------------

pattern({Name, _, _}, Names) when is_map_key(Name, Names) ->
    0;
pattern(_, _) ->
    x.

%%--------------------------------------------------------------------

fuses_map([{from3, Mux3}, {from6, Mux6}], Interconnect) ->
    fuses_epm240(Mux6, Mux3, Interconnect);
fuses_map([{from6, Mux6}, {from3, Mux3}], Interconnect) ->
    fuses_epm240(Mux6, Mux3, Interconnect);
fuses_map([{from3, Mux3}, {from4, Mux4}], Interconnect) ->
    fuses_other(Mux4, Mux3, Interconnect);
fuses_map([{from4, Mux4}, {from3, Mux3}], Interconnect) ->
    fuses_other(Mux4, Mux3, Interconnect).

%%--------------------------------------------------------------------

fuses_epm240(Mux6, Mux3, Interconnect) ->
    {interconnect, Interconnect} =
        output_mux_map:to_row_interconnect(Mux6, Mux3),
    ok.

%%--------------------------------------------------------------------

fuses_other(mux0, mux0, 0) -> ok;
fuses_other(mux0, mux1, 1) -> ok;
fuses_other(mux0, mux2, 2) -> ok;
fuses_other(mux1, mux0, 3) -> ok;
fuses_other(mux1, mux1, 4) -> ok;
fuses_other(mux1, mux2, 5) -> ok;
fuses_other(mux2, mux0, 6) -> ok;
fuses_other(mux2, mux1, 7) -> ok;
fuses_other(mux2, mux2, 8) -> ok;
fuses_other(mux3, mux0, 9) -> ok.

