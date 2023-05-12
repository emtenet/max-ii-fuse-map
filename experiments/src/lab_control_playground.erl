-module(lab_control_playground).

-export([run/0]).

% This playground explores the six (6) control lines per LAB that
% feed the LAB wide clk1, clk2, clr1, clr2, s-load, a-load, ...
%
% The six control lines select a local interconnect via a 6 by 3
% mux similar to LUT inputs.
%
% As diagramed in the `MAX II Device Handbook`:
%
%  * clk1 can select from either control 0 or 1
%  * clr1 can select from either control 4 or 5
%  * a-load can select from either control 2 or 3
%
% with a fuse per these selections.
%
% I assume there is another fuse selcting between the global networks
% and the LAB controls for the two clock lines.

%%====================================================================
%% run
%%====================================================================

run() ->
    %block(epm240, epm240_t100, {lab, 3, 3}),
    %throw(stop),
    density(epm240),
    density(epm570),
    density(epm1270),
    density(epm2210),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    LABs = density:labs(Density),
    [
        block(Density, Device, LAB)
        ||
        LAB <- LABs
    ],
    ok.

%%--------------------------------------------------------------------

block(Density, Device, LAB) ->
    [D, Q | _] = device:gclk_pins(Device),
    Pins = lists:delete(D, lists:delete(Q, device:pins(Device))),
    PinCount = length(Pins),
    Space = PinCount div 26,
    Third = PinCount div 3,
    experiment(Density, [
        source(Device,
               lab:lc(LAB, Index rem 10),
               D,
               pin(Index, Space, 0, Third, PinCount, Pins),
               pin(Index, Space, 1, Third, PinCount, Pins),
               pin(Index, Space, 2, Third, PinCount, Pins),
               Q)
        ||
        Index <- lists:seq(0, 25)
    ]),
    ok.

%%--------------------------------------------------------------------

pin(N, NStep, M, MStep, Count, Pins) ->
    Nth = ((N * NStep) + (M * MStep)) rem Count,
    lists:nth(1 + Nth, Pins).

%%--------------------------------------------------------------------

experiment(Density, Sources) ->
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, {interconnect, _}, _}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, clk}) -> true;
        ({_, enable}) -> true;
        ({_, enable_invert}) -> true;
        ({_, input_delay}) -> true;
        ({_, output}) -> true;
        ({_, output3, _}) -> true;
        ({_, output6, _}) -> true;
        ({_, output_invert}) -> true;
        ({_, schmitt_trigger}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    {Keys, Routes} = routes(Experiments),
    print_keys(Keys),
    fuses(Keys, Routes, Matrix),
    ok.

%%====================================================================
%% source
%%====================================================================

source(Device, LC, D, Clk, Clr, Pr, Q) ->
    source({D, Clk, Clr, Pr, Q}, Device, [
        {location, ff, LC},
        {location, d, D},
        {location, clk, Clk},
        {location, clr, Clr},
        {location, pr, Pr},
        {location, q, Q}
    ]).

%%--------------------------------------------------------------------

source(Title, Device, Settings) ->
    #{
        title => Title,
        device => Device,
        settings => Settings,
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => clr,\n"
            "    prn => pr,\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% routes
%%====================================================================

routes(Experiments) ->
    Init = {#{}, #{}, $A},
    {RouteToKey, KeyToRoute, _} =
        lists:foldl(fun route_keys_experiment/2, Init, Experiments),
    Keys = lists:sort(maps:to_list(KeyToRoute)),
    Routes = lists:map(
        fun (E) -> route_experiment(E, RouteToKey) end,
        Experiments
    ),
    {Keys, Routes}.

%%--------------------------------------------------------------------

route_keys_experiment({_, _, #{signals := Signals}}, Acc0) ->
    #{clk := Clk, clr := Clr, pr := Pr} = Signals,
    Acc1 = route_keys_signal(Clk, Acc0),
    Acc2 = route_keys_signal(Clr, Acc1),
    Acc3 = route_keys_signal(Pr, Acc2),
    Acc3.

%%--------------------------------------------------------------------

route_keys_signal(#{dests := Dests}, Acc0) ->
    lists:foldl(
        fun (Dest, Acc) -> route_keys_dest(Dest, Acc) end,
        Acc0,
        Dests
    ).

%%--------------------------------------------------------------------

route_keys_dest(#{port := Port, route := Route}, Acc0) ->
    [Control = {lab_control_mux,X,Y,0,_},
     Interconnect = {local_interconnect,X,Y,0,_}
     |
     _
    ] = Route,
    Acc1 = route_keys_add({Port, Control}, Acc0),
    Acc2 = route_keys_add({Control, Interconnect}, Acc1),
    Acc2.

%%--------------------------------------------------------------------

route_keys_add(Route, Acc = {ToKey, ToRoute, Key0}) ->
    case ToKey of
        #{Route := _} ->
            Acc;

        _ ->
            Key = <<Key0>>,
            {ToKey#{Route => Key}, ToRoute#{Key => Route}, Key0 + 1}
    end.

%%--------------------------------------------------------------------

route_experiment({_, _, #{signals := Signals}}, RouteToKey) ->
    #{clk := Clk, clr := Clr, pr := Pr} = Signals,
    Acc0 = #{},
    Acc1 = route_signal(Clk, RouteToKey, Acc0),
    Acc2 = route_signal(Clr, RouteToKey, Acc1),
    Acc3 = route_signal(Pr, RouteToKey, Acc2),
    Acc3.

%%--------------------------------------------------------------------

route_signal(#{dests := Dests}, RouteToKey, Init) ->
    lists:foldl(fun (Dest, Acc) ->
        route_dest(Dest, RouteToKey, Acc)
    end, Init, Dests).

%%--------------------------------------------------------------------

route_dest(#{port := Port, route := Route}, RouteToKey, Acc0) ->
    [Control = {lab_control_mux,X,Y,0,_},
     Interconnect = {local_interconnect,X,Y,0,_}
     |
     _
    ] = Route,
    Acc1 = route_add({Port, Control}, RouteToKey, Acc0),
    Acc2 = route_add({Control, Interconnect}, RouteToKey, Acc1),
    Acc2.

%%--------------------------------------------------------------------

route_add(Route = {To, From}, RouteToKey, Acc) ->
    #{Route := Key} = RouteToKey,
    Acc#{To => {Key, From}}.

%%--------------------------------------------------------------------

print_keys(Keys) ->
    [
        io:format("~s: ~w~n", [Key, Route])
        ||
        {Key, Route} <- Keys
    ],
    ok.

%%====================================================================
%% fuses
%%====================================================================

fuses(Keys, Routes, Matrix) ->
    lists:foreach(fun (To) ->
        fuses_for(To, Keys, Routes, Matrix)
    end, lists:usort([
        To
        ||
        {_Key, {To, _From}} <- Keys
    ])).

%%--------------------------------------------------------------------

fuses_for(To, Keys, Routes, Matrix = {matrix, _, AllFuses}) ->
    io:format("~n ==> ~p~n", [To]),
    InitialFuses = [
        {Fuse, #{zeros => false, ones => false}, Name}
        ||
        {Fuse, _, Name} <- AllFuses
    ],
    Fuses = lists:foldl(fun ({Key, _}, Acc) ->
        fuses_reduce(To, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    io:format("        ~s~n", [
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                fuse_bit(Key, Bits)
                ||
                {Key, _} <- Keys
            ]),
            Name
        ])
        ||
        {Fuse, Bits = #{zeros := Zeros}, Name} <- Fuses,
        Zeros
    ],
    ok.

%%--------------------------------------------------------------------

fuses_reduce(To, Key, Routes, Matrix, Fuses) ->
    IsKey = [ route_key_is(Route, To, Key) || Route <- Routes ],
    case lists:member(true, IsKey) of
        true ->
            Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsKey ],
            Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsKey ],
            Fuses0 = matrix:pattern_match(Matrix, Pattern0),
            Fuses1 = matrix:pattern_match(Matrix, Pattern1),
            lists:filtermap(fun ({Fuse, Bits, Name}) ->
                fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name)
            end, Fuses);

        false ->
            Fuses
    end.

%%--------------------------------------------------------------------

fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Key => 0, zeros => true}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

fuse_bit(Key, Bits) ->
    case Bits of
        #{Key := 0} -> <<"-">>;
        #{Key := 1} -> <<"#">>;
        _ -> <<" ">>
    end.

%%====================================================================
%% utilities
%%====================================================================

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

%%--------------------------------------------------------------------

route_key_is(Route, To, Key) ->
    case Route of
        #{To := {Key, _}} ->
            true;

        _ ->
            false
    end.

