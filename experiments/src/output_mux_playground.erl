-module(output_mux_playground).

-export([run/0]).

% This playground is the supporting detail for `output_mux_theory`.
%
% How are IOB local_interconnects muxed into an IOC outputs?

%%====================================================================
%% run
%%====================================================================

run() ->
    block(epm240, epm240_t100, {iob, 8, 2}, {lab, 7, 2}),
    block(epm240, epm240_t100, {iob, 1, 2}, {lab, 2, 2}),
    block(epm240, epm240_t100, {iob, 4, 0}, {lab, 4, 1}),
    block(epm240, epm240_t100, {iob, 3, 5}, {lab, 3, 4}),
    block(epm570, epm570_f256, {iob, 13, 2}, {lab, 12, 2}),
    block(epm570, epm570_f256, {iob, 0, 5}, {lab, 1, 5}),
    block(epm570, epm570_f256, {iob, 11, 0}, {lab, 11, 1}),
    block(epm570, epm570_f256, {iob, 5, 3}, {lab, 5, 4}),
    block(epm570, epm570_f256, {iob, 6, 8}, {lab, 6, 7}),
    ok.

%%--------------------------------------------------------------------

block(Density, Device, IOB, LAB) ->
    io:format(" => ~s ~p ~p~n", [Device, IOB, LAB]),
    {Sources, IOCs} = sources(Device, IOB, LAB),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    Matrix = matrix:build_with_map(Density, Experiments),
    %matrix:print(Matrix),
    {Keys, Routes} = routes(Experiments),
    print_keys(Keys),
    %io:format("~p~n", [Routes]),
    [
        ioc_fuses(IOC, Keys, Routes, Matrix)
        ||
        IOC <- IOCs
    ],
    ok.

%%====================================================================
%% source
%%====================================================================

sources(Device, IOB, LAB) ->
    case pins(Device, IOB) of
        [{Pin0, IOC0}, {Pin1, IOC1}, {Pin2, IOC2}] ->
            {lists:flatten([
                source(Device, LAB, Pin0, N0, Pin1, N1, Pin2, N2)
                ||
                N0 <- lists:seq(0, 9),
                N1 <- lists:seq(0, 9),
                N1 > N0,
                N2 <- lists:seq(0, 9),
                N2 > N1
             ]),
             [IOC0, IOC1, IOC2]
            };

        [{Pin0, IOC0}, {Pin1, IOC1}, {Pin2, IOC2}, {Pin3, IOC3} | _] ->
            {lists:flatten([
                source(Device, LAB, Pin0, N0, Pin1, N1, Pin2, N2, Pin3, N3)
                ||
                N0 <- lists:seq(0, 9),
                N1 <- lists:seq(0, 9),
                N1 > N0,
                N2 <- lists:seq(0, 9),
                N2 > N1,
                N3 <- lists:seq(0, 9),
                N3 > N2
             ]),
             [IOC0, IOC1, IOC2, IOC3]
            }
    end.

%%--------------------------------------------------------------------

pins(Device, IOB) ->
    [
        Pin
        ||
        Pin = {_, IOC} <- device:pins(Device),
        ioc:in_iob(IOC, IOB)
    ].

%%--------------------------------------------------------------------

source(Device, LAB, Pin0, N0, Pin1, N1, Pin2, N2) ->
    #{
        title => {Pin0, N0, Pin1, N1, Pin2, N2, LAB},
        device => Device,
        settings => [
            {location, a, lab:lc(LAB, N0)},
            {location, b, lab:lc(LAB, N1)},
            {location, c, lab:lc(LAB, N2)},
            {location, q, Pin0},
            {location, r, Pin1},
            {location, s, Pin2}
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
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => r\n"
            "  );\n",
            "  c: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source(Device, LAB, Pin0, N0, Pin1, N1, Pin2, N2, Pin3, N3) ->
    #{
        title => {Pin0, N0, Pin1, N1, Pin2, N2, Pin3, N3, LAB},
        device => Device,
        settings => [
            {location, a, lab:lc(LAB, N0)},
            {location, b, lab:lc(LAB, N1)},
            {location, c, lab:lc(LAB, N2)},
            {location, d, lab:lc(LAB, N3)},
            {location, q, Pin0},
            {location, r, Pin1},
            {location, s, Pin2},
            {location, t, Pin3}
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
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC;\n"
            "    t : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => r\n"
            "  );\n",
            "  c: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => s\n"
            "  );\n",
            "  d: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => t\n"
            "  );\n",
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

route_keys_experiment({_, _, #{signals := Signals}}, Acc) ->
    maps:fold(fun route_keys_signal/3, Acc, Signals).

%%--------------------------------------------------------------------

route_keys_signal(_, #{dests := Dests}, Acc0) ->
    lists:foldl(
        fun (Dest, Acc) -> route_keys_dest(Dest, Acc) end,
        Acc0,
        Dests
    ).

%%--------------------------------------------------------------------

route_keys_dest(#{port := data_in, route := Route}, Acc) ->
    case Route of
        [Bypass = {io_bypass_out, _, _, _, _} | _] ->
            route_keys_add([Bypass], Acc);

        [Out, Interconnect | _] ->
            route_keys_add([Out, Interconnect], Acc)
    end.

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
    maps:fold(fun (_, Signal, Acc) ->
         route_signal(Signal, RouteToKey, Acc)
    end, #{}, Signals).

%%--------------------------------------------------------------------

route_signal(#{dests := Dests}, RouteToKey, Init) ->
    lists:foldl(fun (Dest, Acc) ->
        route_dest(Dest, RouteToKey, Acc)
    end, Init, Dests).

%%--------------------------------------------------------------------

route_dest(#{port := data_in, route := Route}, RouteToKey, Acc) ->
    case Route of
        [Bypass = {io_bypass_out, X, Y, N, _} | _] ->
            route_add({ioc, X, Y, N}, [Bypass], RouteToKey, Acc);

        [Out = {io_data_out, X, Y, N, _}, Interconnect | _] ->
            route_add({ioc, X, Y, N}, [Out, Interconnect], RouteToKey, Acc)
    end.

%%--------------------------------------------------------------------

route_add(IOC, Route, RouteToKey, Acc) ->
    #{Route := Key} = RouteToKey,
    Acc#{IOC => {Key, Route}}.

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

ioc_fuses(IOC, Keys, Routes, Matrix = {matrix, _, AllFuses}) ->
    InitialFuses = [
        {Fuse, #{}, Name}
        ||
        {Fuse, _, Name} <- AllFuses
    ],
    Fuses = lists:foldl(fun ({Key, _}, Acc) ->
        ioc_fuses(IOC, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    io:format("~n~w:~n        ~s~n", [
        IOC,
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                ioc_fuse_bit(Key, Bits)
                ||
                {Key, _} <- Keys
            ]),
            Name
        ])
        ||
        {Fuse, Bits, Name} <- Fuses
    ],
    %[ Fuse || {Fuse, _, _} <- Fuses ].
    ok.

%%--------------------------------------------------------------------

ioc_fuses(IOC, Key, Routes, Matrix, Fuses) ->
    IsKey = [ ioc_key_is(Route, IOC, Key) || Route <- Routes ],
    case lists:member(true, IsKey) of
        true ->
            Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsKey ],
            Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsKey ],
            Fuses0 = matrix:pattern_match(Matrix, Pattern0),
            Fuses1 = matrix:pattern_match(Matrix, Pattern1),
            lists:filtermap(fun ({Fuse, Bits, Name}) ->
                ioc_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name)
            end, Fuses);

        false ->
            Fuses
    end.

%%--------------------------------------------------------------------

ioc_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Key => 0}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

ioc_fuse_bit(Key, Bits) ->
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

ioc_key_is(Route, IOC, Key) ->
    case Route of
        #{IOC := {Key, _}} ->
            true;

        _ ->
            false
    end.

