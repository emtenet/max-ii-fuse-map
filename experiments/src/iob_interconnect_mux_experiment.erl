-module(iob_interconnect_mux_experiment).

-export([run/0]).

%-define(PRINT_TABLES, true).

%%====================================================================
%% run
%%====================================================================

run() ->
    %Density = epm570,
    %Device = density:largest_device(Density),
    %ioc(Density, Device, {j2, {ioc, 0, 5, 3}}),
    %ioc(Density, Device, {a6, {ioc, 5, 8, 2}}),
    %ioc(Density, Device, {p7, {ioc, 5, 3, 1}}),
    %ioc(Density, Device, {r13, {ioc, 11, 0, 1}}),
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(fun (IOC) -> ioc(Density, Device, IOC) end,
                  lists:sort(fun ioc_by_number/2, device:iocs(Device))),
    ok.

%%--------------------------------------------------------------------

ioc_by_number({_, A}, {_, B}) ->
    A =< B.

%%--------------------------------------------------------------------

ioc(Density, Device, Out = {_, IOC}) ->
    io:format(" => ~s ~w interconnect~n", [Density, IOC]),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source:lut_out(Device, lab:lc(LAB, N), Out)
        ||
        LAB <- density:labs(Density),
        N <- [0, 3, 6, 9]
    %] ++ [
    %    source:in_out(Device, In, Out)
    %    ||
    %    In <- device:iocs(Device),
    %    In =/= Out
    ]),
    %Minimal = {minimal, density:minimal_fuses(Density)},
    %Matrix0 = matrix:build_with_map(Density, [Minimal | Experiments]),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, enable}) -> true;
        ({_, output_invert}) -> true;
        ({_, fast_out}) -> true;
        ({_, lut_out, _}) -> true;
        ({_, output3, _}) -> true;
        ({_, output4, _}) -> true;
        ({_, output6, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    {Keys, Routes} = routes(Experiments),
    interconnects(Density, Keys, Routes, Matrix),
    ok.

%%====================================================================
%% routes
%%====================================================================

routes(Experiments) ->
    Init = {#{}, #{}, $A},
    {RouteToKey, KeyToRoute, _} =
        lists:foldl(fun route_keys_experiment/2, Init, Experiments),
    Keys = lists:sort(
        fun route_sort/2,
        maps:fold(fun route_key/3, [], KeyToRoute)
    ),
    Routes = lists:map(
        fun (E) -> route_experiment(E, RouteToKey) end,
        Experiments
    ),
    {Keys, Routes}.

%%--------------------------------------------------------------------

route_keys_experiment({_, _, #{signals := Signals}}, Acc) ->
    case Signals of
        #{lut := #{dests := [Dest]}} ->
            #{route := Route} = Dest,
            route_keys_route(Route, Acc);

        #{i := #{dests := [Dest]}} ->
            #{route := Route} = Dest,
            route_keys_route(Route, Acc)
    end.

%%--------------------------------------------------------------------

route_keys_route([{io_bypass_out, _, _, _, _}, {le_buffer, _, _, 0, _}], Acc) ->
    Acc;
route_keys_route([{io_data_out, X, Y, _, _}, Route, From | _], Acc) ->
    {local_interconnect, X, Y, 0, _} = Route,
    route_keys_add(Route, From, Acc).

%%--------------------------------------------------------------------

route_keys_add(Route, From, Acc = {ToKey, ToRoute, Key0}) ->
    case ToKey of
        #{Route := {_, #{From := _}, _}} ->
            Acc;

        #{Route := {Key, ToSub, Sub0}} ->
            Sub = <<Sub0>>,
            #{Key := {Route, ToFrom}} = ToRoute,
            {ToKey#{Route => {Key, ToSub#{From => Sub}, Sub0 + 1}},
             ToRoute#{Key => {Route, ToFrom#{Sub => From}}},
             Key0
            };

        _ ->
            Key = <<Key0>>,
            Sub = <<"a">>,
            {ToKey#{Route => {Key, #{From => Sub}, $b}},
             ToRoute#{Key => {Route, #{Sub => From}}},
             Key0 + 1
            }
    end.

%%--------------------------------------------------------------------

route_experiment({_, _, #{signals := Signals}}, RouteToKey) ->
    case Signals of
        #{lut := #{dests := [Dest]}} ->
            #{route := Route} = Dest,
            route_route(Route, RouteToKey);

        #{i := #{dests := [Dest]}} ->
            #{route := Route} = Dest,
            route_route(Route, RouteToKey)
    end.

%%--------------------------------------------------------------------

route_route([{io_bypass_out, _, _, _, _}, _], _RouteToKey) ->
    fast_out;
route_route([{io_data_out, _, _, _, _}, Route, From | _], RouteToKey) ->
    route_add(Route, From, RouteToKey).

%%--------------------------------------------------------------------

route_add(Route, From, RouteToKey) ->
    #{Route := {Key, FromToSub, _}} = RouteToKey,
    #{From := Sub} = FromToSub,
    {Key, Sub}.

%%--------------------------------------------------------------------

route_key(Key, {Route, SubToFrom}, Keys) ->
    Subs = lists:sort(maps:to_list(SubToFrom)),
    [{Key, Route, Subs} | Keys].

%%--------------------------------------------------------------------

route_sort({_, A, _}, {_, B, _}) ->
    A =< B.

%%====================================================================
%% interconnects
%%====================================================================

interconnects(_, [], _, _) ->
    ok;
interconnects(Density, [{Key, Interconnect, Subs} | Keys], Routes, Matrix) ->
    interconnect(Density, Key, Interconnect, Subs, Routes, Matrix),
    interconnects(Density, Keys, Routes, Matrix).

%%--------------------------------------------------------------------

interconnect(Density, Key, Interconnect, Subs0, Routes, Matrix) ->
    InitialPattern = [
        interconnect_is_key(Route, Key)
        ||
        Route <- Routes
    ],
    InitialFuses = lists:filtermap(fun (Fuse) ->
        interconnect_initial_fuse(Density, Fuse)
    end, matrix:pattern_match(Matrix, InitialPattern)),
    Fuses0 = lists:foldl(fun ({Sub, _}, Acc) ->
        interconnect_fold_fuses(Key, Sub, Routes, Matrix, Acc)
    end, InitialFuses, Subs0),
    %%
    interconnect_print(Interconnect, Subs0, Fuses0),
    %%
    Fuses = lists:filter(
        fun (Fuse) -> interconnect_filter(Interconnect, Fuse) end,
        Fuses0
    ),
    %Interconnect = interconnect_from_fuses(Fuses),
    Subs = lists:map(fun (Sub) -> interconnect_sub(Sub, Fuses) end, Subs0),
    Mapping = lists:map(fun interconnect_found/1, Subs),
    iob_interconnect_mux_database:update(Density, Mapping),
    ok.

%%--------------------------------------------------------------------

interconnect_is_key({Key, _}, Key) -> 'x';
interconnect_is_key(_, _) -> 1.

%%--------------------------------------------------------------------

interconnect_initial_fuse(Density, {Fuse, Name}) ->
    case fuse_map:to_location(Fuse, Density) of
        Location = {_, _, _, _, side, _} ->
            {true, {Fuse, #{}, Location, Name}};

        Location = {_, head, _, cell, _} ->
            {true, {Fuse, #{}, Location, Name}};

        Location = {_, tail, _, cell, _} ->
            {true, {Fuse, #{}, Location, Name}};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

interconnect_fold_fuses(Key, Sub, Routes, Matrix, Fuses) ->
    IsSub = [ interconnect_is_from(Route, Key, Sub) || Route <- Routes ],
    true = lists:member(true, IsSub),
    Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsSub ],
    Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsSub ],
    Fuses0 = matrix:pattern_match(Matrix, Pattern0),
    Fuses1 = matrix:pattern_match(Matrix, Pattern1),
    lists:filtermap(fun ({Fuse, Bits, Location, Name}) ->
        interconnect_fuse_bits(Fuse, Sub, Fuses0, Fuses1, Bits, Location, Name)
    end, Fuses).

%%--------------------------------------------------------------------

interconnect_is_from({Key, Sub}, Key, Sub) -> true;
interconnect_is_from(_, _, _) -> false.

%%--------------------------------------------------------------------

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

%%--------------------------------------------------------------------

interconnect_fuse_bits(Fuse, Sub, Fuses0, Fuses1, Bits, Location, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Sub => 0}, Location, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Sub => 1}, Location, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

-ifdef(PRINT_TABLES).
interconnect_print(Interconnect, Subs, Fuses) ->
    io:format("~n ==> ~w~n", [Interconnect]),
    [
        io:format("~s: ~w~n", [Sub, From])
        ||
        {Sub, From} <- Subs
    ],
    io:format("       ~s~n", [[
        [<<" ">>, Sub]
        ||
        {Sub, _} <- Subs
    ]]),
    [
        io:format("~6b:~s| ~w~n", [Fuse, [
            [<<"|">>, interconnect_print_bit(Bits, Sub)]
            ||
            {Sub, _} <- Subs
        ], Name])
        ||
        {Fuse, Bits, _Location, Name} <- Fuses
    ],
    ok.

%%--------------------------------------------------------------------

interconnect_print_bit(Bits, Sub) ->
    case Bits of
        #{Sub := 0} -> <<" ">>;
        #{Sub := 1} -> <<"#">>
    end.
-else.
interconnect_print(_, _, _) ->
    ok.
-endif.

%%--------------------------------------------------------------------

interconnect_filter({local_interconnect, X, Y, 0, N}, {_, _, _, Name}) ->
    case Name of
        {{iob, X, Y}, {interconnect, N}, direct_link} ->
            true;

        {{iob, X, Y}, {interconnect, N}, from3, _} ->
            true;

        {{iob, X, Y}, {interconnect, N}, from4, _} ->
            true;

        _ ->
            false
    end.

%%--------------------------------------------------------------------

interconnect_sub({Sub, From}, Fuses0) ->
    Fuses = lists:filtermap(
        fun (Fuse) -> interconnect_sub_fuse(Sub, Fuse) end,
        Fuses0
    ),
    interconnect_sub_pairs(Fuses),
    {Sub, From, Fuses}.

%%--------------------------------------------------------------------

interconnect_sub_fuse(Sub, {_, Subs, Location, Name}) ->
    #{Sub := Bit} = Subs,
    case Bit of
        0 ->
            {true, {Location, Name}};

        1 ->
            false
    end.

%%--------------------------------------------------------------------

interconnect_sub_pairs([A]) ->
    true = is_direct_link(A),
    ok;
interconnect_sub_pairs([A, B]) ->
    false = is_direct_link(A),
    false = is_direct_link(B),
    case {interconnect_mux(A), interconnect_mux(B)} of
        {from3, from4} ->
            ok;

        {from4, from3} ->
            ok
    end.

%%--------------------------------------------------------------------

interconnect_mux({{_, _, _, _, side,  7}, _}) -> from3;
interconnect_mux({{_, _, _, 0, side,  8}, _}) -> from3;
interconnect_mux({{_, _, _, 2, side,  8}, _}) -> from3;
interconnect_mux({{_, _, _, _, side,  9}, _}) -> from4;
interconnect_mux({{_, _, _, _, side, 10}, _}) -> from4;
interconnect_mux({{_, head, _, cell,  2}, _}) -> from4;
interconnect_mux({{_, head, _, cell,  3}, _}) -> from4;
interconnect_mux({{_, head, _, cell,  4}, _}) -> from3;
interconnect_mux({{_, head, _, cell,  5}, _}) -> from3;
interconnect_mux({{_, head, _, cell, 18}, _}) -> from3;
interconnect_mux({{_, head, _, cell, 19}, _}) -> from3;
interconnect_mux({{_, head, _, cell, 20}, _}) -> from4;
interconnect_mux({{_, head, _, cell, 21}, _}) -> from4;
interconnect_mux({{_, tail, _, cell,  2}, _}) -> from4;
interconnect_mux({{_, tail, _, cell,  3}, _}) -> from4;
interconnect_mux({{_, tail, _, cell,  4}, _}) -> from3;
interconnect_mux({{_, tail, _, cell,  5}, _}) -> from3;
interconnect_mux({{_, tail, _, cell, 18}, _}) -> from3;
interconnect_mux({{_, tail, _, cell, 19}, _}) -> from3;
interconnect_mux({{_, tail, _, cell, 20}, _}) -> from4;
interconnect_mux({{_, tail, _, cell, 21}, _}) -> from4.

%%--------------------------------------------------------------------

is_direct_link({{_, _, _, 1, side, 8}, _}) ->
    true;
is_direct_link({{_, _, _, 3, side, 8}, _}) ->
    true;
is_direct_link(_) ->
    false.

%%--------------------------------------------------------------------

interconnect_found({_, From, [{_, {IOB, Interconnect, direct_link}}]}) ->
    interconnect_found(IOB, Interconnect, direct_link, From);
interconnect_found({_, From, [
                {_, {IOB, Interconnect, from4, Mux4}},
                {_, {_, _, from3, Mux3}}
            ]}) ->
    interconnect_found(IOB, Interconnect, {Mux4, Mux3}, From);
interconnect_found({_, From, [
                {_, {IOB, Interconnect, from3, Mux3}},
                {_, {_, _, from4, Mux4}
            }]}) ->
    interconnect_found(IOB, Interconnect, {Mux4, Mux3}, From).

%%--------------------------------------------------------------------

interconnect_found(IOB, Interconnect, Select, From) ->
    {IOB, Interconnect, Select, From}.

