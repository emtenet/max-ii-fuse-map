-module(iob_direct_link_experiment).

-export([run/0]).

% How are IOB local_interconnects muxed from direct-links?
%
% Found fuses {X, Y, N, I, side, 8} with
%   * I = 1 and N in 1..8
%   * I = 3 and N in 0..9
% for IOBs on the left & right sides
%
% For example EPM570 fuse {0, 5, 3, 1, side, 8} is:
%   * {le_buffer, 1, 5, 0, 12} -> {local_interconnect, 0, 5, 0, 14}

%-define(PRINT_KEYS, true).
%-define(PRINT_FUSES, true).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(fun ({IOB, LAB}) ->
        block(Density, Device, IOB, LAB)
    end, density:iobs(Density)).

%%--------------------------------------------------------------------

block(Density, Device, IOB, LAB) ->
    io:format(" => ~s ~p ~p~n", [Device, IOB, LAB]),
    {Sources, _IOCs} = output_mux_playground:sources(Device, IOB, LAB),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{lc, _, _, _}, lut_out, _}) -> true;
        ({{ioc, _, _, _}, output3, _}) -> true;
        ({{ioc, _, _, _}, output4, _}) -> true;
        ({{ioc, _, _, _}, output6, _}) -> true;
        ({{ioc, _, _, _}, fast_out, _}) -> true;
        (_) -> false
    end),
    {Keys, Routes} = routes(Experiments),
    print_keys(Keys),
    Interconnects = lists:map(fun (Interconnect) ->
        fuses(Interconnect, Keys, Routes, Matrix)
    end, interconnects(Keys)),
    print_fuses(Interconnects, Keys),
    lists:foreach(fun ({Interconnect, Fuses}) ->
        theory(Density, IOB, Interconnect, Keys, Fuses)
    end, Interconnects),
    ok.

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
        [{io_bypass_out, _, _, _, _}, {le_buffer, _, _, _, _}] ->
            Acc;

        [{io_data_out, _, _, _, _},
         Interconnect = {local_interconnect, _, _, _, _},
         From | _] ->
            route_keys_add([Interconnect, From], Acc)
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
        [{io_bypass_out, _, _, _, _}, {le_buffer, _, _, _, _}] ->
            Acc;

        [{io_data_out, _, _, _, _},
         Interconnect = {local_interconnect, _, _, _, N},
         From | _] ->
            route_add({interconnect, N}, [Interconnect, From], RouteToKey, Acc)
    end.

%%--------------------------------------------------------------------

route_add(Interconnect, Route, RouteToKey, Acc) ->
    #{Route := Key} = RouteToKey,
    Acc#{Interconnect => {Key, Route}}.

%%--------------------------------------------------------------------

-ifdef(PRINT_KEYS).
print_keys(Keys) ->
    [
        io:format("~s: ~w~n", [Key, Route])
        ||
        {Key, Route} <- Keys
    ],
    ok.
-else.
print_keys(_) ->
    ok.
-endif.

%%====================================================================
%% interconnects
%%====================================================================

interconnects(Keys) ->
    interconnects(Keys, []).

%%--------------------------------------------------------------------

interconnects([], Interconnects) ->
    Interconnects;
interconnects([{_, [Local, _]} | Keys], Interconnects) ->
    {local_interconnect, _, _, 0, N} = Local,
    Interconnect = {interconnect, N},
    interconnects(Keys, lists:umerge([Interconnect], Interconnects)).

%%====================================================================
%% fuses
%%====================================================================

fuses(Interconnect, Keys, Routes, Matrix = {matrix, _, AllFuses}) ->
    InitialFuses = [
        {Fuse, #{}, Name}
        ||
        {Fuse, _, Name} <- AllFuses
    ],
    CandidateFuses = lists:foldl(fun ({Key, _}, Acc) ->
        fuses(Interconnect, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    Fuses = lists:filter(fun fuse_has_zero_bits/1, CandidateFuses),
    {Interconnect, Fuses}.

%%--------------------------------------------------------------------

fuses(Interconnect, Key, Routes, Matrix, Fuses) ->
    IsKey = [ key_is(Route, Interconnect, Key) || Route <- Routes ],
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
            {true, {Fuse, Bits#{Key => 0}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

fuse_has_zero_bits({_Fuse, Bits, _Name}) ->
    maps:fold(fun fuse_has_zero_bits/3, false, Bits).

%%--------------------------------------------------------------------

fuse_has_zero_bits(_, 0, _) -> true;
fuse_has_zero_bits(_, 1, Acc) -> Acc.

%%--------------------------------------------------------------------

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

%%--------------------------------------------------------------------

key_is(Route, Interconnect, Key) ->
    case Route of
        #{Interconnect := {Key, _}} ->
            true;

        _ ->
            false
    end.

%%--------------------------------------------------------------------

-ifdef(PRINT_FUSES).
print_fuses(Interconnects, Keys) ->
    lists:foreach(fun ({Interconnect, Fuses}) ->
        print_fuses(Interconnect, Fuses, Keys)
    end, Interconnects).

%%--------------------------------------------------------------------

print_fuses(Interconnect, Fuses, Keys) ->
    io:format("~n~w:~n        ~s~n", [
        Interconnect,
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                print_fuse_bit(Key, Bits)
                ||
                {Key, _} <- Keys
            ]),
            Name
        ])
        ||
        {Fuse, Bits, Name} <- Fuses
    ],
    ok.

%%--------------------------------------------------------------------

print_fuse_bit(Key, Bits) ->
    case Bits of
        #{Key := 0} -> <<"-">>;
        #{Key := 1} -> <<"#">>;
        _ -> <<" ">>
    end.
-else.
print_fuses(_, _) ->
    ok.
-endif.

%%====================================================================
%% theory
%%====================================================================

theory(Density, IOB = {iob, X, Y}, Interconnect, Keys, [{Fuse, Bits, _}]) ->
    {X, Y, N, I, side, 8} = fuse_map:to_location(Fuse, Density),
    Interconnect = direct_link_map(N, I),
    [{Key, 0}] = maps:to_list(Bits),
    {_, [_, Buffer]} = lists:keyfind(Key, 1, Keys),
    Buffer = direct_link_from(X, Y, N, I, Buffer),
    {ok, {IOB, Interconnect, direct_link}} = fuse_map:to_name(Fuse, Density),
    ok;
theory(Density, IOB, Interconnect, _Keys, Fuses) ->
    case edge(Density, IOB, Fuses, undefined, []) of
        {top, Locations} ->
            top_interconnect_theory(Interconnect, Locations);

        {left, Locations} ->
            left_interconnect_theory(Interconnect, Locations);

        {right, Locations} ->
            right_interconnect_theory(Interconnect, Locations);

        {bottom, Locations} ->
            bottom_interconnect_theory(Interconnect, Locations);

        undefined ->
            no_theory
    end.

%%--------------------------------------------------------------------

direct_link_map(1, 1) -> {interconnect,1};
direct_link_map(2, 1) -> {interconnect,3};
direct_link_map(3, 1) -> {interconnect,5};
direct_link_map(4, 1) -> {interconnect,7};
direct_link_map(5, 1) -> {interconnect,16};
direct_link_map(6, 1) -> {interconnect,14};
direct_link_map(7, 1) -> {interconnect,12};
direct_link_map(8, 1) -> {interconnect,10};
direct_link_map(0, 3) -> {interconnect,0};
direct_link_map(1, 3) -> {interconnect,2};
direct_link_map(2, 3) -> {interconnect,4};
direct_link_map(3, 3) -> {interconnect,6};
direct_link_map(4, 3) -> {interconnect,8};
direct_link_map(5, 3) -> {interconnect,17};
direct_link_map(6, 3) -> {interconnect,15};
direct_link_map(7, 3) -> {interconnect,13};
direct_link_map(8, 3) -> {interconnect,11};
direct_link_map(9, 3) -> {interconnect,9}.

%%--------------------------------------------------------------------

direct_link_from(X, Y, 1, 1, _) -> {le_buffer, X + 1, Y, 0,  2};
direct_link_from(X, Y, 2, 1, _) -> {le_buffer, X + 1, Y, 0,  4};
direct_link_from(X, Y, 3, 1, _) -> {le_buffer, X + 1, Y, 0,  6};
direct_link_from(X, Y, 4, 1, _) -> {le_buffer, X + 1, Y, 0,  8};
direct_link_from(X, Y, 5, 1, _) -> {le_buffer, X + 1, Y, 0, 18};
direct_link_from(X, Y, 6, 1, _) -> {le_buffer, X + 1, Y, 0, 16};
direct_link_from(X, Y, 7, 1, _) -> {le_buffer, X + 1, Y, 0, 14};
direct_link_from(X, Y, 8, 1, _) -> {le_buffer, X + 1, Y, 0, 12};
direct_link_from(X, Y, 0, 3, _) -> {le_buffer, X - 1, Y, 0,  1};
direct_link_from(X, Y, 1, 3, _) -> {le_buffer, X - 1, Y, 0,  3};
direct_link_from(X, Y, 2, 3, _) -> {le_buffer, X - 1, Y, 0,  5};
direct_link_from(X, Y, 3, 3, _) -> {le_buffer, X - 1, Y, 0,  7};
direct_link_from(X, Y, 4, 3, _) -> {le_buffer, X - 1, Y, 0,  9};
direct_link_from(X, Y, 5, 3, _) -> {le_buffer, X - 1, Y, 0, 19};
direct_link_from(X, Y, 6, 3, _) -> {le_buffer, X - 1, Y, 0, 17};
direct_link_from(X, Y, 7, 3, _) -> {le_buffer, X - 1, Y, 0, 15};
direct_link_from(X, Y, 8, 3, _) -> {le_buffer, X - 1, Y, 0, 13};
direct_link_from(X, Y, 9, 3, _) -> {le_buffer, X - 1, Y, 0, 11}.

%%--------------------------------------------------------------------

edge(_, _, [], undefined, _) ->
    undefined;
edge(_, _, [], Edge, Locations) ->
    {Edge, Locations};
edge(Density, IOB = {iob, X, Y}, [{Fuse, _, _} | Fuses], Edge, Locations) ->
    case fuse_map:to_location(Fuse, Density) of
        {X, Y, _, 1, side, 8} ->
            % direct-link
            edge(Density, IOB, Fuses, Edge, Locations);

        {X, Y, _, 3, side, 8} ->
            % direct-link
            edge(Density, IOB, Fuses, Edge, Locations);

        Location = {X, Y, _, _, side, _} ->
            edge(Density, IOB, Fuses,
                 left_or_right(X, Edge),
                 [Location | Locations]);

        Location = {X, head, _, cell, _} ->
            edge(Density, IOB, Fuses,
                 top(Edge),
                 [Location | Locations]);

        Location = {X, tail, _, cell, _} ->
            edge(Density, IOB, Fuses,
                 bottom(Edge),
                 [Location | Locations]);

        {_, _, _, _, side, _} ->
            edge(Density, IOB, Fuses, Edge, Locations);

        {_, _, _, _, cell, _} ->
            edge(Density, IOB, Fuses, Edge, Locations)
    end.

%%--------------------------------------------------------------------

left_or_right(X, undefined) ->
    left_or_right(X);
left_or_right(X, Edge) ->
    Edge = left_or_right(X).

%%--------------------------------------------------------------------

left_or_right(0) -> left;
left_or_right(1) -> left;
left_or_right(_) -> right.

%%--------------------------------------------------------------------

top(undefined) -> top;
top(top) -> top.

%%--------------------------------------------------------------------

bottom(undefined) -> bottom;
bottom(bottom) -> bottom.

%%--------------------------------------------------------------------

top_interconnect_theory(_, []) ->
    ok;
top_interconnect_theory(Interconnect, [Location | Locations]) ->
    top_interconnect_map(Interconnect, Location),
    top_interconnect_theory(Interconnect, Locations).

%%--------------------------------------------------------------------

top_interconnect_map({interconnect,  0}, {_, head,  1, cell, 20}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  2, cell, 18}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  2, cell, 21}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  4, cell, 18}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  4, cell, 21}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  6, cell, 18}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  6, cell, 20}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  7, cell, 18}) -> ok;
top_interconnect_map({interconnect,  0}, {_, head,  8, cell, 21}) -> ok;
%
top_interconnect_map({interconnect,  1}, {_, head,  4, cell, 18}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  4, cell, 20}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  4, cell, 21}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  5, cell, 18}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  5, cell, 21}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  6, cell, 21}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  7, cell, 18}) -> ok;
top_interconnect_map({interconnect,  1}, {_, head,  7, cell, 21}) -> ok;
%
top_interconnect_map({interconnect,  2}, {_, head,  5, cell, 18}) -> ok;
top_interconnect_map({interconnect,  2}, {_, head,  5, cell, 19}) -> ok;
top_interconnect_map({interconnect,  2}, {_, head,  5, cell, 21}) -> ok;
top_interconnect_map({interconnect,  2}, {_, head,  6, cell, 18}) -> ok;
top_interconnect_map({interconnect,  2}, {_, head,  6, cell, 20}) -> ok;
top_interconnect_map({interconnect,  2}, {_, head,  6, cell, 21}) -> ok;
%
top_interconnect_map({interconnect,  3}, {_, head,  7, cell, 18}) -> ok;
top_interconnect_map({interconnect,  3}, {_, head,  7, cell, 19}) -> ok;
top_interconnect_map({interconnect,  3}, {_, head,  7, cell, 21}) -> ok;
top_interconnect_map({interconnect,  3}, {_, head,  8, cell, 21}) -> ok;
%
top_interconnect_map({interconnect,  4}, {_, head,  6, cell, 18}) -> ok;
top_interconnect_map({interconnect,  4}, {_, head,  6, cell, 20}) -> ok;
top_interconnect_map({interconnect,  4}, {_, head, 10, cell, 18}) -> ok;
top_interconnect_map({interconnect,  4}, {_, head, 10, cell, 21}) -> ok;
% c4 / r4 ?
top_interconnect_map({interconnect,  _}, {_, head,  _, cell,  2}) -> ok;
top_interconnect_map({interconnect,  _}, {_, head,  _, cell,  3}) -> ok;
top_interconnect_map({interconnect,  _}, {_, head,  _, cell,  4}) -> ok;
top_interconnect_map({interconnect,  _}, {_, head,  _, cell,  5}) -> ok;
top_interconnect_map({interconnect,  _}, {_, head,  _, cell, 10}) -> ok;
%
top_interconnect_map(Interconnect, Location) ->
    io:format("top_interconnect_map(~p, ~p) -> ok;~n",
              [Interconnect, Location]).

%%--------------------------------------------------------------------

left_interconnect_theory(_, []) ->
    ok;
left_interconnect_theory(Interconnect, [Location | Locations]) ->
    left_interconnect_map(Interconnect, Location),
    left_interconnect_theory(Interconnect, Locations).

%%--------------------------------------------------------------------

left_interconnect_map({interconnect,  2}, {_, _, 1, 2, side,  7}) -> ok;
left_interconnect_map({interconnect,  2}, {_, _, 1, 3, side, 10}) -> ok;
%
left_interconnect_map({interconnect,  3}, {_, _, 2, 0, side,  7}) -> ok;
left_interconnect_map({interconnect,  3}, {_, _, 2, 1, side, 10}) -> ok;
%
left_interconnect_map({interconnect,  4}, {_, _, 2, 2, side,  7}) -> ok;
left_interconnect_map({interconnect,  4}, {_, _, 2, 3, side,  7}) -> ok;
left_interconnect_map({interconnect,  4}, {_, _, 2, 3, side,  9}) -> ok;
%
left_interconnect_map({interconnect,  5}, {_, _, 3, 1, side,  7}) -> ok;
left_interconnect_map({interconnect,  5}, {_, _, 3, 1, side,  9}) -> ok;
%
left_interconnect_map({interconnect,  6}, {_, _, 3, 2, side, 10}) -> ok;
left_interconnect_map({interconnect,  6}, {_, _, 3, 3, side,  7}) -> ok;
%
left_interconnect_map({interconnect, 11}, {_, _, 8, 2, side,  7}) -> ok;
left_interconnect_map({interconnect, 11}, {_, _, 8, 3, side,  9}) -> ok;
%
left_interconnect_map({interconnect, 12}, {_, _, 7, 0, side,  7}) -> ok;
left_interconnect_map({interconnect, 12}, {_, _, 7, 1, side,  9}) -> ok;
%
left_interconnect_map({interconnect, 13}, {_, _, 7, 2, side,  7}) -> ok;
left_interconnect_map({interconnect, 13}, {_, _, 7, 2, side, 10}) -> ok;
left_interconnect_map({interconnect, 13}, {_, _, 7, 3, side,  7}) -> ok;
left_interconnect_map({interconnect, 13}, {_, _, 7, 3, side, 10}) -> ok;
%
left_interconnect_map({interconnect, 14}, {_, _, 6, 1, side,  7}) -> ok;
left_interconnect_map({interconnect, 14}, {_, _, 6, 1, side, 10}) -> ok;
%
left_interconnect_map({interconnect, 15}, {_, _, 6, 3, side,  7}) -> ok;
left_interconnect_map({interconnect, 15}, {_, _, 6, 2, side,  9}) -> ok;
%
left_interconnect_map(Interconnect, Location) ->
    io:format("left_interconnect_map(~p, ~p) -> ok;~n",
              [Interconnect, Location]).

%%--------------------------------------------------------------------

right_interconnect_theory(_, []) ->
    ok;
right_interconnect_theory(Interconnect, [Location | Locations]) ->
    right_interconnect_map(Interconnect, Location),
    right_interconnect_theory(Interconnect, Locations).

%%--------------------------------------------------------------------

right_interconnect_map(Interconnect, Location) ->
    io:format("right_interconnect_map(~p, ~p) -> ok;~n",
              [Interconnect, Location]).

%%--------------------------------------------------------------------

bottom_interconnect_theory(_, []) ->
    ok;
bottom_interconnect_theory(Interconnect, [Location | Locations]) ->
    bottom_interconnect_map(Interconnect, Location),
    bottom_interconnect_theory(Interconnect, Locations).

%%--------------------------------------------------------------------

bottom_interconnect_map({interconnect,  0}, {_, tail,  1, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  1, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  1, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  2, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  2, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  2, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  3, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  4, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  4, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail,  9, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  0}, {_, tail, 10, cell, 18}) -> ok;
%
bottom_interconnect_map({interconnect,  1}, {_, tail,  1, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  2, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  3, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  3, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  3, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  4, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  4, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  4, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  5, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  5, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  6, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  6, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  7, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  7, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  8, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  9, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail,  9, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail, 10, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail, 10, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  1}, {_, tail, 10, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  2}, {_, tail,  5, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  5, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  5, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  5, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  6, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  6, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  2}, {_, tail,  6, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  3}, {_, tail,  7, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  7, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  7, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  8, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  8, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  8, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  9, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  3}, {_, tail,  9, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  4}, {_, tail,  9, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail,  9, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail,  9, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail,  9, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail, 10, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail, 10, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  4}, {_, tail, 10, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  5}, {_, tail,  3, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail,  4, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail,  7, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail,  7, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail,  9, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail,  9, cell, 21}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail, 10, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  5}, {_, tail, 10, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  6}, {_, tail,  9, cell, 19}) -> ok;
bottom_interconnect_map({interconnect,  6}, {_, tail, 10, cell, 21}) -> ok;
%
bottom_interconnect_map({interconnect,  8}, {_, tail,  1, cell, 20}) -> ok;
bottom_interconnect_map({interconnect,  8}, {_, tail,  2, cell, 18}) -> ok;
bottom_interconnect_map({interconnect,  8}, {_, tail,  2, cell, 20}) -> ok;
% c4 / r4 ?
bottom_interconnect_map({interconnect,  _}, {_, tail,  _, cell,  2}) -> ok;
bottom_interconnect_map({interconnect,  _}, {_, tail,  _, cell,  3}) -> ok;
bottom_interconnect_map({interconnect,  _}, {_, tail,  _, cell,  4}) -> ok;
bottom_interconnect_map({interconnect,  _}, {_, tail,  _, cell,  5}) -> ok;
%
bottom_interconnect_map(Interconnect, Location) ->
    io:format("bottom_interconnect_map(~p, ~p) -> ok;~n",
              [Interconnect, Location]).

