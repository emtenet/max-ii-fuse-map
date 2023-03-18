-module(lab_interconnect_mux_experiment).

-export([run/0]).

% Looking for the muxes selecting inputs:
%
%   * direct-links (le_buffer),
%   * c4s, and
%   * r4sA
%
% into a LAB X, Y.
%
% Assumption 1: that mux fuses will be in the same LAB
% coorinate space. i.e.
%   {X, Y, ?, ?, cell, ?}A
%
% Initial experiments suggest that the fuses are located:
%   {X, Y, 0..9, 0..3, cell, 2..5} and
%   {X, Y, 0..9, 0..3, cell, 22..25}
%
% For example (EPM240):
%   {local_interconnect,2,4,0,0}
%   a: {c4,2,3,0,1}
%   b: {c4,2,2,0,0}
%   c: {le_buffer,3,4,0,0}
%   d: {r4,3,4,0,8}
%           a b c d
%     9008 |#|-|#|-| {2,4,0,2,cell,22}
%     9264 |-|#|#|#| {2,4,0,2,cell,23}
%     9265 |#|#|-|#| {2,4,0,3,cell,23}
%     9520 |-|#|#|#| {2,4,0,2,cell,24}
%     9776 |#|-|#|#| {2,4,0,2,cell,25}
%     9777 |#|#|#|-| {2,4,0,3,cell,25}
%
% Occationallsy other Sector 0 and 1 fuses come up as
% candidate fuses.
%
% Assumtpion 2: only fuses with:
%   N: 0..9
%   I: 0..3
%   Sector: 2..5 and 22..25
% are considered.
%
% Assumption 3: an interconnect's fuses are grouped by common
%   N,
%   N either low 0..1 or high 2..3
%   Sector either low 2..5 or high 22..25
%   !! 10 * 2 * 2 == 40 combinations !!
%   but only 26 local interconnects exist (see data_mux_theory)
%
% Assumption 4: an interconnect's fuses are combined from
% a two dimentional pair of one-cold muxes OR
% a single one-cold fuse for direct-link (le_buffer).
%
% The above example has:
%   * single one-cold 9265 for {le_buffer,3,4,0,0}
%   * first side mux, sectors 22..23
%   * second side mux, sectors 24..25
% Is the one-cold fuse reused in the two dimentional?
%
% By experiment the single one-cold fuse is either:
%   * {X, Y, ?, ?, 3, cell, 23} or
%   * {X, Y, ?, ?, 3, cell, 4}
% Suggesting that fuses are ordered in contrary motion:
%   *  2,  3,  (4),  5
%   * 25, 24, (23), 22
%
% One example
%    {local_interconnect,1,4,0,22}
%           a b
%     4080 |#|-| {1,4,7,0,cell,2}
%     4336 |-|#| {1,4,7,0,cell,3}
%     4848 |-|-| {1,4,7,0,cell,5}
%     9460 |#|-| {1,4,6,0,cell,23}
%     9715 |#|-| {1,4,6,1,cell,24}
% conflicts with more than two fuses-cold.
%
% Assumption 5: There are only 26 local interconnects so
% 14 of the 40 combinations are for something else. Ignore them.
% Confirmed by lab_interconnect_limit_experiment.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(
        fun (LAB) -> iob(Density, Device, LAB) end,
        density:labs(Density)
    ).

%%--------------------------------------------------------------------

iob(Density, Device, ThruLAB) ->
    io:format(" ==> ~s thru ~p~n", [Density, ThruLAB]),
    {Pin, Thru} = pin(Density, Device, ThruLAB),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        direct_link_experiment:source(Device, LC, Thru, Pin)
        ||
        LAB <- density:labs(Density),
        LAB =/= ThruLAB,
        LC <- lcs(LAB)
    ]),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, enable}) -> true;
        ({_, output_invert}) -> true;
        ({_, fast_out}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, lut_out, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    {Keys, Routes} = routes(Experiments),
    interconnects(Density, Keys, Routes, Matrix),
    ok.

%%--------------------------------------------------------------------

pin(Density, Device, Thru) ->
    pin_from(device:iocs(Device), density:fast_outs(Density), Thru, undefined).

%%--------------------------------------------------------------------

pin_from([], _, Thru, {Pin, _}) ->
    {Pin, lab:lc(Thru, 9)};
pin_from([{Pin, IOC} | Pins], FastOuts, Thru = {lab, X, Y}, Best) ->
    case lists:keyfind(IOC, 1, FastOuts) of
        {_, LC = {lc, X, Y, _}, _} ->
            {Pin, LC};

        _ ->
            case {IOC, Thru} of
                {{ioc, X, PinY, _}, {lab, X, ThruY}} ->
                    Dist = abs(PinY - ThruY),
                    pin_from(Pins, FastOuts, Thru, Best, Pin, Dist);

                {{ioc, PinX, Y, _}, {lab, ThruX, Y}} ->
                    Dist = abs(PinX - ThruX),
                    pin_from(Pins, FastOuts, Thru, Best, Pin, Dist);

                _ ->
                    pin_from(Pins, FastOuts, Thru, Best)
            end

    end.

%%--------------------------------------------------------------------

pin_from(Pins, FastOuts, Thru, undefined, Pin, Dist) ->
    pin_from(Pins, FastOuts, Thru, {Pin, Dist});
pin_from(Pins, FastOuts, Thru, {_, Best}, Pin, Dist) when Dist < Best->
    pin_from(Pins, FastOuts, Thru, {Pin, Dist});
pin_from(Pins, FastOuts, Thru, Best, _, _) ->
    pin_from(Pins, FastOuts, Thru, Best).

%%--------------------------------------------------------------------

lcs(LAB) ->
    lab:lcs(LAB).

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
    #{lut := #{dests := [Dest]}} = Signals,
    route_keys_dest(Dest, Acc).

%%--------------------------------------------------------------------

route_keys_dest(#{route_port := _, route := [Route, From | _]}, Acc) ->
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
    #{lut := #{dests := [Dest]}} = Signals,
    route_dest(Dest, RouteToKey).

%%--------------------------------------------------------------------

route_dest(#{route_port := _, route := [Route, From | _]}, RouteToKey) ->
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
        interconnect_initial_fuse(Density, Fuse, Interconnect)
    end, matrix:pattern_match(Matrix, InitialPattern)),
    Fuses = lists:foldl(fun ({Sub, _}, Acc) ->
        interconnect_fold_fuses(Key, Sub, Routes, Matrix, Acc)
    end, InitialFuses, Subs0),
    %
    Interconnect = interconnect_from_fuses(Fuses),
    Subs = lists:map(fun (Sub) -> interconnect_sub(Sub, Fuses) end, Subs0),
    Mapping = lists:map(fun interconnect_found/1, Subs),
    lab_interconnect_mux_database:update(Density, Mapping),
    ok.

%%--------------------------------------------------------------------

interconnect_is_key({Key, _}, Key) -> 'x';
interconnect_is_key(_, _) -> 1.

%%--------------------------------------------------------------------

interconnect_initial_fuse(Density, {Fuse, Name}, Interconnect) ->
    {local_interconnect, X, Y, 0, _} = Interconnect,
    case fuse_map:to_location(Fuse, Density) of
        Location = {X, Y, _, _, cell, Sector}
                when (Sector >= 2 andalso Sector =< 5) orelse
                     (Sector >= 22 andalso Sector =< 25) ->
            case interconnect_from_fuse(Location) of
                false ->
                    false;

                _ ->
                    {true, {Fuse, #{}, Location, Name}}
            end;

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

interconnect_from_fuses([{_, _, Location, _} | Fuses]) ->
    Interconnect = interconnect_from_fuse(Location),
    interconnect_from_fuses(Interconnect, Fuses).

%%--------------------------------------------------------------------

interconnect_from_fuses(Interconnect, []) ->
    Interconnect;
interconnect_from_fuses(Interconnect, [{_, _, Location, _} | Fuses]) ->
    Interconnect = interconnect_from_fuse(Location),
    interconnect_from_fuses(Interconnect, Fuses).

%%--------------------------------------------------------------------

interconnect_from_fuse({X, Y, N, Index, cell, Sector}) ->
    Number = interconnect_number(
        N,
        interconnect_index(Index),
        interconnect_sector(Sector)
    ),
    case Number of
        false ->
            false;

        _ ->
            {local_interconnect, X, Y, 0, Number}
    end.

%%--------------------------------------------------------------------

interconnect_index(0) -> lo;
interconnect_index(1) -> lo;
interconnect_index(2) -> hi;
interconnect_index(3) -> hi.

%%--------------------------------------------------------------------

interconnect_sector(2) -> lo;
interconnect_sector(3) -> lo;
interconnect_sector(4) -> lo;
interconnect_sector(5) -> lo;
interconnect_sector(22) -> hi;
interconnect_sector(23) -> hi;
interconnect_sector(24) -> hi;
interconnect_sector(25) -> hi.

%%--------------------------------------------------------------------

interconnect_number(0, hi, hi) ->  0;
interconnect_number(1, hi, hi) ->  1;
interconnect_number(2, hi, hi) ->  2;
interconnect_number(3, hi, hi) ->  3;
interconnect_number(4, hi, hi) ->  4;
interconnect_number(0, lo, lo) ->  5;
interconnect_number(0, hi, lo) ->  6;
interconnect_number(1, lo, lo) ->  7;
interconnect_number(1, hi, lo) ->  8;
interconnect_number(2, lo, lo) ->  9;
interconnect_number(2, hi, lo) -> 10;
interconnect_number(3, lo, lo) -> 11;
interconnect_number(3, hi, lo) -> 12;
interconnect_number(5, hi, hi) -> 13;
interconnect_number(6, hi, hi) -> 14;
interconnect_number(7, hi, hi) -> 15;
interconnect_number(8, hi, hi) -> 16;
interconnect_number(9, hi, hi) -> 17;
interconnect_number(5, lo, lo) -> 18;
interconnect_number(5, hi, lo) -> 19;
interconnect_number(6, lo, lo) -> 20;
interconnect_number(6, hi, lo) -> 21;
interconnect_number(7, lo, lo) -> 22;
interconnect_number(7, hi, lo) -> 23;
interconnect_number(8, lo, lo) -> 24;
interconnect_number(8, hi, lo) -> 25;
interconnect_number(_, _, _) ->
    false.

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
    false = is_direct_link_twin(A),
    false = is_direct_link_twin(B),
    case {interconnect_axis(A), interconnect_axis(B)} of
        {lo, hi} ->
            ok;

        {ho, lo} ->
            ok
    end.

%%--------------------------------------------------------------------

interconnect_axis({{_, _, _, _, cell, Sector}, _})
        when (Sector =:= 2) orelse (Sector =:= 3) orelse
             (Sector =:= 22) orelse (Sector =:= 23) ->
    lo;
interconnect_axis({{_, _, _, _, cell, Sector}, _})
        when (Sector =:= 4) orelse (Sector =:= 5) orelse
             (Sector =:= 24) orelse (Sector =:= 25) ->
    hi.

%%--------------------------------------------------------------------

is_direct_link({{_, _, _, 3, cell, 4}, _}) ->
    true;
is_direct_link({{_, _, _, 3, cell, 23}, _}) ->
    true;
is_direct_link(_) ->
    false.

%%--------------------------------------------------------------------

is_direct_link_twin({{_, _, _, 1, cell, 4}, _}) ->
    true;
is_direct_link_twin({{_, _, _, 1, cell, 23}, _}) ->
    true;
is_direct_link_twin(_) ->
    false.

%%--------------------------------------------------------------------

interconnect_found({_, From, [{_, {LAB, Interconnect, direct_link}}]}) ->
    interconnect_found(LAB, Interconnect, direct_link, From);
interconnect_found({_, From, [{_, {LAB, Interconnect, from4, Mux4}}, {_, {_, _, from3, Mux3}}]}) ->
    interconnect_found(LAB, Interconnect, {Mux4, Mux3}, From);
interconnect_found({_, From, [{_, {LAB, Interconnect, from3, Mux3}}, {_, {_, _, from4, Mux4}}]}) ->
    interconnect_found(LAB, Interconnect, {Mux4, Mux3}, From).

%%--------------------------------------------------------------------

interconnect_found(LAB, Interconnect, Select = direct_link, From) ->
    {lab, X, Y} = LAB,
    {interconnect, N} = Interconnect,
    case direct_link_map(X, Y, N) of
        From ->
            ok;

        Got ->
            interconnect_print(LAB, Interconnect, Select, From),
            throw({from, From, got, Got})
    end,
    {LAB, Interconnect, Select, From};
interconnect_found(LAB, Interconnect, Select = {Mux4, Mux3}, From) ->
    {lab, X, Y} = LAB,
    {interconnect, N} = Interconnect,
    case interconnect_map(X, Y, N, Mux4, Mux3) of
        From ->
            ok;

        Got when Got =/= undefined ->
            interconnect_print(LAB, Interconnect, Select, From),
            throw({from, From, got, Got});

        undefined ->
            interconnect_print_if(LAB, Interconnect, Select, From)
    end,
    {LAB, Interconnect, Select, From}.

%%--------------------------------------------------------------------

%interconnect_print_if(LAB, Interconnect = {interconnect, 0}, Select = {mux1, mux2}, From) ->
%    interconnect_print(LAB, Interconnect, Select, From);
%interconnect_print_if(LAB, Interconnect = {interconnect, 0}, Select = {mux3, mux1}, From) ->
%    interconnect_print(LAB, Interconnect, Select, From);
%interconnect_print(LAB = {_, X, Y}, Interconnect, Select, From = {_, X, Y, 0, _}) ->
%    io:format("{~w,~w,~w,~w}~n", [LAB, Interconnect, Select, From]);
%interconnect_print_if(LAB, Interconnect, Select, From) ->
%    interconnect_print(LAB, Interconnect, Select, From).
interconnect_print_if(_, _, _, _) ->
    ok.

%%--------------------------------------------------------------------

interconnect_print(LAB, Interconnect, Select, From) ->
    io:format("{~w,~w,~w,~w}~n", [LAB, Interconnect, Select, From]).

%%--------------------------------------------------------------------

direct_link_map(X, Y,  0) -> {le_buffer, X + 1, Y, 0,  0};
direct_link_map(X, Y,  1) -> {le_buffer, X + 1, Y, 0,  2};
direct_link_map(X, Y,  2) -> {le_buffer, X + 1, Y, 0,  4};
direct_link_map(X, Y,  3) -> {le_buffer, X + 1, Y, 0,  6};
direct_link_map(X, Y,  4) -> {le_buffer, X + 1, Y, 0,  8};
direct_link_map(X, Y,  6) -> {le_buffer, X - 1, Y, 0,  1};
direct_link_map(X, Y,  8) -> {le_buffer, X - 1, Y, 0,  3};
direct_link_map(X, Y, 10) -> {le_buffer, X - 1, Y, 0,  5};
direct_link_map(X, Y, 12) -> {le_buffer, X - 1, Y, 0,  9};
direct_link_map(X, Y, 13) -> {le_buffer, X + 1, Y, 0, 10};
direct_link_map(X, Y, 14) -> {le_buffer, X + 1, Y, 0, 12};
direct_link_map(X, Y, 15) -> {le_buffer, X + 1, Y, 0, 14};
direct_link_map(X, Y, 16) -> {le_buffer, X + 1, Y, 0, 16};
direct_link_map(X, Y, 17) -> {le_buffer, X + 1, Y, 0, 18};
direct_link_map(X, Y, 19) -> {le_buffer, X - 1, Y, 0, 11};
direct_link_map(X, Y, 21) -> {le_buffer, X - 1, Y, 0, 13};
direct_link_map(X, Y, 23) -> {le_buffer, X - 1, Y, 0, 15};
direct_link_map(X, Y, 25) -> {le_buffer, X - 1, Y, 0, 19};
direct_link_map(_X, _Y, _N) ->
    undefined.

%%--------------------------------------------------------------------

interconnect_map(X, Y,  0, mux0, mux1) -> {c4,        X,     Y - 2, 0,  0};
interconnect_map(X, Y,  0, mux1, mux1) -> {r4,        X + 1, Y,     0,  8};
interconnect_map(X, Y,  0, mux2, mux0) -> {c4,        X,     Y - 1, 0,  1};
interconnect_map(X, Y,  0, mux3, mux0) -> {c4,        X,     Y - 3, 0,  1};
interconnect_map(X, Y,  1, mux0, mux0) -> {c4,        X,     Y,     0,  1};
interconnect_map(X, Y,  1, mux2, mux0) -> {c4,        X,     Y,     0,  2};
interconnect_map(X, Y,  4, mux2, mux2) -> {c4,        X,     Y,     0,  3};
interconnect_map(X, Y,  4, mux3, mux2) -> {r4,        X,     Y,     0, 11};
interconnect_map(X, Y, 12, mux2, mux0) -> {le_buffer, X - 1, Y,     0,  7};
interconnect_map(X, Y, 13, mux1, mux1) -> {c4,        X,     Y,     0,  4};
interconnect_map(X, Y, 14, mux1, mux2) -> {c4,        X,     Y,     0,  5};
interconnect_map(X, Y, 15, mux0, mux0) -> {c4,        X,     Y,     0,  6};
interconnect_map(X, Y, 16, mux1, mux0) -> {r4,        X,     Y,     0, 15};
interconnect_map(X, Y, 25, mux0, mux0) -> {le_buffer, X - 1, Y,     0, 17};
interconnect_map(_X, _Y, _N, _Mux4, _Mux3) ->
    undefined.

