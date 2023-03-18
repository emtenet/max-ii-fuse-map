-module(data_mux_playground).

-export([run/0]).

% This playground is the supporting detail for `data_mux_theory`.
%
% How are inputs muxed into the four LUT data inputs?
%   data_a, data_b, data_c & data_d.

%-define(PRINT_MATRIX).
%-define(PRINT_OTHER_FUSES).

-export([source_0/1]).
-export([source_1/2]).
-export([source_2/3]).
-export([source_3/4]).
-export([source_4/5]).
-export([print_routes/1]).

-define(DENSITY, epm240).
-define(DEVICE, epm240_t100).
-define(X, 6).
%-define(X, 5).
-define(Y, 4).
-define(LAB, {lab, ?X, ?Y}).

% lc/ioc pair chosen so that there is an io_bypass_out connection
% between them, this minimizes the number of interconnect fuses
% that are not yet understood.
-define(THRU, {{lc, ?X, ?Y, 0}, pin82, {ioc, ?X, ?Y + 1, 3}}).
%-define(THRU, {{lc, ?X, ?Y, 0}, pin88, {ioc, ?X, ?Y + 1, 3}}).
%-define(THRU, {{lc, ?X, ?Y, 1}, pin87, {ioc, ?X, ?Y + 1, 2}}).
%-define(THRU, {{lc, ?X, ?Y, 2}, pin86, {ioc, ?X, ?Y + 1, 1}}).
%-define(THRU, {{lc, ?X, ?Y, 3}, pin85, {ioc, ?X, ?Y + 1, 0}}).

% for example:
%   #route{
%       data_a = {<<"7">>, [{lc,5,4,7},
%                           {local_line,5,4,0,7}]},
%       data_b = {<<"X">>, [{lc,5,4,6},
%                           {le_buffer,5,4,0,12},
%                           {local_interconnect,5,4,0,20}]},
%       data_c = {<<" ">>, []},
%       data_d = {<<"Y">>, [{lc,5,4,5},
%                           {le_buffer,5,4,0,10},
%                           {c4,4,4,0,0},
%                           {local_interconnect,5,4,0,5}]}
%   }

-record(route, {
    data_a :: {binary(), [tuple()]},
    data_b :: {binary(), [tuple()]},
    data_c :: {binary(), [tuple()]},
    data_d :: {binary(), [tuple()]}
}).

%%====================================================================
%% run
%%====================================================================

run() ->
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(sources()),
    Matrix0 = matrix:build_with_map(?DENSITY, Experiments),
    Matrix1 = matrix:remove_fuses(Matrix0, fun
        ({_, enable}) -> true;
        ({_, output_invert}) -> true;
        ({_, local_line}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix1),
    Matrix = matrix:remove_fuses(Matrix1, fun
        % thru-fuses
        %({head, ?X, cell, _, _}) -> true; % {io_bypass_out,5,5,3,0}
        %({cell, ?X, 16, ?Y, 0, 1}) -> true; % {le_buffer,5,4,0,1}
        %({cell, ?X, 17, ?Y, 0, 1}) -> true; % {le_buffer,5,4,0,1}
        (_) -> false
    end),
    print_matrix(Matrix),
    {Keys, Routes} = routes(Experiments),
    print_keys(Keys),
    %
    %confirm_thru_fuses(Experiments, Matrix1),
    %
    FusesA = port_fuses(data_a, Keys, Routes, Matrix),
    FusesB = port_fuses(data_b, Keys, Routes, Matrix),
    FusesC = port_fuses(data_c, Keys, Routes, Matrix),
    FusesD = port_fuses(data_d, Keys, Routes, Matrix),
    print_other_fuses(FusesA, FusesB, FusesC, FusesD, Matrix),
    ok.

%%--------------------------------------------------------------------

-ifdef(PRINT_MATRIX).
print_matrix(Matrix) ->
    matrix:print(Matrix).
-else.
print_matrix(_) ->
    ok.
-endif.

%%--------------------------------------------------------------------

-ifdef(PRINT_OTHER_FUSES).
print_other_fuses(FusesA, FusesB, FusesC, FusesD, {matrix, _, Fuses}) ->
    io:format("~nOther  fuses:~n", []),
    [
        io:format("~6b ~w~n", [Fuse, Name])
        ||
        {Fuse, _, Name} <- Fuses,
        not lists:member(Fuse, FusesA),
        not lists:member(Fuse, FusesB),
        not lists:member(Fuse, FusesC),
        not lists:member(Fuse, FusesD)
    ],
    ok.
-else.
print_other_fuses(_, _, _, _, _) ->
    ok.
-endif.

%%====================================================================
%% confirm_thru_fuses
%%====================================================================

%confirm_thru_fuses(Experiments, Matrix) ->
%    [_ | Zeros] = [ 0 || _ <- Experiments ],
%    Pattern = [1 | Zeros],
%    [{_, {head, ?X, cell, _, _}}, % {io_bypass_out,5,5,3,0}
%     {_, {cell, ?X, 16, ?Y, 0, 1}} % {le_buffer,5,4,0,1}
%    ] = matrix:pattern_is(Matrix, Pattern).

%%====================================================================
%% route utilities
%%====================================================================

port_fuses(Port, Keys, Routes, Matrix) ->
    Pattern = [
        boolean_to(port_is_empty(Route, Port), 1, x)
        ||
        Route <- Routes
    ],
    InitialFuses = [
        {Fuse, #{}, Name}
        ||
        {Fuse, Name} <- matrix:pattern_match(Matrix, Pattern)
    ],
    Fuses = lists:foldl(fun ({Key, _}, Acc) ->
        port_fuses(Port, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    io:format("~n~s: ~s~n", [
        Port,
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                port_fuse_bit(Key, Bits)
                ||
                {Key, _} <- Keys
            ]),
            Name
        ])
        ||
        {Fuse, Bits, Name} <- Fuses
    ],
    [ Fuse || {Fuse, _, _} <- Fuses ].

%%--------------------------------------------------------------------

port_fuses(Port, Key, Routes, Matrix, Fuses) ->
    IsKey = [ port_key_is(Route, Port, Key) || Route <- Routes ],
    case lists:member(true, IsKey) of
        true ->
            Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsKey ],
            Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsKey ],
            Fuses0 = matrix:pattern_match(Matrix, Pattern0),
            Fuses1 = matrix:pattern_match(Matrix, Pattern1),
            lists:filtermap(fun ({Fuse, Bits, Name}) ->
                port_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name)
            end, Fuses);

        false ->
            Fuses
    end.

%%--------------------------------------------------------------------

port_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Key => 0}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

port_fuse_bit(Key, Bits) ->
    case Bits of
        #{Key := 0} -> <<"-">>;
        #{Key := 1} -> <<"#">>;
        _ -> <<" ">>
    end.

%%====================================================================
%% route utilities
%%====================================================================

port_is_empty(#route{data_a = {_, []}}, data_a) ->
    true;
port_is_empty(#route{data_b = {_, []}}, data_b) ->
    true;
port_is_empty(#route{data_c = {_, []}}, data_c) ->
    true;
port_is_empty(#route{data_d = {_, []}}, data_d) ->
    true;
port_is_empty(_, _) ->
    false.

%%--------------------------------------------------------------------

port_key_is(#route{data_a = {Key, _}}, data_a, Key) ->
    true;
port_key_is(#route{data_b = {Key, _}}, data_b, Key) ->
    true;
port_key_is(#route{data_c = {Key, _}}, data_c, Key) ->
    true;
port_key_is(#route{data_d = {Key, _}}, data_d, Key) ->
    true;
port_key_is(_, _, _) ->
    false.

%%====================================================================
%% generate & print routes
%%====================================================================

routes(Experiments) ->
    Init = {
        maps:from_list([
            {[{lc, ?X, ?Y, N}, {local_line, ?X, ?Y, 0, N}], <<($0 + N)>>}
            ||
            N <- lists:seq(1, 9)
        ]),
        maps:from_list([
            {<<($0 + N)>>, [{lc, ?X, ?Y, N}, {local_line, ?X, ?Y, 0, N}]}
            ||
            N <- lists:seq(1, 9)
        ]),
        $A
    },
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

route_keys_signal(_, #{lc := LC, dests := Dests}, Acc0) ->
    lists:foldl(
        fun (Dest, Acc) -> route_keys_dest(LC, Dest, Acc) end,
        Acc0,
        Dests
    ).

%%--------------------------------------------------------------------

route_keys_dest(_, #{port := data_in}, Acc) ->
    Acc;
route_keys_dest(
            LC,
            #{route_port := _, route := Route0},
            Acc = {ToKey, ToRoute, Key0}
        ) ->
    Route = [LC | lists:reverse(Route0)],
    case ToKey of
        #{Route := _} ->
            Acc;

        _ ->
            Key = <<Key0>>,
            {ToKey#{Route => Key}, ToRoute#{Key => Route}, Key0 + 1}
    end.

%%--------------------------------------------------------------------

route_experiment({_, _, #{signals := Signals}}, RouteToKey) ->
    #route{
        data_a = route_signals(Signals, data_a, RouteToKey),
        data_b = route_signals(Signals, data_b, RouteToKey),
        data_c = route_signals(Signals, data_c, RouteToKey),
        data_d = route_signals(Signals, data_d, RouteToKey)
    }.

%%--------------------------------------------------------------------

route_signals(Signals, Port, RouteToKey) ->
    None = {<<" ">>, []},
    maps:fold(fun (_, Signal, Acc) ->
         route_signal(Signal, Port, RouteToKey, Acc)
    end, None, Signals).

%%--------------------------------------------------------------------

route_signal(#{lc := LC, dests := Dests}, Port, RouteToKey, Init) ->
    lists:foldl(fun (Dest, Acc) ->
        route_dest(LC, Dest, Port, RouteToKey, Acc)
    end, Init, Dests).

%%--------------------------------------------------------------------

route_dest(_, #{port := data_in}, _, _, Acc) ->
    Acc;
route_dest(LC, #{route_port := Port, route := Route0}, Port, RouteToKey, _) ->
    Route = [LC | lists:reverse(Route0)],
    #{Route := Key} = RouteToKey,
    {Key, Route};
route_dest(_, _, _, _, Acc) ->
    Acc.

%%--------------------------------------------------------------------

print_keys(Keys) ->
    [
        io:format("~s: ~w~n", [Key, Route])
        ||
        {Key, Route} <- Keys
    ],
    ok.

%%--------------------------------------------------------------------

print_routes(Routes) ->
    print_routes(Routes, data_a, fun (#route{data_a = {C, _}}) -> C end, []),
    print_routes(Routes, data_b, fun (#route{data_b = {C, _}}) -> C end, []),
    print_routes(Routes, data_c, fun (#route{data_c = {C, _}}) -> C end, []),
    print_routes(Routes, data_d, fun (#route{data_d = {C, _}}) -> C end, []),
    ok.

%%--------------------------------------------------------------------

print_routes([], Port, _, Line) ->
    io:format("~s:~s~n", [Port, lists:reverse(Line)]);
print_routes([Route | Routes], Port, Char, Line) ->
    print_routes(Routes, Port, Char, [Char(Route), <<" ">> | Line]).

%%====================================================================
%% utilities
%%====================================================================

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

%%====================================================================
%% sources
%%====================================================================

sources() ->
    Seq = lists:seq(1, 9),
    Side = lists:seq(0, 9),
    lists:flatten([
        source_0(?THRU),
        [
            source_1(?THRU, {l, A})
            ||
            A <- Side
        ],
        [
            source_1(?THRU, {r, A})
            ||
            A <- Side
        ],
        [
            source_2(?THRU, {l, A}, {r, B})
            ||
            A <- Side,
            B <- Side
        ],
        [
            source_2(?THRU, {l, A}, {l, B})
            ||
            A <- Side,
            B <- Side,
            B > A
        ],
        [
            source_2(?THRU, {r, A}, {r, B})
            ||
            A <- Side,
            B <- Side,
            B > A
        ],
        source_4(?THRU, {l, 0}, {l, 1}, {l, 2}, {l, 3}),
        [
            source_1(?THRU, A)
            ||
            A <- Seq
        ],
        [
            source_2(?THRU, A, B)
            ||
            A <- Seq,
            B <- Seq,
            B > A
        ],
        [
            source_3(?THRU, A, B, C)
            ||
            A <- Seq,
            B <- Seq,
            B > A,
            C <- Seq,
            C > B
        ],
        [
            source_4(?THRU, A, B, C, D)
            ||
            A <- Seq,
            B <- Seq,
            B > A,
            C <- Seq,
            C > B,
            D <- Seq,
            D > C
        ]
    ]).

%%--------------------------------------------------------------------

source_lc({l, N}) ->
    {lc, ?X - 1, ?Y, N};
source_lc({r, N}) ->
    {lc, ?X + 1, ?Y, N};
source_lc(N) ->
    {lc, ?X, ?Y, N}.

%%--------------------------------------------------------------------

source_0({_, Pin, _}) ->
    #{
        title => {out, Pin},
        device => ?DEVICE,
        settings => [
            {location, q, Pin}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  q <= '1';\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_1({Thru, Pin, _}, A) ->
    #{
        title => {A, thru, Pin},
        device => ?DEVICE,
        settings => [
            {location, a, source_lc(A)},
            {location, thru, Thru},
            {location, q, Pin}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "  signal a_q : STD_LOGIC;\n"
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => a_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => a_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_2({Thru, Pin, _}, A, B) ->
    #{
        title => {A, B, thru, Pin},
        device => ?DEVICE,
        settings => [
            {location, a, source_lc(A)},
            {location, b, source_lc(B)},
            {location, thru, Thru},
            {location, q, Pin}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "  signal a_q : STD_LOGIC;\n"
            "  signal b_q : STD_LOGIC;\n"
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => a_q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => b_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => a_q AND b_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_3({Thru, Pin, _}, A, B, C) ->
    #{
        title => {A, B, C, thru, Pin},
        device => ?DEVICE,
        settings => [
            {location, a, source_lc(A)},
            {location, b, source_lc(B)},
            {location, c, source_lc(C)},
            {location, thru, Thru},
            {location, q, Pin}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "  signal a_q : STD_LOGIC;\n"
            "  signal b_q : STD_LOGIC;\n"
            "  signal c_q : STD_LOGIC;\n"
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => a_q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => b_q\n"
            "  );\n",
            "  c: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => c_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => a_q AND b_q AND c_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

source_4({Thru, Pin, _}, A, B, C, D) ->
    #{
        title => {A, B, C, D, thru, Pin},
        device => ?DEVICE,
        settings => [
            {location, a, source_lc(A)},
            {location, b, source_lc(B)},
            {location, c, source_lc(C)},
            {location, d, source_lc(D)},
            {location, thru, Thru},
            {location, q, Pin}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "  signal a_q : STD_LOGIC;\n"
            "  signal b_q : STD_LOGIC;\n"
            "  signal c_q : STD_LOGIC;\n"
            "  signal d_q : STD_LOGIC;\n"
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => a_q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => b_q\n"
            "  );\n",
            "  c: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => c_q\n"
            "  );\n",
            "  d: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => d_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => a_q AND b_q AND c_q AND d_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

