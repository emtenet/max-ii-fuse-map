-module(lut_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    {Rows, VHDLs} = pattern(),
    [
        run_density(Density, Rows, VHDLs)
        ||
        Density <- density:list()
    ],
    ok.

%%--------------------------------------------------------------------

run_density(Density, Rows, VHDLs) ->
    Device = density:largest_device(Density),
    [A, B, C, D, Q | _] = device:pins(Device),
    Settings = [
        {location, a, A},
        {location, b, B},
        {location, c, C},
        {location, d, D},
        {location, q, Q}
    ],
    [
        run_lc(lab:lc(LAB, LC), Density, Device, Settings, Rows, VHDLs)
        ||
        LAB <- device:labs(Device),
        LC <- lists:seq(0, 9)
    ],
    ok.

%%--------------------------------------------------------------------

run_lc(LC, Density, Device, Settings0, Rows, VHDLs) ->
    io:format(" => ~s ~p~n", [Device, LC]),
    Settings = [{location, lut, LC} | Settings0],
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        #{
            title => Name,
            device => Device,
            settings => Settings,
            vhdl => VHDL
        }
        ||
        {Name, VHDL} <- VHDLs
    ]),
    %Matrix = matrix:build(Density, Experiments),
    %matrix:print(Matrix),
    Matrix = matrix:build(Experiments),
    [Order] = lists:usort([ run_rcf(RCF) || {_, _, RCF} <- Experiments ]),
    Fuses = [ run_fuse(Matrix, LC, Row, Order) || Row <- Rows ],
    16 = length(Fuses),
    fuse_database:update(Density, Fuses).

%%--------------------------------------------------------------------

run_rcf(#{signals := #{a := A, b := B, c := C, d := D}}) ->
    #{dests := [#{route_port := DataA}]} = A,
    #{dests := [#{route_port := DataB}]} = B,
    #{dests := [#{route_port := DataC}]} = C,
    #{dests := [#{route_port := DataD}]} = D,
    [{data_a, PortA}, {data_b, PortB}, {data_c, PortC}, {data_d, PortD}]
        = lists:sort([{DataA, a}, {DataB, b}, {DataC, c}, {DataD, d}]),
    {PortA, PortB, PortC, PortD}.

%%--------------------------------------------------------------------

run_fuse(Matrix, LC, {Row, Pattern}, Order) ->
    [{Fuse, _}] = matrix:pattern_is(Matrix, Pattern),
    {Fuse, {LC, lut, run_order(Order, Row)}}.

%%--------------------------------------------------------------------

run_order({OrderA, OrderB, OrderC, OrderD}, Row) ->
    DataA = run_bit(OrderA, Row),
    DataB = run_bit(OrderB, Row),
    DataC = run_bit(OrderC, Row),
    DataD = run_bit(OrderD, Row),
    case {DataA, DataB, DataC, DataD} of
        {0, 0, 0, 0} -> a0b0c0d0;
        {0, 0, 0, 1} -> a0b0c0d1;
        {0, 0, 1, 0} -> a0b0c1d0;
        {0, 0, 1, 1} -> a0b0c1d1;
        {0, 1, 0, 0} -> a0b1c0d0;
        {0, 1, 0, 1} -> a0b1c0d1;
        {0, 1, 1, 0} -> a0b1c1d0;
        {0, 1, 1, 1} -> a0b1c1d1;
        {1, 0, 0, 0} -> a1b0c0d0;
        {1, 0, 0, 1} -> a1b0c0d1;
        {1, 0, 1, 0} -> a1b0c1d0;
        {1, 0, 1, 1} -> a1b0c1d1;
        {1, 1, 0, 0} -> a1b1c0d0;
        {1, 1, 0, 1} -> a1b1c0d1;
        {1, 1, 1, 0} -> a1b1c1d0;
        {1, 1, 1, 1} -> a1b1c1d1
    end.

%%--------------------------------------------------------------------

run_bit(a, {A, _, _, _}) -> A;
run_bit(b, {_, B, _, _}) -> B;
run_bit(c, {_, _, C, _}) -> C;
run_bit(d, {_, _, _, D}) -> D.

%%--------------------------------------------------------------------

pattern() ->
    % pattern chosen so:
    %  * all rows unique
    %  * column 1 has 8 1s
    %  * column 2 has 7 1s
    %  * column 3 has 6 1s
    %  * column 4 has 5 1s
    %  * column 5 has 4 1s
    %  * not symetric
    %  * expressions cannot be reduced to less than 4 variables
    Rows = [
        {{0,0,0,0}, [1,0,0,0,0]},
        {{0,0,0,1}, [0,1,0,0,0]},
        {{0,0,1,0}, [0,0,1,0,0]},
        {{0,0,1,1}, [0,0,0,1,0]},
        {{0,1,0,0}, [0,0,0,0,1]},
        {{0,1,0,1}, [1,1,0,0,0]},
        {{0,1,1,0}, [1,0,1,0,0]},
        {{0,1,1,1}, [0,1,1,0,0]},
        {{1,0,0,0}, [1,0,0,1,0]},
        {{1,0,0,1}, [0,1,0,1,0]},
        {{1,0,1,0}, [0,0,1,1,0]},
        {{1,0,1,1}, [1,0,0,0,1]},
        {{1,1,0,0}, [0,1,0,0,1]},
        {{1,1,0,1}, [1,1,1,0,0]},
        {{1,1,1,0}, [1,1,0,1,0]},
        {{1,1,1,1}, [1,0,1,0,1]}
    ],
    16 = length(lists:usort([ Pattern || {_, Pattern} <- Rows ])),
    VHDLs = [
        vhdl(N, Rows)
        ||
        N <- [
            {experiment_a, 1},
            {experiment_b, 2},
            {experiment_c, 3},
            {experiment_d, 4},
            {experiment_e, 5}
        ]
    ],
    {Rows, VHDLs}.

%%--------------------------------------------------------------------

vhdl({Name, N}, Rows) ->
    Nth = [ {I, lists:nth(N, L)} || {I, L} <- Rows ],
    Expr = expr_or(Nth, <<>>),
    {Name, vhdl(Expr)}.

%%--------------------------------------------------------------------

vhdl(Expr) ->
    <<
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
        "architecture behavioral of experiment is\n"
        "begin\n"
        "  lut: LCELL port map (\n"
        "    a_in => ", Expr/binary, ",\n"
        "    a_out => q\n"
        "  );\n"
        "end behavioral;\n"
     >>.

%%--------------------------------------------------------------------

expr_or([], Expr) ->
    Expr;
expr_or([{_, 0} | Rows], Expr) ->
    expr_or(Rows, Expr);
expr_or([{I, 1} | Rows], Expr = <<>>) ->
    expr_or(Rows, expr_and(I, Expr));
expr_or([{I, 1} | Rows], Expr) ->
    expr_or(Rows, expr_and(I, <<Expr/binary, " OR ">>)).

%%--------------------------------------------------------------------

expr_and({A, B, C, D}, Expr) ->
    Aexpr = case A of 0 -> <<"(NOT a)">>; 1 -> <<"a">> end,
    Bexpr = case B of 0 -> <<"(NOT b)">>; 1 -> <<"b">> end,
    Cexpr = case C of 0 -> <<"(NOT c)">>; 1 -> <<"c">> end,
    Dexpr = case D of 0 -> <<"(NOT d)">>; 1 -> <<"d">> end,
    <<
        Expr/binary,
        "(",
        Aexpr/binary,
        " AND ",
        Bexpr/binary,
        " AND ",
        Cexpr/binary,
        " AND ",
        Dexpr/binary,
        ")"
    >>.

