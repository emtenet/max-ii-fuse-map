-module(epm240_fuse_map).

-export([to_fuse/1]).
-export([to_name/1]).

-type fuse() :: fuse:fuse().
-type name() :: name:name().

%%--------------------------------------------------------------------

-define(LINE_WIDTH, 32).

%%--------------------------------------------------------------------

-define(COLUMN_1, 544).
-define(COLUMN_2, 4128).
-define(COLUMN_3, 11296).
-define(COLUMN_4, 18464).
-define(COLUMN_5, 25632).
-define(COLUMN_6, 32800).
-define(COLUMN_7, 39968).
-define(COLUMN_7_END, 47136).
-define(COLUMN_8, 48928).
-define(COLUMN_8_END, 52512).

%%--------------------------------------------------------------------

-define(SECTOR_LEFT, 11).
-define(SECTOR_SKIP, 103).
-define(SECTOR_RIGHT, 196).

%%--------------------------------------------------------------------

-define(BLOCK_WIDTH, 46).

%%--------------------------------------------------------------------

-define(USER_CODES(),
    ?USER_CODE(0, 511)
    ?USER_CODE(1, 767)
    ?USER_CODE(2, 1023)
    ?USER_CODE(3, 1279)
    ?USER_CODE(4, 1535)
    ?USER_CODE(5, 1791)
    ?USER_CODE(6, 2047)
    ?USER_CODE(7, 2303)
    ?USER_CODE(8, 2559)
    ?USER_CODE(9, 2815)
    ?USER_CODE(10, 515)
    ?USER_CODE(11, 771)
    ?USER_CODE(12, 1027)
    ?USER_CODE(13, 1283)
    ?USER_CODE(14, 1539)
    ?USER_CODE(15, 1795)
    ?USER_CODE(16, 2051)
    ?USER_CODE(17, 2307)
    ?USER_CODE(18, 2563)
    ?USER_CODE(19, 2819)
    ?USER_CODE(20, 516)
    ?USER_CODE(21, 772)
    ?USER_CODE(22, 1028)
    ?USER_CODE(23, 1284)
    ?USER_CODE(24, 1540)
    ?USER_CODE(25, 1796)
    ?USER_CODE(26, 2052)
    ?USER_CODE(27, 2308)
    ?USER_CODE(28, 2564)
    ?USER_CODE(29, 2820)
    ?USER_CODE(30, 2565)
    ?USER_CODE(31, 2821)
).

%%--------------------------------------------------------------------

-define(STRIP_0_LEFT, 1081).
-define(STRIP_1_TOP, 1177).
-define(STRIP_2_RIGHT, 1315).
-define(STRIP_3_BOTTOM, 1429).
-define(STRIP_4_END, 1561).

-define(STRIPS(),
    ?STRIP(1, 1, 3, left, 0);
    ?STRIP(1, 1, 2, left, 1);
    ?STRIP(1, 1, 1, left, 2);
    ?STRIP(1, 1, 0, left, 3);
    ?STRIP(1, 2, 3, left, 4);
    ?STRIP(1, 2, 2, left, 5);
    ?STRIP(1, 2, 1, left, 6);
    ?STRIP(1, 2, 0, left, 7);
    ?STRIP(1, 3, 3, left, 8);
    ?STRIP(1, 3, 2, left, 9);
    ?STRIP(1, 3, 1, left, 10);
    ?STRIP(1, 3, 0, left, 11);
    ?STRIP(1, 4, 3, left, 12);
    ?STRIP(1, 4, 2, left, 13);
    ?STRIP(1, 4, 1, left, 14);
    ?STRIP(1, 4, 0, left, 15);
    ?STRIP(2, 5, 3, top, 0);
    ?STRIP(2, 5, 2, top, 1);
    ?STRIP(2, 5, 1, top, 2);
    ?STRIP(2, 5, 0, top, 3);
    ?STRIP(3, 5, 3, top, 4);
    ?STRIP(3, 5, 2, top, 5);
    ?STRIP(3, 5, 1, top, 6);
    ?STRIP(3, 5, 0, top, 7);
    ?STRIP(4, 5, 2, top, 8);
    ?STRIP(4, 5, 1, top, 9);
    ?STRIP(4, 5, 0, top, 10);
    ?STRIP(5, 5, 3, top, 11);
    ?STRIP(5, 5, 2, top, 12);
    ?STRIP(5, 5, 1, top, 13);
    ?STRIP(5, 5, 0, top, 14);
    ?STRIP(6, 5, 3, top, 15);
    ?STRIP(6, 5, 2, top, 16);
    ?STRIP(6, 5, 1, top, 17);
    ?STRIP(6, 5, 0, top, 18);
    ?STRIP(7, 5, 3, top, 19);
    ?STRIP(7, 5, 2, top, 20);
    ?STRIP(7, 5, 1, top, 21);
    ?STRIP(7, 5, 0, top, 22);
    ?STRIP(8, 4, 0, right, 0);
    ?STRIP(8, 4, 1, right, 1);
    ?STRIP(8, 4, 2, right, 2);
    ?STRIP(8, 4, 3, right, 3);
    ?STRIP(8, 4, 4, right, 4);
    ?STRIP(8, 3, 0, right, 5);
    ?STRIP(8, 3, 1, right, 6);
    ?STRIP(8, 3, 2, right, 7);
    ?STRIP(8, 3, 3, right, 8);
    ?STRIP(8, 3, 4, right, 9);
    ?STRIP(8, 2, 0, right, 10);
    ?STRIP(8, 2, 1, right, 11);
    ?STRIP(8, 2, 2, right, 12);
    ?STRIP(8, 2, 3, right, 13);
    ?STRIP(8, 1, 0, right, 14);
    ?STRIP(8, 1, 1, right, 15);
    ?STRIP(8, 1, 2, right, 16);
    ?STRIP(8, 1, 3, right, 17);
    ?STRIP(8, 1, 4, right, 18);
    ?STRIP(7, 0, 0, bottom, 0);
    ?STRIP(7, 0, 1, bottom, 1);
    ?STRIP(7, 0, 2, bottom, 2);
    ?STRIP(6, 0, 0, bottom, 3);
    ?STRIP(6, 0, 1, bottom, 4);
    ?STRIP(6, 0, 2, bottom, 5);
    ?STRIP(6, 0, 3, bottom, 6);
    ?STRIP(5, 0, 0, bottom, 7);
    ?STRIP(5, 0, 1, bottom, 8);
    ?STRIP(5, 0, 2, bottom, 9);
    ?STRIP(5, 0, 3, bottom, 10);
    ?STRIP(4, 0, 0, bottom, 11);
    ?STRIP(4, 0, 1, bottom, 12);
    ?STRIP(4, 0, 2, bottom, 13);
    ?STRIP(3, 0, 0, bottom, 14);
    ?STRIP(3, 0, 1, bottom, 15);
    ?STRIP(3, 0, 2, bottom, 16);
    ?STRIP(3, 0, 3, bottom, 17);
    ?STRIP(2, 0, 0, bottom, 18);
    ?STRIP(2, 0, 1, bottom, 19);
    ?STRIP(2, 0, 2, bottom, 20);
    ?STRIP(2, 0, 3, bottom, 21)
).

%%--------------------------------------------------------------------

-define(IOC_STRIPS(),
    ?IOC_STRIP(1, 2, bus_hold);
    ?IOC_STRIP(2, 2, enable_guess);
    ?IOC_STRIP(3, 2, weak_pull_up);
).

-define(IOC_SIDES(),
    ?IOC_SIDE(0, 1, invert_guess);
).

-define(IOC_OTHERS(),
    ?IOC_OTHER(6, 2, 5, 2, invert_guess);
    ?IOC_OTHER(6, 4, 5, 3, invert_guess);
    ?IOC_OTHER(6, 201, 0, 3, invert_guess);
    ?IOC_OTHER(6, 203, 0, 2, invert_guess);
    ?IOC_OTHER(10, 2, 5, 0, invert_guess);
    ?IOC_OTHER(10, 4, 5, 1, invert_guess);
    ?IOC_OTHER(10, 201, 0, 1, invert_guess);
    ?IOC_OTHER(10, 203, 0, 0, invert_guess);
).

%%--------------------------------------------------------------------

-define(LAB_BLOCKS(),
    ?LAB_BLOCK(0, 4, 2, clk1_global0);
    ?LAB_BLOCK(0, 4, 3, clk1_global1);
    ?LAB_BLOCK(0, 9, 3, clk1_global3);
    ?LAB_BLOCK(0, 9, 2, clk1_global2);
    ?LAB_BLOCK(1, 4, 2, clk2_global0);
    ?LAB_BLOCK(1, 4, 3, clk2_global1);
    ?LAB_BLOCK(1, 9, 3, clk2_global3);
    ?LAB_BLOCK(1, 9, 2, clk2_global2);
    ?LAB_BLOCK(2, 4, 2, clr1_global0);
    ?LAB_BLOCK(2, 4, 3, clr1_global1);
    ?LAB_BLOCK(2, 9, 3, clr1_global3);
    ?LAB_BLOCK(2, 9, 2, clr1_global2);
).

-define(LAB_OTHERS(),
    ?LAB_OTHER(16, 22, clk2_invert);
    ?LAB_OTHER(12, 21, clk1_invert);
    ?LAB_OTHER(18, 23, clr1_invert);
).

%%--------------------------------------------------------------------

-define(LC_BLOCKS(),
    ?LC_BLOCK(16, 2, clk);
    ?LC_BLOCK(17, 3, clr);
    ?LC_BLOCK(18, 3, local_line);
).

%%--------------------------------------------------------------------

-define(LUT_BLOCKS(),
    ?LUT_BLOCK(12, 0, {a1, b1, c0, d1});
    ?LUT_BLOCK(12, 1, {a1, b0, c0, d1});
    ?LUT_BLOCK(12, 2, {a1, b1, c0, d0});
    ?LUT_BLOCK(12, 3, {a1, b0, c0, d0});
    ?LUT_BLOCK(13, 0, {a0, b1, c0, d1});
    ?LUT_BLOCK(13, 1, {a0, b0, c0, d1});
    ?LUT_BLOCK(13, 2, {a0, b1, c0, d0});
    ?LUT_BLOCK(13, 3, {a0, b0, c0, d0});
    ?LUT_BLOCK(14, 0, {a1, b1, c1, d1});
    ?LUT_BLOCK(14, 1, {a1, b0, c1, d1});
    ?LUT_BLOCK(14, 2, {a1, b0, c1, d0});
    ?LUT_BLOCK(14, 3, {a1, b1, c1, d0});
    ?LUT_BLOCK(15, 0, {a0, b1, c1, d1});
    ?LUT_BLOCK(15, 1, {a0, b0, c1, d1});
    ?LUT_BLOCK(15, 2, {a0, b0, c1, d0});
    ?LUT_BLOCK(15, 3, {a0, b1, c1, d0})
).

%%====================================================================
%% to_fuse
%%====================================================================

-spec to_fuse(name()) -> {ok, fuse()} | false.

-define(USER_CODE(Bit, Fuse), to_fuse({user_code, Bit}) -> {ok, Fuse};).

?USER_CODES()
to_fuse({{ioc, X, Y, N}, Name}) ->
    ioc_to_fuse(X, Y, N, Name);
to_fuse({{lab, X, Y}, Name}) ->
    lab_to_fuse(X, Y, Name);
to_fuse({{lc, X, Y, N}, lut, Name}) ->
    lut_to_fuse(X, Y, N, Name);
to_fuse({{lc, X, Y, N}, Name}) ->
    lc_to_fuse(X, Y, N, Name);
to_fuse(_) ->
    false.

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(IOC_SIDE(Sector, S, Name),
    ioc_to_fuse(X, Y, N, Name) when X =:= 1 orelse X =:= 8 ->
        ioc_to_fuse(Sector, X, Y, N + 2, S)
).
-define(IOC_OTHER(Sector, I, Y, N, Name),
    ioc_to_fuse(X, Y, N, Name) -> sector_to_fuse(Sector, X, I)
).
-define(IOC_STRIP(R, C, Name),
    ioc_to_fuse(X, Y, N, Name) -> strip_to_fuse(X, Y, N, R, C)
).

?IOC_SIDES()
?IOC_OTHERS()
?IOC_STRIPS()
ioc_to_fuse(X, Y, N, Name) ->
    {unknown, {ioc, X, Y, N, Name}}.

-undef(IOC_SIDE).
-undef(IOC_OTHER).
-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    strip_to_fuse(X, Y, N, R, C) -> strip_to_fuse(Side, Index, R, C)
).

?STRIPS().

-undef(STRIP).

%%--------------------------------------------------------------------

strip_to_fuse(left, N, R, C) ->
    strip_to_fuse_base(?STRIP_0_LEFT, N, R, C);
strip_to_fuse(top, N, R, C) ->
    strip_to_fuse_base(?STRIP_1_TOP, N, R, C);
strip_to_fuse(right, N, R, C) ->
    strip_to_fuse_base(?STRIP_2_RIGHT, N, 5 - R, C);
strip_to_fuse(bottom, N, R, C) ->
    strip_to_fuse_base(?STRIP_3_BOTTOM, N, 5 - R, C).

%%--------------------------------------------------------------------

strip_to_fuse_base(Base, N, R, C) ->
    {ok, ((Base + (N * 6) + R) * ?LINE_WIDTH) + C}.

%%--------------------------------------------------------------------

ioc_to_fuse(Sector, X, Y, N, S) when N < 5 ->
    sector_to_fuse(Sector, X, Y, (N * 4) + S);
ioc_to_fuse(Sector, X, Y, N, S) ->
    sector_to_fuse(Sector, X, Y, 9 + (N * 4) - S).

%%--------------------------------------------------------------------

-define(LAB_BLOCK(Sector, N, S, Name),
    lab_to_fuse(X, Y, Name) -> lc_to_fuse(Sector, X, Y, N, S)
).
-define(LAB_OTHER(Sector, I, Name),
    lab_to_fuse(X, Y, Name) -> sector_to_fuse(Sector, X, Y, I)
).

?LAB_BLOCKS()
?LAB_OTHERS()
lab_to_fuse(X, Y, Name) ->
    {unknown, {lab, X, Y, Name}}.

-undef(LAB_BLOCK).
-undef(LAB_OTHER).

%%--------------------------------------------------------------------

-define(LC_BLOCK(Sector, S, Name),
    lc_to_fuse(X, Y, N, Name) -> lc_to_fuse(Sector, X, Y, N, S)
).

?LC_BLOCKS()
lc_to_fuse(X, Y, N, Name) ->
    {unknown, {lc, X, Y, N, Name}}.

-undef(LC_BLOCK).

%%--------------------------------------------------------------------

lc_to_fuse(Sector, X, Y, N, S) when N < 5 ->
    sector_to_fuse(Sector, X, Y, (N * 4) + S);
lc_to_fuse(Sector, X, Y, N, S) ->
    sector_to_fuse(Sector, X, Y, 65 - (N * 4) - S).

%%--------------------------------------------------------------------

-define(LUT_BLOCK(Sector, S, LUT),
    lut_to_fuse(X, Y, N, LUT) -> lc_to_fuse(Sector, X, Y, N, S)
).

?LUT_BLOCKS().

-undef(LUT_BLOCK).

%%--------------------------------------------------------------------

sector_to_fuse(Sector, X, Y, Index) ->
    sector_to_fuse(Sector, X, ?SECTOR_LEFT + ((4 - Y) * ?BLOCK_WIDTH) + Index).

%%--------------------------------------------------------------------

sector_to_fuse(Sector, X, Offset0) ->
    Column = x_to_fuse_base(X),
    Offset1 = if
        Offset0 >= ?SECTOR_SKIP ->
            Offset0 + 1;

        true ->
            Offset0
    end,
    Line = 1 + (Offset1 div (?LINE_WIDTH - 3)),
    Offset2 = Offset1 + (Line * 3),
    {ok, Column + (Sector * 256) + Offset2}.

%%--------------------------------------------------------------------

x_to_fuse_base(1) -> ?COLUMN_1;
x_to_fuse_base(2) -> ?COLUMN_2;
x_to_fuse_base(3) -> ?COLUMN_3;
x_to_fuse_base(4) -> ?COLUMN_4;
x_to_fuse_base(5) -> ?COLUMN_5;
x_to_fuse_base(6) -> ?COLUMN_6;
x_to_fuse_base(7) -> ?COLUMN_7;
x_to_fuse_base(8) -> ?COLUMN_8.

%%====================================================================
%% to_name
%%====================================================================

-spec to_name(fuse()) -> {ok, name()} | false.

-define(USER_CODE(Bit, Fuse), to_name(Fuse) -> {ok, {user_code, Bit}};).

?USER_CODES()
to_name(Fuse) when (Fuse rem ?LINE_WIDTH) < 3 ->
    strip_row_to_name(Fuse div ?LINE_WIDTH, Fuse rem ?LINE_WIDTH);
to_name(Fuse) when Fuse < ?COLUMN_1 -> {unknown, top};
to_name(Fuse) when Fuse < ?COLUMN_2 -> side_to_name(Fuse - ?COLUMN_1, 1);
to_name(Fuse) when Fuse < ?COLUMN_3 -> column_to_name(Fuse - ?COLUMN_2, 2);
to_name(Fuse) when Fuse < ?COLUMN_4 -> column_to_name(Fuse - ?COLUMN_3, 3);
to_name(Fuse) when Fuse < ?COLUMN_5 -> column_to_name(Fuse - ?COLUMN_4, 4);
to_name(Fuse) when Fuse < ?COLUMN_6 -> column_to_name(Fuse - ?COLUMN_5, 5);
to_name(Fuse) when Fuse < ?COLUMN_7 -> column_to_name(Fuse - ?COLUMN_6, 6);
to_name(Fuse) when Fuse < ?COLUMN_7_END -> column_to_name(Fuse - ?COLUMN_7, 7);
to_name(Fuse) when Fuse < ?COLUMN_8 -> {unknown, middle};
to_name(Fuse) when Fuse < ?COLUMN_8_END -> side_to_name(Fuse - ?COLUMN_8, 8);
to_name(_) ->
    {unknown, bottom}.

-undef(USER_CODE).

%%--------------------------------------------------------------------

strip_row_to_name(Row, Col) when Row < ?STRIP_0_LEFT ->
    {unknown, {strip, Row, Col}};
strip_row_to_name(Row0, Col) when Row0 < ?STRIP_1_TOP ->
    Row = Row0 - ?STRIP_0_LEFT,
    strip_to_name(left, Row div 6, Row rem 6, Col);
strip_row_to_name(Row0, Col) when Row0 < ?STRIP_2_RIGHT ->
    Row = Row0 - ?STRIP_1_TOP,
    strip_to_name(top, Row div 6, Row rem 6, Col);
strip_row_to_name(Row0, Col) when Row0 < ?STRIP_3_BOTTOM ->
    Row = Row0 - ?STRIP_2_RIGHT,
    strip_to_name(right, Row div 6, 5 - (Row rem 6), Col);
strip_row_to_name(Row0, Col) when Row0 < ?STRIP_4_END ->
    Row = Row0 - ?STRIP_3_BOTTOM,
    strip_to_name(bottom, Row div 6, 5 - (Row rem 6), Col);
strip_row_to_name(Row, Col) ->
    {unknown, {strip, Row, Col}}.

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    strip_to_name(Side, Index, R, C) -> strip_to_name({ioc, X, Y, N}, R, C)
).

?STRIPS().

-undef(STRIP).

%%--------------------------------------------------------------------

-define(IOC_STRIP(R, C, Name),
    strip_to_name(IOC, R, C) -> {ok, {IOC, Name}}
).

?IOC_STRIPS()
strip_to_name(IOC, R, C) ->
    {unknown, {strip, IOC, R, C}}.

-undef(IOC_STRIP).

%%--------------------------------------------------------------------

side_to_name(Fuse, X) ->
    Sector = Fuse div 256,
    case sector_to_name(Fuse rem 256) of
        {Y, Index} when Index < 20 ->
            side_to_name(Sector, X, Y, Index div 4, Index rem 4);

        {Y, Index} when Index < 26 ->
            side_to_name(Sector, X, Y, Index);

        {Y, Index0} ->
            Index = Index0 - 6,
            side_to_name(Sector, X, Y, Index div 4, 3 - (Index rem 4));

        Name ->
            side_to_name(Sector, X, Name)
    end.

%%--------------------------------------------------------------------

-define(IOC_SIDE(Sector, S, Name),
    side_to_name(Sector, X, Y, N, S) -> {ok, {{ioc, X, Y, N - 2}, Name}}
).

?IOC_SIDES()
side_to_name(Sector, X, Y, N, S) ->
    {unknown, {side, Sector, X, Y, N, S}}.

-undef(IOC_SIDE).

%%--------------------------------------------------------------------

side_to_name(Sector, X, Y, Index) ->
    {unknown, {side, Sector, X, Y, Index}}.

%%--------------------------------------------------------------------

side_to_name(Sector, X, Name) ->
    {unknown, {side, Sector, X, Name}}.

%%--------------------------------------------------------------------

column_to_name(Fuse, X) ->
    Sector = Fuse div 256,
    case sector_to_name(Fuse rem 256) of
        {Y, Index} when Index < 20 ->
            column_to_name(Sector, X, Y, Index div 4, Index rem 4);

        {Y, Index} when Index < 26 ->
            column_to_name(Sector, X, Y, Index);

        {Y, Index0} ->
            Index = 65 - Index0,
            column_to_name(Sector, X, Y, Index div 4, Index rem 4);

        Name ->
            column_to_name(Sector, X, Name)
    end.


%%--------------------------------------------------------------------

-define(LAB_BLOCK(Sector, N, S, Name),
    column_to_name(Sector, X, Y, N, S) -> {ok, {{lab, X, Y}, Name}}
).
-define(LC_BLOCK(Sector, S, Name),
    column_to_name(Sector, X, Y, N, S) -> {ok, {{lc, X, Y, N}, Name}}
).
-define(LUT_BLOCK(Sector, S, LUT),
    column_to_name(Sector, X, Y, N, S) -> {ok, {{lc, X, Y, N}, lut, LUT}}
).

?LAB_BLOCKS()
?LUT_BLOCKS();
?LC_BLOCKS()
column_to_name(Sector, X, Y, N, S) ->
    {unknown, {column, Sector, X, Y, N, S}}.

-undef(LAB_BLOCK).
-undef(LC_BLOCK).
-undef(LUT_BLOCK).

%%--------------------------------------------------------------------

-define(LAB_OTHER(Sector, I, Name),
    column_to_name(Sector, X, Y, I) -> {ok, {{lab, X, Y}, Name}}
).

?LAB_OTHERS()
column_to_name(Sector, X, Y, Index) ->
    {unknown, {column, Sector, X, Y, Index}}.

-undef(LAB_BLOCK).

%%--------------------------------------------------------------------

-define(IOC_OTHER(Sector, I, Y, N, Name),
    column_to_name(Sector, X, I) -> {ok, {{ioc, X, Y, N}, Name}}
).

?IOC_OTHERS()
column_to_name(Sector, X, Name) ->
    {unknown, {column, Sector, X, Name}}.

-undef(IOC_OTHER).

%%--------------------------------------------------------------------

sector_to_name(Strip) when Strip rem ?LINE_WIDTH < 3 ->
    strip;
sector_to_name(Padded) ->
    Padding = 3 + (3 * (Padded div ?LINE_WIDTH)),
    case Padded - Padding of
        Fuse when Fuse < ?SECTOR_LEFT ->
            Fuse;

        Fuse when Fuse < ?SECTOR_SKIP ->
            Trimmed = Fuse - ?SECTOR_LEFT,
            {4 - (Trimmed div ?BLOCK_WIDTH), Trimmed rem ?BLOCK_WIDTH};

        Fuse when Fuse =:= ?SECTOR_SKIP ->
            skip;

        Fuse when Fuse < ?SECTOR_RIGHT ->
            Trimmed = Fuse - ?SECTOR_LEFT - 1,
            {4 - (Trimmed div ?BLOCK_WIDTH), Trimmed rem ?BLOCK_WIDTH};

        Fuse ->
            Fuse - 1
    end.

