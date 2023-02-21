-module(fuse_map).

-export([run/0]).
-export([run/2]).

-export([from_name/2]).
-export([to_name/2]).

-type density() :: density:density().
-type fuse() :: fuse:fuse().
-type name() :: name:name().

-record(with, {
    density :: density(),
    strip_width :: 32 | 64,
    offset_x :: 0 | 1,
    side_x :: 8 | 13 | 17 | 21,
    grow_x :: 5 | 8 | 10 | 12,
    short_y :: 4 | 7 | 10,
    long_y :: 4 | 7 | 10 | 13,
    top_y :: 5 | 8 | 11 | 14,
    short_sector :: 256 | 384 | 512,
    long_sector :: undefined | 384 | 512 | 704,
    left_base :: non_neg_integer(),
    short_base :: non_neg_integer(),
    grow_base :: non_neg_integer(),
    long_base :: non_neg_integer(),
    gap_base :: non_neg_integer(),
    right_base :: non_neg_integer(),
    end_base :: non_neg_integer(),
    sector_skip :: non_neg_integer(),
    left_strip :: non_neg_integer(),
    top_strip :: non_neg_integer(),
    right_strip :: non_neg_integer(),
    bottom_strip :: non_neg_integer(),
    end_strip :: non_neg_integer()
}).

-define(HEAD_WIDTH, 11).
-define(LINE_WIDTH, 46).

-define(HEADER_SECTORS, 2).
-define(SIDE_SECTORS, 14).
-define(COLUMN_SECTORS, 28).
-define(SHORT_SECTORS, 17).
-define(LONG_SECTORS, (?COLUMN_SECTORS - ?SHORT_SECTORS)).

-define(IOC_SIDES(),
    ?IOC_SIDE(0, 1, invert_guess);
).

-define(IOC_HEADS(),
    ?IOC_HEAD(6, 2, 2, invert_guess);
    ?IOC_HEAD(6, 4, 3, invert_guess);
    ?IOC_HEAD(10, 2, 0, invert_guess);
    ?IOC_HEAD(10, 4, 1, invert_guess);
).

-define(IOC_TAILS(),
    ?IOC_TAIL(6, 2, 2, invert_guess);
    ?IOC_TAIL(6, 4, 3, invert_guess);
    ?IOC_TAIL(10, 2, 0, invert_guess);
    ?IOC_TAIL(10, 4, 1, invert_guess);
).

-define(IOC_STRIPS(),
    ?IOC_STRIP(1, 2, bus_hold);
    ?IOC_STRIP(2, 2, enable_guess);
    ?IOC_STRIP(3, 2, weak_pull_up);
).

-define(LAB_CELLS(),
    ?LAB_CELL(0, 4, 2, clk1_global0);
    ?LAB_CELL(0, 4, 3, clk1_global1);
    ?LAB_CELL(0, 9, 3, clk1_global3);
    ?LAB_CELL(0, 9, 2, clk1_global2);
    ?LAB_CELL(1, 4, 2, clk2_global0);
    ?LAB_CELL(1, 4, 3, clk2_global1);
    ?LAB_CELL(1, 9, 3, clk2_global3);
    ?LAB_CELL(1, 9, 2, clk2_global2);
    ?LAB_CELL(2, 4, 2, clr1_global0);
    ?LAB_CELL(2, 4, 3, clr1_global1);
    ?LAB_CELL(2, 9, 3, clr1_global3);
    ?LAB_CELL(2, 9, 2, clr1_global2);
).

-define(LAB_LINES(),
    ?LAB_LINE(16, 22, clk2_invert);
    ?LAB_LINE(12, 21, clk1_invert);
    ?LAB_LINE(18, 23, clr1_invert);
).

-define(LC_CELLS(),
    ?LC_CELL(16, 2, clk);
    ?LC_CELL(17, 3, clr);
    ?LC_CELL(18, 3, local_line);
).

-define(LUT_CELLS(),
    ?LUT_CELL(12, 0, a1b1c0d1);
    ?LUT_CELL(12, 1, a1b0c0d1);
    ?LUT_CELL(12, 2, a1b1c0d0);
    ?LUT_CELL(12, 3, a1b0c0d0);
    ?LUT_CELL(13, 0, a0b1c0d1);
    ?LUT_CELL(13, 1, a0b0c0d1);
    ?LUT_CELL(13, 2, a0b1c0d0);
    ?LUT_CELL(13, 3, a0b0c0d0);
    ?LUT_CELL(14, 0, a1b1c1d1);
    ?LUT_CELL(14, 1, a1b0c1d1);
    ?LUT_CELL(14, 2, a1b0c1d0);
    ?LUT_CELL(14, 3, a1b1c1d0);
    ?LUT_CELL(15, 0, a0b1c1d1);
    ?LUT_CELL(15, 1, a0b0c1d1);
    ?LUT_CELL(15, 2, a0b0c1d0);
    ?LUT_CELL(15, 3, a0b1c1d0);
).

-define(EPM240_USER_CODES(),
    ?USER_CODE(0, 511);
    ?USER_CODE(1, 767);
    ?USER_CODE(2, 1023);
    ?USER_CODE(3, 1279);
    ?USER_CODE(4, 1535);
    ?USER_CODE(5, 1791);
    ?USER_CODE(6, 2047);
    ?USER_CODE(7, 2303);
    ?USER_CODE(8, 2559);
    ?USER_CODE(9, 2815);
    ?USER_CODE(10, 515);
    ?USER_CODE(11, 771);
    ?USER_CODE(12, 1027);
    ?USER_CODE(13, 1283);
    ?USER_CODE(14, 1539);
    ?USER_CODE(15, 1795);
    ?USER_CODE(16, 2051);
    ?USER_CODE(17, 2307);
    ?USER_CODE(18, 2563);
    ?USER_CODE(19, 2819);
    ?USER_CODE(20, 516);
    ?USER_CODE(21, 772);
    ?USER_CODE(22, 1028);
    ?USER_CODE(23, 1284);
    ?USER_CODE(24, 1540);
    ?USER_CODE(25, 1796);
    ?USER_CODE(26, 2052);
    ?USER_CODE(27, 2308);
    ?USER_CODE(28, 2564);
    ?USER_CODE(29, 2820);
    ?USER_CODE(30, 2565);
    ?USER_CODE(31, 2821);
).

-define(EPM240_STRIPS(),
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
    ?STRIP(2, 0, 3, bottom, 21);
).

-define(EPM570_USER_CODES(),
    ?USER_CODE(0, 65786);
    ?USER_CODE(1, 65530);
    ?USER_CODE(2, 65274);
    ?USER_CODE(3, 65018);
    ?USER_CODE(4, 64762);
    ?USER_CODE(5, 64506);
    ?USER_CODE(6, 65785);
    ?USER_CODE(7, 65529);
    ?USER_CODE(8, 65273);
    ?USER_CODE(9, 65017);
    ?USER_CODE(10, 64761);
    ?USER_CODE(11, 64505);
    ?USER_CODE(12, 64249);
    ?USER_CODE(13, 63993);
    ?USER_CODE(14, 63737);
    ?USER_CODE(15, 63481);
    ?USER_CODE(16, 63225);
    ?USER_CODE(17, 62969);
    ?USER_CODE(18, 62713);
    ?USER_CODE(19, 62457);
    ?USER_CODE(20, 62201);
    ?USER_CODE(21, 61945);
    ?USER_CODE(22, 61689);
    ?USER_CODE(23, 61433);
    ?USER_CODE(24, 61177);
    ?USER_CODE(25, 60921);
    ?USER_CODE(26, 60665);
    ?USER_CODE(27, 60409);
    ?USER_CODE(28, 64250);
    ?USER_CODE(29, 63994);
    ?USER_CODE(30, 63738);
    ?USER_CODE(31, 63482);
).

-define(EPM570_STRIPS(),
    ?STRIP(1, 3, 0, left, 0);
    ?STRIP(1, 3, 1, left, 1);
    ?STRIP(1, 3, 2, left, 2);
    ?STRIP(1, 3, 3, left, 3);
    ?STRIP(0, 4, 6, left, 4);
    ?STRIP(0, 4, 5, left, 5);
    ?STRIP(0, 4, 4, left, 6);
    ?STRIP(0, 4, 3, left, 7);
    ?STRIP(0, 4, 2, left, 8);
    ?STRIP(0, 4, 1, left, 9);
    ?STRIP(0, 4, 0, left, 10);
    ?STRIP(0, 5, 6, left, 11);
    ?STRIP(0, 5, 5, left, 12);
    ?STRIP(0, 5, 4, left, 13);
    ?STRIP(0, 5, 3, left, 14);
    ?STRIP(0, 5, 2, left, 15);
    ?STRIP(0, 5, 1, left, 16);
    ?STRIP(0, 5, 0, left, 17);
    ?STRIP(0, 6, 6, left, 18);
    ?STRIP(0, 6, 5, left, 19);
    ?STRIP(0, 6, 4, left, 20);
    ?STRIP(0, 6, 3, left, 21);
    ?STRIP(0, 6, 2, left, 22);
    ?STRIP(0, 6, 1, left, 23);
    ?STRIP(0, 6, 0, left, 24);
    ?STRIP(0, 7, 6, left, 25);
    ?STRIP(0, 7, 5, left, 26);
    ?STRIP(0, 7, 4, left, 27);
    ?STRIP(0, 7, 3, left, 28);
    ?STRIP(0, 7, 2, left, 29);
    ?STRIP(0, 7, 1, left, 30);
    ?STRIP(0, 7, 0, left, 31);
    ?STRIP(1, 8, 3, top, 0);
    ?STRIP(1, 8, 2, top, 1);
    ?STRIP(1, 8, 1, top, 2);
    ?STRIP(1, 8, 0, top, 3);
    ?STRIP(2, 8, 3, top, 4);
    ?STRIP(2, 8, 2, top, 5);
    ?STRIP(2, 8, 1, top, 6);
    ?STRIP(2, 8, 0, top, 7);
    ?STRIP(3, 8, 3, top, 8);
    ?STRIP(3, 8, 2, top, 9);
    ?STRIP(3, 8, 1, top, 10);
    ?STRIP(3, 8, 0, top, 11);
    ?STRIP(4, 8, 3, top, 12);
    ?STRIP(4, 8, 2, top, 13);
    ?STRIP(4, 8, 1, top, 14);
    ?STRIP(4, 8, 0, top, 15);
    ?STRIP(5, 8, 2, top, 16);
    ?STRIP(5, 8, 1, top, 17);
    ?STRIP(5, 8, 0, top, 18);
    ?STRIP(6, 8, 3, top, 19);
    ?STRIP(6, 8, 2, top, 20);
    ?STRIP(6, 8, 1, top, 21);
    ?STRIP(6, 8, 0, top, 22);
    ?STRIP(7, 8, 3, top, 23);
    ?STRIP(7, 8, 2, top, 24);
    ?STRIP(7, 8, 1, top, 25);
    ?STRIP(7, 8, 0, top, 26);
    ?STRIP(8, 8, 3, top, 27);
    ?STRIP(8, 8, 2, top, 28);
    ?STRIP(8, 8, 1, top, 29);
    ?STRIP(8, 8, 0, top, 30);
    ?STRIP(9, 8, 2, top, 31);
    ?STRIP(9, 8, 1, top, 32);
    ?STRIP(9, 8, 0, top, 33);
    ?STRIP(10, 8, 3, top, 34);
    ?STRIP(10, 8, 2, top, 35);
    ?STRIP(10, 8, 1, top, 36);
    ?STRIP(10, 8, 0, top, 37);
    ?STRIP(11, 8, 3, top, 38);
    ?STRIP(11, 8, 2, top, 39);
    ?STRIP(11, 8, 1, top, 40);
    ?STRIP(11, 8, 0, top, 41);
    ?STRIP(12, 8, 3, top, 42);
    ?STRIP(12, 8, 2, top, 43);
    ?STRIP(12, 8, 1, top, 44);
    ?STRIP(12, 8, 0, top, 45);
    ?STRIP(13, 7, 0, right, 0);
    ?STRIP(13, 7, 1, right, 1);
    ?STRIP(13, 7, 2, right, 2);
    ?STRIP(13, 7, 3, right, 3);
    ?STRIP(13, 7, 4, right, 4);
    ?STRIP(13, 7, 5, right, 5);
    ?STRIP(13, 6, 0, right, 6);
    ?STRIP(13, 6, 1, right, 7);
    ?STRIP(13, 6, 2, right, 8);
    ?STRIP(13, 6, 3, right, 9);
    ?STRIP(13, 6, 4, right, 10);
    ?STRIP(13, 6, 5, right, 11);
    ?STRIP(13, 5, 0, right, 12);
    ?STRIP(13, 5, 1, right, 13);
    ?STRIP(13, 5, 2, right, 14);
    ?STRIP(13, 5, 3, right, 15);
    ?STRIP(13, 5, 4, right, 16);
    ?STRIP(13, 5, 5, right, 17);
    ?STRIP(13, 4, 0, right, 18);
    ?STRIP(13, 4, 1, right, 19);
    ?STRIP(13, 4, 2, right, 20);
    ?STRIP(13, 4, 3, right, 21);
    ?STRIP(13, 4, 4, right, 22);
    ?STRIP(13, 4, 5, right, 23);
    ?STRIP(13, 3, 0, right, 24);
    ?STRIP(13, 3, 1, right, 25);
    ?STRIP(13, 3, 2, right, 26);
    ?STRIP(13, 3, 3, right, 27);
    ?STRIP(13, 3, 4, right, 28);
    ?STRIP(13, 3, 5, right, 29);
    ?STRIP(13, 2, 0, right, 30);
    ?STRIP(13, 2, 1, right, 31);
    ?STRIP(13, 2, 2, right, 32);
    ?STRIP(13, 2, 3, right, 33);
    ?STRIP(13, 2, 4, right, 34);
    ?STRIP(13, 2, 5, right, 35);
    ?STRIP(13, 1, 0, right, 36);
    ?STRIP(13, 1, 1, right, 37);
    ?STRIP(13, 1, 2, right, 38);
    ?STRIP(13, 1, 3, right, 39);
    ?STRIP(13, 1, 4, right, 40);
    ?STRIP(13, 1, 5, right, 41);
    ?STRIP(12, 0, 0, bottom, 0);
    ?STRIP(12, 0, 1, bottom, 1);
    ?STRIP(12, 0, 2, bottom, 2);
    ?STRIP(12, 0, 3, bottom, 3);
    ?STRIP(11, 0, 0, bottom, 4);
    ?STRIP(11, 0, 1, bottom, 5);
    ?STRIP(11, 0, 2, bottom, 6);
    ?STRIP(11, 0, 3, bottom, 7);
    ?STRIP(10, 0, 0, bottom, 8);
    ?STRIP(10, 0, 1, bottom, 9);
    ?STRIP(10, 0, 2, bottom, 10);
    ?STRIP(10, 0, 3, bottom, 11);
    ?STRIP(8, 3, 0, bottom, 12);
    ?STRIP(8, 3, 1, bottom, 13);
    ?STRIP(8, 3, 2, bottom, 14);
    ?STRIP(8, 3, 3, bottom, 15);
    ?STRIP(7, 3, 0, bottom, 16);
    ?STRIP(7, 3, 1, bottom, 17);
    ?STRIP(7, 3, 2, bottom, 18);
    ?STRIP(7, 3, 3, bottom, 19);
    ?STRIP(6, 3, 0, bottom, 20);
    ?STRIP(6, 3, 1, bottom, 21);
    ?STRIP(6, 3, 2, bottom, 22);
    ?STRIP(6, 3, 3, bottom, 23);
    ?STRIP(5, 3, 0, bottom, 24);
    ?STRIP(5, 3, 1, bottom, 25);
    ?STRIP(5, 3, 2, bottom, 26);
    ?STRIP(5, 3, 3, bottom, 27);
    ?STRIP(4, 3, 0, bottom, 28);
    ?STRIP(4, 3, 1, bottom, 29);
    ?STRIP(4, 3, 2, bottom, 30);
    ?STRIP(4, 3, 3, bottom, 31);
    ?STRIP(3, 3, 0, bottom, 32);
    ?STRIP(3, 3, 1, bottom, 33);
    ?STRIP(3, 3, 2, bottom, 34);
    ?STRIP(3, 3, 3, bottom, 35);
    ?STRIP(2, 3, 0, bottom, 36);
    ?STRIP(2, 3, 1, bottom, 37);
    ?STRIP(2, 3, 2, bottom, 38);
    ?STRIP(2, 3, 3, bottom, 39);
).

-define(EPM1270_USER_CODES(),
    ?USER_CODE(0, 120225);
    ?USER_CODE(1, 119841);
    ?USER_CODE(2, 119457);
    ?USER_CODE(3, 119073);
    ?USER_CODE(4, 118689);
    ?USER_CODE(5, 118305);
    ?USER_CODE(6, 120224);
    ?USER_CODE(7, 119840);
    ?USER_CODE(8, 119456);
    ?USER_CODE(9, 119072);
    ?USER_CODE(10, 118688);
    ?USER_CODE(11, 118304);
    ?USER_CODE(12, 117920);
    ?USER_CODE(13, 117536);
    ?USER_CODE(14, 117152);
    ?USER_CODE(15, 116768);
    ?USER_CODE(16, 116384);
    ?USER_CODE(17, 116000);
    ?USER_CODE(18, 115616);
    ?USER_CODE(19, 115232);
    ?USER_CODE(20, 114848);
    ?USER_CODE(21, 114464);
    ?USER_CODE(22, 114080);
    ?USER_CODE(23, 113696);
    ?USER_CODE(24, 113312);
    ?USER_CODE(25, 112928);
    ?USER_CODE(26, 112544);
    ?USER_CODE(27, 112160);
    ?USER_CODE(28, 117921);
    ?USER_CODE(29, 117537);
    ?USER_CODE(30, 117153);
    ?USER_CODE(31, 116769);
).

-define(EPM1270_STRIPS(),
    ?STRIP(1, 3, 2, left, 0);
    ?STRIP(1, 3, 3, left, 1);
    ?STRIP(0, 4, 6, left, 2);
    ?STRIP(0, 4, 5, left, 3);
    ?STRIP(0, 4, 4, left, 4);
    ?STRIP(0, 4, 3, left, 5);
    ?STRIP(0, 4, 2, left, 6);
    ?STRIP(0, 4, 1, left, 7);
    ?STRIP(0, 4, 0, left, 8);
    ?STRIP(0, 5, 6, left, 9);
    ?STRIP(0, 5, 5, left, 10);
    ?STRIP(0, 5, 4, left, 11);
    ?STRIP(0, 5, 3, left, 12);
    ?STRIP(0, 5, 2, left, 13);
    ?STRIP(0, 5, 1, left, 14);
    ?STRIP(0, 5, 0, left, 15);
    ?STRIP(0, 6, 6, left, 16);
    ?STRIP(0, 6, 5, left, 17);
    ?STRIP(0, 6, 4, left, 18);
    ?STRIP(0, 6, 3, left, 19);
    ?STRIP(0, 6, 2, left, 20);
    ?STRIP(0, 6, 1, left, 21);
    ?STRIP(0, 6, 0, left, 22);
    ?STRIP(0, 7, 6, left, 23);
    ?STRIP(0, 7, 5, left, 24);
    ?STRIP(0, 7, 4, left, 25);
    ?STRIP(0, 7, 3, left, 26);
    ?STRIP(0, 7, 2, left, 27);
    ?STRIP(0, 7, 1, left, 28);
    ?STRIP(0, 7, 0, left, 29);
    ?STRIP(0, 8, 6, left, 30);
    ?STRIP(0, 8, 5, left, 31);
    ?STRIP(0, 8, 4, left, 32);
    ?STRIP(0, 8, 3, left, 33);
    ?STRIP(0, 8, 2, left, 34);
    ?STRIP(0, 8, 1, left, 35);
    ?STRIP(0, 8, 0, left, 36);
    ?STRIP(0, 9, 6, left, 37);
    ?STRIP(0, 9, 5, left, 38);
    ?STRIP(0, 9, 4, left, 39);
    ?STRIP(0, 9, 3, left, 40);
    ?STRIP(0, 9, 2, left, 41);
    ?STRIP(0, 9, 1, left, 42);
    ?STRIP(0, 9, 0, left, 43);
    ?STRIP(0, 10, 6, left, 44);
    ?STRIP(0, 10, 5, left, 45);
    ?STRIP(0, 10, 4, left, 46);
    ?STRIP(0, 10, 3, left, 47);
    ?STRIP(0, 10, 2, left, 48);
    ?STRIP(0, 10, 1, left, 49);
    ?STRIP(0, 10, 0, left, 50);
    ?STRIP(1, 11, 2, top, 0);
    ?STRIP(1, 11, 1, top, 1);
    ?STRIP(1, 11, 0, top, 2);
    ?STRIP(2, 11, 3, top, 3);
    ?STRIP(2, 11, 2, top, 4);
    ?STRIP(2, 11, 1, top, 5);
    ?STRIP(2, 11, 0, top, 6);
    ?STRIP(3, 11, 2, top, 7);
    ?STRIP(3, 11, 1, top, 8);
    ?STRIP(3, 11, 0, top, 9);
    ?STRIP(4, 11, 2, top, 10);
    ?STRIP(4, 11, 1, top, 11);
    ?STRIP(4, 11, 0, top, 12);
    ?STRIP(5, 11, 2, top, 13);
    ?STRIP(5, 11, 1, top, 14);
    ?STRIP(5, 11, 0, top, 15);
    ?STRIP(6, 11, 3, top, 16);
    ?STRIP(6, 11, 2, top, 17);
    ?STRIP(6, 11, 1, top, 18);
    ?STRIP(6, 11, 0, top, 19);
    ?STRIP(7, 11, 2, top, 20);
    ?STRIP(7, 11, 1, top, 21);
    ?STRIP(7, 11, 0, top, 22);
    ?STRIP(8, 11, 2, top, 23);
    ?STRIP(8, 11, 1, top, 24);
    ?STRIP(8, 11, 0, top, 25);
    ?STRIP(9, 11, 3, top, 26);
    ?STRIP(9, 11, 2, top, 27);
    ?STRIP(9, 11, 1, top, 28);
    ?STRIP(9, 11, 0, top, 29);
    ?STRIP(10, 11, 2, top, 30);
    ?STRIP(10, 11, 1, top, 31);
    ?STRIP(10, 11, 0, top, 32);
    ?STRIP(11, 11, 3, top, 33);
    ?STRIP(11, 11, 2, top, 34);
    ?STRIP(11, 11, 1, top, 35);
    ?STRIP(11, 11, 0, top, 36);
    ?STRIP(12, 11, 2, top, 37);
    ?STRIP(12, 11, 1, top, 38);
    ?STRIP(12, 11, 0, top, 39);
    ?STRIP(13, 11, 2, top, 40);
    ?STRIP(13, 11, 1, top, 41);
    ?STRIP(13, 11, 0, top, 42);
    ?STRIP(14, 11, 2, top, 43);
    ?STRIP(14, 11, 1, top, 44);
    ?STRIP(14, 11, 0, top, 45);
    ?STRIP(15, 11, 3, top, 46);
    ?STRIP(15, 11, 2, top, 47);
    ?STRIP(15, 11, 1, top, 48);
    ?STRIP(15, 11, 0, top, 49);
    ?STRIP(16, 11, 2, top, 50);
    ?STRIP(16, 11, 1, top, 51);
    ?STRIP(16, 11, 0, top, 52);
    ?STRIP(17, 10, 0, right, 0);
    ?STRIP(17, 10, 1, right, 1);
    ?STRIP(17, 10, 2, right, 2);
    ?STRIP(17, 10, 3, right, 3);
    ?STRIP(17, 10, 4, right, 4);
    ?STRIP(17, 9, 0, right, 5);
    ?STRIP(17, 9, 1, right, 6);
    ?STRIP(17, 9, 2, right, 7);
    ?STRIP(17, 9, 3, right, 8);
    ?STRIP(17, 9, 4, right, 9);
    ?STRIP(17, 8, 0, right, 10);
    ?STRIP(17, 8, 1, right, 11);
    ?STRIP(17, 8, 2, right, 12);
    ?STRIP(17, 8, 3, right, 13);
    ?STRIP(17, 8, 4, right, 14);
    ?STRIP(17, 8, 5, right, 15);
    ?STRIP(17, 7, 0, right, 16);
    ?STRIP(17, 7, 1, right, 17);
    ?STRIP(17, 7, 2, right, 18);
    ?STRIP(17, 7, 3, right, 19);
    ?STRIP(17, 7, 4, right, 20);
    ?STRIP(17, 6, 0, right, 21);
    ?STRIP(17, 6, 1, right, 22);
    ?STRIP(17, 6, 2, right, 23);
    ?STRIP(17, 6, 3, right, 24);
    ?STRIP(17, 6, 4, right, 25);
    ?STRIP(17, 6, 5, right, 26);
    ?STRIP(17, 5, 0, right, 27);
    ?STRIP(17, 5, 1, right, 28);
    ?STRIP(17, 5, 2, right, 29);
    ?STRIP(17, 5, 3, right, 30);
    ?STRIP(17, 5, 4, right, 31);
    ?STRIP(17, 5, 5, right, 32);
    ?STRIP(17, 4, 0, right, 33);
    ?STRIP(17, 4, 1, right, 34);
    ?STRIP(17, 4, 2, right, 35);
    ?STRIP(17, 4, 3, right, 36);
    ?STRIP(17, 4, 4, right, 37);
    ?STRIP(17, 3, 0, right, 38);
    ?STRIP(17, 3, 1, right, 39);
    ?STRIP(17, 3, 2, right, 40);
    ?STRIP(17, 3, 3, right, 41);
    ?STRIP(17, 3, 4, right, 42);
    ?STRIP(17, 3, 5, right, 43);
    ?STRIP(17, 2, 0, right, 44);
    ?STRIP(17, 2, 1, right, 45);
    ?STRIP(17, 2, 2, right, 46);
    ?STRIP(17, 2, 3, right, 47);
    ?STRIP(17, 2, 4, right, 48);
    ?STRIP(17, 1, 0, right, 49);
    ?STRIP(17, 1, 1, right, 50);
    ?STRIP(17, 1, 2, right, 51);
    ?STRIP(17, 1, 3, right, 52);
    ?STRIP(17, 1, 4, right, 53);
    ?STRIP(17, 1, 5, right, 54);
    ?STRIP(16, 0, 0, bottom, 0);
    ?STRIP(16, 0, 1, bottom, 1);
    ?STRIP(16, 0, 2, bottom, 2);
    ?STRIP(16, 0, 3, bottom, 3);
    ?STRIP(15, 0, 0, bottom, 4);
    ?STRIP(15, 0, 1, bottom, 5);
    ?STRIP(15, 0, 2, bottom, 6);
    ?STRIP(14, 0, 0, bottom, 7);
    ?STRIP(14, 0, 1, bottom, 8);
    ?STRIP(14, 0, 2, bottom, 9);
    ?STRIP(14, 0, 3, bottom, 10);
    ?STRIP(13, 0, 0, bottom, 11);
    ?STRIP(13, 0, 1, bottom, 12);
    ?STRIP(13, 0, 2, bottom, 13);
    ?STRIP(13, 0, 3, bottom, 14);
    ?STRIP(12, 0, 0, bottom, 15);
    ?STRIP(12, 0, 1, bottom, 16);
    ?STRIP(12, 0, 2, bottom, 17);
    ?STRIP(10, 3, 0, bottom, 18);
    ?STRIP(10, 3, 1, bottom, 19);
    ?STRIP(10, 3, 2, bottom, 20);
    ?STRIP(10, 3, 3, bottom, 21);
    ?STRIP(9, 3, 0, bottom, 22);
    ?STRIP(9, 3, 1, bottom, 23);
    ?STRIP(9, 3, 2, bottom, 24);
    ?STRIP(8, 3, 0, bottom, 25);
    ?STRIP(8, 3, 1, bottom, 26);
    ?STRIP(8, 3, 2, bottom, 27);
    ?STRIP(8, 3, 3, bottom, 28);
    ?STRIP(7, 3, 0, bottom, 29);
    ?STRIP(7, 3, 1, bottom, 30);
    ?STRIP(7, 3, 2, bottom, 31);
    ?STRIP(7, 3, 3, bottom, 32);
    ?STRIP(6, 3, 0, bottom, 33);
    ?STRIP(6, 3, 1, bottom, 34);
    ?STRIP(6, 3, 2, bottom, 35);
    ?STRIP(5, 3, 0, bottom, 36);
    ?STRIP(5, 3, 1, bottom, 37);
    ?STRIP(5, 3, 2, bottom, 38);
    ?STRIP(5, 3, 3, bottom, 39);
    ?STRIP(4, 3, 0, bottom, 40);
    ?STRIP(4, 3, 1, bottom, 41);
    ?STRIP(4, 3, 2, bottom, 42);
    ?STRIP(4, 3, 3, bottom, 43);
    ?STRIP(3, 3, 0, bottom, 44);
    ?STRIP(3, 3, 1, bottom, 45);
    ?STRIP(3, 3, 2, bottom, 46);
    ?STRIP(2, 3, 0, bottom, 47);
    ?STRIP(2, 3, 1, bottom, 48);
    ?STRIP(2, 3, 2, bottom, 49);
    ?STRIP(2, 3, 3, bottom, 50);
    ?STRIP(1, 3, 0, bottom, 51);
    ?STRIP(1, 3, 1, bottom, 52);
).

-define(EPM2210_USER_CODES(),
    ?USER_CODE(0, 188977);
    ?USER_CODE(1, 188465);
    ?USER_CODE(2, 187953);
    ?USER_CODE(3, 187441);
    ?USER_CODE(4, 186929);
    ?USER_CODE(5, 186417);
    ?USER_CODE(6, 188976);
    ?USER_CODE(7, 188464);
    ?USER_CODE(8, 187952);
    ?USER_CODE(9, 187440);
    ?USER_CODE(10, 186928);
    ?USER_CODE(11, 186416);
    ?USER_CODE(12, 185904);
    ?USER_CODE(13, 185392);
    ?USER_CODE(14, 184880);
    ?USER_CODE(15, 184368);
    ?USER_CODE(16, 183856);
    ?USER_CODE(17, 183344);
    ?USER_CODE(18, 182832);
    ?USER_CODE(19, 182320);
    ?USER_CODE(20, 181808);
    ?USER_CODE(21, 181296);
    ?USER_CODE(22, 180784);
    ?USER_CODE(23, 180272);
    ?USER_CODE(24, 179760);
    ?USER_CODE(25, 179248);
    ?USER_CODE(26, 178736);
    ?USER_CODE(27, 178224);
    ?USER_CODE(28, 185905);
    ?USER_CODE(29, 185393);
    ?USER_CODE(30, 184881);
    ?USER_CODE(31, 184369);
).

-define(EPM2210_STRIPS(),
    ?STRIP(0, 4, 6, left, 0);
    ?STRIP(0, 4, 5, left, 1);
    ?STRIP(0, 4, 4, left, 2);
    ?STRIP(0, 4, 3, left, 3);
    ?STRIP(0, 4, 2, left, 4);
    ?STRIP(0, 4, 1, left, 5);
    ?STRIP(0, 4, 0, left, 6);
    ?STRIP(0, 5, 6, left, 7);
    ?STRIP(0, 5, 5, left, 8);
    ?STRIP(0, 5, 4, left, 9);
    ?STRIP(0, 5, 3, left, 10);
    ?STRIP(0, 5, 2, left, 11);
    ?STRIP(0, 5, 1, left, 12);
    ?STRIP(0, 5, 0, left, 13);
    ?STRIP(0, 6, 5, left, 14);
    ?STRIP(0, 6, 4, left, 15);
    ?STRIP(0, 6, 3, left, 16);
    ?STRIP(0, 6, 2, left, 17);
    ?STRIP(0, 6, 1, left, 18);
    ?STRIP(0, 6, 0, left, 19);
    ?STRIP(0, 7, 6, left, 20);
    ?STRIP(0, 7, 5, left, 21);
    ?STRIP(0, 7, 4, left, 22);
    ?STRIP(0, 7, 3, left, 23);
    ?STRIP(0, 7, 2, left, 24);
    ?STRIP(0, 7, 1, left, 25);
    ?STRIP(0, 7, 0, left, 26);
    ?STRIP(0, 8, 6, left, 27);
    ?STRIP(0, 8, 5, left, 28);
    ?STRIP(0, 8, 4, left, 29);
    ?STRIP(0, 8, 3, left, 30);
    ?STRIP(0, 8, 2, left, 31);
    ?STRIP(0, 8, 1, left, 32);
    ?STRIP(0, 8, 0, left, 33);
    ?STRIP(0, 9, 6, left, 34);
    ?STRIP(0, 9, 5, left, 35);
    ?STRIP(0, 9, 4, left, 36);
    ?STRIP(0, 9, 3, left, 37);
    ?STRIP(0, 9, 2, left, 38);
    ?STRIP(0, 9, 1, left, 39);
    ?STRIP(0, 9, 0, left, 40);
    ?STRIP(0, 10, 6, left, 41);
    ?STRIP(0, 10, 5, left, 42);
    ?STRIP(0, 10, 4, left, 43);
    ?STRIP(0, 10, 3, left, 44);
    ?STRIP(0, 10, 2, left, 45);
    ?STRIP(0, 10, 1, left, 46);
    ?STRIP(0, 10, 0, left, 47);
    ?STRIP(0, 11, 5, left, 48);
    ?STRIP(0, 11, 4, left, 49);
    ?STRIP(0, 11, 3, left, 50);
    ?STRIP(0, 11, 2, left, 51);
    ?STRIP(0, 11, 1, left, 52);
    ?STRIP(0, 11, 0, left, 53);
    ?STRIP(0, 12, 6, left, 54);
    ?STRIP(0, 12, 5, left, 55);
    ?STRIP(0, 12, 4, left, 56);
    ?STRIP(0, 12, 3, left, 57);
    ?STRIP(0, 12, 2, left, 58);
    ?STRIP(0, 12, 1, left, 59);
    ?STRIP(0, 12, 0, left, 60);
    ?STRIP(0, 13, 6, left, 61);
    ?STRIP(0, 13, 5, left, 62);
    ?STRIP(0, 13, 4, left, 63);
    ?STRIP(0, 13, 3, left, 64);
    ?STRIP(0, 13, 2, left, 65);
    ?STRIP(0, 13, 1, left, 66);
    ?STRIP(0, 13, 0, left, 67);
    ?STRIP(1, 14, 3, top, 0);
    ?STRIP(1, 14, 2, top, 1);
    ?STRIP(1, 14, 1, top, 2);
    ?STRIP(1, 14, 0, top, 3);
    ?STRIP(2, 14, 2, top, 4);
    ?STRIP(2, 14, 1, top, 5);
    ?STRIP(2, 14, 0, top, 6);
    ?STRIP(3, 14, 3, top, 7);
    ?STRIP(3, 14, 2, top, 8);
    ?STRIP(3, 14, 1, top, 9);
    ?STRIP(3, 14, 0, top, 10);
    ?STRIP(4, 14, 2, top, 11);
    ?STRIP(4, 14, 1, top, 12);
    ?STRIP(4, 14, 0, top, 13);
    ?STRIP(5, 14, 2, top, 14);
    ?STRIP(5, 14, 1, top, 15);
    ?STRIP(5, 14, 0, top, 16);
    ?STRIP(6, 14, 2, top, 17);
    ?STRIP(6, 14, 1, top, 18);
    ?STRIP(6, 14, 0, top, 19);
    ?STRIP(7, 14, 2, top, 20);
    ?STRIP(7, 14, 1, top, 21);
    ?STRIP(7, 14, 0, top, 22);
    ?STRIP(8, 14, 2, top, 23);
    ?STRIP(8, 14, 1, top, 24);
    ?STRIP(8, 14, 0, top, 25);
    ?STRIP(9, 14, 3, top, 26);
    ?STRIP(9, 14, 2, top, 27);
    ?STRIP(9, 14, 1, top, 28);
    ?STRIP(9, 14, 0, top, 29);
    ?STRIP(10,14, 2, top, 30);
    ?STRIP(10,14, 1, top, 31);
    ?STRIP(10,14, 0, top, 32);
    ?STRIP(11,14, 3, top, 33);
    ?STRIP(11,14, 2, top, 34);
    ?STRIP(11,14, 1, top, 35);
    ?STRIP(11,14, 0, top, 36);
    ?STRIP(12,14, 2, top, 37);
    ?STRIP(12,14, 1, top, 38);
    ?STRIP(12,14, 0, top, 39);
    ?STRIP(13,14, 2, top, 40);
    ?STRIP(13,14, 1, top, 41);
    ?STRIP(13,14, 0, top, 42);
    ?STRIP(14,14, 2, top, 43);
    ?STRIP(14,14, 1, top, 44);
    ?STRIP(14,14, 0, top, 45);
    ?STRIP(15,14, 2, top, 46);
    ?STRIP(15,14, 1, top, 47);
    ?STRIP(15,14, 0, top, 48);
    ?STRIP(16,14, 2, top, 49);
    ?STRIP(16,14, 1, top, 50);
    ?STRIP(16,14, 0, top, 51);
    ?STRIP(17,14, 3, top, 52);
    ?STRIP(17,14, 2, top, 53);
    ?STRIP(17,14, 1, top, 54);
    ?STRIP(17,14, 0, top, 55);
    ?STRIP(18,14, 2, top, 56);
    ?STRIP(18,14, 1, top, 57);
    ?STRIP(18,14, 0, top, 58);
    ?STRIP(19,14, 3, top, 59);
    ?STRIP(19,14, 2, top, 60);
    ?STRIP(19,14, 1, top, 61);
    ?STRIP(19,14, 0, top, 62);
    ?STRIP(20,14, 2, top, 63);
    ?STRIP(20,14, 1, top, 64);
    ?STRIP(20,14, 0, top, 65);
    ?STRIP(21,13, 0, right, 0);
    ?STRIP(21,13, 1, right, 1);
    ?STRIP(21,13, 2, right, 2);
    ?STRIP(21,13, 3, right, 3);
    ?STRIP(21,13, 4, right, 4);
    ?STRIP(21,13, 5, right, 5);
    ?STRIP(21,12, 0, right, 6);
    ?STRIP(21,12, 1, right, 7);
    ?STRIP(21,12, 2, right, 8);
    ?STRIP(21,12, 3, right, 9);
    ?STRIP(21,12, 4, right, 10);
    ?STRIP(21,11, 0, right, 11);
    ?STRIP(21,11, 1, right, 12);
    ?STRIP(21,11, 2, right, 13);
    ?STRIP(21,11, 3, right, 14);
    ?STRIP(21,11, 4, right, 15);
    ?STRIP(21,11, 5, right, 16);
    ?STRIP(21,10, 0, right, 17);
    ?STRIP(21,10, 1, right, 18);
    ?STRIP(21,10, 2, right, 19);
    ?STRIP(21,10, 3, right, 20);
    ?STRIP(21,10, 4, right, 21);
    ?STRIP(21, 9, 0, right, 22);
    ?STRIP(21, 9, 1, right, 23);
    ?STRIP(21, 9, 2, right, 24);
    ?STRIP(21, 9, 3, right, 25);
    ?STRIP(21, 9, 4, right, 26);
    ?STRIP(21, 8, 0, right, 27);
    ?STRIP(21, 8, 1, right, 28);
    ?STRIP(21, 8, 2, right, 29);
    ?STRIP(21, 8, 3, right, 30);
    ?STRIP(21, 8, 4, right, 31);
    ?STRIP(21, 8, 5, right, 32);
    ?STRIP(21, 7, 0, right, 33);
    ?STRIP(21, 7, 1, right, 34);
    ?STRIP(21, 7, 2, right, 35);
    ?STRIP(21, 7, 3, right, 36);
    ?STRIP(21, 7, 4, right, 37);
    ?STRIP(21, 7, 5, right, 38);
    ?STRIP(21, 6, 0, right, 39);
    ?STRIP(21, 6, 1, right, 40);
    ?STRIP(21, 6, 2, right, 41);
    ?STRIP(21, 6, 3, right, 42);
    ?STRIP(21, 6, 4, right, 43);
    ?STRIP(21, 5, 0, right, 44);
    ?STRIP(21, 5, 1, right, 45);
    ?STRIP(21, 5, 2, right, 46);
    ?STRIP(21, 5, 3, right, 47);
    ?STRIP(21, 5, 4, right, 48);
    ?STRIP(21, 5, 5, right, 49);
    ?STRIP(21, 4, 0, right, 50);
    ?STRIP(21, 4, 1, right, 51);
    ?STRIP(21, 4, 2, right, 52);
    ?STRIP(21, 4, 3, right, 53);
    ?STRIP(21, 4, 4, right, 54);
    ?STRIP(21, 3, 0, right, 55);
    ?STRIP(21, 3, 1, right, 56);
    ?STRIP(21, 3, 2, right, 57);
    ?STRIP(21, 3, 3, right, 58);
    ?STRIP(21, 3, 4, right, 59);
    ?STRIP(21, 3, 5, right, 60);
    ?STRIP(21, 2, 0, right, 61);
    ?STRIP(21, 2, 1, right, 62);
    ?STRIP(21, 2, 2, right, 63);
    ?STRIP(21, 2, 3, right, 64);
    ?STRIP(21, 2, 4, right, 65);
    ?STRIP(21, 1, 0, right, 66);
    ?STRIP(21, 1, 1, right, 67);
    ?STRIP(21, 1, 2, right, 68);
    ?STRIP(21, 1, 3, right, 69);
    ?STRIP(21, 1, 4, right, 70);
    ?STRIP(21, 1, 5, right, 71);
    ?STRIP(20, 0, 0, bottom, 0);
    ?STRIP(20, 0, 1, bottom, 1);
    ?STRIP(20, 0, 2, bottom, 2);
    ?STRIP(19, 0, 0, bottom, 3);
    ?STRIP(19, 0, 1, bottom, 4);
    ?STRIP(19, 0, 2, bottom, 5);
    ?STRIP(19, 0, 3, bottom, 6);
    ?STRIP(18, 0, 0, bottom, 7);
    ?STRIP(18, 0, 1, bottom, 8);
    ?STRIP(18, 0, 2, bottom, 9);
    ?STRIP(17, 0, 0, bottom, 10);
    ?STRIP(17, 0, 1, bottom, 11);
    ?STRIP(17, 0, 2, bottom, 12);
    ?STRIP(17, 0, 3, bottom, 13);
    ?STRIP(16, 0, 0, bottom, 14);
    ?STRIP(16, 0, 1, bottom, 15);
    ?STRIP(16, 0, 2, bottom, 16);
    ?STRIP(15, 0, 0, bottom, 17);
    ?STRIP(15, 0, 1, bottom, 18);
    ?STRIP(15, 0, 2, bottom, 19);
    ?STRIP(15, 0, 3, bottom, 20);
    ?STRIP(14, 0, 0, bottom, 21);
    ?STRIP(14, 0, 1, bottom, 22);
    ?STRIP(14, 0, 2, bottom, 23);
    ?STRIP(12, 3, 0, bottom, 24);
    ?STRIP(12, 3, 1, bottom, 25);
    ?STRIP(12, 3, 2, bottom, 26);
    ?STRIP(12, 3, 3, bottom, 27);
    ?STRIP(11, 3, 0, bottom, 28);
    ?STRIP(11, 3, 1, bottom, 29);
    ?STRIP(11, 3, 2, bottom, 30);
    ?STRIP(10, 3, 0, bottom, 31);
    ?STRIP(10, 3, 1, bottom, 32);
    ?STRIP(10, 3, 2, bottom, 33);
    ?STRIP(10, 3, 3, bottom, 34);
    ?STRIP(9, 3, 0, bottom, 35);
    ?STRIP(9, 3, 1, bottom, 36);
    ?STRIP(9, 3, 2, bottom, 37);
    ?STRIP(8, 3, 0, bottom, 38);
    ?STRIP(8, 3, 1, bottom, 39);
    ?STRIP(8, 3, 2, bottom, 40);
    ?STRIP(8, 3, 3, bottom, 41);
    ?STRIP(7, 3, 0, bottom, 42);
    ?STRIP(7, 3, 1, bottom, 43);
    ?STRIP(7, 3, 2, bottom, 44);
    ?STRIP(6, 3, 0, bottom, 45);
    ?STRIP(6, 3, 1, bottom, 46);
    ?STRIP(6, 3, 2, bottom, 47);
    ?STRIP(6, 3, 3, bottom, 48);
    ?STRIP(5, 3, 0, bottom, 49);
    ?STRIP(5, 3, 1, bottom, 50);
    ?STRIP(5, 3, 2, bottom, 51);
    ?STRIP(4, 3, 0, bottom, 52);
    ?STRIP(4, 3, 1, bottom, 53);
    ?STRIP(4, 3, 2, bottom, 54);
    ?STRIP(4, 3, 3, bottom, 55);
    ?STRIP(3, 3, 0, bottom, 56);
    ?STRIP(3, 3, 1, bottom, 57);
    ?STRIP(3, 3, 2, bottom, 58);
    ?STRIP(2, 3, 0, bottom, 59);
    ?STRIP(2, 3, 1, bottom, 60);
    ?STRIP(2, 3, 2, bottom, 61);
    ?STRIP(2, 3, 3, bottom, 62);
    ?STRIP(1, 3, 0, bottom, 63);
    ?STRIP(1, 3, 1, bottom, 64);
    ?STRIP(1, 3, 2, bottom, 65);
).

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Count = density:fuse_count(Density),
    Db = fuse_database:read(Density),
    run(0, Count, Density, Db).

%%--------------------------------------------------------------------

run(Stop, Stop, _, _) ->
    ok;
run(Fuse, Stop, Density, Db) ->
    run(Fuse, Density, Db),
    run(Fuse + 1, Stop, Density, Db).

%%====================================================================
%% run
%%====================================================================

-spec run(fuse(), density()) -> ok.

run(Fuse, Density) ->
    Db = fuse_database:read(Density),
    run(Fuse, Density, Db).

%%--------------------------------------------------------------------

run(Fuse, Density, Db) ->
    case fuse_database:find(Fuse, Db) of
        {ok, DbName} ->
            case to_name(Fuse, Density) of
                {ok, MapName} when MapName =:= DbName ->
                    case from_name(DbName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({
                                fuse, Density, Fuse,
                                db, DbName,
                                from_name, different, MapFuse
                            });

                        {error, Error} ->
                            throw({
                                fuse, Density, Fuse,
                                to, DbName,
                                from_name, error, Error
                            })
                    end;

                {ok, MapName} ->
                    throw({
                        fuse, Density, Fuse,
                        db, DbName,
                        to_name, different, MapName
                    });

                {error, Error} ->
                    throw({
                        fuse, Density, Fuse,
                        db, DbName,
                        to_name, error, Error
                    })
            end;

        false ->
            case to_name(Fuse, Density) of
                {ok, MapName} ->
                    case from_name(MapName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({
                                fuse, Density, Fuse,
                                to, MapName,
                                from_name, different, MapFuse
                            });

                        {error, Error} ->
                            throw({
                                fuse, Density, Fuse,
                                to, MapName,
                                from_name, error, Error
                            })
                    end;

                {error, _} ->
                    ok
            end
    end.

%%====================================================================
%% from_name
%%====================================================================

-spec from_name(name(), density()) -> {ok, fuse()} | {error, term()}.

from_name(Name, epm240) ->
    from_epm240(Name);
from_name(Name, epm570) ->
    from_epm570(Name);
from_name(Name, epm1270) ->
    from_epm1270(Name);
from_name(Name, epm2210) ->
    from_epm2210(Name).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm240({user_code, Bit}) -> {ok, Fuse}
).

?EPM240_USER_CODES()
from_epm240(Name) ->
    from_density(Name, epm240()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm570({user_code, Bit}) -> {ok, Fuse}
).

?EPM570_USER_CODES()
from_epm570(Name) ->
    from_density(Name, epm570()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm1270({user_code, Bit}) -> {ok, Fuse}
).

?EPM1270_USER_CODES()
from_epm1270(Name) ->
    from_density(Name, epm1270()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm2210({user_code, Bit}) -> {ok, Fuse}
).

?EPM2210_USER_CODES()
from_epm2210(Name) ->
    from_density(Name, epm2210()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

from_density({{ioc, X0, Y, N}, Name}, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X =:= 0 andalso X < With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            from_ioc_side(X, Y, N, Name, With);

        X when X =:= 0 andalso X =:= With#with.grow_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            from_ioc_side(X, Y, N, Name, With);

        X when X > 0 andalso X =< With#with.grow_x andalso
               (Y =:= 3 orelse Y =:= With#with.top_y) ->
            from_ioc(X, Y, N, Name, With);

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               (Y =:= 0 orelse Y =:= With#with.top_y) ->
            from_ioc(X, Y, N, Name, With);

        X when X =:= With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            from_ioc_side(X, Y, N, Name, With)
    end;
from_density({{lab, X0, Y}, Name}, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X > 0 andalso X =< With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            from_lab(X, Y, Name, With);

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            from_lab(X, Y, Name, With)
    end;
from_density({{lc, X0, Y, N}, Name}, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X > 0 andalso X =< With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            from_lc(X, Y, N, Name, With);

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            from_lc(X, Y, N, Name, With);

        X ->
            throw({from_density, {X, Y}, Name, With})
    end;
from_density({{lc, X0, Y, N}, lut, Name}, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X > 0 andalso X =< With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            from_lut(X, Y, N, Name, With);

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            from_lut(X, Y, N, Name, With)
    end;
from_density(_Name, _With = #with{}) ->
    {error, density}.

%%--------------------------------------------------------------------

-define(IOC_SIDE(Sector, Index, Name),
    from_ioc_side(X, Y, N, Name, With) ->
        from_side(X, Sector, Y, N + 2, Index, With)
).
-define(IOC_STRIP(R, C, Name),
    from_ioc_side(X, Y, N, Name, With) ->
        from_ioc_strip(X, Y, N, R, C, With)
).

?IOC_SIDES()
?IOC_STRIPS()
from_ioc_side(X, Y, N, Name, _With) ->
    {error, {ioc_side, X, Y, N, Name}}.

-undef(IOC_SIDE).
-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(IOC_HEAD(Sector, Index, N, Name),
    from_ioc(X, Y, N, Name, With = #with{top_y = Y}) ->
        from_head(X, Sector, Index, With)
).
-define(IOC_TAIL(Sector, Index, N, Name),
    from_ioc(X, 3, N, invert_guess, With) when X =< With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.short_y);
    from_ioc(X, 0, N, invert_guess, With) when X > With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.long_y)
).
-define(IOC_STRIP(R, C, Name),
    from_ioc(X, Y, N, Name, With) ->
        from_ioc_strip(X, Y, N, R, C, With)
).

?IOC_HEADS()
?IOC_TAILS()
?IOC_STRIPS()
from_ioc(X, Y, N, Name, _With) ->
    {error, {ioc, X, Y, N, Name}}.

-undef(IOC_HEAD).
-undef(IOC_TAIL).
-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(LAB_CELL(Sector, N, I, Name),
    from_lab(X, Y, Name, With) ->
        from_cell(X, Sector, Y, N, I, With)
).
-define(LAB_LINE(Sector, Index, Name),
    from_lab(X, Y, Name, With) ->
        from_line(X, Sector, Y, Index, With)
).

?LAB_CELLS()
?LAB_LINES()
from_lab(X, Y, Name, _With) ->
    {error, {lab, X, Y, Name}}.

-undef(LAB_CELL).
-undef(LAB_LINE).

%%--------------------------------------------------------------------

-define(LC_CELL(Sector, I, Name),
    from_lc(X, Y, N, Name, With) ->
        from_cell(X, Sector, Y, N, I, With)
).

?LC_CELLS()
from_lc(X, Y, N, Name, _With) ->
    {error, {lc, X, Y, N, Name}}.

-undef(LC_CELL).

%%--------------------------------------------------------------------

-define(LUT_CELL(Sector, I, Name),
    from_lut(X, Y, N, Name, With) ->
        from_cell(X, Sector, Y, N, I, With)
).

?LUT_CELLS()
from_lut(X, Y, N, Name, _With) ->
    {error, {lut, X, Y, N, Name}}.

-undef(LUT_CELL).

%%--------------------------------------------------------------------

from_ioc_strip(X, Y, N, R, C, With = #with{density = epm240}) ->
    from_epm240_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm570}) ->
    from_epm570_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm1270}) ->
    from_epm1270_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm2210}) ->
    from_epm2210_strip(X + With#with.offset_x, Y, N, R, C, With).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm240_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM240_STRIPS()
from_epm240_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm570_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM570_STRIPS()
from_epm570_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm1270_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM1270_STRIPS()
from_epm1270_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm2210_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM2210_STRIPS()
from_epm2210_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

from_side_strip(left, Index, R, C, With = #with{left_strip = Base}) ->
    from_base_strip(Base, Index, R, C, With);
from_side_strip(top, Index, R, C, With = #with{top_strip = Base}) ->
    from_base_strip(Base, Index, R, C, With);
from_side_strip(right, Index, R, C, With = #with{right_strip = Base})
        when With#with.density =:= epm1270 orelse
             With#with.density =:= epm2210 ->
    from_base_strip7(Base, Index, 5 - R, C, With);
from_side_strip(right, Index, R, C, With = #with{right_strip = Base}) ->
    from_base_strip(Base, Index, 5 - R, C, With);
from_side_strip(bottom, Index, R, C, With = #with{bottom_strip = Base}) ->
    from_base_strip(Base, Index, 5 - R, C, With).

%%--------------------------------------------------------------------

from_base_strip(Base, Index, R, C, #with{strip_width = Width}) ->
    {ok, ((Base + (Index * 6) + R) * Width) + C}.

%%--------------------------------------------------------------------

from_base_strip7(Base, Index, R, C, #with{strip_width = Width}) ->
    {ok, ((Base + (Index * 7) + R) * Width) + C}.

%%--------------------------------------------------------------------

from_side(X, Sector, Y, N, I, With) when N < 5 ->
    from_line(X, Sector, Y, (N * 4) + I, With);
from_side(X, Sector, Y, N, I, With) ->
    from_line(X, Sector, Y, 9 + (N * 4) - I, With).

%%--------------------------------------------------------------------

from_head(X, Sector, Offset, With) ->
    from_sector_skip(
        X,
        Sector,
        Offset,
        With
    ).

%%--------------------------------------------------------------------

from_tail(X, Sector, Offset, With, Lines) ->
    End = ?HEAD_WIDTH + (Lines * ?LINE_WIDTH) + 10,
    from_sector_skip(
        X,
        Sector,
        End - Offset,
        With
    ).

%%--------------------------------------------------------------------

from_cell(X, Sector, Y, N, I, With) when N < 5 ->
    from_line(X, Sector, Y, (N * 4) + I, With);
from_cell(X, Sector, Y, N, I, With) ->
    from_line(X, Sector, Y, 65 - (N * 4) - I, With).

%%--------------------------------------------------------------------

from_line(X, Sector, Y, Offset, With = #with{long_y = TopY}) ->
    from_sector_skip(
        X,
        Sector,
        ?HEAD_WIDTH + ((TopY - Y) * ?LINE_WIDTH) + Offset,
        With
    ).

%%--------------------------------------------------------------------

from_sector_skip(X, Sector, Offset, With = #with{})
        when Offset >= With#with.sector_skip  ->
    from_sector_pad(X, Sector, Offset + 1, With);
from_sector_skip(X, Sector, Offset, With) ->
    from_sector_pad(X, Sector, Offset, With).

%%--------------------------------------------------------------------

from_sector_pad(X, Sector, Offset, With = #with{}) ->
    Line = 1 + (Offset div (With#with.strip_width - 3)),
    from_sector(X, Sector, Offset + (Line * 3), With).

%%--------------------------------------------------------------------

from_sector(0, Sector, Offset, With = #with{}) ->
    {ok,
        With#with.left_base +
        (Sector * With#with.short_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X < With#with.grow_x orelse
             (X =:= With#with.grow_x andalso Sector < ?SHORT_SECTORS) ->
    {ok,
        With#with.short_base +
        ((X - 1) * ?COLUMN_SECTORS * With#with.short_sector) +
        (Sector * With#with.short_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X =:= With#with.grow_x ->
    {ok,
        With#with.grow_base +
        (?SHORT_SECTORS * With#with.short_sector) +
        ((Sector - ?SHORT_SECTORS) * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X < With#with.side_x ->
    {ok,
        With#with.long_base +
        ((X - 1 - With#with.grow_x) * ?COLUMN_SECTORS * With#with.long_sector) +
        (Sector * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X =:= With#with.side_x ->
    {ok,
        With#with.right_base +
        (Sector * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, _With) ->
    {error, {sector, X, Sector, Offset}}.

%%====================================================================
%% to_name
%%====================================================================

-spec to_name(fuse(), density()) -> {ok, name()} | {error, term()}.

to_name(Fuse, epm240) ->
    to_epm240(Fuse);
to_name(Fuse, epm570) ->
    to_epm570(Fuse);
to_name(Fuse, epm1270) ->
    to_epm1270(Fuse);
to_name(Fuse, epm2210) ->
    to_epm2210(Fuse).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm240(Fuse) -> {ok, {user_code, Bit}}
).

?EPM240_USER_CODES()
to_epm240(Name) ->
    to_density(Name, epm240()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm570(Fuse) -> {ok, {user_code, Bit}}
).

?EPM570_USER_CODES()
to_epm570(Name) ->
    to_density(Name, epm570()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm1270(Fuse) -> {ok, {user_code, Bit}}
).

?EPM1270_USER_CODES()
to_epm1270(Name) ->
    to_density(Name, epm1270()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm2210(Fuse) -> {ok, {user_code, Bit}}
).

?EPM2210_USER_CODES()
to_epm2210(Name) ->
    to_density(Name, epm2210()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

to_density(Fuse, With = #with{})
        when (Fuse rem With#with.strip_width) < 3 ->
    Row = Fuse div With#with.strip_width,
    Col = Fuse rem With#with.strip_width,
    to_strip(Row, Col, With);
to_density(Fuse, With = #with{}) ->
    OffsetX = With#with.offset_x,
    ShortSector = With#with.short_sector,
    LongSector = With#with.long_sector,
    ShortColumn = ?COLUMN_SECTORS * ShortSector,
    LongColumn = ?COLUMN_SECTORS * LongSector,
    if
        Fuse < With#with.left_base ->
            {error, {header, Fuse}};

        Fuse < With#with.short_base ->
            SectorOffset = Fuse - With#with.left_base,
            to_column(
                OffsetX,
                side,
                SectorOffset div ShortSector,
                SectorOffset rem ShortSector,
                With,
                With#with.short_y
            );

        Fuse < With#with.grow_base ->
            ColumnOffset = Fuse - With#with.short_base,
            SectorOffset = ColumnOffset rem ShortColumn,
            to_column(
                OffsetX + 1 + (ColumnOffset div ShortColumn),
                cell,
                SectorOffset div ShortSector,
                SectorOffset rem ShortSector,
                With,
                With#with.short_y
            );

        Fuse < With#with.long_base ->
            GrowOffset = ?SHORT_SECTORS * ShortSector,
            case Fuse - With#with.grow_base of
                SectorOffset when SectorOffset < GrowOffset ->
                    to_column(
                        OffsetX + With#with.grow_x,
                        cell,
                        SectorOffset div ShortSector,
                        SectorOffset rem ShortSector,
                        With,
                        With#with.short_y
                    );

                SectorOffset0 ->
                    SectorOffset = SectorOffset0 - GrowOffset,
                    to_column(
                        OffsetX + With#with.grow_x,
                        cell,
                        (SectorOffset div LongSector) + ?SHORT_SECTORS,
                        SectorOffset rem LongSector,
                        With,
                        With#with.long_y
                    )
            end;

        Fuse < With#with.gap_base ->
            ColumnOffset = Fuse - With#with.long_base,
            SectorOffset = ColumnOffset rem LongColumn,
            to_column(
                OffsetX + With#with.grow_x + 1 + (ColumnOffset div LongColumn),
                cell,
                SectorOffset div LongSector,
                SectorOffset rem LongSector,
                With,
                With#with.long_y
            );

        Fuse < With#with.right_base ->
            {error, {middle, Fuse}};

        Fuse < With#with.end_base ->
            SectorOffset = Fuse - With#with.right_base,
            to_column(
                OffsetX + With#with.side_x,
                side,
                SectorOffset div LongSector,
                SectorOffset rem LongSector,
                With,
                With#with.long_y
            );

        true ->
            {error, {footer, Fuse}}
    end.

%%--------------------------------------------------------------------

to_strip(Row, Col, With = #with{}) when Row < With#with.left_strip ->
    {error, {strip, Row, Col}};
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.top_strip ->
    Row = Row0 - With#with.left_strip,
    to_strip(left, Row div 6, Row rem 6, Col, With);
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.right_strip ->
    Row = Row0 - With#with.top_strip,
    to_strip(top, Row div 6, Row rem 6, Col, With);
to_strip(Row0, Col, With = #with{})
        when Row0 < With#with.bottom_strip andalso
             (With#with.density =:= epm1270 orelse
              With#with.density =:= epm2210) ->
    Row = Row0 - With#with.right_strip,
    to_strip(right, Row div 7, 5 - (Row rem 7), Col, With);
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.bottom_strip ->
    Row = Row0 - With#with.right_strip,
    to_strip(right, Row div 6, 5 - (Row rem 6), Col, With);
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.end_strip ->
    Row = Row0 - With#with.bottom_strip,
    to_strip(bottom, Row div 6, 5 - (Row rem 6), Col, With);
to_strip(Row, Col, _With) ->
    {error, {strip, Row, Col}}.

%%--------------------------------------------------------------------

to_strip(Side, Index, R, C, #with{density = epm240}) ->
    to_epm240_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm570}) ->
    to_epm570_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm1270}) ->
    to_epm1270_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm2210}) ->
    to_epm2210_strip(Side, Index, R, C).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm240_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM240_STRIPS()
to_epm240_strip(Side, Index, R, C) ->
    {error, {strip, Side, Index, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm570_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM570_STRIPS()
to_epm570_strip(Side, Index, R, C) ->
    {error, {strip, Side, Index, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm1270_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM1270_STRIPS()
to_epm1270_strip(Side, Index, R, C) ->
    {error, {strip, Side, Index, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm2210_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM2210_STRIPS()
to_epm2210_strip(Side, Index, R, C) ->
    {error, {strip, Side, Index, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(IOC_STRIP(R, C, Name),
    to_ioc_strip(X, Y, N, R, C) -> {ok, {{ioc, X, Y, N}, Name}}
).

?IOC_STRIPS()
to_ioc_strip(X, Y, N, R, C) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(IOC_STRIP).

%%--------------------------------------------------------------------

to_column(X, Type, Sector, Offset0, With = #with{}, Lines) ->
    Skip = With#with.sector_skip,
    Top = With#with.long_y,
    End = ?HEAD_WIDTH + 1 + (Lines * ?LINE_WIDTH),
    Padding = 3 * (1 + (Offset0 div With#with.strip_width)),
    case Offset0 - Padding of
        Offset when Offset < ?HEAD_WIDTH ->
            to_head(X, Type, Sector, Offset, With);

        Offset when Offset < Skip ->
            Trimmed = Offset - ?HEAD_WIDTH,
            to_line(
                X,
                Type,
                Sector,
                Top - (Trimmed div ?LINE_WIDTH),
                Trimmed rem ?LINE_WIDTH,
                With
            );

        Offset when Offset =:= Skip ->
            {error, {sector, X, Type, Sector, skip}};

        Offset when Offset < End ->
            Trimmed = Offset - ?HEAD_WIDTH - 1,
            to_line(
                X,
                Type,
                Sector,
                Top - (Trimmed div ?LINE_WIDTH),
                Trimmed rem ?LINE_WIDTH,
                With
            );

        Offset when Offset < End + ?HEAD_WIDTH ->
            to_tail(X, Type, Sector, 10 + End - Offset, With);

        Offset ->
            {error, {column, X, Type, Sector, Offset - 1}}
    end.

%%--------------------------------------------------------------------

-define(IOC_HEAD(Sector, Index, N, Name),
    to_head(X, cell, Sector, Index, With) ->
        {ok, {{ioc, X, With#with.top_y, N}, Name}}
).

?IOC_HEADS()
to_head(X, Type, Sector, Offset, _With) ->
    {error, {head, X, Type, Sector, Offset}}.

-undef(IOC_HEAD).

%%--------------------------------------------------------------------

-define(IOC_TAIL(Sector, Index, N, Name),
    to_tail(X, cell, Sector, Index, With) when X =< With#with.grow_x ->
        {ok, {{ioc, X, 3, N}, Name}};
    to_tail(X, cell, Sector, Index, With) when X > With#with.grow_x ->
        {ok, {{ioc, X, 0, N}, Name}}
).

?IOC_TAILS()
to_tail(X, Type, Sector, Offset, _With) ->
    {error, {tail, X, Type, Sector, Offset}}.

-undef(IOC_TAIL).

%%--------------------------------------------------------------------

to_line(X, side, Sector, Y, Index, With) when Index < 20 ->
    to_side(X, Sector, Y, Index div 4, Index rem 4, With);
to_line(X, side, Sector, Y, Index, With) when Index < 26 ->
    to_side_line(X, Sector, Y, Index, With);
to_line(X, side, Sector, Y, Index0, With) ->
    Index = Index0 - 6,
    to_side(X, Sector, Y, Index div 4, 3 - (Index rem 4), With);
to_line(X, cell, Sector, Y, Index, With) when Index < 20 ->
    to_cell(X, Sector, Y, Index div 4, Index rem 4, With);
to_line(X, cell, Sector, Y, Index, With) when Index < 26 ->
    to_cell_line(X, Sector, Y, Index, With);
to_line(X, cell, Sector, Y, Index0, With) ->
    Index = 65 - Index0,
    to_cell(X, Sector, Y, Index div 4, Index rem 4, With).

%%--------------------------------------------------------------------

to_side_line(X, Sector, Y, Offset, _With) ->
    {error, {side_line, X, Sector, Y, Offset}}.

%%--------------------------------------------------------------------

-define(IOC_SIDE(Sector, Index, Name),
    to_side(X, Sector, Y, N, Index, _) when N >= 2 andalso N =< 8 ->
        {ok, {{ioc, X, Y, N - 2}, Name}};
    to_side(X, Sector, Y, N, Index, _) ->
        {error, {side, X, Sector, Y, N, Index, Name}}
).

?IOC_SIDES()
to_side(X, Sector, Y, N, I, _With) ->
    {error, {side, X, Sector, Y, N, I}}.

-undef(IOC_SIDE).

%%--------------------------------------------------------------------

-define(LAB_LINE(Sector, Index, Name),
    to_cell_line(X, Sector, Y, Index, _) ->
        {ok, {{lab, X, Y}, Name}}
).

to_cell_line(X, Sector, Y, Offset, With = #with{})
        when X =< With#with.grow_x andalso Y =< 3 ->
    {error, {cell_line, X, Sector, Y, Offset}};
?LAB_LINES()
to_cell_line(X, Sector, Y, Offset, _With) ->
    {error, {cell_line, X, Sector, Y, Offset}}.

-undef(LAB_LINE).

%%--------------------------------------------------------------------

-define(LAB_CELL(Sector, N, I, Name),
    to_cell(X, Sector, Y, N, I, _) ->
        {ok, {{lab, X, Y}, Name}}
).
-define(LC_CELL(Sector, I, Name),
    to_cell(X, Sector, Y, N, I, _) ->
        {ok, {{lc, X, Y, N}, Name}}
).
-define(LUT_CELL(Sector, I, Name),
    to_cell(X, Sector, Y, N, I, _) ->
        {ok, {{lc, X, Y, N}, lut, Name}}
).

to_cell(X, Sector, Y, N, I, With = #with{})
        when X =< With#with.grow_x andalso Y =< 3 ->
    {error, {cell, X, Sector, Y, N, I}};
?LAB_CELLS()
?LC_CELLS()
?LUT_CELLS()
to_cell(X, Sector, Y, N, I, _With) ->
    {error, {cell, X, Sector, Y, N, I}}.

-undef(LAB_CELL).
-undef(LC_CELL).
-undef(LUT_CELL).

%%====================================================================
%% density
%%====================================================================

epm240() ->
    #with{
        density = epm240,
        strip_width = 32,
        offset_x = 1,
        side_x = 7, % after offset subtracted
        % EPM240 does not have short and long columns,
        % Treat all columns as long
        grow_x = 0,
        short_y = 4,
        long_y = 4,
        top_y = 5,
        short_sector = 256,
        long_sector = 256,
        left_base = 544,
        short_base = 4128,
        grow_base = 4128,
        long_base = 4128,
        gap_base = 47136,
        right_base = 48928,
        end_base = 52512,
        sector_skip = 103,
        left_strip = 1081,
        top_strip = 1177,
        right_strip = 1315,
        bottom_strip = 1429,
        end_strip = 1561
    }.

%%--------------------------------------------------------------------

epm570() ->
    #with{
        density = epm570,
        strip_width = 32,
        offset_x = 0,
        side_x = 13,
        grow_x = 9,
        short_y = 4,
        long_y = 7,
        top_y = 8,
        short_sector = 256,
        long_sector = 384,
        left_base = 544,
        short_base = 4128,
        grow_base = 61472,
        long_base = 70048,
        gap_base = 102304,
        right_base = 104992,
        end_base = 108576,
        sector_skip = 149,
        left_strip = 2369,
        top_strip = 2561,
        right_strip = 2837,
        bottom_strip = 3089,
        end_strip = 3329
    }.

%%--------------------------------------------------------------------

epm1270() ->
    #with{
        density = epm1270,
        strip_width = 64,
        offset_x = 0,
        side_x = 17,
        grow_x = 11,
        short_y = 7,
        long_y = 10,
        top_y = 11,
        short_sector = 384,
        long_sector = 512,
        left_base = 832,
        short_base = 6208,
        grow_base = 113728,
        long_base = 125888,
        gap_base = 197568,
        right_base = 201152,
        end_base = 206848,
        sector_skip = 241,
        left_strip = 1848,
        top_strip = 2154,
        right_strip = 2472,
        bottom_strip = 2857,
        end_strip = 3175
    }.

%%--------------------------------------------------------------------

epm2210() ->
    #with{
        density = epm2210,
        strip_width = 64,
        offset_x = 0,
        side_x = 21,
        grow_x = 13,
        short_y = 10,
        long_y = 13,
        top_y = 14,
        short_sector = 512,
        long_sector = 704,
        left_base = 1088,
        short_base = 8256,
        grow_base = 180288,
        long_base = 196736,
        gap_base = 334720,
        right_base = 339648,
        end_base = 346816,
        sector_skip = 287,
        left_strip = 3647,
        top_strip = 4055,
        right_strip = 4451,
        bottom_strip = 4955,
        end_strip = 5351
    }.

