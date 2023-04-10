-module(density).

-export([list/0]).
-export([devices/1]).
-export([largest_device/1]).
-export([fuse_count/1]).
-export([or_device/1]).
-export([minimal_fuses/1]).
-export([fast_outs/1]).
-export([iobs/1]).
-export([labs/1]).
-export([metric/1]).
-export([top_io/1]).
-export([top_lab/1]).
-export([left_io/2]).
-export([left_lab/2]).
-export([right_io/1]).
-export([right_lab/1]).
-export([bottom_io/2]).
-export([bottom_lab/2]).
-export([columns/1]).
-export([rows/1]).

-export_type([density/0]).

-include("max_ii.hrl").

-type density() ::
    epm240 |
    epm570 |
    epm1270 |
    epm2210.

-type device() :: device:device().
-type fuse() :: fuse:fuse().
-type iob() :: iob:iob().
-type ioc() :: ioc:ioc().
-type lab() :: lab:lab().
-type lc() :: lc:lc().
-type x() :: max_ii:x().
-type y() :: max_ii:y().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [density()].

list() ->
    [epm240,
     epm570,
     epm1270,
     epm2210
    ].

%%====================================================================
%% devices
%%====================================================================

-spec devices(density()) -> [device()].

devices(epm240) ->
    [
        epm240_m100,
        epm240_f100,
        epm240_t100
    ];
devices(epm570) ->
    [
        epm570_m100,
        epm570_f100,
        epm570_t100,
        epm570_t144,
        epm570_m256,
        epm570_f256
    ];
devices(epm1270) ->
    [
        epm1270_m144,
        epm1270_t144,
        epm1270_m256
    ];
devices(epm2210) ->
    [
        epm2210_f256,
        epm2210_f324
    ].

%%====================================================================
%% largest_device
%%====================================================================

-spec largest_device(density()) -> device().

largest_device(epm240) ->
    epm240_t100;
largest_device(epm570) ->
    epm570_f256;
largest_device(epm1270) ->
    epm1270_m256;
largest_device(epm2210) ->
    epm2210_f324.

%%====================================================================
%% fuse_count
%%====================================================================

-spec fuse_count(density() | device()) -> 53248 | 110592 | 208896 | 348160.

fuse_count(epm240) -> 53248;
fuse_count(epm570) -> 110592;
fuse_count(epm1270) -> 208896;
fuse_count(epm2210) -> 348160.

%%====================================================================
%% or_device
%%====================================================================

-spec or_device(density() | device()) -> density().

or_device(Density = epm240) -> Density;
or_device(Density = epm570) -> Density;
or_device(Density = epm1270) -> Density;
or_device(Density = epm2210) -> Density;
or_device(Device) ->
    device:density(Device).

%%====================================================================
%% minimal_fuses
%%====================================================================

-spec minimal_fuses(density() | device()) -> [fuse()].

minimal_fuses(epm240) -> epm240_minimal:fuses();
minimal_fuses(epm570) -> epm570_minimal:fuses();
minimal_fuses(epm1270) -> epm1270_minimal:fuses();
minimal_fuses(epm2210) -> epm2210_minimal:fuses();
minimal_fuses(Device) ->
    minimal_fuses(device:density(Device)).

%%====================================================================
%% fast_outs
%%====================================================================

-spec fast_outs(density() | device()) -> [{ioc(), lc(), left | right}].

fast_outs(epm240) -> epm240_fast_out:iocs();
fast_outs(epm570) -> epm570_fast_out:iocs();
fast_outs(epm1270) -> epm1270_fast_out:iocs();
fast_outs(epm2210) -> epm2210_fast_out:iocs();
fast_outs(Device) ->
    fast_outs(device:density(Device)).

%%====================================================================
%% iobs
%%====================================================================

-spec iobs(density()) -> [{iob(), lab()}].

-define(IOB_LEFT(X, Y), {{iob, X, Y}, {lab, X + 1, Y}}).
-define(IOB_BOTTOM(X, Y), {{iob, X, Y}, {lab, X, Y + 1}}).
-define(IOB_RIGHT(X, Y), {{iob, X, Y}, {lab, X - 1, Y}}).
-define(IOB_TOP(X, Y), {{iob, X, Y}, {lab, X, Y - 1}}).

iobs(epm240) ->
    [?IOB_LEFT(1, 4),
     ?IOB_LEFT(1, 3),
     ?IOB_LEFT(1, 2),
     ?IOB_LEFT(1, 1),
     ?IOB_BOTTOM(2, 0),
     ?IOB_BOTTOM(3, 0),
     ?IOB_BOTTOM(4, 0),
     ?IOB_BOTTOM(5, 0),
     ?IOB_BOTTOM(6, 0),
     ?IOB_BOTTOM(7, 0),
     ?IOB_RIGHT(8, 4),
     ?IOB_RIGHT(8, 3),
     ?IOB_RIGHT(8, 2),
     ?IOB_RIGHT(8, 1),
     ?IOB_TOP(7, 5),
     ?IOB_TOP(6, 5),
     ?IOB_TOP(5, 5),
     ?IOB_TOP(4, 5),
     ?IOB_TOP(3, 5),
     ?IOB_TOP(2, 5)];
iobs(epm570) ->
    [?IOB_LEFT(0, 7),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 4),
     ?IOB_BOTTOM(1, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(8, 3),
     %?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(10, 0),
     ?IOB_BOTTOM(11, 0),
     ?IOB_BOTTOM(12, 0),
     ?IOB_RIGHT(13, 1),
     ?IOB_RIGHT(13, 2),
     ?IOB_RIGHT(13, 3),
     ?IOB_RIGHT(13, 4),
     ?IOB_RIGHT(13, 5),
     ?IOB_RIGHT(13, 6),
     ?IOB_RIGHT(13, 7),
     ?IOB_TOP(12, 8),
     ?IOB_TOP(11, 8),
     ?IOB_TOP(10, 8),
     ?IOB_TOP(9, 8),
     ?IOB_TOP(8, 8),
     ?IOB_TOP(7, 8),
     ?IOB_TOP(6, 8),
     ?IOB_TOP(5, 8),
     ?IOB_TOP(4, 8),
     ?IOB_TOP(3, 8),
     ?IOB_TOP(2, 8),
     ?IOB_TOP(1, 8)];
iobs(epm1270) ->
    [?IOB_LEFT(0, 10),
     ?IOB_LEFT(0, 9),
     ?IOB_LEFT(0, 8),
     ?IOB_LEFT(0, 7),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 4),
     ?IOB_BOTTOM(1, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(8, 3),
     ?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(10, 3),
     %?IOB_BOTTOM(11, 3),
     ?IOB_BOTTOM(12, 0),
     ?IOB_BOTTOM(13, 0),
     ?IOB_BOTTOM(14, 0),
     ?IOB_BOTTOM(15, 0),
     ?IOB_BOTTOM(16, 0),
     ?IOB_RIGHT(17, 1),
     ?IOB_RIGHT(17, 2),
     ?IOB_RIGHT(17, 3),
     ?IOB_RIGHT(17, 4),
     ?IOB_RIGHT(17, 5),
     ?IOB_RIGHT(17, 6),
     ?IOB_RIGHT(17, 7),
     ?IOB_RIGHT(17, 8),
     ?IOB_RIGHT(17, 9),
     ?IOB_RIGHT(17, 10),
     ?IOB_TOP(16, 11),
     ?IOB_TOP(15, 11),
     ?IOB_TOP(14, 11),
     ?IOB_TOP(13, 11),
     ?IOB_TOP(12, 11),
     ?IOB_TOP(11, 11),
     ?IOB_TOP(10, 11),
     ?IOB_TOP(9, 11),
     ?IOB_TOP(8, 11),
     ?IOB_TOP(7, 11),
     ?IOB_TOP(6, 11),
     ?IOB_TOP(5, 11),
     ?IOB_TOP(4, 11),
     ?IOB_TOP(3, 11),
     ?IOB_TOP(2, 11),
     ?IOB_TOP(1, 11)];
iobs(epm2210) ->
    [?IOB_LEFT(0, 13),
     ?IOB_LEFT(0, 12),
     ?IOB_LEFT(0, 11),
     ?IOB_LEFT(0, 10),
     ?IOB_LEFT(0, 9),
     ?IOB_LEFT(0, 8),
     ?IOB_LEFT(0, 7),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 4),
     ?IOB_BOTTOM(1, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(8, 3),
     ?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(10, 3),
     ?IOB_BOTTOM(11, 3),
     ?IOB_BOTTOM(12, 3),
     %?IOB_BOTTOM(13, 3),
     ?IOB_BOTTOM(14, 0),
     ?IOB_BOTTOM(15, 0),
     ?IOB_BOTTOM(16, 0),
     ?IOB_BOTTOM(17, 0),
     ?IOB_BOTTOM(18, 0),
     ?IOB_BOTTOM(19, 0),
     ?IOB_BOTTOM(20, 0),
     ?IOB_RIGHT(21, 1),
     ?IOB_RIGHT(21, 2),
     ?IOB_RIGHT(21, 3),
     ?IOB_RIGHT(21, 4),
     ?IOB_RIGHT(21, 5),
     ?IOB_RIGHT(21, 6),
     ?IOB_RIGHT(21, 7),
     ?IOB_RIGHT(21, 8),
     ?IOB_RIGHT(21, 9),
     ?IOB_RIGHT(21, 10),
     ?IOB_RIGHT(21, 11),
     ?IOB_RIGHT(21, 12),
     ?IOB_RIGHT(21, 13),
     ?IOB_TOP(20, 14),
     ?IOB_TOP(19, 14),
     ?IOB_TOP(18, 14),
     ?IOB_TOP(17, 14),
     ?IOB_TOP(16, 14),
     ?IOB_TOP(15, 14),
     ?IOB_TOP(14, 14),
     ?IOB_TOP(13, 14),
     ?IOB_TOP(12, 14),
     ?IOB_TOP(11, 14),
     ?IOB_TOP(10, 14),
     ?IOB_TOP(9, 14),
     ?IOB_TOP(8, 14),
     ?IOB_TOP(7, 14),
     ?IOB_TOP(6, 14),
     ?IOB_TOP(5, 14),
     ?IOB_TOP(4, 14),
     ?IOB_TOP(3, 14),
     ?IOB_TOP(2, 14),
     ?IOB_TOP(1, 14)].

%%====================================================================
%% labs
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fuses_test() ->
    ?assertEqual(24, length(labs(epm240))),
    ?assertEqual(57, length(labs(epm570))),
    ?assertEqual(127, length(labs(epm1270))),
    ?assertEqual(221, length(labs(epm2210))).

-endif.

-spec labs(density()) -> [lab()].

-define(COLUMN4(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3}
).
-define(COLUMN7(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6}
).
-define(COLUMN10(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6},
    {lab, X, Y - 7},
    {lab, X, Y - 8},
    {lab, X, Y - 9}
).
-define(COLUMN13(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6},
    {lab, X, Y - 7},
    {lab, X, Y - 8},
    {lab, X, Y - 9},
    {lab, X, Y - 10},
    {lab, X, Y - 11},
    {lab, X, Y - 12}
).

labs(epm240) ->
    [?COLUMN4(2, 4),
     ?COLUMN4(3, 4),
     ?COLUMN4(4, 4),
     ?COLUMN4(5, 4),
     ?COLUMN4(6, 4),
     ?COLUMN4(7, 4)
    ];
labs(epm570) ->
    [?COLUMN4(1, 7),
     ?COLUMN4(2, 7),
     ?COLUMN4(3, 7),
     ?COLUMN4(4, 7),
     ?COLUMN4(5, 7),
     ?COLUMN4(6, 7),
     ?COLUMN4(7, 7),
     ?COLUMN4(8, 7),
     ?COLUMN4(9, 7),
     ?COLUMN7(10, 7),
     ?COLUMN7(11, 7),
     ?COLUMN7(12, 7)
    ];
labs(epm1270) ->
    [?COLUMN7(1, 10),
     ?COLUMN7(2, 10),
     ?COLUMN7(3, 10),
     ?COLUMN7(4, 10),
     ?COLUMN7(5, 10),
     ?COLUMN7(6, 10),
     ?COLUMN7(7, 10),
     ?COLUMN7(8, 10),
     ?COLUMN7(9, 10),
     ?COLUMN7(10, 10),
     ?COLUMN7(11, 10),
     ?COLUMN10(12, 10),
     ?COLUMN10(13, 10),
     ?COLUMN10(14, 10),
     ?COLUMN10(15, 10),
     ?COLUMN10(16, 10)
    ];
labs(epm2210) ->
    [?COLUMN10(1, 13),
     ?COLUMN10(2, 13),
     ?COLUMN10(3, 13),
     ?COLUMN10(4, 13),
     ?COLUMN10(5, 13),
     ?COLUMN10(6, 13),
     ?COLUMN10(7, 13),
     ?COLUMN10(8, 13),
     ?COLUMN10(9, 13),
     ?COLUMN10(10, 13),
     ?COLUMN10(11, 13),
     ?COLUMN10(12, 13),
     ?COLUMN10(13, 13),
     ?COLUMN13(14, 13),
     ?COLUMN13(15, 13),
     ?COLUMN13(16, 13),
     ?COLUMN13(17, 13),
     ?COLUMN13(18, 13),
     ?COLUMN13(19, 13),
     ?COLUMN13(20, 13)
    ].

%%====================================================================
%% metric
%%====================================================================

-spec metric(density()) -> #metric{}.

-define(METRIC(L, LL, R, B, BB, T), #metric{
    density = Density,
    top_io = T,
    top_lab = T - 1,
    left_io = L,
    left_lab = L + 1,
    right_io = R,
    right_lab = R - 1,
    bottom_io = B,
    bottom_lab = B + 1,
    indent_left_io = LL,
    indent_left_lab = LL + 1,
    indent_bottom_io = BB,
    indent_bottom_lab = BB + 1
}).

metric(Density = epm240) -> ?METRIC(1, 8, 8, 0, 0, 5);
metric(Density = epm570) -> ?METRIC(0, 9, 13, 0, 3, 8);
metric(Density = epm1270) -> ?METRIC(0, 11, 17, 0, 3, 11);
metric(Density = epm2210) -> ?METRIC(0, 13, 21, 0, 3, 14).

%%--------------------------------------------------------------------

-spec top_io(density()) -> y().

top_io(Density) ->
    Metric = metric(Density),
    Metric#metric.top_io.

%%--------------------------------------------------------------------

-spec top_lab(density()) -> y().

top_lab(Density) ->
    Metric = metric(Density),
    Metric#metric.top_lab.

%%--------------------------------------------------------------------

-spec left_io(y(), density()) -> x().

left_io(Y, Density) ->
    case metric(Density) of
        Metric when Y =< Metric#metric.indent_bottom_io ->
            Metric#metric.indent_left_io;

        Metric ->
            Metric#metric.left_io
    end.

%%--------------------------------------------------------------------

-spec left_lab(y(), density()) -> x().

left_lab(Y, Density) ->
    case metric(Density) of
        Metric when Y =< Metric#metric.indent_bottom_io ->
            Metric#metric.indent_left_lab;

        Metric ->
            Metric#metric.left_lab
    end.

%%--------------------------------------------------------------------

-spec right_io(density()) -> x().

right_io(Density) ->
    Metric = metric(Density),
    Metric#metric.right_io.

%%--------------------------------------------------------------------

-spec right_lab(density()) -> x().

right_lab(Density) ->
    Metric = metric(Density),
    Metric#metric.right_lab.

%%--------------------------------------------------------------------

-spec bottom_io(x(), density()) -> y().

bottom_io(X, Density) ->
    case metric(Density) of
        Metric when X =< Metric#metric.indent_left_io ->
            Metric#metric.indent_bottom_io;

        Metric ->
            Metric#metric.bottom_io
    end.

%%--------------------------------------------------------------------

-spec bottom_lab(x(), density()) -> y().

bottom_lab(X, Density) ->
    case metric(Density) of
        Metric when X =< Metric#metric.indent_left_io ->
            Metric#metric.indent_bottom_lab;

        Metric ->
            Metric#metric.bottom_lab
    end.

%%====================================================================
%% columns
%%====================================================================

-spec columns(density()) -> [max_ii:x()].

columns(epm240) -> [2,3,4,5,6,7];
columns(epm570) -> [1,2,3,4,5,6,7,8,9,10,11,12];
columns(epm1270) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
columns(epm2210) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20].

%%====================================================================
%% rows
%%====================================================================

-spec rows(density()) -> [max_ii:y()].

rows(epm240) -> [1,2,3,4];
rows(epm570) -> [1,2,3,4,5,6,7];
rows(epm1270) -> [1,2,3,4,5,6,7,8,9,10];
rows(epm2210) -> [1,2,3,4,5,6,7,8,9,10,11,12,13].

