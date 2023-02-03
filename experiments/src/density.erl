-module(density).

-export([list/0]).
-export([devices/1]).
-export([largest_device/1]).
-export([fuse_count/1]).
-export([or_device/1]).
-export([labs/1]).

-export_type([density/0]).

-type density() ::
    epm240 |
    epm570 |
    epm1270 |
    epm2210.

-type device() :: device:device().
-type lab() :: lab:lab().

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

