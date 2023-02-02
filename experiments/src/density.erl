-module(density).

-export([list/0]).
-export([devices/1]).
-export([or_device/1]).

-export_type([density/0]).

-type density() ::
    epm240 |
    epm570 |
    epm1270 |
    epm2210.

-type device() :: device:device().

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
%% or_device
%%====================================================================

-spec or_device(density() | device()) -> density().

or_device(Density = epm240) -> Density;
or_device(Density = epm570) -> Density;
or_device(Density = epm1270) -> Density;
or_device(Density = epm2210) -> Density;
or_device(Device) ->
    device:density(Device).

