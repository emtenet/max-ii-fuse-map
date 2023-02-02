-module(package).

-export([list/0]).
-export([title/1]).
-export([gclk_pins/1]).

-export_type([package/0]).

-type package() ::
    m100 |
    f100 |
    t100 |
    t144 |
    m256 |
    f256 |
    f324.

-type pin() :: pin:pin().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [package()].

list() ->
    [m100,
     f100,
     t100,
     t144,
     m256,
     f256,
     f324
    ].

%%====================================================================
%% title
%%====================================================================

-spec title(package()) -> string().

title(m100) -> "100-Pin Micro FineLine BGA";
title(f100) -> "100-Pin FineLine BGA";
title(t100) -> "100-Pin TQFP";
title(t144) -> "144-Pin TQFP";
title(m256) -> "256-Pin Micro FineLine BGA";
title(f256) -> "256-Pin FineLine BGA";
title(f324) -> "324-Pin FineLine BGA".

%%====================================================================
%% gclk_pins
%%====================================================================

-spec gclk_pins(package()) -> [pin()].

gclk_pins(m100) -> [f2, e1, f10, g11];
gclk_pins(f100) -> [e2, e1, f8, e10];
gclk_pins(t100) -> [pin12, pin14, pin62, pin64];
gclk_pins(t144) -> [pin18 ,pin20, pin89, pin91];
gclk_pins(m256) -> [k1, l1, m20, l20];
gclk_pins(f256) -> [h5, j5, j12, h12];
gclk_pins(f324) -> [j6, k6, k13, j13].

