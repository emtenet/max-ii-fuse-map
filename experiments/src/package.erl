-module(package).

-export([list/0]).
-export([title/1]).

-export_type([package/0]).

-type package() ::
    m100 |
    f100 |
    t100 |
    t144 |
    m256 |
    f256 |
    f324.

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

