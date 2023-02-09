-module(device).

-export([list/0]).
-export([name/1]).
-export([from_name/1]).
-export([density/1]).
-export([package/1]).
-export([labs/1]).
-export([pins/1]).
-export([gclk_pins/1]).

-export_type([device/0]).

-type density() :: density:density().

-type device() ::
    epm240_m100 |
    epm240_f100 |
    epm240_t100 |
    epm570_m100 |
    epm570_f100 |
    epm570_t100 |
    epm570_t144 |
    epm570_m256 |
    epm570_f256 |
    epm1270_t144 |
    epm1270_m256 |
    epm1270_f256 |
    epm2210_f256 |
    epm2210_f324.

-type ioc() :: ioc:ioc().
-type lab() :: lab:lab().
-type package() :: package:package().
-type pin() :: pin:pin().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [device()].

list() ->
    [epm240_m100,
     epm240_f100,
     epm240_t100,
     epm570_m100,
     epm570_f100,
     epm570_t100,
     epm570_t144,
     epm570_m256,
     epm570_f256,
     epm1270_t144,
     epm1270_m256,
     epm1270_f256,
     epm2210_f256,
     epm2210_f324
    ].

%%====================================================================
%% name
%%====================================================================

-spec name(device()) -> binary().

name(epm240_m100) -> <<"EPM240M100C5">>;
name(epm240_f100) -> <<"EPM240F100C5">>;
name(epm240_t100) -> <<"EPM240T100C5">>;
name(epm570_m100) -> <<"EPM570M100C5">>;
name(epm570_f100) -> <<"EPM570F100C5">>;
name(epm570_t100) -> <<"EPM570T100C5">>;
name(epm570_t144) -> <<"EPM570T144C5">>;
name(epm570_m256) -> <<"EPM570M256C5">>;
name(epm570_f256) -> <<"EPM570F256C5">>;
name(epm1270_t144) -> <<"EPM1270T144C5">>;
name(epm1270_m256) -> <<"EPM1270M256C5">>;
name(epm1270_f256) -> <<"EPM1270F256C5">>;
name(epm2210_f256) -> <<"EPM2210F256C5">>;
name(epm2210_f324) -> <<"EPM2210F324C5">>.

%%====================================================================
%% from_name
%%====================================================================

-spec from_name(device()) -> binary().

from_name(<<"EPM240M100C5">>) -> epm240_m100;
from_name(<<"EPM240F100C5">>) -> epm240_f100;
from_name(<<"EPM240T100C5">>) -> epm240_t100;
from_name(<<"EPM570M100C5">>) -> epm570_m100;
from_name(<<"EPM570F100C5">>) -> epm570_f100;
from_name(<<"EPM570T100C5">>) -> epm570_t100;
from_name(<<"EPM570T144C5">>) -> epm570_t144;
from_name(<<"EPM570M256C5">>) -> epm570_m256;
from_name(<<"EPM570F256C5">>) -> epm570_f256;
from_name(<<"EPM1270T144C5">>) -> epm1270_t144;
from_name(<<"EPM1270M256C5">>) -> epm1270_m256;
from_name(<<"EPM1270F256C5">>) -> epm1270_f256;
from_name(<<"EPM2210F256C5">>) -> epm2210_f256;
from_name(<<"EPM2210F324C5">>) -> epm2210_f324.

%%====================================================================
%% density
%%====================================================================

-spec density(device()) -> density().

density(epm240_m100) -> epm240;
density(epm240_f100) -> epm240;
density(epm240_t100) -> epm240;
density(epm570_m100) -> epm570;
density(epm570_f100) -> epm570;
density(epm570_t100) -> epm570;
density(epm570_t144) -> epm570;
density(epm570_m256) -> epm570;
density(epm570_f256) -> epm570;
density(epm1270_t144) -> epm1270;
density(epm1270_m256) -> epm1270;
density(epm1270_f256) -> epm1270;
density(epm2210_f256) -> epm2210;
density(epm2210_f324) -> epm2210.

%%====================================================================
%% gclk_pins
%%====================================================================

-spec gclk_pins(device()) -> [pin()].

gclk_pins(Device) ->
    package:gclk_pins(package(Device)).

%%====================================================================
%% labs
%%====================================================================

-spec labs(device()) -> [lab()].

labs(Device) ->
    density:labs(density(Device)).

%%====================================================================
%% package
%%====================================================================

-spec package(device()) -> package().

package(epm240_m100) -> m100;
package(epm240_f100) -> f100;
package(epm240_t100) -> t100;
package(epm570_m100) -> m100;
package(epm570_f100) -> f100;
package(epm570_t100) -> t100;
package(epm570_t144) -> t144;
package(epm570_m256) -> m256;
package(epm570_f256) -> f256;
package(epm1270_t144) -> t144;
package(epm1270_m256) -> m256;
package(epm1270_f256) -> f256;
package(epm2210_f256) -> f256;
package(epm2210_f324) -> f324.

%%====================================================================
%% pins
%%====================================================================

-spec pins(device()) -> [{pin(), ioc()}].

pins(epm240_m100) -> epm240_m100:pins();
pins(epm240_f100) -> epm240_f100:pins();
pins(epm240_t100) -> epm240_t100:pins();
pins(epm570_m100) -> epm570_m100:pins();
pins(epm570_f100) -> epm570_f100:pins();
pins(epm570_t100) -> epm570_t100:pins();
pins(epm570_t144) -> epm570_t144:pins();
pins(epm570_m256) -> epm570_m256:pins();
pins(epm570_f256) -> epm570_f256:pins();
pins(epm1270_t144) -> epm1270_t144:pins();
pins(epm1270_m256) -> epm1270_m256:pins();
pins(epm1270_f256) -> epm1270_f256:pins();
pins(epm2210_f256) -> epm2210_f256:pins();
pins(epm2210_f324) -> epm2210_f324:pins().

