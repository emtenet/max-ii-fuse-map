-module(device).

-export([list/0]).
-export([name/1]).
-export([from_name/1]).
-export([density/1]).
-export([package/1]).
-export([gclk_pins/1]).
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
-export([pins/1]).
-export([iocs/1]).
-export([top_pins/2]).
-export([top_iocs/2]).
-export([left_pins/2]).
-export([left_iocs/2]).
-export([right_pins/2]).
-export([right_iocs/2]).
-export([bottom_pins/2]).
-export([bottom_iocs/2]).

-export_type([device/0]).

-include("max_ii.hrl").

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

-type iob() :: iob:iob().
-type ioc() :: ioc:ioc().
-type lab() :: lab:lab().
-type package() :: package:package().
-type pin() :: pin:pin().
-type x() :: max_ii:x().
-type y() :: max_ii:y().

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

-spec from_name(binary()) -> device().

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
%% iobs
%%====================================================================

-spec iobs(device()) -> [{iob(), lab()}].

iobs(Device) ->
    density:iobs(density(Device)).

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
%% metric
%%====================================================================

-spec metric(device()) -> #metric{}.

metric(Device) ->
    density:metric(density(Device)).

%%--------------------------------------------------------------------

-spec top_io(device()) -> y().

top_io(Device) ->
    density:top_io(density(Device)).

%%--------------------------------------------------------------------

-spec top_lab(device()) -> y().

top_lab(Device) ->
    density:top_lab(density(Device)).

%%--------------------------------------------------------------------

-spec left_io(y(), device()) -> x().

left_io(Y, Device) ->
    density:left_io(Y, density(Device)).

%%--------------------------------------------------------------------

-spec left_lab(y(), device()) -> x().

left_lab(Y, Device) ->
    density:left_lab(Y, density(Device)).

%%--------------------------------------------------------------------

-spec right_io(device()) -> x().

right_io(Device) ->
    density:right_io(density(Device)).

%%--------------------------------------------------------------------

-spec right_lab(device()) -> x().

right_lab(Device) ->
    density:right_lab(density(Device)).

%%--------------------------------------------------------------------

-spec bottom_io(x(), device()) -> y().

bottom_io(X, Device) ->
    density:bottom_io(X, density(Device)).

%%--------------------------------------------------------------------

-spec bottom_lab(x(), device()) -> y().

bottom_lab(X, Device) ->
    density:bottom_lab(X, density(Device)).

%%====================================================================
%% iocs
%%====================================================================

-spec iocs(device()) -> [{pin(), ioc()}].

iocs(epm240_m100) -> epm240_m100:iocs();
iocs(epm240_f100) -> epm240_f100:iocs();
iocs(epm240_t100) -> epm240_t100:iocs();
iocs(epm570_m100) -> epm570_m100:iocs();
iocs(epm570_f100) -> epm570_f100:iocs();
iocs(epm570_t100) -> epm570_t100:iocs();
iocs(epm570_t144) -> epm570_t144:iocs();
iocs(epm570_m256) -> epm570_m256:iocs();
iocs(epm570_f256) -> epm570_f256:iocs();
iocs(epm1270_t144) -> epm1270_t144:iocs();
iocs(epm1270_m256) -> epm1270_m256:iocs();
iocs(epm1270_f256) -> epm1270_f256:iocs();
iocs(epm2210_f256) -> epm2210_f256:iocs();
iocs(epm2210_f324) -> epm2210_f324:iocs().

%%====================================================================
%% pins
%%====================================================================

-spec pins(device()) -> [pin()].

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

%%====================================================================
%% top_iocs
%%====================================================================

-spec top_iocs(x(), device()) -> [{pin(), ioc()}].

top_iocs(X, epm240_m100) -> epm240_m100:top_iocs(X);
top_iocs(X, epm240_f100) -> epm240_f100:top_iocs(X);
top_iocs(X, epm240_t100) -> epm240_t100:top_iocs(X);
top_iocs(X, epm570_m100) -> epm570_m100:top_iocs(X);
top_iocs(X, epm570_f100) -> epm570_f100:top_iocs(X);
top_iocs(X, epm570_t100) -> epm570_t100:top_iocs(X);
top_iocs(X, epm570_t144) -> epm570_t144:top_iocs(X);
top_iocs(X, epm570_m256) -> epm570_m256:top_iocs(X);
top_iocs(X, epm570_f256) -> epm570_f256:top_iocs(X);
top_iocs(X, epm1270_t144) -> epm1270_t144:top_iocs(X);
top_iocs(X, epm1270_m256) -> epm1270_m256:top_iocs(X);
top_iocs(X, epm1270_f256) -> epm1270_f256:top_iocs(X);
top_iocs(X, epm2210_f256) -> epm2210_f256:top_iocs(X);
top_iocs(X, epm2210_f324) -> epm2210_f324:top_iocs(X).

%%====================================================================
%% top_pins
%%====================================================================

-spec top_pins(x(), device()) -> [pin()].

top_pins(X, epm240_m100) -> epm240_m100:top_pins(X);
top_pins(X, epm240_f100) -> epm240_f100:top_pins(X);
top_pins(X, epm240_t100) -> epm240_t100:top_pins(X);
top_pins(X, epm570_m100) -> epm570_m100:top_pins(X);
top_pins(X, epm570_f100) -> epm570_f100:top_pins(X);
top_pins(X, epm570_t100) -> epm570_t100:top_pins(X);
top_pins(X, epm570_t144) -> epm570_t144:top_pins(X);
top_pins(X, epm570_m256) -> epm570_m256:top_pins(X);
top_pins(X, epm570_f256) -> epm570_f256:top_pins(X);
top_pins(X, epm1270_t144) -> epm1270_t144:top_pins(X);
top_pins(X, epm1270_m256) -> epm1270_m256:top_pins(X);
top_pins(X, epm1270_f256) -> epm1270_f256:top_pins(X);
top_pins(X, epm2210_f256) -> epm2210_f256:top_pins(X);
top_pins(X, epm2210_f324) -> epm2210_f324:top_pins(X).

%%====================================================================
%% left_iocs
%%====================================================================

-spec left_iocs(y(), device()) -> [{pin(), ioc()}].

left_iocs(Y, epm240_m100) -> epm240_m100:left_iocs(Y);
left_iocs(Y, epm240_f100) -> epm240_f100:left_iocs(Y);
left_iocs(Y, epm240_t100) -> epm240_t100:left_iocs(Y);
left_iocs(Y, epm570_m100) -> epm570_m100:left_iocs(Y);
left_iocs(Y, epm570_f100) -> epm570_f100:left_iocs(Y);
left_iocs(Y, epm570_t100) -> epm570_t100:left_iocs(Y);
left_iocs(Y, epm570_t144) -> epm570_t144:left_iocs(Y);
left_iocs(Y, epm570_m256) -> epm570_m256:left_iocs(Y);
left_iocs(Y, epm570_f256) -> epm570_f256:left_iocs(Y);
left_iocs(Y, epm1270_t144) -> epm1270_t144:left_iocs(Y);
left_iocs(Y, epm1270_m256) -> epm1270_m256:left_iocs(Y);
left_iocs(Y, epm1270_f256) -> epm1270_f256:left_iocs(Y);
left_iocs(Y, epm2210_f256) -> epm2210_f256:left_iocs(Y);
left_iocs(Y, epm2210_f324) -> epm2210_f324:left_iocs(Y).

%%====================================================================
%% left_pins
%%====================================================================

-spec left_pins(y(), device()) -> [pin()].

left_pins(Y, epm240_m100) -> epm240_m100:left_pins(Y);
left_pins(Y, epm240_f100) -> epm240_f100:left_pins(Y);
left_pins(Y, epm240_t100) -> epm240_t100:left_pins(Y);
left_pins(Y, epm570_m100) -> epm570_m100:left_pins(Y);
left_pins(Y, epm570_f100) -> epm570_f100:left_pins(Y);
left_pins(Y, epm570_t100) -> epm570_t100:left_pins(Y);
left_pins(Y, epm570_t144) -> epm570_t144:left_pins(Y);
left_pins(Y, epm570_m256) -> epm570_m256:left_pins(Y);
left_pins(Y, epm570_f256) -> epm570_f256:left_pins(Y);
left_pins(Y, epm1270_t144) -> epm1270_t144:left_pins(Y);
left_pins(Y, epm1270_m256) -> epm1270_m256:left_pins(Y);
left_pins(Y, epm1270_f256) -> epm1270_f256:left_pins(Y);
left_pins(Y, epm2210_f256) -> epm2210_f256:left_pins(Y);
left_pins(Y, epm2210_f324) -> epm2210_f324:left_pins(Y).

%%====================================================================
%% right_iocs
%%====================================================================

-spec right_iocs(y(), device()) -> [{pin(), ioc()}].

right_iocs(Y, epm240_m100) -> epm240_m100:right_iocs(Y);
right_iocs(Y, epm240_f100) -> epm240_f100:right_iocs(Y);
right_iocs(Y, epm240_t100) -> epm240_t100:right_iocs(Y);
right_iocs(Y, epm570_m100) -> epm570_m100:right_iocs(Y);
right_iocs(Y, epm570_f100) -> epm570_f100:right_iocs(Y);
right_iocs(Y, epm570_t100) -> epm570_t100:right_iocs(Y);
right_iocs(Y, epm570_t144) -> epm570_t144:right_iocs(Y);
right_iocs(Y, epm570_m256) -> epm570_m256:right_iocs(Y);
right_iocs(Y, epm570_f256) -> epm570_f256:right_iocs(Y);
right_iocs(Y, epm1270_t144) -> epm1270_t144:right_iocs(Y);
right_iocs(Y, epm1270_m256) -> epm1270_m256:right_iocs(Y);
right_iocs(Y, epm1270_f256) -> epm1270_f256:right_iocs(Y);
right_iocs(Y, epm2210_f256) -> epm2210_f256:right_iocs(Y);
right_iocs(Y, epm2210_f324) -> epm2210_f324:right_iocs(Y).

%%====================================================================
%% right_pins
%%====================================================================

-spec right_pins(y(), device()) -> [pin()].

right_pins(Y, epm240_m100) -> epm240_m100:right_pins(Y);
right_pins(Y, epm240_f100) -> epm240_f100:right_pins(Y);
right_pins(Y, epm240_t100) -> epm240_t100:right_pins(Y);
right_pins(Y, epm570_m100) -> epm570_m100:right_pins(Y);
right_pins(Y, epm570_f100) -> epm570_f100:right_pins(Y);
right_pins(Y, epm570_t100) -> epm570_t100:right_pins(Y);
right_pins(Y, epm570_t144) -> epm570_t144:right_pins(Y);
right_pins(Y, epm570_m256) -> epm570_m256:right_pins(Y);
right_pins(Y, epm570_f256) -> epm570_f256:right_pins(Y);
right_pins(Y, epm1270_t144) -> epm1270_t144:right_pins(Y);
right_pins(Y, epm1270_m256) -> epm1270_m256:right_pins(Y);
right_pins(Y, epm1270_f256) -> epm1270_f256:right_pins(Y);
right_pins(Y, epm2210_f256) -> epm2210_f256:right_pins(Y);
right_pins(Y, epm2210_f324) -> epm2210_f324:right_pins(Y).

%%====================================================================
%% bottom_iocs
%%====================================================================

-spec bottom_iocs(x(), device()) -> [{pin(), ioc()}].

bottom_iocs(X, epm240_m100) -> epm240_m100:bottom_iocs(X);
bottom_iocs(X, epm240_f100) -> epm240_f100:bottom_iocs(X);
bottom_iocs(X, epm240_t100) -> epm240_t100:bottom_iocs(X);
bottom_iocs(X, epm570_m100) -> epm570_m100:bottom_iocs(X);
bottom_iocs(X, epm570_f100) -> epm570_f100:bottom_iocs(X);
bottom_iocs(X, epm570_t100) -> epm570_t100:bottom_iocs(X);
bottom_iocs(X, epm570_t144) -> epm570_t144:bottom_iocs(X);
bottom_iocs(X, epm570_m256) -> epm570_m256:bottom_iocs(X);
bottom_iocs(X, epm570_f256) -> epm570_f256:bottom_iocs(X);
bottom_iocs(X, epm1270_t144) -> epm1270_t144:bottom_iocs(X);
bottom_iocs(X, epm1270_m256) -> epm1270_m256:bottom_iocs(X);
bottom_iocs(X, epm1270_f256) -> epm1270_f256:bottom_iocs(X);
bottom_iocs(X, epm2210_f256) -> epm2210_f256:bottom_iocs(X);
bottom_iocs(X, epm2210_f324) -> epm2210_f324:bottom_iocs(X).

%%====================================================================
%% bottom_pins
%%====================================================================

-spec bottom_pins(x(), device()) -> [pin()].

bottom_pins(X, epm240_m100) -> epm240_m100:bottom_pins(X);
bottom_pins(X, epm240_f100) -> epm240_f100:bottom_pins(X);
bottom_pins(X, epm240_t100) -> epm240_t100:bottom_pins(X);
bottom_pins(X, epm570_m100) -> epm570_m100:bottom_pins(X);
bottom_pins(X, epm570_f100) -> epm570_f100:bottom_pins(X);
bottom_pins(X, epm570_t100) -> epm570_t100:bottom_pins(X);
bottom_pins(X, epm570_t144) -> epm570_t144:bottom_pins(X);
bottom_pins(X, epm570_m256) -> epm570_m256:bottom_pins(X);
bottom_pins(X, epm570_f256) -> epm570_f256:bottom_pins(X);
bottom_pins(X, epm1270_t144) -> epm1270_t144:bottom_pins(X);
bottom_pins(X, epm1270_m256) -> epm1270_m256:bottom_pins(X);
bottom_pins(X, epm1270_f256) -> epm1270_f256:bottom_pins(X);
bottom_pins(X, epm2210_f256) -> epm2210_f256:bottom_pins(X);
bottom_pins(X, epm2210_f324) -> epm2210_f324:bottom_pins(X).

