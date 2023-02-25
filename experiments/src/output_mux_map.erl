-module(output_mux_map).

-export([to_interconnect4/2]).
-export([from_interconnect4/1]).

-export([to_interconnect7/2]).
-export([from_interconnect7/1]).

-export([mux6s/0]).
-export([mux4s/0]).
-export([mux3s/0]).

-export_type([mux6/0]).
-export_type([mux4/0]).
-export_type([mux3/0]).

-type interconnect4() :: iob:interconnect4().
-type interconnect7() :: iob:interconnect7().

-type mux6() :: mux0 | mux1 | mux2 | mux3 | mux4 | mux5.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.

%%====================================================================
%% table4
%%====================================================================

-define(MAPPINGS4(),
    ?UNDEFINED   (mux0, mux0)
    ?INTERCONNECT(mux0, mux1, 0);
    ?INTERCONNECT(mux0, mux2, 1);
    ?INTERCONNECT(mux1, mux0, 2);
    ?INTERCONNECT(mux1, mux1, 3);
    ?INTERCONNECT(mux1, mux2, 4);
    ?BYPASS      (mux2, mux0);
    ?INTERCONNECT(mux2, mux1, 5);
    ?INTERCONNECT(mux2, mux2, 6);
    ?INTERCONNECT(mux3, mux0, 7);
    ?INTERCONNECT(mux3, mux1, 8);
    ?INTERCONNECT(mux3, mux2, 9)
).

%%====================================================================
%% to_interconnect4
%%====================================================================

-spec to_interconnect4(mux4(), mux3()) -> interconnect4() | undefined.

-define(UNDEFINED(Mux4, Mux3),
    to_interconnect4(Mux4, Mux3) ->
        undefined;
).
-define(INTERCONNECT(Mux4, Mux3, N),
    to_interconnect4(Mux4, Mux3) ->
        {interconnect, N}
).
-define(BYPASS(Mux4, Mux3),
    to_interconnect4(Mux4, Mux3) ->
        bypass
).

?MAPPINGS4().

-undef(UNDEFINED).
-undef(INTERCONNECT).
-undef(BYPASS).

%%====================================================================
%% from_interconnect4
%%====================================================================

-spec from_interconnect4(interconnect4()) -> {ok, mux6(), mux3()}.

-define(UNDEFINED(Mux4, Mux3), ).
-define(INTERCONNECT(Mux4, Mux3, N),
    from_interconnect4({interconnect, N}) ->
        {ok, Mux4, Mux3}
).
-define(BYPASS(Mux4, Mux3),
    from_interconnect4(bypass) ->
        {ok, Mux4, Mux3}
).

?MAPPINGS4().

-undef(UNDEFINED).
-undef(INTERCONNECT).
-undef(BYPASS).

%%====================================================================
%% table7
%%====================================================================

-define(MAPPINGS7(),
    ?INTERCONNECT(mux0, mux0, 0);
    ?INTERCONNECT(mux0, mux1, 1);
    ?INTERCONNECT(mux0, mux2, 2);
    ?INTERCONNECT(mux1, mux0, 3);
    ?INTERCONNECT(mux1, mux1, 4);
    ?INTERCONNECT(mux1, mux2, 5);
    ?INTERCONNECT(mux2, mux0, 6);
    ?INTERCONNECT(mux2, mux1, 7);
    ?INTERCONNECT(mux2, mux2, 8);
    ?INTERCONNECT(mux3, mux0, 9);
    ?INTERCONNECT(mux3, mux1, 10);
    ?INTERCONNECT(mux3, mux2, 11);
    ?INTERCONNECT(mux4, mux0, 12);
    ?INTERCONNECT(mux4, mux1, 13);
    ?INTERCONNECT(mux4, mux2, 14);
    ?INTERCONNECT(mux5, mux0, 15);
    ?INTERCONNECT(mux5, mux1, 16);
    ?INTERCONNECT(mux5, mux2, 17)
).

%%====================================================================
%% to_interconnect7
%%====================================================================

-spec to_interconnect7(mux6(), mux3()) -> interconnect7().

-define(INTERCONNECT(Mux6, Mux3, N),
    to_interconnect7(Mux6, Mux3) ->
        {interconnect, N}
).

?MAPPINGS7().

-undef(INTERCONNECT).

%%====================================================================
%% from_interconnect7
%%====================================================================

-spec from_interconnect7(interconnect7()) -> {ok, mux6(), mux3()}.

-define(INTERCONNECT(Mux6, Mux3, N),
    from_interconnect7({interconnect, N}) ->
        {ok, Mux6, Mux3}
).

?MAPPINGS7().

-undef(INTERCONNECT).

%%====================================================================
%% mux6s
%%====================================================================

-spec mux6s() -> [mux6()].

mux6s() ->
    [mux0, mux1, mux2, mux3, mux4, mux5].

%%====================================================================
%% mux4s
%%====================================================================

-spec mux4s() -> [mux4()].

mux4s() ->
    [mux0, mux1, mux2, mux3].

%%====================================================================
%% mux3s
%%====================================================================

-spec mux3s() -> [mux3()].

mux3s() ->
    [mux0, mux1, mux2].

%%====================================================================
%% tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

coverage4_test() ->
    Interconnects = lists:usort([
        to_interconnect4(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(1 + 10 + 1, length(Interconnects)).

%%--------------------------------------------------------------------

inverse4_test() ->
    [
        inverse4_test(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

inverse4_test(Mux4, Mux3) ->
    case to_interconnect4(Mux4, Mux3) of
        undefined ->
            ok;

        Interconnect ->
            ?assertEqual({ok, Mux4, Mux3}, from_interconnect4(Interconnect))
    end.

%%--------------------------------------------------------------------

coverage7_test() ->
    Interconnects = lists:usort([
        to_interconnect7(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(18, length(Interconnects)).

%%--------------------------------------------------------------------

inverse7_test() ->
    [
        inverse7_test(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

inverse7_test(Mux6, Mux3) ->
    Interconnect = to_interconnect7(Mux6, Mux3),
    ?assertEqual({ok, Mux6, Mux3}, from_interconnect7(Interconnect)).

-endif.

