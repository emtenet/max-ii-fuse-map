-module(data_mux_map).

-export([to_interconnect/3]).
-export([from_interconnect/2]).

-export([mux6s/0]).
-export([mux3s/0]).

-export_type([mux6/0]).
-export_type([mux3/0]).

-type input() :: lc:input().
-type interconnect() :: lab:interconnect_or_local_line().

-type mux6() :: mux0 | mux1 | mux2 | mux3 | mux4 | mux5.
-type mux3() :: mux0 | mux1 | mux2.

%%====================================================================
%% table
%%====================================================================

-define(MAPPINGS(),
    ?MAPPING(data_a, mux0, mux0, interconnect, 0);
    ?MAPPING(data_a, mux0, mux1, interconnect, 3);
    ?MAPPING(data_a, mux0, mux2, interconnect, 8);
    ?MAPPING(data_a, mux1, mux0, interconnect, 9);
    ?MAPPING(data_a, mux1, mux1, interconnect, 11);
    ?MAPPING(data_a, mux1, mux2, interconnect, 14);
    ?MAPPING(data_a, mux2, mux0, interconnect, 18);
    ?MAPPING(data_a, mux2, mux1, interconnect, 22);
    ?MAPPING(data_a, mux2, mux2, interconnect, 25);
    ?MAPPING(data_a, mux3, mux0, local_line, 4);
    ?MAPPING(data_a, mux3, mux1, local_line, 5);
    ?MAPPING(data_a, mux3, mux2, local_line, 6);
    ?MAPPING(data_a, mux4, mux0, interconnect, 1);
    ?MAPPING(data_a, mux4, mux1, interconnect, 6);
    ?MAPPING(data_a, mux4, mux2, interconnect, 15);
    ?MAPPING(data_a, mux5, mux0, interconnect, 19);
    ?MAPPING(data_a, mux5, mux1, local_line, 3);
    ?MAPPING(data_a, mux5, mux2, local_line, 8);
    ?MAPPING(data_b, mux0, mux0, interconnect, 17);
    ?MAPPING(data_b, mux0, mux1, interconnect, 7);
    ?MAPPING(data_b, mux0, mux2, interconnect, 2);
    ?MAPPING(data_b, mux1, mux0, local_line, 7);
    ?MAPPING(data_b, mux1, mux1, local_line, 0);
    ?MAPPING(data_b, mux1, mux2, interconnect, 21);
    ?MAPPING(data_b, mux2, mux0, interconnect, 10);
    ?MAPPING(data_b, mux2, mux1, interconnect, 5);
    ?MAPPING(data_b, mux2, mux2, interconnect, 4);
    ?MAPPING(data_b, mux3, mux0, interconnect, 16);
    ?MAPPING(data_b, mux3, mux1, interconnect, 13);
    ?MAPPING(data_b, mux3, mux2, interconnect, 12);
    ?MAPPING(data_b, mux4, mux0, interconnect, 24);
    ?MAPPING(data_b, mux4, mux1, interconnect, 23);
    ?MAPPING(data_b, mux4, mux2, interconnect, 20);
    ?MAPPING(data_b, mux5, mux0, local_line, 9);
    ?MAPPING(data_b, mux5, mux1, local_line, 2);
    ?MAPPING(data_b, mux5, mux2, local_line, 1);
    ?MAPPING(data_c, mux0, mux0, interconnect, 0);
    ?MAPPING(data_c, mux0, mux1, interconnect, 3);
    ?MAPPING(data_c, mux0, mux2, interconnect, 8);
    ?MAPPING(data_c, mux1, mux0, interconnect, 9);
    ?MAPPING(data_c, mux1, mux1, interconnect, 11);
    ?MAPPING(data_c, mux1, mux2, interconnect, 14);
    ?MAPPING(data_c, mux2, mux0, interconnect, 18);
    ?MAPPING(data_c, mux2, mux1, interconnect, 22);
    ?MAPPING(data_c, mux2, mux2, interconnect, 25);
    ?MAPPING(data_c, mux3, mux0, local_line, 4);
    ?MAPPING(data_c, mux3, mux1, local_line, 5);
    ?MAPPING(data_c, mux3, mux2, local_line, 6);
    ?MAPPING(data_c, mux4, mux0, interconnect, 2);
    ?MAPPING(data_c, mux4, mux1, interconnect, 7);
    ?MAPPING(data_c, mux4, mux2, interconnect, 17);
    ?MAPPING(data_c, mux5, mux0, interconnect, 21);
    ?MAPPING(data_c, mux5, mux1, local_line, 0);
    ?MAPPING(data_c, mux5, mux2, local_line, 7);
    ?MAPPING(data_d, mux0, mux0, interconnect, 10);
    ?MAPPING(data_d, mux0, mux1, interconnect, 5);
    ?MAPPING(data_d, mux0, mux2, interconnect, 4);
    ?MAPPING(data_d, mux1, mux0, interconnect, 16);
    ?MAPPING(data_d, mux1, mux1, interconnect, 13);
    ?MAPPING(data_d, mux1, mux2, interconnect, 12);
    ?MAPPING(data_d, mux2, mux0, interconnect, 24);
    ?MAPPING(data_d, mux2, mux1, interconnect, 23);
    ?MAPPING(data_d, mux2, mux2, interconnect, 20);
    ?MAPPING(data_d, mux3, mux0, local_line, 9);
    ?MAPPING(data_d, mux3, mux1, local_line, 2);
    ?MAPPING(data_d, mux3, mux2, local_line, 1);
    ?MAPPING(data_d, mux4, mux0, interconnect, 15);
    ?MAPPING(data_d, mux4, mux1, interconnect, 6);
    ?MAPPING(data_d, mux4, mux2, interconnect, 1);
    ?MAPPING(data_d, mux5, mux0, local_line, 8);
    ?MAPPING(data_d, mux5, mux1, local_line, 3);
    ?MAPPING(data_d, mux5, mux2, interconnect, 19)
).

%%====================================================================
%% to_interconnect
%%====================================================================

-spec to_interconnect(input(), mux6(), mux3())
    -> interconnect().

-define(MAPPING(Input, Mux6, Mux3, Interconnect, N),
    to_interconnect(Input, Mux6, Mux3) ->
        {Interconnect, N}
).

?MAPPINGS().

-undef(MAPPING).

%%====================================================================
%% from_interconnect
%%====================================================================

-spec from_interconnect(input(), interconnect())
    -> {ok, mux6(), mux3()} | false.

-define(MAPPING(Input, Mux6, Mux3, Interconnect, N),
    from_interconnect(Input, {Interconnect, N}) ->
        {ok, Mux6, Mux3}
).

?MAPPINGS();
from_interconnect(_, _) ->
    false.

-undef(MAPPING).

%%====================================================================
%% mux6s
%%====================================================================

-spec mux6s() -> [mux6()].

mux6s() ->
    [mux0, mux1, mux2, mux3, mux4, mux5].

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

coverage_test() ->
    Interconnects = lists:usort([
        to_interconnect(Input, Mux6, Mux3)
        ||
        Input <- lc:inputs(),
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(10 + 26, length(Interconnects)).

%%--------------------------------------------------------------------

inverse_test() ->
    [
        inverse_test(Input, Mux6, Mux3)
        ||
        Input <- lc:inputs(),
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

inverse_test(Input, Mux6, Mux3) ->
    Interconnect = to_interconnect(Input, Mux6, Mux3),
    ?assertEqual({ok, Mux6, Mux3}, from_interconnect(Input, Interconnect)).

%%--------------------------------------------------------------------

pairs_test() ->
    [
        pairs_test({local_line, N})
        ||
        N <- lists:seq(0, 9)
    ],
    [
        pairs_test({interconnect, N})
        ||
        N <- lists:seq(0, 25)
    ],
    ok.

%%--------------------------------------------------------------------

pairs_test(Interconnect) ->
    Pairs = lists:sort([
        from_interconnect(data_a, Interconnect),
        from_interconnect(data_b, Interconnect),
        from_interconnect(data_c, Interconnect),
        from_interconnect(data_d, Interconnect)
    ]),
    ?assertMatch([false, false, {ok, _, _}, {ok, _, _}], Pairs).

-endif.

