-module(pins).

-export([choose/2]).
-export([choose/3]).

-type pin() :: pin:pin().

-type from() :: pin | {pin(), any()}.

%%====================================================================
%% choose
%%====================================================================

-spec choose([from()], [pin()]) -> {pin(), [from()]}.

choose([{Pin, _} | Pins], Except) ->
    case lists:member(Pin, Except) of
        true ->
            choose(Pins, Except);

        false ->
            {Pin, Pins}
    end;
choose([Pin | Pins], Except) ->
    case lists:member(Pin, Except) of
        true ->
            choose(Pins, Except);

        false ->
            {Pin, Pins}
    end.

%%====================================================================
%% choose
%%====================================================================

-spec choose(non_neg_integer(), [from()], [pin()]) -> {[pin()], [from()]}.

choose(N, Pins, Except) ->
    choose(N, Pins, Except, []).

%%--------------------------------------------------------------------

choose(0, Pins, _, Acc) ->
    {lists:reverse(Acc), Pins};
choose(N, [{Pin, _} | Pins], Except, Acc) ->
    case lists:member(Pin, Except) of
        true ->
            choose(N, Pins, Except, Acc);

        false ->
            choose(N - 1, Pins, Except, [Pin | Acc])
    end;
choose(N, [Pin | Pins], Except, Acc) ->
    case lists:member(Pin, Except) of
        true ->
            choose(N, Pins, Except, Acc);

        false ->
            choose(N - 1, Pins, Except, [Pin | Acc])
    end.

