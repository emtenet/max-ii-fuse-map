-module(ioc).

-export([name/1]).
-export([parse/1]).

-export_type([ioc/0]).

-type ioc() :: {ioc, non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% name
%%====================================================================

-spec name(ioc()) -> binary().

name({ioc, X, Y, N}) when X < 10 andalso Y < 10 ->
    <<"IOC_X", (X + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({ioc, X, Y, N}) when X < 10 ->
    <<"IOC_X", (X + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>;
name({ioc, X, Y, N}) when Y < 10 ->
    <<"IOC_X1", ((X rem 10) + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({ioc, X, Y, N}) ->
    <<"IOC_X1", ((X rem 10) + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> ioc().

parse(<<"IOC_X", X, "_Y", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {ioc, X - $0, Y - $0, N};
parse(<<"IOC_X", X, "_Y1", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {ioc, X - $0, 10 + Y - $0, N};
parse(<<"IOC_X1", X, "_Y", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {ioc, 10 + X - $0, Y - $0, N};
parse(<<"IOC_X1", X, "_Y1", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {ioc, 10 + X - $0, 10 + Y - $0, N}.

