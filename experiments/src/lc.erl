-module(lc).

-export([name/1]).
-export([parse/1]).
-export([lab/1]).

-export_type([lc/0]).

-type lab() :: lab:lab().
-type lc() :: {lc, non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% name
%%====================================================================

-spec name(lc()) -> binary().

name({lc, X, Y, N}) when X < 10 andalso Y < 10 ->
    <<"LC_X", (X + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) when X < 10 ->
    <<"LC_X", (X + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) when Y < 10 ->
    <<"LC_X1", ((X rem 10) + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) ->
    <<"LC_X1", ((X rem 10) + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> lc().

parse(<<"LC_X", X, "_Y", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {lc, X - $0, Y - $0, N};
parse(<<"LC_X", X, "_Y1", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {lc, X - $0, 10 + Y - $0, N};
parse(<<"LC_X1", X, "_Y", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {lc, 10 + X - $0, Y - $0, N};
parse(<<"LC_X1", X, "_Y1", Y, "_N", N>>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso ?IS_DIGIT(N) ->
    {lc, 10 + X - $0, 10 + Y - $0, N}.

%%====================================================================
%% lab
%%====================================================================

-spec lab(lc()) -> lab().

lab({lc, X, Y, _}) ->
    {lab, X, Y}.

