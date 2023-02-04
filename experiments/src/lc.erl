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

parse(<<"LC_X", X, "_Y", Y, "_N", N>>) ->
    {lc, number(X), number(Y), number(N)};
parse(<<"LC_X", X, "_Y", Y10, Y1, "_N", N>>) ->
    {lc, number(X), number(Y10, Y1), number(N)};
parse(<<"LC_X", X10, X1, "_Y", Y, "_N", N>>) ->
    {lc, number(X10, X1), number(Y), number(N)};
parse(<<"LC_X", X10, X1, "_Y", Y10, Y1, "_N", N>>) ->
    {lc, number(X10, X1), number(Y10, Y1), number(N)}.

%%--------------------------------------------------------------------

number(D1) when ?IS_DIGIT(D1) ->
    (D1 - $0).

%%--------------------------------------------------------------------

number(D10, D1) when ?IS_DIGIT(D10) andalso ?IS_DIGIT(D1) ->
    (10 * (D10 - $0)) + (D1 - $0).

%%====================================================================
%% lab
%%====================================================================

-spec lab(lc()) -> lab().

lab({lc, X, Y, _}) ->
    {lab, X, Y}.

