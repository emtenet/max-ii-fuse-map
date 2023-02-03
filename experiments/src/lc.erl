-module(lc).

-export([name/1]).
-export([lab/1]).

-export_type([lc/0]).

-type lab() :: lab:lab().
-type lc() :: {lc, non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%%====================================================================
%% name
%%====================================================================

-spec name(lc()) -> binary().

name({lc, X, Y, N}) when X < 10 andalso Y < 10 ->
    <<"LAB_X", (X + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) when X < 10 ->
    <<"LAB_X", (X + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) when Y < 10 ->
    <<"LAB_X1", ((X rem 10) + $0), "_Y", (Y + $0), "_N", (N + $0)>>;
name({lc, X, Y, N}) ->
    <<"LAB_X1", ((X rem 10) + $0), "_Y1", ((Y rem 10) + $0), "_N", (N + $0)>>.

%%====================================================================
%% lab
%%====================================================================

-spec lab(lc()) -> lab().

lab({lc, X, Y, _}) ->
    {lab, X, Y}.

