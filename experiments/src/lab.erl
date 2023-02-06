-module(lab).

-export([name/1]).
-export([lcs/1]).
-export([lc/2]).

-export_type([lab/0]).

-type lab() :: {lab, non_neg_integer(), non_neg_integer()}.
-type lc() :: lc:lc().

%%====================================================================
%% name
%%====================================================================

-spec name(lab()) -> binary().

name({lab, X, Y}) when X < 10 andalso Y < 10 ->
    <<"LAB_X", (X + $0), "_Y", (Y + $0)>>;
name({lab, X, Y}) when X < 10 ->
    <<"LAB_X", (X + $0), "_Y1", ((Y rem 10) + $0)>>;
name({lab, X, Y}) when X < 20 andalso Y < 10 ->
    <<"LAB_X1", ((X rem 10) + $0), "_Y", (Y + $0)>>;
name({lab, X, Y}) when Y < 10 ->
    <<"LAB_X2", ((X rem 10) + $0), "_Y", (Y + $0)>>;
name({lab, X, Y}) when X < 20 ->
    <<"LAB_X1", ((X rem 10) + $0), "_Y1", ((Y rem 10) + $0)>>;
name({lab, X, Y}) ->
    <<"LAB_X2", ((X rem 10) + $0), "_Y1", ((Y rem 10) + $0)>>.

%%====================================================================
%% lcs
%%====================================================================

-spec lcs(lab()) -> [lc()].

lcs({lab, X, Y}) ->
    [{lc, X, Y, 0},
     {lc, X, Y, 1},
     {lc, X, Y, 2},
     {lc, X, Y, 3},
     {lc, X, Y, 4},
     {lc, X, Y, 5},
     {lc, X, Y, 6},
     {lc, X, Y, 7},
     {lc, X, Y, 8},
     {lc, X, Y, 9}
    ].

%%====================================================================
%% lc
%%====================================================================

-spec lc(lab(), 0..9) -> lc().

lc({lab, X, Y}, N) when N >= 0 andalso N < 10 ->
    {lc, X, Y, N}.

