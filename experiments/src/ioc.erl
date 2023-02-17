-module(ioc).

-export([name/1]).
-export([parse/1]).
-export([write/2]).
-export([in_iob/2]).

-export_type([ioc/0]).

-type iob() :: iob:iob().

-type ioc() :: {ioc, non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% name
%%====================================================================

-spec name(ioc()) -> binary().

name(IOC) ->
    write(<<>>, IOC).

%%====================================================================
%% from
%%====================================================================

-spec from(0..21, 0..14, 0..6) -> ioc().

from(X, Y, N)
        when (X >= 0 andalso X =< 21) andalso
             (Y >= 0 andalso Y =< 14) andalso
             (N >= 0 andalso N =< 6) ->
    {ioc, X, Y, N}.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> ioc().

parse(Name) ->
    {ok, [X, Y, N], Rest} = parse:format(Name, [
        <<"IOC_X">>,
        number,
        <<"_Y">>,
        number,
        <<"_N">>,
        number
    ]),
    {ok, from(X, Y, N), Rest}.

%%====================================================================
%% write
%%====================================================================

-spec write(binary(), ioc()) -> binary().

write(To, {ioc, X, Y, N}) ->
    write:format(To, [
        <<"IOC_X">>,
        X,
        <<"_Y">>,
        Y,
        <<"_N">>,
        N
    ]).

%%====================================================================
%% in_iob
%%====================================================================

-spec in_iob(ioc(), iob()) -> boolean().

in_iob({ioc, X, Y, _}, {iob, X, Y}) ->
    true;
in_iob({ioc, _, _, _}, {iob, _, _}) ->
    false.

