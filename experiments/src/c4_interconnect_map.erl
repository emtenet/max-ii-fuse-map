-module(c4_interconnect_map).

-export([run/0]).

-export([from_mux/3]).
-export([to_mux/2]).

-export_type([block/0]).
-export_type([mux/0]).

-type density() :: density:density().
-type c4() :: max_ii:c4().

-type block() :: {c4, max_ii:x(), max_ii:y()}.
-type mux() :: {mux, 0..13}.

-record(with, {
    left :: non_neg_integer(),
    right :: non_neg_integer(),
    top :: non_neg_integer(),
    bottom :: non_neg_integer(),
    indent_top :: non_neg_integer(),
    indent_right :: non_neg_integer(),
    offset_x :: non_neg_integer(),
    offset_y :: non_neg_integer()
}).

-define(MIDDLE, 5).
-define(BOTTOM, 0).
-define(INDENT_MIDDLE, 8).
-define(INDENT_BOTTOM, 3).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun test/1, density:list()).

%%--------------------------------------------------------------------

test(Density) ->
    io:format(" => ~s~n", [Density]),
    [
        test_from_mux(X, Y, Index, Density)
        ||
        X <- lists:seq(0, 22),
        Y <- lists:seq(0, 14),
        Index <- lists:seq(0, 13)
    ],
    [
        test_to_mux(X, Y, I, Density)
        ||
        X <- lists:seq(0, 22),
        Y <- lists:seq(0, 14),
        I <- lists:seq(0, 63)
    ],
    test_database(Density),
    ok.

%%--------------------------------------------------------------------

test_from_mux(X, Y, Index, Density) ->
    case from_mux({c4, X, Y}, {mux, Index}, Density) of
        false ->
            ok;

        undefined ->
            ok;

        {ok, Interconnect} ->
            case to_mux(Interconnect, Density) of
                {ok, {c4, X, Y}, {mux, Index}} ->
                    ok;

                {ok, Block, Mux} ->
                    throw({
                        {c4, X, Y}, {mux, Index},
                        from_mux, Interconnect,
                        to_mux, Block, Mux
                    });

                Other ->
                    throw({
                        {c4, X, Y}, {mux, Index},
                        from_mux, Interconnect,
                        to_mux, Other
                    })
            end
    end.

%%--------------------------------------------------------------------

test_to_mux(X, Y, I, Density) ->
    case to_mux({c4, X, Y, 0, I}, Density) of
        false ->
            ok;

        undefined ->
            ok;

        {ok, Block, Mux} ->
            case from_mux(Block, Mux, Density) of
                {ok, {c4, X, Y, 0, I}} ->
                    ok;

                {ok, Interconnect} ->
                    throw({
                        {c4, X, Y, 0, I},
                        to_mux, Block, Mux,
                        from_mux, Interconnect
                    });

                Other ->
                    throw({
                        {c4, X, Y, 0, I},
                        to_mux, Block, Mux,
                        from_mux, Other
                    })
            end
    end.

%%--------------------------------------------------------------------

test_database(Density) ->
    {ok, Blocks} = c4_interconnect_database:open(Density),
    maps:foreach(fun (Block, Indexes) ->
        test_database_block(Density, Block, Indexes)
    end, Blocks).

%%--------------------------------------------------------------------

test_database_block(Density, Block, Indexes) ->
    maps:foreach(fun (Index, {Interconnect, _}) ->
        test_database_index(Density, Block, Index, Interconnect)
    end, Indexes).

%%--------------------------------------------------------------------

test_database_index(Density, Block, Index, Interconnect) ->
    {ok, Interconnect} = c4_interconnect_map:from_mux(Block, Index, Density),
    ok.

%%====================================================================
%% from_mux
%%====================================================================

-spec from_mux(block(), mux(), density()) -> {ok, c4()} | false | undefined.

from_mux(_, {mux, Index}, _) when Index < 0 orelse Index > 13 ->
    false;
from_mux({c4, X, Y}, {mux, Index}, Density) ->
    case with(Density) of
        With when X < With#with.indent_right andalso
                  Y =< With#with.indent_top ->
            false;

        With when X < With#with.left orelse X > With#with.right ->
            false;

        With when Y =< With#with.bottom orelse Y >= With#with.top ->
            false;

        With ->
            from_mux(X, Y, Index, With)
    end.

%%--------------------------------------------------------------------

from_mux(X, Y, Index, _) when Index < 7 ->
    {ok, {c4, X, Y + 1, 0, Index}};
from_mux(X, Y, Index, With)
        when X < With#with.indent_right andalso
             Y < ?INDENT_MIDDLE ->
    I = to_pattern(X, Y, With) + (4 * (Index - 7)),
    {ok, {c4, X, ?INDENT_BOTTOM, 0, I}};
from_mux(X, Y, Index, With)
        when X < With#with.indent_right andalso
             Y =:= ?INDENT_MIDDLE ->
    {ok, {c4, X, ?INDENT_BOTTOM + 1, 0, Index + 21}};
from_mux(X, Y, Index, With)
        when Y < ?MIDDLE ->
    I = to_pattern(X, Y, With) + (4 * (Index - 7)),
    {ok, {c4, X, ?BOTTOM, 0, I}};
from_mux(X, Y, Index, _)
        when Y =:= ?MIDDLE ->
    {ok, {c4, X, ?BOTTOM + 1, 0, Index + 21}};
from_mux(X, Y, Index, _) ->
    {ok, {c4, X, Y - 4, 0, Index}}.

%%====================================================================
%% to_mux
%%====================================================================

-spec to_mux(c4(), density()) -> {ok, block(), mux()} | false.

to_mux({c4, _, _, Z, _}, _) when Z =/= 0 ->
    false;
to_mux({c4, _, _, _, I}, _) when I < 0 orelse I > 34 ->
    false;
to_mux({c4, X, Y, 0, I}, Density) ->
    case with(Density) of
        With when Y < With#with.bottom orelse Y > With#with.top ->
            false;

        With when Y < With#with.indent_top andalso
                  (X < With#with.indent_right orelse X > With#with.right) ->
            false;

        With when X < With#with.left orelse X > With#with.right ->
            false;

        With when X < With#with.indent_right ->
            to_mux_indent(X, Y, I, With);

        With ->
            to_mux(X, Y, I, With)
    end.

%%--------------------------------------------------------------------

to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM andalso I >= 28 ->
    false;
to_mux_indent(X, Y, I, With) when Y =:= ?INDENT_BOTTOM ->
    YY = from_pattern(X, I, 4, 4, With),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux_indent(_, Y, _, With)
        when Y =:= ?INDENT_BOTTOM + 1 andalso
             With#with.top =< ?INDENT_MIDDLE ->
    false;
to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 andalso I < 28 ->
    false;
to_mux_indent(X, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 ->
    {ok, {c4, X, ?INDENT_MIDDLE}, {mux, I - 21}};
to_mux_indent(X, Y, I, _) when I < 7 ->
    {ok, {c4, X, Y - 1}, {mux, I}};
to_mux_indent(_, Y, _, With) when Y + 4 >= With#with.top ->
    false;
to_mux_indent(X, Y, I, _) when I < 14 ->
    {ok, {c4, X, Y + 4}, {mux, I}};
to_mux_indent(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------

to_mux(_, Y, I, _) when Y =:= ?BOTTOM andalso I >= 28 ->
    false;
to_mux(X, Y, I, With) when Y =:= ?BOTTOM andalso I < 28 ->
    YY = from_pattern(X, I, 1, 3, With),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux(_, Y, _, With)
        when Y =:= ?BOTTOM + 1 andalso
             With#with.top =< ?MIDDLE ->
    false;
to_mux(_, Y, I, _) when Y =:= ?BOTTOM + 1 andalso I < 28 ->
    false;
to_mux(X, Y, I, _) when Y =:= ?BOTTOM + 1 ->
    {ok, {c4, X, ?MIDDLE}, {mux, I - 21}};
to_mux(X, Y, I, _) when I < 7 ->
    {ok, {c4, X, Y - 1}, {mux, I}};
to_mux(_, Y, _, With) when Y + 4 >= With#with.top ->
    false;
to_mux(X, Y, I, _) when I < 14 ->
    {ok, {c4, X, Y + 4}, {mux, I}};
to_mux(_, _, _, _) ->
    false.

%%====================================================================
%% helpers
%%====================================================================

with(epm240) ->
    #with{
       left = 1,
       right = 7,
       top = 5,
       bottom = 0,
       indent_top = 0,
       indent_right = 1,
       offset_x = 1,
       offset_y = 3
    };
with(epm570) ->
    #with{
       left = 0,
       right = 12,
       top = 8,
       bottom = 0,
       indent_top = 3,
       indent_right = 9,
       offset_x = 0,
       offset_y = 0
    };
with(epm1270) ->
    #with{
       left = 0,
       right = 16,
       top = 11,
       bottom = 0,
       indent_top = 3,
       indent_right = 11,
       offset_x = 0,
       offset_y = 0
    };
with(epm2210) ->
    #with{
       left = 0,
       right = 20,
       top = 14,
       bottom = 0,
       indent_top = 3,
       indent_right = 13,
       offset_x = 0,
       offset_y = 0
    }.

%%--------------------------------------------------------------------

to_pattern(X, Y, #with{offset_x = OffsetX, offset_y = OffsetY}) ->
    to_pattern((X + OffsetX) rem 4, (Y + OffsetY) rem 4).

%%--------------------------------------------------------------------

to_pattern(0, 0) -> 3;
to_pattern(0, 1) -> 2;
to_pattern(0, 2) -> 1;
to_pattern(0, 3) -> 0;
to_pattern(1, 0) -> 1;
to_pattern(1, 1) -> 0;
to_pattern(1, 2) -> 3;
to_pattern(1, 3) -> 2;
to_pattern(2, 0) -> 2;
to_pattern(2, 1) -> 1;
to_pattern(2, 2) -> 0;
to_pattern(2, 3) -> 3;
to_pattern(3, 0) -> 0;
to_pattern(3, 1) -> 3;
to_pattern(3, 2) -> 2;
to_pattern(3, 3) -> 1.

%%--------------------------------------------------------------------

from_pattern(X, Index, Bottom, Bias, #with{offset_x = OffsetX, offset_y = OffsetY}) ->
    Y = from_pattern((X + OffsetX) rem 4, Index rem 4),
    Bottom + ((Bias + Y - OffsetY) rem 4).

%%--------------------------------------------------------------------

from_pattern(0, 0) -> 3;
from_pattern(0, 1) -> 2;
from_pattern(0, 2) -> 1;
from_pattern(0, 3) -> 0;
from_pattern(1, 0) -> 1;
from_pattern(1, 1) -> 0;
from_pattern(1, 2) -> 3;
from_pattern(1, 3) -> 2;
from_pattern(2, 0) -> 2;
from_pattern(2, 1) -> 1;
from_pattern(2, 2) -> 0;
from_pattern(2, 3) -> 3;
from_pattern(3, 0) -> 0;
from_pattern(3, 1) -> 3;
from_pattern(3, 2) -> 2;
from_pattern(3, 3) -> 1.

