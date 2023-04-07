-module(c4_interconnect_map).

-export([run/0]).

-export([from_mux/3]).
-export([to_mux/2]).

-export_type([block/0]).
-export_type([mux/0]).

-type density() :: density:density().
-type c4() :: max_ii:c4().

-type block() :: max_ii:c4_block().
-type mux() :: max_ii:c4_index().

-record(with, {
    io_top :: non_neg_integer(),
    io_left :: non_neg_integer(),
    io_right :: non_neg_integer(),
    io_bottom :: non_neg_integer(),
    lab_top :: non_neg_integer(),
    lab_left :: non_neg_integer(),
    lab_right :: non_neg_integer(),
    lab_bottom :: non_neg_integer(),
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
        {error, _} ->
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

                Error = {error, _} ->
                    throw({
                        {c4, X, Y}, {mux, Index},
                        from_mux, Interconnect,
                        to_mux, Error
                    })
            end
    end.

%%--------------------------------------------------------------------

test_to_mux(X, Y, I, Density) ->
    case to_mux({c4, X, Y, 0, I}, Density) of
        {error, _} ->
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

                Error = {error, _} ->
                    throw({
                        {c4, X, Y, 0, I},
                        to_mux, Block, Mux,
                        from_mux, Error
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

-spec from_mux(block(), mux(), density()) -> {ok, c4()} | {error, term()}.

from_mux(_, {mux, Index}, _) when Index < 0 orelse Index > 13 ->
    {error, mux_out_of_bounds};
from_mux({c4, X, Y}, {mux, Index}, Density) ->
    case with(Density) of
        With when Y > With#with.io_top ->
            {error, top};

        With when Y =:= With#with.io_top ->
            from_io_top(X, Index, With);

        With when Y =< With#with.io_bottom ->
            {error, bottom};

        With when X < With#with.io_left ->
            {error, left};

        With when X >= With#with.io_right ->
            {error, right};

        With when X < With#with.indent_right andalso
                  Y =< With#with.indent_top ->
            {error, indent};

        With ->
            from_mux(X, Y, Index, With)
    end.

%%--------------------------------------------------------------------

from_io_top(X, _, With) when X < With#with.lab_left ->
    {error, io_top_left};
from_io_top(X, _, With) when X > With#with.lab_right ->
    {error, io_top_right};
from_io_top(_, Index, _) when Index < 7 orelse Index > 9 ->
    {error, io_top_index};
from_io_top(X, Index, With) ->
    {ok, {c4, X, With#with.lab_top, 0, Index}}.

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
from_mux(X, Y, Index, With)
        when (Index >= 7 andalso Index =< 9) andalso
             Y =:= With#with.io_top ->
    {ok, {c4, X, With#with.lab_top, 0, Index}};
from_mux(X, Y, Index, _) ->
    {ok, {c4, X, Y - 4, 0, Index}}.

%%====================================================================
%% to_mux
%%====================================================================

-spec to_mux(c4(), density()) -> {ok, block(), mux()} | {error, term()}.

to_mux({c4, _, _, Z, _}, _) when Z =/= 0 ->
    {error, s_non_zero};
to_mux({c4, _, _, _, I}, _) when I < 0 orelse I > 34 ->
    {error, i_out_of_bounds};
to_mux({c4, X, Y, 0, I}, Density) ->
    case with(Density) of
        With when Y > With#with.io_top ->
            {error, top};

        With when Y < With#with.io_bottom ->
            {error, bottom};

        With when X < With#with.io_left ->
            {error, left};

        With when X >= With#with.io_right ->
            {error, right};

        With when Y < With#with.indent_top andalso
                  X < With#with.indent_right ->
            {error, indent};

        With when X < With#with.indent_right ->
            to_mux_indent(X, Y, I, With);

        With ->
            to_mux(X, Y, I, With)
    end.

%%--------------------------------------------------------------------

to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM andalso I >= 28 ->
    {error, indent_bottom};
to_mux_indent(X, Y, I, With) when Y =:= ?INDENT_BOTTOM ->
    YY = from_pattern(X, I, 4, 4, With),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux_indent(_, Y, _, With)
        when Y =:= ?INDENT_BOTTOM + 1 andalso
             With#with.io_top =< ?INDENT_MIDDLE ->
    {error, indent_bottom_overflow};
to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 andalso I < 28 ->
    {error, indent_bottom_plus_1};
to_mux_indent(X, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 ->
    {ok, {c4, X, ?INDENT_MIDDLE}, {mux, I - 21}};
to_mux_indent(X, Y, I, With) ->
    to_mux_common(X, Y, I, With).

%%--------------------------------------------------------------------

to_mux(_, Y, I, _) when Y =:= ?BOTTOM andalso I >= 28 ->
    {error, indent_bottom};
to_mux(X, Y, I, With) when Y =:= ?BOTTOM andalso I < 28 ->
    YY = from_pattern(X, I, 1, 3, With),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux(_, Y, _, With)
        when Y =:= ?BOTTOM + 1 andalso
             With#with.io_top =< ?MIDDLE ->
    {error, indent_bottom_overflow};
to_mux(_, Y, I, _) when Y =:= ?BOTTOM + 1 andalso I < 28 ->
    {error, indent_bottom_plus_1};
to_mux(X, Y, I, _) when Y =:= ?BOTTOM + 1 ->
    {ok, {c4, X, ?MIDDLE}, {mux, I - 21}};
to_mux(X, Y, I, With) ->
    to_mux_common(X, Y, I, With).

%%--------------------------------------------------------------------

to_mux_common(X, Y, I, _) when I < 7 ->
    {ok, {c4, X, Y - 1}, {mux, I}};
to_mux_common(X, Y, I, With)
        when Y =:= With#with.lab_top andalso
             X >= With#with.lab_left andalso
             I >= 7 andalso I =< 9 ->
    {ok, {c4, X, With#with.io_top}, {mux, I}};
to_mux_common(_, Y, _, With) when Y + 4 > With#with.lab_top ->
    {error, y_plus_4_above};
to_mux_common(X, Y, I, _) when I < 14 ->
    {ok, {c4, X, Y + 4}, {mux, I}};
to_mux_common(_, _, _, _) ->
    {error, undefined}.

%%====================================================================
%% helpers
%%====================================================================

with(epm240) ->
    #with{
        io_top = 5,
        io_left = 1,
        io_right = 8,
        io_bottom = 0,
        lab_top = 4,
        lab_left = 2,
        lab_right = 7,
        lab_bottom = 1,
        indent_top = 0,
        indent_right = 1,
        offset_x = 1,
        offset_y = 3
    };
with(epm570) ->
    #with{
        io_top = 8,
        io_left = 0,
        io_right = 13,
        io_bottom = 0,
        lab_top = 7,
        lab_left = 1,
        lab_right = 12,
        lab_bottom = 1,
        indent_top = 3,
        indent_right = 9,
        offset_x = 0,
        offset_y = 0
    };
with(epm1270) ->
    #with{
        io_top = 11,
        io_left = 0,
        io_right = 17,
        io_bottom = 0,
        lab_top = 10,
        lab_left = 1,
        lab_right = 16,
        lab_bottom = 1,
        indent_top = 3,
        indent_right = 11,
        offset_x = 0,
        offset_y = 0
    };
with(epm2210) ->
    #with{
        io_top = 14,
        io_left = 0,
        io_right = 21,
        io_bottom = 0,
        lab_top = 13,
        lab_left = 1,
        lab_right = 20,
        lab_bottom = 1,
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

