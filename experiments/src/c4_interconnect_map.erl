-module(c4_interconnect_map).

-export([run/0]).

-export([from_mux/3]).
-export([to_mux/2]).

-export_type([block/0]).
-export_type([mux/0]).

-include("max_ii.hrl").

-type density() :: density:density().
-type c4() :: max_ii:c4().

-type block() :: max_ii:c4_block().
-type mux() :: max_ii:c4_index().

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
    case c4_interconnect_map:from_mux(Block, Index, Density) of
        {ok, Interconnect} ->
            ok;

        {error, top_io} ->
            ok
    end.

%%====================================================================
%% from_mux
%%====================================================================

-spec from_mux(block(), mux(), density()) -> {ok, c4()} | {error, term()}.

from_mux(_, {mux, Index}, _) when Index < 0 orelse Index > 13 ->
    {error, mux_out_of_bounds};
from_mux({c4, X, Y}, {mux, Index}, Density) ->
    case density:metric(Density) of
        Metric when Y > Metric#metric.top_io ->
            {error, top};

        Metric when X < Metric#metric.left_io ->
            {error, left};

        Metric when X >= Metric#metric.right_io ->
            {error, right};

        Metric when Y =:= Metric#metric.top_io ->
            {error, top_io};

        Metric when X < Metric#metric.indent_left_io andalso
                  Y =< Metric#metric.indent_bottom_io ->
            {error, indent};

        Metric when X < Metric#metric.indent_left_io ->
            from_mux(X, Y, Index, Metric, ?INDENT_MIDDLE, ?INDENT_BOTTOM);

        Metric when Y =< Metric#metric.bottom_io ->
            {error, bottom};

        Metric ->
            from_mux(X, Y, Index, Metric, ?MIDDLE, ?BOTTOM)
    end.

%%--------------------------------------------------------------------

from_mux(X, Y, Index, _, _, _) when Index < 7 ->
    {ok, {c4, X, Y + 1, 0, Index}};
from_mux(X, Y, Index, Metric, Middle, Bottom) when Y < Middle ->
    I = to_pattern(X, Y, Metric) + (4 * (Index - 7)),
    {ok, {c4, X, Bottom, 0, I}};
from_mux(X, Y, Index, _, Middle, Bottom) when Y =:= Middle ->
    {ok, {c4, X, Bottom + 1, 0, Index + 21}};
from_mux(X, Y, Index, _, _, _) ->
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
    case density:metric(Density) of
        Metric when Y > Metric#metric.top_io ->
            {error, top};

        Metric when X < Metric#metric.left_io ->
            {error, left};

        Metric when X >= Metric#metric.right_io ->
            {error, right};

        Metric when Y < Metric#metric.indent_bottom_io andalso
                  X < Metric#metric.indent_left_io ->
            {error, indent_bottom};

        Metric when X < Metric#metric.indent_left_io ->
            to_mux_indent(X, Y, I, Metric);

        Metric when Y < Metric#metric.bottom_io ->
            {error, bottom};

        Metric ->
            to_mux(X, Y, I, Metric)
    end.

%%--------------------------------------------------------------------

to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM andalso I >= 28 ->
    {error, indent_bottom_i};
to_mux_indent(X, Y, I, Metric) when Y =:= ?INDENT_BOTTOM ->
    YY = from_pattern(X, I, 4, 4, Metric),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux_indent(_, Y, _, Metric)
        when Y =:= ?INDENT_BOTTOM + 1 andalso
             Metric#metric.top_io =< ?INDENT_MIDDLE ->
    {error, indent_bottom_overflow};
to_mux_indent(_, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 andalso I < 28 ->
    {error, indent_bottom_plus_1};
to_mux_indent(X, Y, I, _) when Y =:= ?INDENT_BOTTOM + 1 ->
    {ok, {c4, X, ?INDENT_MIDDLE}, {mux, I - 21}};
to_mux_indent(X, Y, I, Metric) ->
    to_mux_common(X, Y, I, Metric).

%%--------------------------------------------------------------------

to_mux(_, Y, I, _) when Y =:= ?BOTTOM andalso I >= 28 ->
    {error, bottom_i};
to_mux(X, Y, I, Metric) when Y =:= ?BOTTOM andalso I < 28 ->
    YY = from_pattern(X, I, 1, 3, Metric),
    Index = 7 + (I div 4),
    {ok, {c4, X, YY}, {mux, Index}};
to_mux(_, Y, _, Metric)
        when Y =:= ?BOTTOM + 1 andalso
             Metric#metric.top_io =< ?MIDDLE ->
    {error, indent_bottom_overflow};
to_mux(_, Y, I, _) when Y =:= ?BOTTOM + 1 andalso I < 28 ->
    {error, indent_bottom_plus_1};
to_mux(X, Y, I, _) when Y =:= ?BOTTOM + 1 ->
    {ok, {c4, X, ?MIDDLE}, {mux, I - 21}};
to_mux(X, Y, I, Metric) ->
    to_mux_common(X, Y, I, Metric).

%%--------------------------------------------------------------------

to_mux_common(X, Y, I, _) when I < 7 ->
    {ok, {c4, X, Y - 1}, {mux, I}};
to_mux_common(_, Y, _, Metric) when Y + 4 > Metric#metric.top_lab ->
    {error, y_plus_4_above};
to_mux_common(X, Y, I, _) when I < 14 ->
    {ok, {c4, X, Y + 4}, {mux, I}};
to_mux_common(_, _, _, _) ->
    {error, undefined}.

%%====================================================================
%% helpers
%%====================================================================

to_pattern(X, Y, #metric{pattern_x = OffsetX, pattern_y = OffsetY}) ->
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

from_pattern(X, Index, Bottom, Bias, #metric{pattern_x = OffsetX, pattern_y = OffsetY}) ->
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

