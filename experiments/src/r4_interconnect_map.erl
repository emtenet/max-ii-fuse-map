-module(r4_interconnect_map).

-export([run/0]).

-export([from_mux/3]).
-export([to_mux/2]).

-export_type([block/0]).
-export_type([mux/0]).

-type density() :: density:density().
-type r4() :: max_ii:r4().

-type block() :: {r4, max_ii:x(), max_ii:y()}.
-type mux() :: {mux, 0..15}.

-record(with, {
    zero :: non_neg_integer(),
    zero_columns :: {column(), column(), column(), column()},
    left :: non_neg_integer(),
    right :: non_neg_integer(),
    top :: non_neg_integer(),
    bottom :: non_neg_integer(),
    indent_top :: non_neg_integer(),
    indent_right :: non_neg_integer(),
    indent_columns :: {column(), column(), column(), column()},
    pattern_offset :: non_neg_integer(),
    stride :: non_neg_integer()
}).

-type column() :: non_neg_integer() | undefined.

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
        Index <- lists:seq(0, 15)
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
    case from_mux({r4, X, Y}, {mux, Index}, Density) of
        false ->
            ok;

        undefined ->
            ok;

        {ok, Interconnect} ->
            case to_mux(Interconnect, Density) of
                {ok, {r4, X, Y}, {mux, Index}} ->
                    ok;

                {ok, Block, Mux} ->
                    throw({
                        {r4, X, Y}, {mux, Index},
                        from_mux, Interconnect,
                        to_mux, Block, Mux
                    });

                Other ->
                    throw({
                        {r4, X, Y}, {mux, Index},
                        from_mux, Interconnect,
                        to_mux, Other
                    })
            end
    end.

%%--------------------------------------------------------------------

test_to_mux(X, Y, I, Density) ->
    case to_mux({r4, X, Y, 0, I}, Density) of
        false ->
            ok;

        undefined ->
            ok;

        {ok, Block, Mux} ->
            case from_mux(Block, Mux, Density) of
                {ok, {r4, X, Y, 0, I}} ->
                    ok;

                {ok, Interconnect} ->
                    throw({
                        {r4, X, Y, 0, I},
                        to_mux, Block, Mux,
                        from_mux, Interconnect
                    });

                Other ->
                    throw({
                        {r4, X, Y, 0, I},
                        to_mux, Block, Mux,
                        from_mux, Other
                    })
            end
    end.

%%--------------------------------------------------------------------

test_database(Density) ->
    {ok, Blocks} = r4_interconnect_database:open(Density),
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
    {ok, Interconnect} = r4_interconnect_map:from_mux(Block, Index, Density),
    ok.

%%====================================================================
%% from_mux
%%====================================================================

-spec from_mux(block(), mux(), density()) -> {ok, r4()} | false | undefined.

from_mux(_, {mux, Index}, _) when Index < 0 orelse Index > 15 ->
    false;
from_mux({r4, X, Y}, {mux, Index}, Density) ->
    case with(Density) of
        With when X < With#with.indent_right andalso
                  Y < With#with.indent_top ->
            false;

        With when X < With#with.left orelse X > With#with.right ->
            false;

        With when Y < With#with.bottom orelse Y > With#with.top ->
            false;

        With ->
            from_mux(X, Y, Index, With)
    end.

%%--------------------------------------------------------------------

from_mux(X, Y, Index, With)
        when Index < 8 andalso
             X =:= With#with.right ->
    I = Index + 8,
    {ok, {r4, X - 1, Y, 0, I}};
from_mux(X, Y, Index, With)
        when Y < With#with.indent_top andalso
             X =:= With#with.indent_right andalso
             Index < 8 ->
    undefined;
from_mux(X, Y, Index, With)
        when Index < 8 ->
    I = to_pattern(X, Y, Index, With),
    {ok, {r4, X, Y, 0, I}};
from_mux(X, Y, Index, With)
        when Y < With#with.indent_top andalso
             X =:= With#with.indent_right ->
    I = With#with.stride + ((Index - 8) * 4),
    {ok, {r4, X, Y, 0, I}};
from_mux(X, Y, Index, With)
        when Y < With#with.indent_top andalso
             X < With#with.indent_right + 4 ->
    Pattern = to_pattern(X, Y, Index, With),
    Column = (23 - X) rem 4,
    I = Column + (4 * Pattern),
    {ok, {r4, With#with.indent_right, Y, 0, I}};
from_mux(X, Y, Index, With)
        when X < With#with.left + 3 ->
    Pattern = to_pattern(X, Y, Index, With),
    Column = (23 - X) rem 4,
    I = Column + (4 * Pattern),
    {ok, {r4, With#with.zero, Y, 0, I}};
from_mux(X, Y, Index, With) ->
    I = to_pattern(X, Y, Index, With),
    {ok, {r4, X - 3, Y, 0, I}}.

%%====================================================================
%% to_mux
%%====================================================================

-spec to_mux(r4(), density()) -> {ok, block(), mux()} | false.

to_mux({r4, _, _, Z, _}, _) when Z =/= 0 ->
    false;
to_mux({r4, _, _, _, I}, _) when I < 0 orelse I > 63 ->
    false;
to_mux({r4, X, Y, 0, I}, Density) ->
    case with(Density) of
        With when Y < With#with.bottom orelse Y > With#with.top ->
            false;

        With when Y < With#with.indent_top andalso
                  (X < With#with.indent_right orelse X > With#with.right + 3) ->
            false;

        With when X < With#with.zero orelse X >= With#with.right ->
            false;

        With ->
            to_mux(X, Y, I, With)
    end.

%%--------------------------------------------------------------------

to_mux(X, Y, I, With)
        when Y < With#with.indent_top andalso
             X =:= With#with.indent_right andalso
             I < 32 andalso
             I rem 4 =:= With#with.stride ->
    Index = 8 + (I div 4),
    {ok, {r4, X, Y}, {mux, Index}};
to_mux(X, _, I, With)
        when I < 16 andalso
             X < With#with.left ->
    false;
to_mux(X, Y, I, With)
        when I < 16 andalso
             X =< With#with.indent_right andalso
             Y < With#with.indent_top ->
    false;
to_mux(X, _, I, With)
        when I < 8 andalso
             X >= With#with.right ->
    false;
to_mux(X, Y, I, With)
        when I < 8 ->
    Index = from_pattern(X, Y, I, With),
    {ok, {r4, X, Y}, {mux, Index}};
to_mux(X, Y, I, With)
        when I < 16 andalso
             X =:= With#with.right - 1 ->
    {ok, {r4, X + 1, Y}, {mux, I - 8}};
to_mux(X, _, I, With)
        when I < 16 andalso
             X > With#with.right - 3 ->
    false;
to_mux(X, Y, I, With) when I < 16 ->
    Index = from_pattern(X + 3, Y, I, With),
    {ok, {r4, X + 3, Y}, {mux, Index}};
to_mux(_, _, I, _) when I < 32 ->
    false;
to_mux(X, Y, I, With = #with{zero = X})  ->
    case element(1 + (I rem 4), With#with.zero_columns) of
        undefined ->
            false;

        XX ->
            Pattern = I div 4,
            Index = from_pattern(XX, Y, Pattern, With),
            {ok, {r4, XX, Y}, {mux, Index}}
    end;
to_mux(X, Y, I, With = #with{indent_right = X})
        when Y < With#with.indent_top ->
    case element(1 + (I rem 4), With#with.indent_columns) of
        undefined ->
            false;

        XX ->
            Pattern = I div 4,
            Index = from_pattern(XX, Y, Pattern, With),
            {ok, {r4, XX, Y}, {mux, Index}}
    end;
to_mux(_, _, _, _) ->
    false.

%%====================================================================
%% helpers
%%====================================================================

with(epm240) ->
    #with{
       zero = 1,
       zero_columns = {3, 2, undefined, 4},
       left = 2,
       right = 8,
       top = 4,
       bottom = 1,
       indent_top = 0,
       indent_right = 1,
       indent_columns = {undefined, undefined, undefined, undefined},
       pattern_offset = 0,
       stride = 1
    };
with(epm570) ->
    #with{
       zero = 0,
       zero_columns = {3, 2, 1, undefined},
       left = 1,
       right = 13,
       top = 7,
       bottom = 1,
       indent_top = 4,
       indent_right = 9,
       indent_columns = {11, 10, undefined, 12},
       pattern_offset = 1,
       stride = 3
    };
with(epm1270) ->
    #with{
       zero = 0,
       zero_columns = {3, 2, 1, undefined},
       left = 1,
       right = 17,
       top = 10,
       bottom = 1,
       indent_top = 4,
       indent_right = 11,
       indent_columns = {undefined, 14, 13, 12},
       pattern_offset = 0,
       stride = 1
    };
with(epm2210) ->
    #with{
       zero = 0,
       zero_columns = {3, 2, 1, undefined},
       left = 1,
       right = 21,
       top = 13,
       bottom = 1,
       indent_top = 4,
       indent_right = 13,
       indent_columns = {15, 14, undefined, 16},
       pattern_offset = 2,
       stride = 3
    }.

%%--------------------------------------------------------------------

to_pattern(X, Y, Index, #with{pattern_offset = Offset}) ->
    U = (X + Y + Offset) rem 3,
    V = Index rem 4,
    to_pattern(U, V) + Index - V.

%%--------------------------------------------------------------------

to_pattern(0, 0) -> 0;
to_pattern(1, 0) -> 1;
to_pattern(2, 0) -> 0;
to_pattern(0, 1) -> 3;
to_pattern(1, 1) -> 3;
to_pattern(2, 1) -> 2;
to_pattern(0, 2) -> 2;
to_pattern(1, 2) -> 0;
to_pattern(2, 2) -> 3;
to_pattern(0, 3) -> 1;
to_pattern(1, 3) -> 2;
to_pattern(2, 3) -> 1.

%%--------------------------------------------------------------------

from_pattern(X, Y, I, #with{pattern_offset = Offset}) ->
    U = (X + Y + Offset) rem 3,
    V = I rem 4,
    from_pattern(U, V) + I - V.

%%--------------------------------------------------------------------

from_pattern(0, 0) -> 0;
from_pattern(0, 1) -> 3;
from_pattern(0, 2) -> 2;
from_pattern(0, 3) -> 1;
from_pattern(1, 0) -> 2;
from_pattern(1, 1) -> 0;
from_pattern(1, 2) -> 3;
from_pattern(1, 3) -> 1;
from_pattern(2, 0) -> 0;
from_pattern(2, 1) -> 3;
from_pattern(2, 2) -> 1;
from_pattern(2, 3) -> 2.

