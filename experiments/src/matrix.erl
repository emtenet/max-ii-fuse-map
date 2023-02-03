-module(matrix).

-export([build/1]).
-export([is_empty/1]).
-export([print/1]).

-type fuse() :: fuses:fuse().

-type experiment_name() :: atom() | binary() | string().
-type experiment() :: {experiment_name(), [fuse()]}.

-type matrix() :: {matrix, [experiment_name()], [{fuse(), [off | on]}]}.

%%====================================================================
%% build
%%====================================================================

-spec build([experiment()]) -> matrix().

build(Experiments) ->
    Names = [ Name || {Name, _} <- Experiments ],
    Results = [ Result || {_, Result} <- Experiments ],
    Matrix = build_diff(Results, []),
    {matrix, Names, Matrix}.

%%--------------------------------------------------------------------

build_diff(Results, Matrix) ->
    case build_min_max(Results) of
        finished ->
            lists:reverse(Matrix);

        {Fuse, Fuse} ->
            build_diff(
                build_drop(Results, Fuse),
                Matrix
             );

        {Fuse, _} ->
            build_diff(
                build_drop(Results, Fuse),
                [build_row(Results, Fuse) | Matrix]
             )
    end.

%%--------------------------------------------------------------------

build_min_max([]) ->
    finished;
build_min_max([[] | Results]) ->
    build_min_max(Results);
build_min_max([[Fuse | _] | Results]) ->
    build_min_max(Results, Fuse, Fuse).

%%--------------------------------------------------------------------

build_min_max([], Min, Max) ->
    {Min, Max};
build_min_max([[] | Results], Min, Max) ->
    build_min_max(Results, Min, Max);
build_min_max([[Fuse | _] | Results], Min, Max) ->
    build_min_max(Results, min(Min, Fuse), max(Max, Fuse)).

%%--------------------------------------------------------------------

build_drop(Results, Drop) ->
    build_drop(Results, Drop, []).

%%--------------------------------------------------------------------

build_drop([], _, Tails) ->
    lists:reverse(Tails);
build_drop([[Drop | Tail] | Heads], Drop, Tails) ->
    build_drop(Heads, Drop, [Tail | Tails]);
build_drop([Tail | Heads], Drop, Tails) ->
    build_drop(Heads, Drop, [Tail | Tails]).

%%--------------------------------------------------------------------

build_row(Results, Fuse) ->
    build_row(Results, Fuse, []).

%%--------------------------------------------------------------------

build_row([], Fuse, Row) ->
    {Fuse, lists:reverse(Row)};
build_row([[Fuse | _] | Results], Fuse, Row) ->
    build_row(Results, Fuse, [on | Row]);
build_row([_ | Results], Fuse, Row) ->
    build_row(Results, Fuse, [off | Row]).

%%====================================================================
%% is_empty
%%====================================================================

-spec is_empty(matrix()) -> boolean().

is_empty({matrix, _, []}) ->
    true;
is_empty({matrix, _, [_ | _]}) ->
    false.

%%====================================================================
%% print
%%====================================================================

-spec print
    (matrix()) -> ok;
    ([experiment()]) -> ok.

print({matrix, Names, Matrix}) ->
    print_headers(Names),
    print_rows(Matrix),
    ok;
print(Experiments) ->
    print(build(Experiments)).

%%--------------------------------------------------------------------

print_headers(Names) ->
    print_keys(Names, 0),
    io:format("       ", []),
    print_header(Names, 0),
    io:format("~n", []).

%%--------------------------------------------------------------------

print_key(Key) when Key >= 0 andalso Key < 26 ->
    <<($a + Key)>>;
print_key(Key) when Key >= 26 andalso Key < 36 ->
    <<($0 + Key - 26)>>.

%%--------------------------------------------------------------------

print_keys([], _) ->
    ok;
print_keys([Name | Names], Key) when is_atom(Name) orelse is_binary(Name) ->
    io:format("     ~s: ~s~n", [print_key(Key), Name]),
    print_keys(Names, Key + 1);
print_keys([Name | Names], Key) ->
    io:format("     ~s: ~p~n", [print_key(Key), Name]),
    print_keys(Names, Key + 1).

%%--------------------------------------------------------------------

print_header([], _) ->
    ok;
print_header([_ | Names], Key) ->
    io:format(" ~s", [print_key(Key)]),
    print_header(Names, Key + 1).

%%--------------------------------------------------------------------

print_rows([]) ->
    ok;
print_rows([{Fuse, Fuses} | Rows]) ->
    io:format("~6b |", [Fuse]),
    print_fuses(Fuses),
    io:format("~n", []),
    print_rows(Rows).

%%--------------------------------------------------------------------

print_fuses([]) ->
    ok;
print_fuses([on | Fuses]) ->
    io:format("*|", []),
    print_fuses(Fuses);
print_fuses([off | Fuses]) ->
    io:format(" |", []),
    print_fuses(Fuses).

