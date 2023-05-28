-module(matrix).

-export([build/1]).
-export([build/2]).
-export([build_with_location/2]).
-export([build_with_map/2]).
-export([is_empty/1]).
-export([find_fuse/2]).
-export([all_ones/1]).
-export([all_zeros/1]).
-export([single_ones/1]).
-export([single_zeros/1]).
-export([pattern_is/2]).
-export([pattern_match/2]).
-export([remove_fuses/2]).
-export([print/1]).
-export([print_key/1]).

-export_type([experiment/0]).
-export_type([experiment_name/0]).

-export_type([matrix/0]).
-export_type([bits/0]).
-export_type([match/0]).

-export_type([fuse_name_fn/0]).

-type fuse() :: fuse:fuse().
-type fuse_name() :: fuse:name() | undefined.

-type density() :: density:density().
-type device() :: device:device().

-type experiment_name() :: term().
-type experiment() ::
    {experiment_name(), [fuse()]} |
    {experiment_name(), [fuse()], rcf_file:rcf()}.

-type matrix() ::
    {matrix, [experiment_name()], [{fuse(), bits(), fuse_name()}]}.
-type bits() :: [0 | 1].
-type match() :: [0 | 1 | x].

-type fuse_name_fn() :: fun((fuse_name()) -> boolean()).

%%====================================================================
%% build
%%====================================================================

-spec build([experiment()]) -> matrix().

build(Experiments) ->
    build_with(fun name_is_undefined/1, Experiments).

%%--------------------------------------------------------------------

name_is_undefined(_) ->
    undefined.

%%====================================================================
%% build
%%====================================================================

-spec build(density() | device(), [experiment()]) -> matrix().

build(DensityOrDevice, Experiments) ->
    Database = fuse_database:read(DensityOrDevice),
    With = fun (Fuse) -> fuse_database:name(Fuse, Database) end,
    build_with(With, Experiments).

%%====================================================================
%% build_with_location
%%====================================================================

-spec build_with_location(density() | device(), [experiment()]) -> matrix().

build_with_location(DensityOrDevice, Experiments) ->
    Density = density:or_device(DensityOrDevice),
    With = fun (Fuse) -> fuse_map:to_location(Fuse, Density) end,
    build_with(With, Experiments).

%%====================================================================
%% build_with_map
%%====================================================================

-spec build_with_map(density() | device(), [experiment()]) -> matrix().

build_with_map(DensityOrDevice, Experiments) ->
    Density = density:or_device(DensityOrDevice),
    With = fun (Fuse) -> fuse_map_to_name(Fuse, Density) end,
    build_with(With, Experiments).

%%--------------------------------------------------------------------

fuse_map_to_name(Fuse, Density) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, N} -> N;
        {error, E} -> E
    end.

%%====================================================================
%% build_with
%%====================================================================

build_with(With, Experiments) ->
    Names = lists:map(fun build_name/1, Experiments),
    Results = lists:map(fun build_result/1, Experiments),
    Matrix = build_diff(Results, With, []),
    {matrix, Names, Matrix}.

%%--------------------------------------------------------------------

build_name({Name, _}) ->
    Name;
build_name({Name, _, _}) ->
    Name.

%%--------------------------------------------------------------------

build_result({_, Result}) ->
    Result;
build_result({_, Result, _}) ->
    Result.

%%--------------------------------------------------------------------

build_diff(Results, With, Matrix) ->
    case build_min_max(Results) of
        finished ->
            lists:reverse(Matrix);

        {Fuse, Fuse} ->
            build_diff(
                build_drop(Results, Fuse),
                With,
                Matrix
             );

        {Fuse, _} ->
            Name = With(Fuse),
            build_diff(
                build_drop(Results, Fuse),
                With,
                [build_row(Results, Fuse, Name) | Matrix]
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

build_row(Results, Fuse, Name) ->
    build_row(Results, Fuse, Name, []).

%%--------------------------------------------------------------------

build_row([], Fuse, Name, Row) ->
    {Fuse, lists:reverse(Row), Name};
build_row([[Fuse | _] | Results], Fuse, Name, Row) ->
    % fuse is in the "absent" list so bit == 0
    build_row(Results, Fuse, Name, [0 | Row]);
build_row([_ | Results], Fuse, Name, Row) ->
    build_row(Results, Fuse, Name, [1 | Row]).

%%====================================================================
%% is_empty
%%====================================================================

-spec is_empty(matrix()) -> boolean().

is_empty({matrix, _, []}) ->
    true;
is_empty({matrix, _, [_ | _]}) ->
    false.

%%====================================================================
%% find_fuse
%%====================================================================

-spec find_fuse(matrix(), fuse_name()) -> {ok, bits()} | false.

find_fuse({matrix, _, Fuses}, Fuse) ->
    find_fuse_in(Fuses, Fuse).

%%--------------------------------------------------------------------

find_fuse_in([], _) ->
    false;
find_fuse_in([{_, Bits, Fuse} | _], Fuse) ->
    {ok, Bits};
find_fuse_in([_ | Fuses], Fuse) ->
    find_fuse_in(Fuses, Fuse).

%%====================================================================
%% single_ones & single_zeros
%%====================================================================

-spec single_ones(matrix()) -> [{fuse(), experiment_name()}].

single_ones({matrix, Names, Fuses}) ->
    singles(1, Fuses, Names, []).

%%--------------------------------------------------------------------

-spec single_zeros(matrix()) -> [{fuse(), experiment_name()}].

single_zeros({matrix, Names, Fuses}) ->
    singles(0, Fuses, Names, []).

%%--------------------------------------------------------------------

singles(_, [], _, Singles) ->
    lists:reverse(Singles);
singles(Bit, [{Fuse, Bits, _Name} | Fuses], Names, Singles) ->
    case single(Bit, Bits, Names) of
        {ok, Name} ->
            singles(Bit, Fuses, Names, [{Fuse, Name} | Singles]);

        false ->
            singles(Bit, Fuses, Names, Singles)
    end.

%%--------------------------------------------------------------------

single(_, [], []) ->
    false;
single(Bit, [Bit | Bits], [Name | _]) ->
    single_ok(Bit, Name, Bits);
single(Bit, [_ | Bits], [_ | Names]) ->
    single(Bit, Bits, Names).

%%--------------------------------------------------------------------

single_ok(_, Name, []) ->
    {ok, Name};
single_ok(Bit, _, [Bit | _]) ->
    false;
single_ok(Bit, Name, [_ | Bits]) ->
    single_ok(Bit, Name, Bits).

%%====================================================================
%% all_ones & all_zeros
%%====================================================================

-spec all_ones(matrix()) -> [fuse()].

all_ones({matrix, _, Fuses}) ->
    alls(1, Fuses, []).

%%--------------------------------------------------------------------

-spec all_zeros(matrix()) -> [fuse()].

all_zeros({matrix, _, Fuses}) ->
    alls(0, Fuses, []).

%%--------------------------------------------------------------------

alls(_, [], Alls) ->
    lists:reverse(Alls);
alls(Bit, [{Fuse, Bits, _Name} | Fuses], Alls) ->
    case all(Bit, Bits) of
        true ->
            alls(Bit, Fuses, [Fuse | Alls]);

        false ->
            alls(Bit, Fuses, Alls)
    end.

%%--------------------------------------------------------------------

all(_, []) ->
    true;
all(Bit, [Bit | Bits]) ->
    all(Bit, Bits);
all(_, [_ | _]) ->
    false.

%%====================================================================
%% pattern_is
%%====================================================================

-spec pattern_is(matrix(), bits()) -> [{fuse(), fuse_name()}].

pattern_is({matrix, _, Fuses}, Pattern) ->
    pattern_is(Fuses, Pattern, []).

%%--------------------------------------------------------------------

pattern_is([], _, Found) ->
    lists:reverse(Found);
pattern_is([{Fuse, Pattern, Name} | Fuses], Pattern, Found) ->
    pattern_is(Fuses, Pattern, [{Fuse, Name} | Found]);
pattern_is([_ | Fuses], Pattern, Found) ->
    pattern_is(Fuses, Pattern, Found).

%%====================================================================
%% pattern_match
%%====================================================================

-spec pattern_match(matrix(), match()) -> [{fuse(), fuse_name()}].

pattern_match({matrix, _, Fuses}, Match) ->
    pattern_match(Fuses, Match, []).

%%--------------------------------------------------------------------

pattern_match([], _, Found) ->
    lists:reverse(Found);
pattern_match([{Fuse, Pattern, Name} | Fuses], Match, Found) ->
    case match(Pattern, Match) of
        true ->
            pattern_match(Fuses, Match, [{Fuse, Name} | Found]);

        false ->
            pattern_match(Fuses, Match, Found)
    end.

%%--------------------------------------------------------------------

match([], []) ->
    true;
match([_ | Pattern], [x | Match]) ->
    match(Pattern, Match);
match([X | Pattern], [X | Match]) ->
    match(Pattern, Match);
match(_, _) ->
    false.

%%====================================================================
%% remove_fuses
%%====================================================================

-spec remove_fuses(matrix(), [fuse_name()] | fuse_name_fn()) -> matrix().

remove_fuses({matrix, Experiments, Fuses}, Remove) when is_list(Remove) ->
    {matrix, Experiments, lists:filter(fun ({_, _, Name}) ->
        not lists:member(Name, Remove)
    end, Fuses)};
remove_fuses({matrix, Experiments, Fuses}, Remove)
        when is_function(Remove, 1) ->
    {matrix, Experiments, lists:filter(fun ({_, _, Name}) ->
        not Remove(Name)
    end, Fuses)}.

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
    case length(Names) of
        N when N =< 26 ->
            io:format("       ", []),
            print_header0(Names, 0),
            io:format("~n", []);

        N when N =< 27 * 26 ->
            io:format("       ", []),
            print_header1(Names, 0),
            io:format("~n", []),
            io:format("       ", []),
            print_header0(Names, 0),
            io:format("~n", [])
    end.

%%--------------------------------------------------------------------

print_key(Key) when Key >= 0 andalso Key < 26 ->
    <<" ", ($a + Key)>>;
print_key(Key0) when Key0 >= 26 andalso Key0 < 27 * 26 ->
    Key = Key0 - 26,
    <<($a + (Key div 26)), ($a + (Key rem 26))>>.

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

print_header1([], _) ->
    ok;
print_header1([_ | Names], Key) ->
    <<Header:1/binary, _>> = print_key(Key),
    io:format(" ~s", [Header]),
    print_header1(Names, Key + 1).

%%--------------------------------------------------------------------

print_header0([], _) ->
    ok;
print_header0([_ | Names], Key) ->
    <<_, Header:1/binary>> = print_key(Key),
    io:format(" ~s", [Header]),
    print_header0(Names, Key + 1).

%%--------------------------------------------------------------------

print_rows([]) ->
    ok;
print_rows([{Fuse, Fuses, Name} | Rows]) ->
    io:format("~6b |", [Fuse]),
    print_fuses(Fuses),
    case Name of
        _ when is_integer(Name) ->
            io:format("~n", []);

        _ ->
            io:format(" ~w~n", [Name])
    end,
    print_rows(Rows).

%%--------------------------------------------------------------------

print_fuses([]) ->
    ok;
print_fuses([1 | Fuses]) ->
    io:format("*|", []),
    print_fuses(Fuses);
print_fuses([0 | Fuses]) ->
    io:format(" |", []),
    print_fuses(Fuses).

