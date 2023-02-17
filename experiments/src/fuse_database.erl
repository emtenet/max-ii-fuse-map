-module(fuse_database).

-export([find/2]).
-export([name/2]).

-export([empty/0]).
-export([read/1]).
-export([update/2]).

-export_type([database/0]).

-type density() :: density:density().
-type device() :: device:device().
-type fuse() :: fuse:fuse().
-type name() :: fuse:name().

-opaque database() :: {fuses, density() | undefined, #{fuse() => name()}}.

%%====================================================================
%% find
%%====================================================================

-spec find(fuse(), database()) -> {ok, name()} | false.

find(Fuse, {fuses, _, FuseToName}) ->
    case FuseToName of
        #{Fuse := Name} ->
            {ok, Name};

        _ ->
            false
    end.

%%====================================================================
%% name
%%====================================================================

-spec name(fuse(), database()) -> term().

name(Fuse, {fuses, _, FuseToName}) ->
    case FuseToName of
        #{Fuse := Name} ->
            Name;

        _ ->
            Fuse
    end.

%%====================================================================
%% empty
%%====================================================================

-spec empty() -> database().

empty() ->
    {fuses, undefined, #{}}.

%%====================================================================
%% read
%%====================================================================

-spec read(density() | device()) -> database().

read(DensityOrDevice) ->
    Density = density:or_device(DensityOrDevice),
    File = database_file(Density),
    case read_file(File) of
        {ok, Fuses} ->
            {fuses, Density, Fuses};

        false ->
            {fuses, Density, #{}}
    end.

%%--------------------------------------------------------------------

read_file(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            Lines = binary:split(Binary, <<"\n">>, [global]),
            read_lines(Lines, -1, #{});

        {error, enoent} ->
            false
    end.

%%--------------------------------------------------------------------

read_lines([], _, Fuses) ->
    {ok, Fuses};
read_lines([<<>>], _, Fuses) ->
    {ok, Fuses};
read_lines([Line | Lines], Previous, Fuses) ->
    case read_line(Line) of
        {Fuse, Name} when Fuse > Previous ->
            read_lines(Lines, Fuse, Fuses#{Fuse => Name})
    end.

%%--------------------------------------------------------------------

read_line(<<FuseBinary:6/binary, ": ", NameBinary/binary>>) ->
    Fuse = binary_to_integer(FuseBinary),
    NameString = binary_to_list(<<NameBinary/binary, ".">>),
    {ok, NameTokens, _} = erl_scan:string(NameString),
    {ok, Name} = erl_parse:parse_term(NameTokens),
    {Fuse, Name}.

%%====================================================================
%% update
%%====================================================================

-spec update(density() | device(), [{fuse(), name()}]) -> ok.

update(_, []) ->
    ok;
update(DensityOrDevice, UpdateList) ->
    update_check(UpdateList),
    Density = density:or_device(DensityOrDevice),
    File = database_file(Density),
    case read_file(File) of
        {ok, ExistingMap} ->
            update_merge(UpdateList, false, File, ExistingMap);

        false ->
            update_merge(UpdateList, true, File, #{})
    end.

%%--------------------------------------------------------------------

update_check([]) ->
    ok;
update_check([{Fuse, _Name} | Fuses]) when is_integer(Fuse) ->
    update_check(Fuses).

%%--------------------------------------------------------------------

update_merge([], false, _, _) ->
    ok;
update_merge([], true, File, Map) ->
    update_file(File, Map);
update_merge([{Fuse, Name} | Fuses], Changed, File, Map) ->
    case Map of
        #{Fuse := Existing} when Name =:= Existing ->
            update_merge(Fuses, Changed, File, Map);

        #{Fuse := Existing} ->
            throw({fuse_database, Fuse, existing, Existing, new, Name});

        _ ->
            update_merge(Fuses, true, File, Map#{Fuse => Name})
    end.

%%--------------------------------------------------------------------

update_file(File, Fuses) ->
    Sorted = lists:sort(maps:to_list(Fuses)),
    Lines = [
        io_lib:format("~6..0b: ~w~n", [Fuse, Name])
        ||
        {Fuse, Name} <- Sorted
    ],
    ok = file:write_file(File, Lines).

%%====================================================================
%% helpers
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.fuses", [Density])).

