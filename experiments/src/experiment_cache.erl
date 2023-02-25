-module(experiment_cache).

-export([load/1]).
-export([store/2]).
-export([flush/1]).

-export([iterate/0]).
-export([iterate/1]).

-export([read_pof/1]).
-export([read_rcf/1]).

-export_type([slot/0]).
-export_type([iterator/0]).

-type slot() :: {slot, binary(), file:filename_all()}.

-opaque iterator() ::
    {cache,
     [file:filename_all()],
     [file:filename_all()],
     [file:filename_all()]
    }.

-type device() :: device:device().
-type files() :: experiment_compile:files().
-type source() :: experiment_compile:source().

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% load
%%====================================================================

-spec load(source()) -> {hit, experiment:result()} | {miss, slot()}.

load(#{device := Device, settings := Settings, vhdl := VHDL}) ->
    Source = source(Device, Settings, VHDL),
    Dir = dir(Source),
    case read_source(Dir) of
        {ok, Source} ->
            {hit, {cached, Dir}};

        {error, enoent} ->
            {miss, {slot, Source, Dir}}
    end.

%%====================================================================
%% store
%%====================================================================

-spec store(slot(), files()) -> ok.

store({slot, Source, Dir}, #{pof := POF, rcf := RCF}) ->
    make_dir(Dir),
    write_pof(Dir, POF),
    write_rcf(Dir, RCF),
    write_source(Dir, Source),
    ok.

%%====================================================================
%% flush
%%====================================================================

-spec flush(source()) -> ok.

flush(#{device := Device, settings := Settings, vhdl := VHDL}) ->
    Source = source(Device, Settings, VHDL),
    Dir = dir(Source),
    ok = rm_dir(Dir).

%%====================================================================
%% iterate
%%====================================================================

-spec iterate()
    -> {device(), experiment:result(), iterator()} | false.

iterate() ->
    {ok, Outers} = file:list_dir("cache"),
    iterate([], undefined, lists:sort(Outers)).

%%--------------------------------------------------------------------

-spec iterate(iterator())
    -> {device(), experiment:result(), iterator()} | false.

iterate({cache, Inners, Outer, Outers}) ->
    iterate(Inners, Outer, Outers).

%%--------------------------------------------------------------------

iterate([], _, []) ->
    io:format("                           \r", []),
    false;
iterate([], _, [Outer | Outers]) ->
    {ok, Inners} = file:list_dir(filename:join("cache", Outer)),
    io:format("cache ~s\r", [Outer]),
    iterate(Inners, Outer, Outers);
iterate([Inner | Inners], Outer, Outers) ->
    Next = {cache, Inners, Outer, Outers},
    Dir = filename:join(["cache", Outer, Inner]),
    case read_source(Dir) of
        {ok, Source} ->
            [Device0, _] = binary:split(Source, <<"\n">>),
            Device = device:from_name(Device0),
            Result = {cached, Dir},
            {Device, Result, Next};

        {error, enoent} ->
            iterate(Inners, Outer, Outers)
    end.

%%====================================================================
%% internal
%%====================================================================

source(Device, Settings, VHDL) ->
    iolist_to_binary([
        Device,
        <<"\n">>,
        Settings,
        <<"\n====================\n">>,
        VHDL
    ]).

%%--------------------------------------------------------------------

dir(Source) ->
    Hash = crypto:hash(sha256, Source),
    Base64 = base64url:encode(Hash),
    <<Head:2/binary, Tail/binary>> = Base64,
    %make_dir(filename:join("cache", Head)),
    filename:join(["cache", Head, Tail]).

%%--------------------------------------------------------------------

make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%%--------------------------------------------------------------------

rm_dir(Dir) ->
    case file:list_dir_all(Dir) of
        {ok, Names} ->
            rm_dir(Dir, Names);

        {error, enoent} ->
            ok;

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

rm_dir(Dir, []) ->
    file:del_dir(Dir);
rm_dir(Dir, [Name | Names]) ->
    case rm_file(filename:join(Dir, Name)) of
        ok ->
            rm_dir(Dir, Names);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

rm_file(File) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            rm_dir(File);

        {ok, _} ->
            file:delete(File);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

read_pof(Dir) ->
    File = filename:join(Dir, "experiment.pof"),
    file:read_file(File).

%%--------------------------------------------------------------------

read_rcf(Dir) ->
    File = filename:join(Dir, "experiment.rcf"),
    file:read_file(File).

%%--------------------------------------------------------------------

read_source(Dir) ->
    File = filename:join(Dir, "source"),
    file:read_file(File).

%%--------------------------------------------------------------------

write_pof(Dir, POF) ->
    File = filename:join(Dir, "experiment.pof"),
    ok = file:write_file(File, POF).

%%--------------------------------------------------------------------

write_rcf(Dir, RCF) ->
    File = filename:join(Dir, "experiment.rcf"),
    ok = file:write_file(File, RCF).

%%--------------------------------------------------------------------

write_source(Dir, Source) ->
    File = filename:join(Dir, "source"),
    ok = file:write_file(File, Source).

