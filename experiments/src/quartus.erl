-module(quartus).

-export([cache/1]).
-export([pof/1]).
-export([rcf/1]).

-export_type([compile/0]).
-export_type([cache/0]).

-type compile() :: #{
    title => binary(),
    device := binary(),
    settings := binary(),
    vhdl := binary()
}.

-type cache() ::
    {cache, hit, file:filename_all()} |
    {cache, miss, binary(), binary()}.

-define(SEPARATOR, <<"\n====================\n">>).

%%====================================================================
%% cache
%%====================================================================

-spec cache(compile()) -> {ok, cache()} | error.

cache(Compile = #{device := Device, settings := Settings, vhdl := VHDL})
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    Source = cache_source(Device, Settings, VHDL),
    CacheDir = cache_dir(Source),
    case cache_read_source(CacheDir) of
        {ok, Source} ->
            cache_hit(CacheDir);

        {error, enoent} ->
            cache_miss(Source, CacheDir, Compile)
    end.

%%--------------------------------------------------------------------

compile(Compile) ->
    compile_connect(),
    Result = gen_server:call({global, quartus}, {compile, Compile}, 10000),
    compile_result(Result).

%%--------------------------------------------------------------------

compile_connect() ->
    case nodes() of
        [] ->
            pong = net_adm:ping('quartus@SILENT-PC'),
            ok = global:sync(),
            case global:whereis_name(quartus) of
                Pid when is_pid(Pid) ->
                    ok
            end;

        [_] ->
            ok
    end.

%%--------------------------------------------------------------------

compile_result({ok, Files}) ->
    {ok, Files};
compile_result({error, {quartus_map, Exit, Out}}) ->
    compile_exit("MAP", Exit, Out);
compile_result({error, {quartus_fit, Exit, Out}}) ->
    compile_exit("FIT", Exit, Out);
compile_result({error, {quartus_asm, Exit, Out}}) ->
    compile_exit("ASM", Exit, Out);
compile_result({error, {quartus_cdb, Exit, Out}}) ->
    compile_exit("CDB", Exit, Out);
compile_result({error, Error}) ->
    io:format("QUARTUS: ~p~n", [Error]),
    error.

%%--------------------------------------------------------------------

compile_exit(Who, Exit, Out) ->
    io:format("========================================~n", []),
    io:format("~s~n", [Out]),
    io:format("========================================~n", []),
    io:format("QUARTUS ~s: exit # ~p~n", [Who, Exit]),
    error.

%%--------------------------------------------------------------------

cache_source(Device, Settings, VHDL) ->
    iolist_to_binary([
        Device,
        <<"\n">>,
        Settings,
        ?SEPARATOR,
        VHDL
    ]).

%%--------------------------------------------------------------------

cache_dir(Source) ->
    Hash = crypto:hash(sha256, Source),
    Base64 = base64url:encode(Hash),
    cache_make_dir(filename:join("cache", Base64)).

%%--------------------------------------------------------------------

cache_make_dir(CacheDir) ->
    case file:make_dir(CacheDir) of
        ok ->
            CacheDir;

        {error, eexist} ->
            CacheDir
    end.

%%--------------------------------------------------------------------

cache_read_pof(CacheDir) ->
    File = filename:join(CacheDir, "experiment.pof"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_read_rcf(CacheDir) ->
    File = filename:join(CacheDir, "experiment.rcf"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_read_source(CacheDir) ->
    File = filename:join(CacheDir, "source"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_hit(CacheDir) ->
    {ok, {cache, hit, CacheDir}}.

%%--------------------------------------------------------------------

cache_miss(Source, CacheDir, Compile) ->
    case compile(Compile) of
        {ok, #{pof := POF, rcf := RCF}} ->
            cache_write_pof(CacheDir, POF),
            cache_write_rcf(CacheDir, RCF),
            cache_write_source(CacheDir, Source),
            {ok, {cache, miss, POF, RCF}};

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

cache_write_pof(CacheDir, POF) ->
    File = filename:join(CacheDir, "experiment.pof"),
    ok = file:write_file(File, POF).

%%--------------------------------------------------------------------

cache_write_rcf(CacheDir, RCF) ->
    File = filename:join(CacheDir, "experiment.rcf"),
    ok = file:write_file(File, RCF).

%%--------------------------------------------------------------------

cache_write_source(CacheDir, Source) ->
    File = filename:join(CacheDir, "source"),
    ok = file:write_file(File, Source).

%%====================================================================
%% pof
%%====================================================================

-spec pof(cache()) -> {ok, pof_file:pof()}.

pof({cache, hit, CacheDir}) ->
    {ok, POF} = cache_read_pof(CacheDir),
    pof_file:decode(POF);
pof({cache, miss, POF, _}) ->
    pof_file:decode(POF).

%%====================================================================
%% rcf
%%====================================================================

-spec rcf(cache()) -> {ok, pof_file:pof()}.

rcf({cache, hit, CacheDir}) ->
    {ok, RCF} = cache_read_rcf(CacheDir),
    RCF;
rcf({cache, miss, _, RCF}) ->
    RCF.

