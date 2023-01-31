-module(quartus).

-export([compile/1]).

-define(SEPARATOR, <<"\n====================\n">>).

%%====================================================================
%% compile
%%====================================================================

compile(Request = #{device := Device, settings := Settings, vhdl := VHDL})
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    With = cache_with(Device, Settings, VHDL),
    CacheDir = cache_dir(With),
    case cache_read_with(CacheDir) of
        {ok, With} ->
            cache_hit(CacheDir);

        {error, enoent} ->
            cache_miss(With, CacheDir, Request)
    end.

%%--------------------------------------------------------------------

compile_request(Request) ->
    compile_connect(),
    Response = gen_server:call({global, quartus}, {compile, Request}, 10000),
    compile_response(Response).

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

compile_response({ok, POF}) ->
    {ok, POF};
compile_response({error, {quartus_map, Exit, Out}}) ->
    compile_exit("MAP", Exit, Out);
compile_response({error, {quartus_fit, Exit, Out}}) ->
    compile_exit("FIT", Exit, Out);
compile_response({error, {quartus_asm, Exit, Out}}) ->
    compile_exit("ASM", Exit, Out);
compile_response({error, Error}) ->
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

cache_with(Device, Settings, VHDL) ->
    iolist_to_binary([
        Device,
        <<"\n">>,
        Settings,
        ?SEPARATOR,
        VHDL
    ]).

%%--------------------------------------------------------------------

cache_dir(With) ->
    Hash = crypto:hash(sha256, With),
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

cache_read_with(CacheDir) ->
    File = filename:join(CacheDir, "with"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_hit(CacheDir) ->
    {ok, POF} = cache_read_pof(CacheDir),
    pof_file:decode(POF).

%%--------------------------------------------------------------------

cache_miss(With, CacheDir, Request) ->
    case compile_request(Request) of
        {ok, POF} ->
            cache_write_pof(CacheDir, POF),
            cache_write_with(CacheDir, With),
            pof_file:decode(POF);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

cache_write_pof(CacheDir, POF) ->
    File = filename:join(CacheDir, "experiment.pof"),
    ok = file:write_file(File, POF).

%%--------------------------------------------------------------------

cache_write_with(CacheDir, With) ->
    File = filename:join(CacheDir, "with"),
    ok = file:write_file(File, With).

