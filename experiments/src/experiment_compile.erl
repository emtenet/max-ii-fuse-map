-module(experiment_compile).

-export([prepare/1]).
-export([connect/0]).
-export([request/1]).
-export([response/1]).

-export_type([files/0]).
-export_type([response/0]).
-export_type([source/0]).

-type compile() :: experiment:compile().

-type files() :: #{
    pof := binary(),
    rcf := binary()
}.

-type response() :: {ok, files()} | {error, term()}.

-type source() :: #{
    title := experiment:title(),
    device := binary(),
    settings := binary(),
    vhdl := binary()
}.

%%====================================================================
%% prepare
%%====================================================================

-spec prepare(compile()) -> source().

prepare(#{title := Title, device := Device, settings := Settings, vhdl := VHDL})
        when is_atom(Device) andalso
             is_list(Settings) andalso
             is_binary(VHDL) ->
    #{
        title => Title,
        device => device:name(Device),
        settings => setting:encode(Settings),
        vhdl => VHDL
    }.

%%====================================================================
%% connect
%%====================================================================

-spec connect() -> ok.

connect() ->
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

%%====================================================================
%% request
%%====================================================================

-spec request(source()) -> response().

request(Source) ->
    gen_server:call({global, quartus}, {compile, Source}, 10000).

%%====================================================================
%% response
%%====================================================================

-spec response(response()) -> {ok, files()} | error.

response({ok, Files}) ->
    {ok, Files};
response({error, {quartus_map, Exit, Out}}) ->
    response_out("MAP", Exit, Out);
response({error, {quartus_fit, Exit, Out}}) ->
    response_out("FIT", Exit, Out);
response({error, {quartus_asm, Exit, Out}}) ->
    response_out("ASM", Exit, Out);
response({error, {quartus_cdb, Exit, Out}}) ->
    response_out("CDB", Exit, Out);
response({error, Error}) ->
    io:format("QUARTUS: ~p~n", [Error]),
    error.

%%--------------------------------------------------------------------

response_out(Who, Exit, Out) ->
    io:format("========================================~n", []),
    io:format("~s~n", [Out]),
    io:format("========================================~n", []),
    io:format("QUARTUS ~s: exit # ~p~n", [Who, Exit]),
    error.

