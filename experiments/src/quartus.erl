-module(quartus).

-export([compile/1]).

%%====================================================================
%% compile
%%====================================================================

compile(Request = #{device := Device, settings := Settings, vhdl := VHDL})
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    connect_to_windows(),
    compile_result(gen_server:call({global, quartus}, {compile, Request})).

%%--------------------------------------------------------------------

connect_to_windows() ->
    case nodes() of
        [] ->
            pong = net_adm:ping('quartus@SILENT-PC'),
            ok;

        [_] ->
            ok
    end.

%%--------------------------------------------------------------------

compile_result({ok, POF}) ->
    pof_file:decode(POF);
compile_result({error, {quartus_map, Exit, Out}}) ->
    compile_exit("MAP", Exit, Out);
compile_result({error, {quartus_fit, Exit, Out}}) ->
    compile_exit("FIT", Exit, Out);
compile_result({error, {quartus_asm, Exit, Out}}) ->
    compile_exit("ASM", Exit, Out);
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

