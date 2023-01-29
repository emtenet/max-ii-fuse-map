-module(quartus).

-export([compile/3]).

%%====================================================================
%% compile
%%====================================================================

compile(Device, Settings, VHDL)
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    connect_to_windows(),
    {ok, POF} = gen_server:call({global, quartus}, {compile, #{
        device => Device,
        settings => Settings,
        vhdl => VHDL
    }}),
    pof_file:decode(POF).

%%--------------------------------------------------------------------

connect_to_windows() ->
    case nodes() of
        [] ->
            pong = net_adm:ping('quartus@SILENT-PC'),
            ok;

        [_] ->
            ok
    end.

