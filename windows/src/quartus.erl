-module(quartus).

-export([child_spec/0]).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% application
%%====================================================================

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []}
    }.

%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, undefined, []).

%%====================================================================
%% gen_server
%%====================================================================

-record(state, {
}).

%%--------------------------------------------------------------------

init(undefined) ->
    State = #state{},
    {ok, State}.

%%--------------------------------------------------------------------

handle_call({compile, Compile}, _From, State) ->
    {reply, compile("a", Compile), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Cast, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% internal
%%====================================================================

compile(InDir, Compile = #{device := Device, settings := Settings, vhdl := VHDL})
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    case Compile of
        #{title := Title} ->
            io:format("[~s] ~s ~s~n", [InDir, Device, Title]);

        _ ->
            io:format("[~s] ~s~n", [InDir, Device])
    end,
    Dir = filename:join("run", InDir),
    case file:make_dir(Dir) of
        ok ->
            compile_clear(Dir, Device, Settings, VHDL);

        {error, eexist} ->
            compile_clear(Dir, Device, Settings, VHDL);

        {error, Reason} ->
            {error, {make_dir, Reason}}
    end;
compile(_, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------

compile_clear(Dir, Device, Settings, VHDL) ->
    case clear_dir(Dir) of
        ok ->
            compile_qpf(Dir, Device, Settings, VHDL);

        {error, Reason} ->
            {error, {clear_dir, Reason}}
    end.

%%--------------------------------------------------------------------

compile_qpf(Dir, Device, Settings, VHDL) ->
    File = filename:join(Dir, "experiment.qpf"),
    Data = <<
        "QUARTUS_VERSION = \"13.1\"\n"
        "DATE = \"11:38:48  January 28, 2023\"\n"
        "PROJECT_REVISION = \"experiment\"\n"
    >>,
    case file:write_file(File, Data) of
        ok ->
            compile_qsf(Dir, Device, Settings, VHDL);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

compile_qsf(Dir, Device, Settings, VHDL) ->
    File = filename:join(Dir, "experiment.qsf"),
    Data = <<
        "set_global_assignment -name FAMILY \"MAX II\"\n"
        "set_global_assignment -name DEVICE ", Device/binary, "\n"
        "set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files\n"
        "set_global_assignment -name VHDL_FILE experiment.vhd\n"
        "set_global_assignment -name TOP_LEVEL_ENTITY experiment\n",
        Settings/binary
    >>,
    case file:write_file(File, Data) of
        ok ->
            compile_vhd(Dir, Device, VHDL);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

compile_vhd(Dir, Device, VHDL) ->
    File = filename:join(Dir, "experiment.vhd"),
    case file:write_file(File, VHDL) of
        ok ->
            compile_map(Dir, Device);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

compile_map(Dir, Device) ->
    Bin = "quartus_map",
    Args = [
        "experiment",
        "--source=experiment.vhd",
        "--family=MAX II"
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            compile_fit(Dir, Device);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_map, Exit, Out}}
    end.

%%--------------------------------------------------------------------

compile_fit(Dir, Device) ->
    Bin = "quartus_fit",
    Args = [
        "experiment",
        "--part=" ++ binary_to_list(Device)
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            compile_asm(Dir);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_fit, Exit, Out}}
    end.

%%--------------------------------------------------------------------

compile_asm(Dir) ->
    Bin ="quartus_asm",
    Args = [
        "experiment"
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            compile_read(Dir);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_asm, Exit, Out}}
    end.

%%--------------------------------------------------------------------

compile_read(Dir) ->
    File = filename:join([Dir, "output_files", "experiment.pof"]),
    case file:read_file(File) of
        Ok = {ok, _} ->
            Ok;

        {error, Reason} ->
            {error, {pof_file, Reason}}
    end.

%%====================================================================
%% helpers
%%====================================================================

clear_dir(Dir) ->
    case file:list_dir_all(Dir) of
        {ok, Names} ->
            clear_dir(Dir, Names);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

clear_dir(_, []) ->
    ok;
clear_dir(Dir, [Name | Names]) ->
    case clear_file(filename:join(Dir, Name)) of
        ok ->
            clear_dir(Dir, Names);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

clear_file(File) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            case clear_dir(File) of
                ok ->
                    file:del_dir(File);

                Error ->
                    Error
            end;

        {ok, _} ->
            file:delete(File);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

exec(Dir, Arg0, Args) ->
    Path = "C:\\Applications\\Altera\\13.1\\quartus\\bin64",
    Exec = {spawn_executable, filename:join(Path, Arg0)},
    Opts = [
        {args, Args},
        {cd, Dir},
        stream,
        exit_status,
        use_stdio,
        binary,
        eof
    ],
    Port = erlang:open_port(Exec, Opts),
    Result = exec(Port, []),
    erlang:port_close(Port),
    case Result of
        {0, _} ->
            ok;

        {Exit, Out} ->
            %io:format("[[[[~n~s~n]]]]~nEXIT: ~p~n", [Out, Exit]),
            {error, {exit, Exit, Out}}
    end.

%%--------------------------------------------------------------------

exec(Port, Out) ->
    receive
        {Port, eof} ->
            receive
                {Port, {exit_status, Exit}} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {exit_status, Exit}} ->
            receive
                {Port, eof} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {data, Data}} ->
            exec(Port, [Data | Out])
    end.
