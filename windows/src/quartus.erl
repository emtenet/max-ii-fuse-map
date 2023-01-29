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
    {reply, compile(Compile), State};
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

compile(#{device := Device, settings := Settings, vhdl := VHDL})
        when is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(VHDL) ->
    RunIn = "a",
    RunDir = filename:join("run", RunIn),
    make_dir(RunDir),
    clear_dir(RunDir),
    make_file(RunDir, "experiment.qpf", <<
        "QUARTUS_VERSION = \"13.1\"\n"
        "DATE = \"11:38:48  January 28, 2023\"\n"
        "PROJECT_REVISION = \"experiment\"\n"
    >>),
    make_file(RunDir, "experiment.qsf", <<
        "set_global_assignment -name FAMILY \"MAX II\"\n"
        "set_global_assignment -name DEVICE ", Device/binary, "\n"
        "set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files\n"
        "set_global_assignment -name VHDL_FILE experiment.vhd\n"
        "set_global_assignment -name TOP_LEVEL_ENTITY experiment\n",
        Settings/binary
    >>),
    make_file(RunDir, "experiment.vhd", VHDL),
    ok = exec(RunDir, "quartus_map", [
        "experiment",
        "--source=experiment.vhd",
        "--family=MAX II"
    ]),
    ok = exec(RunDir, "quartus_fit", [
        "experiment",
        "--part=" ++ binary_to_list(Device)
    ]),
    ok = exec(RunDir, "quartus_asm", [
        "experiment"
    ]),
    {ok, POF} = read_file(RunDir, ["output_files", "experiment.pof"]),
    {ok, POF};
compile(_) ->
    {error, badarg}.

%%====================================================================
%% helpers
%%====================================================================

clear_dir(Dir) ->
    {ok, Names} = file:list_dir_all(Dir),
    lists:foreach(fun (Name) ->
        clear_file(filename:join(Dir, Name))
    end, Names).

%%--------------------------------------------------------------------

clear_file(File) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            clear_dir(File),
            ok = file:del_dir(File);

        {ok, _} ->
            ok = file:delete(File)
    end.

%%--------------------------------------------------------------------

exec(RunDir, Arg0, Args) ->
    Path = "C:\\Applications\\Altera\\13.1\\quartus\\bin64",
    Exec = {spawn_executable, filename:join(Path, Arg0)},
    Opts = [
        {args, Args},
        {cd, RunDir},
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
            {error, Exit, Out}
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

%%--------------------------------------------------------------------

make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%%--------------------------------------------------------------------

make_file(Dir, Name, Data) ->
    File = filename:join(Dir, Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

read_file(Dir, Path) ->
    File = filename:join([Dir | Path]),
    file:read_file(File).
