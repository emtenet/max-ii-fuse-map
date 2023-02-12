-module(quartus).

-export([submit/1]).

% application
-export([child_spec/0]).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-export_type([job_ref/0]).

-define(JOB_LIMIT, 16).

-type source() :: quartus_compile:source().
-type result() :: quartus_compile:result().

-type job_ref() :: reference().

%%====================================================================
%% submit
%%====================================================================

-spec submit(source()) -> {ok, job_ref()} | busy.

submit(Source) ->
    gen_server:call(?MODULE, {submit, Source}).

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

-type dir() :: string().
-type monitor_ref() :: reference().

-record(job, {
    from :: pid(),
    dir :: dir(),
    monitor :: monitor_ref()
}).

-record(state, {
    dir :: dir(),
    dirs :: #{dir() => job_ref()},
    jobs :: #{job_ref() => #job{}},
    monitors :: #{monitor_ref() => job_ref()}
}).

%%--------------------------------------------------------------------

init(undefined) ->
    State = #state{
        dir = dir_first(),
        dirs = #{},
        jobs = #{},
        monitors = #{}
    },
    {ok, State}.

%%--------------------------------------------------------------------

handle_call({submit, Source, From}, _From, State) ->
    submit(Source, From, self(), State);
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Cast, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'DOWN', MonitorRef, process, _, _}, State = #state{}) ->
    case State#state.monitors of
        #{MonitorRef := JobRef} ->
            {noreply, submit_result(JobRef, {error, 'DOWN'}, State)};

        _ ->
            {noreply, State}
    end;
handle_info({submit_result, JobRef, Result}, State) ->
    {noreply, submit_result(JobRef, Result, State)};
handle_info(Info, State) ->
    io:format("info ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% submit
%%====================================================================

-spec submit(source(), pid(), pid(), #state{}) -> {reply, {ok, job_ref()} | busy, #state{}}.

submit(_, _From, _Self, State = #state{jobs = Jobs}) when map_size(Jobs) >= ?JOB_LIMIT ->
    {reply, busy, State};
submit(Source, From, Self, State = #state{dir = Dir0, dirs = Dirs}) ->
    Dir = dir_next_free(Dir0, Dirs),
    JobRef = make_ref(),
    {_Pid, MonitorRef} = spawn_monitor(fun () ->
        Self ! {submit_result, JobRef, quartus_compile:in_dir(Dir, Source)}
    end),
    Jobs = State#state.jobs,
    Monitors = State#state.monitors,
    Job = #job{
        from = From,
        dir = Dir,
        monitor = MonitorRef
    },
    {reply, {ok, JobRef}, State#state{
        dir = dir_next(Dir),
        dirs = Dirs#{Dir => JobRef},
        jobs = Jobs#{JobRef => Job},
        monitors = Monitors#{MonitorRef => JobRef}
    }}.

%%====================================================================
%% submit_result
%%====================================================================

-spec submit_result(job_ref(), result(), #state{}) -> #state{}.

submit_result(JobRef, Result, State = #state{}) ->
    #{JobRef := Job} = State#state.jobs,
    true = demonitor(Job#job.monitor, [flush]),
    Job#job.from ! {submit_result, JobRef, Result},
    State#state{
        dirs = maps:remove(Job#job.dir, State#state.dirs),
        jobs = maps:remove(JobRef, State#state.jobs),
        monitors = maps:remove(Job#job.monitor, State#state.monitors)
    }.

%%====================================================================
%% dirs
%%====================================================================

dir_first() ->
    "a".

%%--------------------------------------------------------------------

dir_next("z") ->
    "a";
dir_next([C]) ->
    [C + 1].

%%--------------------------------------------------------------------

dir_next_free(Dir, Dirs) when is_map_key(Dir, Dirs) ->
    dir_next_free(dir_next(Dir), Dirs);
dir_next_free(Dir, _) ->
    Dir.
