-module(quartus).

-export([submit/1]).
-export([pickup/2]).

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
%% pickup
%%====================================================================

-spec pickup([job_ref()], timer:time()) -> {ok, #{job_ref() => result()}} | false.

pickup(JobRefs, Timeout) ->
    gen_server:call(?MODULE, {pickup, JobRefs, Timeout}).

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
-type pickup_ref() :: reference().
-type timer_ref() :: timer:tref().

-record(job, {
    dir :: dir(),
    monitor :: monitor_ref(),
    pickup_ref :: pickup_ref() | undefined
}).

-record(pickup, {
    from :: gen_server:from(),
    timer_ref :: timer_ref()
}).

-record(state, {
    dir :: dir(),
    dirs :: #{dir() => job_ref()},
    jobs :: #{job_ref() => #job{}},
    monitors :: #{monitor_ref() => job_ref()},
    results :: #{job_ref() => result()},
    pickups :: #{pickup_ref() => #pickup{}}
}).

%%--------------------------------------------------------------------

init(undefined) ->
    State = #state{
        dir = dir_first(),
        dirs = #{},
        jobs = #{},
        monitors = #{},
        pickups = #{}
    },
    {ok, State}.

%%--------------------------------------------------------------------

handle_call({submit, Source}, _From, State) ->
    submit(Source, self(), State);
handle_call({pickup, JobRefs, Timeout}, From, State) ->
    pickup(JobRefs, Timeout, From, State);
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Cast, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'DOWN', MonitorRef, process, _, _}, State = #state{}) ->
    case State#state.monitors of
        #{MonitorRef := JobRef} ->
            {noreply, compile_done(JobRef, {error, 'DOWN'}, State)};

        _ ->
            {noreply, State}
    end;
handle_info({compile_done, JobRef, Result}, State) ->
    {noreply, compile_done(JobRef, Result, State)};
handle_info({pickup_timeout, PickupRef}, State) ->
    {noreply, pickup_timeout(PickupRef, State)};
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

submit(_, _Self, State = #state{jobs = Jobs}) when map_size(Jobs) >= ?JOB_LIMIT ->
    {reply, busy, State};
submit(Source, Self, State = #state{dir = Dir0, dirs = Dirs}) ->
    Dir = dir_next_free(Dir0, Dirs),
    JobRef = make_ref(),
    {_Pid, MonitorRef} = spawn_monitor(fun () ->
        Self ! {compile_done, JobRef, quartus_compile:in_dir(Dir, Source)}
    end),
    Jobs = State#state.jobs,
    Monitors = State#state.monitors,
    Job = #job{
        dir = Dir,
        monitor = MonitorRef,
        pickup_ref = undefined
    },
    {reply, {ok, JobRef}, State#state{
        dir = dir_next(Dir),
        dirs = Dirs#{Dir => JobRef},
        jobs = Jobs#{JobRef => Job},
        monitors = Monitors#{MonitorRef => JobRef}
    }}.

%%====================================================================
%% compile_done
%%====================================================================

compile_done(JobRef, Result, State0 = #state{}) ->
    #{JobRef := Job} = State0#state.jobs,
    true = demonitor(Job#job.monitor, [flush]),
    State = State0#state{
        dirs = maps:remove(Job#job.dir, State0#state.dirs),
        jobs = maps:remove(JobRef, State0#state.jobs),
        monitors = maps:remove(Job#job.monitor, State0#state.monitors)
    },
    case Job#job.pickup_ref of
        undefined ->
            State#state{
                results = maps:put(JobRef, Result, State#state.results)
            };

        PickupRef ->
            compile_pickup(PickupRef, JobRef, Result, State)
    end.

%%--------------------------------------------------------------------

compile_pickup(PickupRef, JobRef, Result, State = #state{}) ->
    case State#state.pickups of
        #{PickupRef := #pickup{from = From, timer_ref = TimerRef}} ->
            ok = gen_server:reply(From, {ok, #{JobRef => Result}}),
            timer:cancel(TimerRef),
            State#state{
                pickups = maps:remove(PickupRef, State#state.pickups)
            };

        _ ->
            State#state{
                results = maps:put(JobRef, Result, State#state.results)
            }
    end.

%%====================================================================
%% pickup
%%====================================================================

pickup(JobRefs, Timeout, From, State0 = #state{}) ->
    case pickup_collect(JobRefs, State0) of
        {ok, Replies, State} ->
            {reply, {ok, Replies}, State};

        false ->
            PickupRef = make_ref(),
            {ok, TimerRef} = timer:send_after(Timeout, {pickup_timeout, PickupRef}),
            Pickup = #pickup{
                from = From,
                timer_ref = TimerRef
            },
            State = State0#state{
                jobs = pickup_register(JobRefs, PickupRef, State0#state.jobs),
                pickups = maps:put(PickupRef, Pickup, State0#state.pickups)
            },
            {noreply, State}
    end.

%%--------------------------------------------------------------------

pickup_collect(JobRefs, State) ->
    pickup_collect(JobRefs, State, #{}).

%%--------------------------------------------------------------------

pickup_collect([], _State, Replies) when map_size(Replies) =:= 0 ->
    false;
pickup_collect([], State, Replies)  ->
    {ok, Replies, State};
pickup_collect([JobRef | JobRefs], State0 = #state{results = Results}, Replies) ->
    case Results of
        #{JobRef := Result} ->
            State = State0#state{results = maps:remove(JobRef, Results)},
            pickup_collect(JobRefs, State, Replies#{JobRef => Result});

        _ ->
            pickup_collect(JobRefs, State0, Replies)
    end.

%%--------------------------------------------------------------------

pickup_register([], _, Jobs) ->
    Jobs;
pickup_register([JobRef | JobRefs], PickupRef, Jobs) ->
    case Jobs of
        #{JobRef := Job = #job{}} ->
            pickup_register(JobRefs, PickupRef, Jobs#{
                JobRef => Job#job{pickup_ref = PickupRef}
            });

        _ ->
            pickup_register(JobRefs, PickupRef, Jobs)
    end.

%%====================================================================
%% pickup_timeout
%%====================================================================

pickup_timeout(PickupRef, State) ->
    case State#state.pickups of
        #{PickupRef := #pickup{from = From}} ->
            ok = gen_server:reply(From, false),
            State#state{
                pickups = maps:remove(PickupRef, State#state.pickups)
            };

        _ ->
            State
    end.

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
