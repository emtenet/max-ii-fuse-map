-module(experiment_server).

-export([submit/1]).
-export([pickup_check/1]).
-export([pickup_sleep/1]).

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

-export_type([submit_reply/0]).
-export_type([pickup_reply/0]).

-type cached() :: experiment:result().
-type job_ref() :: experiment_compile:job_ref().
-type result() :: experiment_compile:result().
-type source() :: experiment_compile:source().

-type submit_reply() ::
    {ok, cached()} |
    {pickup, job_ref()} |
    busy.

-type pickup_reply() ::
    {ok, #{job_ref() => result()}} |
    false.

-define(SUBMIT_TIMEOUT, 10000). % milliseconds

%%====================================================================
%% submit
%%====================================================================

-spec submit(source()) -> submit_reply().

submit(Source) ->
    gen_server:call(?MODULE, {submit, Source}).

%%====================================================================
%% pickup_check
%%====================================================================

-spec pickup_check([job_ref()]) -> pickup_reply().

pickup_check(Refs) ->
    gen_server:call(?MODULE, {pickup, Refs, 500}).

%%====================================================================
%% pickup_sleep
%%====================================================================

-spec pickup_sleep([job_ref()]) -> pickup_reply().

pickup_sleep(Refs) ->
    gen_server:call(?MODULE, {pickup, Refs, 4000}).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%%====================================================================
%% gen_server
%%====================================================================

-type pickup_ref() :: reference().
-type slot() :: experiment_cache:slot().

-record(submit, {
    slot :: slot(),
    pickup_ref :: pickup_ref() | undefined,
    timer_ref :: timer:tref()
}).

-record(pickup, {
    from :: gen_server:from(),
    timer_ref :: timer:tref()
}).

-record(state, {
    submits :: #{job_ref() => #submit{}},
    results :: #{job_ref() => result()},
    pickups :: #{pickup_ref() => #pickup{}}
}).

%%--------------------------------------------------------------------

init(undefined) ->
    State = #state{
        submits = #{},
        results = #{},
        pickups = #{}
    },
    {ok, State}.

%%--------------------------------------------------------------------

handle_call({submit, Source}, _From, State) ->
    submit(Source, State);
handle_call({pickup, JobRefs, Timeout}, From, State) ->
    pickup(JobRefs, Timeout, From, State);
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Cast, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({submit_result, JobRef, Result}, State) ->
    {noreply, submit_result(JobRef, Result, State)};
handle_info({submit_timeout, JobRef}, State) ->
    {noreply, submit_result(JobRef, {error, timeout}, State)};
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

-spec submit(source(), #state{}) -> {reply, submit_reply(), #state{}}.

submit(Source, State0) ->
    case experiment_cache:load(Source) of
        {hit, Cached} ->
            Reply = {ok, Cached},
            {reply, Reply, State0};

        {miss, Slot} ->
            experiment_compile:connect(),
            case submit_to_quartus(Source) of
                {ok, JobRef} ->
                    Reply = {pickup, JobRef},
                    State = submit_insert(JobRef, Slot, State0),
                    {reply, Reply, State};

                busy ->
                    {reply, busy, State0}
            end
    end.

%%--------------------------------------------------------------------

submit_to_quartus(Source) ->
    experiment_compile:connect(),
    gen_server:call({global, quartus}, {submit, Source, self()}).

%%--------------------------------------------------------------------

submit_insert(JobRef, Slot, State) ->
    TimeoutMessage = {submit_timeout, JobRef},
    {ok, TimerRef} = timer:send_after(?SUBMIT_TIMEOUT, TimeoutMessage),
    Submit = #submit{
        slot = Slot,
        pickup_ref = undefined,
        timer_ref = TimerRef
    },
    Submits = State#state.submits,
    State#state{
        submits = Submits#{JobRef => Submit}
    }.

%%====================================================================
%% submit_result
%%====================================================================

-spec submit_result(job_ref(), result(), #state{}) -> #state{}.

submit_result(JobRef, Result, State) ->
    case State#state.submits of
        #{JobRef := Submit} ->
            submit_result(JobRef, Result, Submit, State);

        _ ->
            State
    end.

%%--------------------------------------------------------------------

submit_result(JobRef, Result, Submit, State) ->
    _ = timer:cancel(Submit#submit.timer_ref),
    case Result of
        {ok, Files} ->
            experiment_cache:store(Submit#submit.slot, Files);

        _ ->
            ok
    end,
    case Submit#submit.pickup_ref of
        undefined ->
            result_store(JobRef, Result, State);

        PickupRef ->
            pickup_reply(PickupRef, JobRef, Result, State)
    end.

%%--------------------------------------------------------------------

result_store(JobRef, Result, State) ->
    Submits = State#state.submits,
    Results = State#state.results,
    State#state{
        submits = maps:remove(JobRef, Submits),
        results = Results#{JobRef => Result}
    }.

%%--------------------------------------------------------------------

pickup_reply(PickupRef, JobRef, Result, State) ->
    case State#state.pickups of
        #{PickupRef := #pickup{from = From, timer_ref = TimerRef}} ->
            _ = timer:cancel(TimerRef),
            ok = gen_server:reply(From, {ok, #{JobRef => Result}}),
            State#state{
                submits = maps:remove(JobRef, State#state.submits),
                pickups = maps:remove(PickupRef, State#state.pickups)
            };

        _ ->
            result_store(JobRef, Result, State)
    end.

%%====================================================================
%% pickup
%%====================================================================

-spec pickup([job_ref()], pos_integer(), gen_server:from(), #state{})
    -> {reply, pickup_reply(), #state{}} |
       {no_reply, #state{}}.

pickup(JobRefs, Timeout, From, State0 = #state{}) ->
    case pickup_collect(JobRefs, State0) of
        {ok, Replies, State} ->
            {reply, {ok, Replies}, State};

        false ->
            PickupRef = make_ref(),
            TimeoutMessage = {pickup_timeout, PickupRef},
            {ok, TimerRef} = timer:send_after(Timeout, TimeoutMessage),
            Pickup = #pickup{
                from = From,
                timer_ref = TimerRef
            },
            Submits = State0#state.submits,
            Pickups = State0#state.pickups,
            State = State0#state{
                submits = pickup_register(JobRefs, PickupRef, Submits),
                pickups = Pickups#{PickupRef => Pickup}
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
pickup_collect([JobRef | JobRefs], State0 = #state{}, Replies) ->
    Results = State0#state.results,
    case Results of
        #{JobRef := Result} ->
            State = State0#state{results = maps:remove(JobRef, Results)},
            pickup_collect(JobRefs, State, Replies#{JobRef => Result});

        _ ->
            pickup_collect(JobRefs, State0, Replies)
    end.

%%--------------------------------------------------------------------

pickup_register([], _, Submits) ->
    Submits;
pickup_register([JobRef | JobRefs], PickupRef, Submits) ->
    case Submits of
        #{JobRef := Submit = #submit{}} ->
            pickup_register(JobRefs, PickupRef, Submits#{
                JobRef => Submit#submit{pickup_ref = PickupRef}
            });

        _ ->
            pickup_register(JobRefs, PickupRef, Submits)
    end.

%%====================================================================
%% pickup_timeout
%%====================================================================

-spec pickup_timeout(pickup_ref(), #state{}) -> #state{}.

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

