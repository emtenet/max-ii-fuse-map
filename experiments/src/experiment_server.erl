-module(experiment_server).

-export([child_spec/0]).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-type response() :: experiment_compile:response().
-type result() :: experiment:result().
-type source() :: experiment_compile:source().

-define(LIMIT, 8).

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

-type request_ref() :: reference().
-type compile_ref() :: reference().
-type monitor_ref() :: reference().

-type index() :: non_neg_integer().
-type reply() :: {ok, result()} | error.

-record(request, {
    from :: gen_server:from(),
    size :: non_neg_integer(),
    replies :: #{index() => reply()}
}).

-record(queue, {
    request :: request_ref(),
    source :: source(),
    index :: index(),
    slot :: experiment_cache:slot()
}).

-record(compile, {
    request :: request_ref(),
    monitor :: monitor_ref(),
    index :: index(),
    slot :: experiment_cache:slot()
}).

-record(state, {
    requests :: #{request_ref() => #request{}},
    queue :: queue:queue(#queue{}),
    compiles :: #{compile_ref() => #compile{}},
    monitors :: #{monitor_ref() => compile_ref()}
}).

%%--------------------------------------------------------------------

init(undefined) ->
    State = #state{
        requests = #{},
        queue = queue:new(),
        compiles = #{},
        monitors = #{}
    },
    {ok, State}.

%%--------------------------------------------------------------------

handle_call({compile, Source}, From, State) ->
    {noreply, request(Source, self(), From, State)};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Cast, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({'DOWN', MonitorRef, process, _, _}, State0) ->
    State = compile_down(MonitorRef, State0),
    {noreply, compile_dequeue(self(), State)};
handle_info({compile_done, CompileRef, Response}, State0) ->
    Response = experiment_compile:response(Response),
    State = compile_done(CompileRef, Response, State0),
    {noreply, compile_dequeue(self(), State)};
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
%% internal
%%====================================================================

-spec request([source()], pid(), gen_server:from(), #state{}) -> #state{}.

request(Sources, Self, From, State0) when is_list(Sources) ->
    experiment_compile:connect(),
    RequestRef = make_ref(),
    case request_add(Sources, Self, RequestRef, 1, #{}, State0) of
        {Size, Replies, _} when Size =:= map_size(Replies) ->
            Reply = request_reply(Size, Replies, []),
            ok = gen_server:reply(From, Reply),
            State0;

        {Size, Replies, State} ->
            Request = #request{
                from = From,
                size = Size,
                replies = Replies
            },
            Requests = State#state.requests,
            State#state{
                requests = Requests#{RequestRef => Request}
            }
    end;
request(_, _, From, State) ->
    ok = gen_server:reply(From, badarg),
    State.

%%--------------------------------------------------------------------

request_add([], _, _, Index, Replies, State) ->
    {Index - 1, Replies, State};
request_add([Source | Sources], Self, RequestRef, Index, Replies0, State0) ->
    case experiment_cache:load(Source) of
        {hit, Compiled} ->
            Replies = Replies0#{Index => {ok, Compiled}},
            request_add(Sources, Self, RequestRef, Index + 1, Replies, State0);

        {miss, Slot} ->
            State = compile_add(Source, Self, RequestRef, Index, Slot, State0),
            request_add(Sources, Self, RequestRef, Index + 1, Replies0, State)
    end.

%%--------------------------------------------------------------------

compile_add(Source, Self, RequestRef, Index, Slot, State) ->
    case map_size(State#state.compiles) of
        Active when Active < ?LIMIT ->
            compile_start(Source, Self, RequestRef, Index, Slot, State);

        _ ->
            compile_queue(Source, RequestRef, Index, Slot, State)
    end.

%%--------------------------------------------------------------------

compile_start(Source, Self, RequestRef, Index, Slot, State) ->
    CompileRef = make_ref(),
    {_Pid, MonitorRef} = spawn_monitor(fun () ->
        Response = experiment_compile:request(Source),
        Self ! {compile_done, CompileRef, Response}
    end),
    Compile = #compile{
        request = RequestRef,
        monitor = MonitorRef,
        index = Index,
        slot = Slot
    },
    Compiles = State#state.compiles,
    State#state{
        compiles = Compiles#{CompileRef => Compile}
    }.

%%--------------------------------------------------------------------

compile_queue(Source, RequestRef, Index, Slot, State) ->
    In = #queue{
        request = RequestRef,
        source = Source,
        index = Index,
        slot = Slot
    },
    Queue = State#state.queue,
    State#state{
        queue = queue:in(In, Queue)
    }.

%%--------------------------------------------------------------------

-spec compile_dequeue(pid(), #state{}) -> #state{}.

compile_dequeue(Self, State0 = #state{compiles = Compiles})
        when map_size(Compiles) < ?LIMIT ->
    case queue:out(State0#state.queue) of
        {empty, _} ->
            State0;

        {{value, Out = #queue{}}, Queue} ->
            RequestRef = Out#queue.request,
            Source = Out#queue.source,
            Index = Out#queue.index,
            Slot = Out#queue.slot,
            State = State0#state{queue = Queue},
            compile_start(Source, Self, RequestRef, Index, Slot, State)
    end;
compile_dequeue(_, State) ->
    State.

%%--------------------------------------------------------------------

-spec compile_down(monitor_ref(), #state{}) -> #state{}.

compile_down(MonitorRef, State = #state{}) ->
    case State#state.monitors of
        #{MonitorRef := CompileRef} ->
            Response = {error, down},
            compile_done(CompileRef, Response, State);

        _ ->
            State
    end.

%%--------------------------------------------------------------------

-spec compile_done(compile_ref(), response(), #state{}) -> #state{}.

compile_done(CompileRef, Response, State = #state{}) ->
    #{CompileRef := Compile} = State#state.compiles,
    MonitorRef = Compile#compile.monitor,
    true = demonitor(MonitorRef, [flush]),
    RequestRef = Compile#compile.request,
    Index = Compile#compile.index,
    Slot = Compile#compile.slot,
    Reply = case experiment_compile:response(Response) of
        {ok, Files} ->
            {ok, experiment_cache:store(Slot, Files)};

        error ->
            error
    end,
    request_done(RequestRef, Index, Reply, State#state{
        compiles = maps:remove(CompileRef, State#state.compiles),
        monitors = maps:remove(MonitorRef, State#state.monitors)
    }).

%%--------------------------------------------------------------------

request_done(RequestRef, Index, Reply0, State = #state{}) ->
    #{RequestRef := Request} = Requests = State#state.requests,
    case Request#request.size of
        Size when Size =:= map_size(Request#request.replies) + 1 ->
            Replies = Request#request.replies,
            Reply = request_reply(Size, Replies#{Index => Reply0}, []),
            ok = gen_server:reply(Request#request.from, Reply),
            State#state{
                requests = maps:remove(RequestRef, Requests)
            };

        _ ->
            Replies = Request#request.replies,
            State#state{
                requests = Requests#{
                    RequestRef => Request#request{
                        replies = Replies#{Index => Reply0}
                    }
                }
            }
    end.

%%--------------------------------------------------------------------

request_reply(0, _, List) ->
    {ok, List};
request_reply(Index, Map, List) when is_map_key(Index, Map) ->
    #{Index := {ok, Reply}} = Map,
    request_reply(Index - 1, Map, [Reply | List]);
request_reply(_, _, _) ->
    error.

