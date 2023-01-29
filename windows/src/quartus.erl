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

handle_call(ping, From, State) ->
    io:format("ping from ~p~n", [From]),
    {reply, pong, State};
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
