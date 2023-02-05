-module(quartus_app).

-export([start/0]).

-behaviour(application).
-export([start/2]).
-export([stop/1]).


%%====================================================================
%% public
%%====================================================================

start() ->
	application:start(quartus).

%%====================================================================
%% application
%%====================================================================

start(_Type, _Args) ->
    {ok, Sup} = quartus_sup:start_link(),
    {ok, Sup}.

%%--------------------------------------------------------------------

stop(_State) ->
    ok.
