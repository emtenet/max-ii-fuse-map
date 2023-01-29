-module(quartus_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

%%====================================================================
%% application
%%====================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link(?MODULE, undefined).

%%====================================================================
%% supervisor
%%====================================================================

-spec init(undefined)
    -> {ok, {supervisor:sup_flags(), list(supervisor:child_spec())}}.

init(undefined) ->
    Children = [
        quartus:child_spec()
    ],
    Flags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    {ok, {Flags, Children}}.
