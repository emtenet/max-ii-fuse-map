-module(experiment).

-export([compile/1]).
-export([compile_to_fuses/1]).
-export([compile_to_fuses_and_rcf/1]).
-export([flush/1]).
-export([pof/1]).
-export([rcf/1]).

-export_type([compile/0]).
-export_type([result/0]).
-export_type([title/0]).

-type compile() :: #{
    title := title(),
    device := device(),
    settings := [setting()],
    vhdl := binary()
}.

-type device() :: device:device().

-type result() ::
    {cached, file:filename_all()} |
    {compiled, binary(), binary()}.

-type fuses() :: [fuse:fuse()].
-type setting() :: setting:setting().
-type title() :: term().

%%====================================================================
%% compile
%%====================================================================

-spec compile(compile()) -> {ok, result()} | error;
             ([compile()]) -> {ok, [result()]} | error.

compile(Compile) when is_map(Compile) ->
    Source = experiment_compile:prepare(Compile),
    case compile_sources([Source]) of
        {ok, [Result]} ->
            {ok, Result};

        error ->
            error
    end;
compile(Compiles) when is_list(Compiles) ->
    Sources = lists:map(fun experiment_compile:prepare/1, Compiles),
    compile_sources(Sources).

%%--------------------------------------------------------------------

compile_sources(Sources) ->
    %Start = erlang:system_time(millisecond),
    Answer = gen_server:call(experiment_server, {compile, Sources}, 20000),
    %Stop = erlang:system_time(millisecond),
    %io:format(" -> ~ps~n", [(Stop - Start) / 1000]),
    Answer.

%%====================================================================
%% compile_to_fuses
%%====================================================================

-spec compile_to_fuses([compile()]) -> {ok, [{title(), fuses()}]} | error.

compile_to_fuses(Compiles) when is_list(Compiles) ->
    case compile(Compiles) of
        {ok, Results} ->
            compile_to_fuses(Compiles, Results, []);

        error ->
            error
    end.

%%--------------------------------------------------------------------

compile_to_fuses([], [], Answers) ->
    {ok, lists:reverse(Answers)};
compile_to_fuses([Compile | Compiles], [Result | Results], Answers) ->
    #{title := Title} = Compile,
    {ok, Fuses} = fuses(Result),
    Answer = {Title, Fuses},
    compile_to_fuses(Compiles, Results, [Answer | Answers]).

%%====================================================================
%% compile_to_fuses_and_rcf
%%====================================================================

-spec compile_to_fuses_and_rcf([compile()])
    -> {ok, [{title(), fuses(), rcf_file:rcf()}]} | error.

compile_to_fuses_and_rcf(Compiles) when is_list(Compiles) ->
    case compile(Compiles) of
        {ok, Results} ->
            compile_to_fuses_and_rcf(Compiles, Results, []);

        error ->
            error
    end.

%%--------------------------------------------------------------------

compile_to_fuses_and_rcf([], [], Answers) ->
    {ok, lists:reverse(Answers)};
compile_to_fuses_and_rcf([Compile | Compiles], [Result | Results], Answers) ->
    #{title := Title} = Compile,
    {ok, Fuses} = fuses(Result),
    {ok, RCF} = rcf(Result),
    Answer = {Title, Fuses, RCF},
    compile_to_fuses_and_rcf(Compiles, Results, [Answer | Answers]).

%%====================================================================
%% flush
%%====================================================================

-spec flush(compile()) -> ok;
           ([compile()]) -> ok.

flush(Compile) when is_map(Compile) ->
    Source = experiment_compile:prepare(Compile),
    experiment_cache:flush(Source);
flush(Compiles) when is_list(Compiles) ->
    Sources = lists:map(fun experiment_compile:prepare/1, Compiles),
    lists:foreach(fun experiment_cache:flush/1, Sources).

%%====================================================================
%% fuses
%%====================================================================

-spec fuses(result()) -> {ok, fuses()}.

fuses(Result) ->
    {ok, POF} = pof(Result),
    {ok, pof_file:fuses(POF)}.

%%====================================================================
%% pof
%%====================================================================

-spec pof(result()) -> {ok, pof_file:pof()}.

pof({cached, Dir}) ->
    {ok, POF} = experiment_cache:read_pof(Dir),
    pof_file:decode(POF);
pof({compiled, POF, _}) ->
    pof_file:decode(POF).

%%====================================================================
%% rcf
%%====================================================================

-spec rcf(result()) -> {ok, rcf_file:rcf()}.

rcf({cached, Dir}) ->
    {ok, RCF} = experiment_cache:read_rcf(Dir),
    rcf_file:decode(RCF);
rcf({compiled, _, RCF}) ->
    rcf_file:decode(RCF).

