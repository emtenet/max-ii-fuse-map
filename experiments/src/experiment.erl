-module(experiment).

-export([compile/1]).
-export([compile_to_fuses/1]).
-export([compile_to_fuses_and_rcf/1]).
-export([compile_to_rcf/1]).
-export([flush/1]).
-export([fuses/1]).
-export([pof/1]).
-export([rcf/1]).
-export([source/1]).

-export_type([compile/0]).
-export_type([fuses/0]).
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
    Source = experiment_compile:pre(Compile),
    submit_single(Source);
compile([]) ->
    {ok, []};
compile(Compiles) when is_list(Compiles) ->
    batch(Compiles).

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
%% compile_to_rcf
%%====================================================================

-spec compile_to_rcf([compile()]) -> {ok, [{title(), rcf_file:rcf()}]} | error.

compile_to_rcf(Compiles) when is_list(Compiles) ->
    case compile(Compiles) of
        {ok, Results} ->
            compile_to_rcf(Compiles, Results, []);

        error ->
            error
    end.

%%--------------------------------------------------------------------

compile_to_rcf([], [], Answers) ->
    {ok, lists:reverse(Answers)};
compile_to_rcf([Compile | Compiles], [Result | Results], Answers) ->
    #{title := Title} = Compile,
    {ok, RCF} = rcf(Result),
    Answer = {Title, RCF},
    compile_to_rcf(Compiles, Results, [Answer | Answers]).

%%====================================================================
%% flush
%%====================================================================

-spec flush(compile()) -> ok;
           ([compile()]) -> ok.

flush(Compile) when is_map(Compile) ->
    Source = experiment_compile:pre(Compile),
    experiment_cache:flush(Source);
flush(Compiles) when is_list(Compiles) ->
    Sources = lists:map(fun experiment_compile:pre/1, Compiles),
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

%%====================================================================
%% source
%%====================================================================

-spec source(result()) -> {ok, binary()}.

source({cached, Dir}) ->
    experiment_cache:read_source(Dir).

%%====================================================================
%% single
%%====================================================================

submit_single(Source) ->
    case experiment_server:submit(Source) of
        {ok, Cached} ->
            {ok, Cached};

        {pickup, JobRef} ->
            pickup_single(JobRef);

        busy ->
            submit_single(Source)
    end.

%%--------------------------------------------------------------------

pickup_single(JobRef) ->
    case experiment_server:pickup_sleep([JobRef]) of
        {ok, Replies} ->
            #{JobRef := Result} = Replies,
            experiment_compile:post(Result);

        false ->
            pickup_single(JobRef)
    end.

%%====================================================================
%% batch
%%====================================================================

-type index() :: non_neg_integer().
-type job_ref() :: experiment_compile:job_ref().
-type source() :: experiment_compile:source().

-record(batch, {
    count :: non_neg_integer(),
    answers :: #{index() => term()},
    answer_index :: index(),
    results :: #{index() => experiment_compile:result()},
    pickups :: #{job_ref() => index()},
    source :: source() | undefined,
    source_index :: -1 | index(),
    compiles :: [compile()]
}).

-define(PROGRESS_SIZE, 60).
-define(PROGRESS_DONE,
    <<"############################################################">>
).
-define(PROGRESS_SENT,
    <<"............................................................">>
).
-define(PROGRESS_TODO,
    <<"                                                            ">>
).

batch(Compiles) ->
    io:format("[~s]\r", [?PROGRESS_TODO]),
    Complete = batch(normal, #batch{
        count = length(Compiles),
        answers = #{},
        answer_index = 0,
        results = #{},
        pickups = #{},
        source = undefined,
        source_index = -1,
        compiles = Compiles
    }),
    io:format(" ~s \r", [?PROGRESS_TODO]),
    Complete.

%%--------------------------------------------------------------------

batch(_, Batch = #batch{count = Count, answer_index = Count}) ->
    batch_progress(Batch),
    Count = map_size(Batch#batch.answers),
    0 = map_size(Batch#batch.results),
    0 = map_size(Batch#batch.pickups),
    Count = Batch#batch.source_index + 1,
    [] = Batch#batch.compiles,
    batch_collect(Count, Batch#batch.answers, []);
batch(State, Batch = #batch{answer_index = Index})
        when is_map_key(Index, Batch#batch.answers) ->
    batch(State, Batch#batch{answer_index = Index + 1});
batch(State, Batch = #batch{answer_index = Index})
        when is_map_key(Index, Batch#batch.results) ->
    {Result, Results} = maps:take(Index, Batch#batch.results),
    case experiment_compile:post(Result) of
        {ok, Answer} ->
            Answers = Batch#batch.answers,
            batch(State, Batch#batch{
                answers = Answers#{Index => Answer},
                answer_index = Index + 1,
                results = Results
            });

        error ->
            error
    end;
batch(normal, Batch = #batch{compiles = [Compile | Compiles]})
        when Batch#batch.source =:= undefined ->
    Source = experiment_compile:pre(Compile),
    batch(normal, Batch#batch{
        source = Source,
        source_index = Batch#batch.source_index + 1,
        compiles = Compiles
    });
batch(normal, Batch = #batch{source = Source}) when Source =/= undefined ->
    Index = Batch#batch.source_index,
    case experiment_server:submit(Source) of
        {ok, Answer} ->
            batch_got_answer(Index, Answer, Batch);

        {pickup, JobRef} ->
            batch_got_pickup(Index, JobRef, Batch);

        busy ->
            batch(busy, Batch)
    end;
batch(normal, Batch = #batch{pickups = Pickups}) ->
    JobRefs = [_ | _] = maps:keys(Pickups),
    case experiment_server:pickup_sleep(JobRefs) of
        {ok, Results} ->
            batch_got_results(maps:to_list(Results), Batch);

        false ->
            %io:format("waiting...                                \n", []),
            %batch_progress(Batch),
            batch(normal, Batch)
    end;
batch(busy, Batch = #batch{pickups = Pickups}) ->
    JobRefs = [_ | _] = maps:keys(Pickups),
    case experiment_server:pickup_check(JobRefs) of
        {ok, Results} ->
            batch_got_results(maps:to_list(Results), Batch);

        false ->
            %io:format("waiting...                                \n", []),
            %batch_progress(Batch),
            batch(busy, Batch)
    end.

%%--------------------------------------------------------------------

batch_collect(0, _, Answers) ->
    {ok, Answers};
batch_collect(PreviousIndex, Map, Answers) ->
    Index = PreviousIndex - 1,
    #{Index := Answer} = Map,
    batch_collect(Index, Map, [Answer | Answers]).

%%--------------------------------------------------------------------

batch_got_answer(Index, Answer, Batch = #batch{answers = Answers}) ->
    AnswersIndex = case Batch#batch.answer_index of
        Index ->
            Index + 1;

        _ ->
            Batch#batch.answer_index
    end,
    batch_made_progress(Batch#batch{
        answers = Answers#{Index => Answer},
        answer_index = AnswersIndex,
        source = undefined
    }).

%%--------------------------------------------------------------------

batch_got_pickup(Index, JobRef, Batch = #batch{pickups = Pickups}) ->
    batch_made_progress(Batch#batch{
        pickups = Pickups#{JobRef => Index},
        source = undefined
    }).

%%--------------------------------------------------------------------

batch_got_results([], Batch) ->
    batch_made_progress(Batch);
batch_got_results([{JobRef, Result} | More], Batch) ->
    {Index, Pickups} = maps:take(JobRef, Batch#batch.pickups),
    Results = Batch#batch.results,
    batch_got_results(More, Batch#batch{
        results = Results#{Index => Result},
        pickups = Pickups
    }).

%%--------------------------------------------------------------------

batch_made_progress(Batch) ->
    batch_progress(Batch),
    batch(normal, Batch).

%%--------------------------------------------------------------------

batch_progress(Batch = #batch{count = Count}) ->
    DoneCount = map_size(Batch#batch.answers) + map_size(Batch#batch.results),
    SentCount = DoneCount + map_size(Batch#batch.pickups),
    DoneWidth = (DoneCount * ?PROGRESS_SIZE) div Count,
    SentWidth = ((SentCount * ?PROGRESS_SIZE) div Count) - DoneWidth,
    TodoWidth = ?PROGRESS_SIZE - DoneWidth - SentWidth,
    io:format("[~s~s~s]\r", [
        binary:part(?PROGRESS_DONE, 0, DoneWidth),
        binary:part(?PROGRESS_SENT, 0, SentWidth),
        binary:part(?PROGRESS_TODO, 0, TodoWidth)
    ]).

