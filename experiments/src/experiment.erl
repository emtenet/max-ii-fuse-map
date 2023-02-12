-module(experiment).

-export([compile/1]).
-export([compile_to_fuses/1]).
-export([compile_to_fuses_and_rcf/1]).
-export([compile_to_rcf/1]).
-export([flush/1]).
-export([fuses/1]).
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
    source_index :: index(),
    compiles :: [compile()]
}).

batch([Compile | Compiles]) ->
    Source = experiment_compile:pre(Compile),
    batch_submit(#batch{
        count = 1 + length(Compiles),
        answers = #{},
        answer_index = 0,
        results = #{},
        pickups = #{},
        source = Source,
        source_index = 0,
        compiles = Compiles
    }).

%%--------------------------------------------------------------------

batch_submit(Batch = #batch{source = undefined}) ->
    [] = Batch#batch.compiles,
    batch_pickup(Batch);
batch_submit(Batch0 = #batch{}) ->
    Index = Batch0#batch.source_index,
    case experiment_server:submit(Batch0#batch.source) of
        {ok, Answer} ->
            Batch1 = batch_source(Batch0),
            case batch_answer(Index, Answer, Batch1) of
                {continue, Batch} ->
                    batch_submit(Batch);

                Complete ->
                    Complete
            end;

        {pickup, JobRef} ->
            Pickups = Batch0#batch.pickups,
            Batch = batch_source(Batch0#batch{
                pickups = Pickups#{JobRef => Index}
            }),
            batch_submit(Batch);

        busy ->
            batch_busy(Batch0)
    end.

%%--------------------------------------------------------------------

batch_source(Batch = #batch{compiles = []}) ->
    Batch#batch{
        source = undefined,
        source_index = Batch#batch.source_index + 1
    };
batch_source(Batch = #batch{compiles = [Compile | Compiles]}) ->
    Source = experiment_compile:pre(Compile),
    Batch#batch{
        source = Source,
        source_index = Batch#batch.source_index + 1,
        compiles = Compiles
    }.

%%--------------------------------------------------------------------

batch_busy(Batch = #batch{pickups = Pickups}) ->
    JobRefs = maps:keys(Pickups),
    case experiment_server:pickup_check(JobRefs) of
        {ok, Results} ->
            batch_results(maps:to_list(Results), Batch);

        false ->
            batch_submit(Batch)
    end.

%%--------------------------------------------------------------------

batch_pickup(Batch = #batch{pickups = Pickups}) ->
    JobRefs = maps:keys(Pickups),
    case experiment_server:pickup_sleep(JobRefs) of
        {ok, Results} ->
            batch_results(maps:to_list(Results), Batch);

        false ->
            batch_pickup(Batch)
    end.

%%--------------------------------------------------------------------

batch_results([], Batch) ->
    batch_submit(Batch);
batch_results([{JobRef, Result} | Results], Batch0) ->
    {Index, Pickups} = maps:take(JobRef, Batch0#batch.pickups),
    case batch_result(Index, Result, Batch0#batch{pickups = Pickups}) of
        {continue, Batch} ->
            batch_results(Results, Batch);

        Complete ->
            Complete
    end.

%%--------------------------------------------------------------------

batch_result(Index, Result, Batch) when Index =:= Batch#batch.answer_index ->
    case experiment_compile:post(Result) of
        {ok, Answer} ->
            Answers = Batch#batch.answers,
            batch_next_answer(Index + 1, Batch#batch{
                answers = Answers#{Index => Answer},
                answer_index = Index + 1
            });

        error ->
            error
    end;
batch_result(Index, Result, Batch = #batch{results = Results}) ->
    {continue, Batch#batch{
        results = Results#{Index => Result}
    }}.

%%--------------------------------------------------------------------

batch_answer(Index, Answer, Batch) when Index =:= Batch#batch.answer_index ->
    Answers = Batch#batch.answers,
    batch_next_answer(Index + 1, Batch#batch{
        answers = Answers#{Index => Answer},
        answer_index = Index + 1
    });
batch_answer(Index, Answer, Batch) ->
    Answers = Batch#batch.answers,
    {continue, Batch#batch{
        answers = Answers#{Index => Answer}
    }}.

%%--------------------------------------------------------------------

batch_next_answer(Count, Batch) when Count =:= Batch#batch.count ->
    0 = map_size(Batch#batch.results),
    Count = Batch#batch.source_index,
    [] = Batch#batch.compiles,
    batch_collect_answers(Count, Batch#batch.answers, []);
batch_next_answer(Index, Batch = #batch{}) ->
    case maps:take(Index, Batch#batch.results) of
        {Result, Results} ->
            batch_result(Index, Result, Batch#batch{
                results = Results
            });

        _ ->
            {continue, Batch}
    end.

%%--------------------------------------------------------------------

batch_collect_answers(0, _, Answers) ->
    {ok, Answers};
batch_collect_answers(PreviousIndex, Map, Answers) ->
    Index = PreviousIndex - 1,
    #{Index := Answer} = Map,
    batch_collect_answers(Index, Map, [Answer | Answers]).

