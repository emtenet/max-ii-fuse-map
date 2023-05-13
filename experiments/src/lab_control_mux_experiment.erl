-module(lab_control_mux_experiment).

-export([run/0]).

% This experiment confirms the LAB control mux fuses and their mapping
% to local interconnects or local lines.
%
% NOTE: This will crash if a local line is encountered. No cached
% experiments have yet included a local line.

%%====================================================================
%% run
%%====================================================================

run() ->
    %build(epm240),
    %build(epm570),
    %build(epm1270),
    %build(epm2210),
    lists:foreach(fun build/1, density:list()),
    ok.

%%--------------------------------------------------------------------

build(Density) ->
    Acc0 = #{},
    %{ok, Acc0} = open(Density),
    {ok, Cache} = route_cache:open(Density),
    _Acc1 = route_cache:fold_blocks(
        lab_control_mux,
        fun (Block, Indexes, Acc) ->
            build_block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ),
    %save(Density, Acc1),
    ok.

%%--------------------------------------------------------------------

build_block(Density, Block, Indexes, Acc0) ->
    io:format(" ==> ~p:~n", [Block]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            build_index(Density, Block, Index, Froms, Acc)
        end,
        Acc0,
        Indexes
    ).

%%--------------------------------------------------------------------

build_index(Density, {lab_control_mux, X, Y}, Index, Froms, Acc0) ->
    LAB = {lab, X, Y},
    route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            build_from(Density, LAB, Index, From, Cached, Acc)
        end,
        Acc0,
        Froms
    ).

%%--------------------------------------------------------------------

build_from(Density, LAB, Index, From, Cached, Acc) ->
    case build_fuses(Density, Cached) of
        {ok, Fuses} ->
            io:format("LAB ~p ~p ~p ~p~n", [Density, LAB, {control, Index}, From]),
            case build_mux(Density, LAB, Index, Fuses) of
                {ok, Mux} ->
                    build_mapping(Index, Mux, From);

                false ->
                    io:format(" ???~n", [])
            end,
            Acc;

        false ->
            Acc
    end.

%%--------------------------------------------------------------------

build_fuses(Density, Cached) ->
    route_cache:fold_cached(
        fun build_fuses_reduce0/2,
        Density,
        fun build_fuses_reduce/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

build_fuses_reduce0(Experiment, Density) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:subtract(Fuses, density:minimal_fuses(Density)).

%%--------------------------------------------------------------------

build_fuses_reduce(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

%%--------------------------------------------------------------------

build_mux(Density, LAB, Index, Fuses) ->
    build_mux(Density, LAB, Index, Fuses, undefined, undefined).

%%--------------------------------------------------------------------

build_mux(_, _, _, _, Mux6, Mux3)
        when Mux6 =/= undefined andalso
             Mux3 =/= undefined ->
    {ok, {Mux6, Mux3}};
build_mux(_, _, _, [], _, _) ->
    false;
build_mux(Density, LAB, Index, [Fuse | Fuses], Mux6, Mux3) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {LAB, {control, Index}, from6, Mux}} ->
            build_mux(Density, LAB, Index, Fuses, Mux, Mux3);

        {ok, {LAB, {control, Index}, from3, Mux}} ->
            build_mux(Density, LAB, Index, Fuses, Mux6, Mux);

        {ok, _Name} ->
            %io:format("  ~w~n", [_Name]),
            build_mux(Density, LAB, Index, Fuses, Mux6, Mux3);

        {error, Location} ->
            io:format("  ~w~n", [Location]),
            build_mux(Density, LAB, Index, Fuses, Mux6, Mux3)
    end.

%%--------------------------------------------------------------------

build_mapping(Index, Mux, {local_interconnect, _, _, 0, N}) ->
    case to_interconnect(Index, Mux) of
        {interconnect, N} ->
            ok;

        Got ->
            throw({mapped, Mux, to, Got, expecting, {interconnect, N}})
    end.

%%--------------------------------------------------------------------

to_interconnect(0, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_c, Mux6, Mux3);
to_interconnect(1, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_d, Mux6, Mux3);
to_interconnect(2, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_c, Mux6, Mux3);
to_interconnect(3, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_d, Mux6, Mux3);
to_interconnect(4, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_c, Mux6, Mux3);
to_interconnect(5, {Mux6, Mux3}) ->
    data_mux_map:to_interconnect(data_d, Mux6, Mux3).

