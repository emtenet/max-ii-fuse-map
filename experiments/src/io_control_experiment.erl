-module(io_control_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    %pin(epm240, epm240_t100, {pin6, {ioc,1,3,0}}, {pin7, {ioc,1,3,1}}),
    %pin(epm240, epm240_t100, {pin7, {ioc,1,3,1}}, {pin8, {ioc,1,3,2}}),
    %pin(epm240, epm240_t100, {pin8, {ioc,1,3,2}}, {pin12,{ioc,1,3,3}}),
    %pin(epm240, epm240_t100, {pin12,{ioc,1,3,3}}, {pin6, {ioc,1,3,0}}),
    %density(epm240),
    %throw(stop),
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    IOBs = lists:sort(device:iobs(Device)),
    lists:foreach(fun (IOB) -> block(Density, Device, IOB) end, IOBs).

%%--------------------------------------------------------------------

block(Density, Device, {IOB, _}) ->
    Pins = lists:sort([
        {IOC, Pin}
        ||
        {Pin, IOC} <- device:iocs(Device),
        ioc:iob(IOC) =:= IOB
    ]),
    Last = lists:last(Pins),
    pins(Density, Device, Pins, Last).

%%--------------------------------------------------------------------

pins(_, _, [], _) ->
    ok;
pins(Density, Device, [Pin | Pins], Other) ->
    pin(Density, Device, Pin, Other),
    pins(Density, Device, Pins, Pin).

%%--------------------------------------------------------------------

pin(Density, Device, Pin, Other) ->
    io:format(" => ~s ~w~n", [Device, source:ioc(Pin)]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source:in_out(Device, input, [], Pin, Other),
        source:in_out(Device, schmitt_trigger, [
            {io_standard, i, v3_3_schmitt_trigger},
            {io_standard, o, v3_3_ttl}
        ], Pin, Other),
        source:in_out(Device, no_delay, [
            {input_delay, i, false}
        ], Pin, Other),
        source:open_drain(Device, Other, Pin),
        source:in_out(Device, minimum, [
            {current_strength, o, minimum}
        ], Other, Pin)
    ]),
    Matrix = matrix:build_with_map(Device, Experiments),
    %matrix:print(Matrix),
    [{SchmittTrigger, _}] = matrix:pattern_is(Matrix, [1,0,1,0,0]),
    [{InputDelay, _}] = matrix:pattern_is(Matrix, [0,0,1,1,1]),
    [{StrengthA, _}, {StrengthB, _}] = matrix:pattern_is(Matrix, [1,1,1,1,0]),
    [{OpenDrain, _}] = matrix:pattern_is(Matrix, [1,1,1,0,1]),
    {Strength0, Strength1} = current_strength(Density, StrengthA, StrengthB),
    Outputs = matrix:pattern_is(Matrix, [1,1,1,0,0]),
    Output = output(Density, SchmittTrigger, Outputs),
    IOC = source:ioc(Pin),
    fuse_database:update(Density, [
        {Output,            {IOC, output}},
        {SchmittTrigger,    {IOC, schmitt_trigger}},
        {InputDelay,        {IOC, input_delay}},
        {Strength0,         {IOC, current_strength_0}},
        {Strength1,         {IOC, current_strength_1}},
        {OpenDrain,         {IOC, open_drain}}
    ]).

%%--------------------------------------------------------------------

current_strength(Density, StrengthA, StrengthB) ->
    % sort paid {0, 1} by location NOT fuse number
    A = fuse_map:to_location(StrengthA, Density),
    B = fuse_map:to_location(StrengthB, Density),
    case A < B of
        true ->
            {StrengthA, StrengthB};

        false ->
            {StrengthB, StrengthA}
    end.

%%--------------------------------------------------------------------

output(Density, SchmittTrigger, Candidates) ->
    case fuse_map:to_location(SchmittTrigger, Density) of
        {X, Y, 0, 1, side, 0} ->
            output_candidate(Density, {X, Y, 0, 0, side, 0}, Candidates);

        {X, Y, 2, 0, side, 0} ->
            output_candidate(Density, {X, Y, 1, 3, side, 0}, Candidates);

        {X, Y, 3, 2, side, 0} ->
            output_candidate(Density, {X, Y, 3, 1, side, 0}, Candidates);

        {X, Y, line, 21, side, 0} ->
            output_candidate(Density, {X, Y, line, 20, side, 0}, Candidates);

        {X, Y, 5, 1, side, 0} ->
            output_candidate(Density, {X, Y, 5, 2, side, 0}, Candidates);

        {X, Y, 7, 3, side, 0} ->
            output_candidate(Density, {X, Y, 6, 0, side, 0}, Candidates);

        {X, Y, 8, 0, side, 0} ->
            output_candidate(Density, {X, Y, 8, 1, side, 0}, Candidates);

        {X, head, 0, cell, N} ->
            output_candidate(Density, {X, head, 0, cell, N - 1}, Candidates);

        {X, tail, 0, cell, N} ->
            output_candidate(Density, {X, tail, 0, cell, N - 1}, Candidates);

        Location ->
            throw({Location, Candidates})
    end.

%%--------------------------------------------------------------------

output_candidate(Density, Location, [{Fuse, _} | Candidates]) ->
    case fuse_map:to_location(Fuse, Density) of
        Location ->
            Fuse;

        _ ->
            output_candidate(Density, Location, Candidates)
    end.

