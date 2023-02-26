-module(generate_fast_out).

-export([run/0]).

% Generate files:
%
%   src/<density>_fast_out.erl
%
% with list of IOC -> LC, left | right

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins = lists:sort([
        pin(Device, Pin, IOC)
        ||
        {Pin, IOC} <- device:iocs(Device)
    ]),
    Data = [<<
        "-module(">>, atom_to_binary(Density), <<"_fast_out).\n"
        "\n"
        "-export([iocs/0]).\n"
        "\n"
        "-type ioc() :: ioc:ioc().\n"
        "-type lc() :: lc:lc().\n"
        "\n"
        "-spec iocs() -> [{ioc(), lc(), left | right}].\n"
        "\n"
        "iocs() ->\n">>,
        density_iocs(Pins, []), <<
        "\n"
    >>],
    Name = lists:flatten(io_lib:format("~s_fast_out.erl", [Density])),
    File = filename:join("src", Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

density_iocs([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
density_iocs([{IOC, LC, Side} | Pins], Lines) ->
    Line = [
        <<"     ">>,
        io_lib:format("{~w,~w,~w}", [IOC, LC, Side]),
        <<",\n">>
    ],
    density_iocs(Pins, [Line | Lines]).

%%====================================================================
%% pins
%%====================================================================

pin(Device, Pin, IOC) ->
    io:format(" => ~s ~p~n", [Device, Pin]),
    IOB = ioc:iob(IOC),
    {_, LAB} = lists:keyfind(IOB, 1, device:iobs(Device)),
    {ok, Experiments} = experiment:compile_to_rcf([
        minimal_experiment:source_lc(N, Device, Pin, lab:lc(LAB, N))
        ||
        N <- max_ii:n_list()
    ]),
    {Bypass, LC, Side} = pin_fast_out(Experiments),
    {ioc, X, Y, N} = IOC,
    {io_bypass_out, X, Y, N, 0} = Bypass,
    {IOC, LC, Side}.

%%--------------------------------------------------------------------

pin_fast_out([{_, #{signals := #{lut := LUT}}} | Experiments]) ->
    #{lc := LC, dests := [#{route := Route}]} = LUT,
    case Route of
        [Bypass = {io_bypass_out,_,_,_,_}, Buffer = {le_buffer,_,_,_,_}] ->
            {lc, X, Y, N} = LC,
            case Buffer of
                {le_buffer, X, Y, 0, B} when B =:= N * 2 ->
                    {Bypass, LC, left};

                {le_buffer, X, Y, 0, B} when B =:= (N * 2) + 1 ->
                    {Bypass, LC, right}
            end;

        _ ->
            pin_fast_out(Experiments)
    end.

