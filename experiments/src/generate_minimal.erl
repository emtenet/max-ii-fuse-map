-module(generate_minimal).

-export([run/0]).

% Since minimal_experiment:source/3 cannot generate an exeriment with
% no LCs or IOCs used, we generate a small set of experiments and
% only keep the common minimal fuses.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    {ok, Experiments} = experiment:compile_to_fuses([
        source:out_constant(Device, Pin, 0)
        ||
        Pin <- pins(Device)
    ]),
    Matrix = matrix:build(Experiments),
    %matrix:print(Matrix),
    Zeros = [ Fuse || {Fuse, _} <- matrix:single_zeros(Matrix) ],
    Ones = [ Fuse || {Fuse, _} <- matrix:single_ones(Matrix) ],
    %io:format("zeros:~n  ~p~n", [Zeros]),
    %io:format("ones:~n  ~p~n", [Ones]),
    [{_, Fuses0} | _] = Experiments,
    Fuses = fuses:union(fuses:subtract(Fuses0, Zeros), Ones),
    density_file(Density, Fuses).

%%--------------------------------------------------------------------

pins(Device) ->
    [{A, _}, {B, _}, {C, _}, {D, _} | _] = device:iobs(Device),
    IOCS = device:iocs(Device),
    [pin(IOCS, A),
     pin(IOCS, B),
     pin(IOCS, C),
     pin(IOCS, D)
    ].

%%--------------------------------------------------------------------

pin([Pin = {_, IOC} | Pins], IOB) ->
    case ioc:in_iob(IOC, IOB) of
        true ->
            Pin;

        false ->
            pin(Pins, IOB)
    end.

%%====================================================================
%% density_file
%%====================================================================

density_file(Density, Fuses) ->
    Data = [<<
        "-module(">>, atom_to_binary(Density), <<"_minimal).\n"
        "\n"
        "-export([fuses/0]).\n"
        "\n"
        "-type fuse() :: fuse:fuse().\n"
        "\n"
        "-spec fuses() -> [fuse()].\n"
        "\n"
        "fuses() ->\n">>,
        device_fuses(Fuses, []), <<
        "\n"
    >>],
    Name = lists:flatten(io_lib:format("~s_minimal.erl", [Density])),
    File = filename:join("src", Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

device_fuses([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_fuses([Fuse | Fuses], Lines) ->
    Line = [
        <<"     ">>,
        io_lib:format("~p", [Fuse]),
        <<",\n">>
    ],
    device_fuses(Fuses, [Line | Lines]).

