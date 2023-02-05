-module(bus_hold_experiment).

-export([run/0]).

-define(FEATURE, bus_hold).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    [LAB | _] = device:labs(Device),
    LC = lab:lc(LAB, 0),
    [A, B, C, D | Pins = [Q | _]] = device:pins(Device),
    run(Density, Device, A, B, C, D, LC, Q),
    run(Density, Device, LC, D, Pins).

%%--------------------------------------------------------------------

run(_, _, _, _, []) ->
    ok;
run(Density, Device, LC, Q, [A, B, C, D, E, F, G]) ->
    run(Density, Device, A, B, C, D, LC, Q),
    run(Density, Device, D, E, F, G, LC, C),
    ok;
run(Density, Device, LC, Q, [A, B, C, D, E, F]) ->
    run(Density, Device, A, B, C, D, LC, Q),
    run(Density, Device, C, D, E, F, LC, B),
    ok;
run(Density, Device, LC, Q, [A, B, C, D, E]) ->
    run(Density, Device, A, B, C, D, LC, Q),
    run(Density, Device, B, C, D, E, LC, A),
    ok;
run(Density, Device, LC, Q, [A, B, C, D | Pins]) ->
    run(Density, Device, A, B, C, D, LC, Q),
    run(Density, Device, LC, D, Pins).

%%--------------------------------------------------------------------

run(Density, Device, {A, Alc}, {B, Blc}, {C, Clc}, {D, Dlc}, LC, {Q, _}) ->
    io:format(" => ~s ~s ~s ~s ~s~n", [Device, A, B, C, D]),
    {ok, Experiments} = experiment:compile_to_fuses([
        run(Device, control, A, B, C, D, LC, Q, []),
        run(Device, {Alc, ?FEATURE}, A, B, C, D, LC, Q, [{?FEATURE, a, true}]),
        run(Device, {Blc, ?FEATURE}, A, B, C, D, LC, Q, [{?FEATURE, b, true}]),
        run(Device, {Clc, ?FEATURE}, A, B, C, D, LC, Q, [{?FEATURE, c, true}]),
        run(Device, {Dlc, ?FEATURE}, A, B, C, D, LC, Q, [{?FEATURE, d, true}])
    ]),
    Matrix = matrix:build(Experiments),
    matrix:print(Matrix),
    Fuses = matrix:singles(Matrix),
    4 = length(Fuses),
    fuse_database:update(Density, Fuses).

%%--------------------------------------------------------------------

run(Device, Name, A, B, C, D, LC, Q, Settings) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, a, A},
            {location, b, B},
            {location, c, C},
            {location, d, D},
            {location, lut, LC},
            {location, q, Q}
            |
            Settings
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.


