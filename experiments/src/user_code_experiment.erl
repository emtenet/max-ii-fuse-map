-module(user_code_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    {ok, Controls} = experiment:compile_to_fuses([
        experiment(Device, <<"00000000">>, none),
        experiment(Device, <<"FFFFFFFF">>, all),
        experiment(Device)
    ]),
    [{_, Zeros}, {_, Ones}, {_, Default}] = Controls,
    io:format("00000000 => ~p fuses~n", [length(Zeros)]),
    io:format("FFFFFFFF => ~p fuses~n", [length(Ones)]),
    Shortest = length(Zeros),
    Shortest = length(Ones) - 32,
    % want default with least amount of fuses
    Zeros = Default,
    {ok, Experiments} = experiment:compile_to_fuses([
        experiment(Device, <<"00000001">>, 0),
        experiment(Device, <<"00000002">>, 1),
        experiment(Device, <<"00000004">>, 2),
        experiment(Device, <<"00000008">>, 3),
        experiment(Device, <<"00000010">>, 4),
        experiment(Device, <<"00000020">>, 5),
        experiment(Device, <<"00000040">>, 6),
        experiment(Device, <<"00000080">>, 7),
        experiment(Device, <<"00000100">>, 8),
        experiment(Device, <<"00000200">>, 9),
        experiment(Device, <<"00000400">>, 10),
        experiment(Device, <<"00000800">>, 11),
        experiment(Device, <<"00001000">>, 12),
        experiment(Device, <<"00002000">>, 13),
        experiment(Device, <<"00004000">>, 14),
        experiment(Device, <<"00008000">>, 15),
        experiment(Device, <<"00010000">>, 16),
        experiment(Device, <<"00020000">>, 17),
        experiment(Device, <<"00040000">>, 18),
        experiment(Device, <<"00080000">>, 19),
        experiment(Device, <<"00100000">>, 20),
        experiment(Device, <<"00200000">>, 21),
        experiment(Device, <<"00400000">>, 22),
        experiment(Device, <<"00800000">>, 23),
        experiment(Device, <<"01000000">>, 24),
        experiment(Device, <<"02000000">>, 25),
        experiment(Device, <<"04000000">>, 26),
        experiment(Device, <<"08000000">>, 27),
        experiment(Device, <<"10000000">>, 28),
        experiment(Device, <<"20000000">>, 29),
        experiment(Device, <<"40000000">>, 30),
        experiment(Device, <<"80000000">>, 31)
    ]),
    Matrix = matrix:build(Experiments),
    matrix:print(Matrix),
    % the user code bits are stored invered so look for a
    % single zero (0) bit
    Fuses = matrix:single_zeros(Matrix),
    32 = length(Fuses),
    fuse_database:update(Density, Fuses).

%%--------------------------------------------------------------------

experiment(Device) ->
    [{Pin, _} | _] = device:pins(Device),
    [LAB | _] = device:labs(Device),
    LUT = lab:lc(LAB, 0),
    #{
        title => {user_code, default},
        device => Device,
        settings => [
            {location, q, Pin},
            {location, lut, LUT}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => '0',\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

experiment(Device, Code, Bit) ->
    [{Pin, _} | _] = device:pins(Device),
    [LAB | _] = device:labs(Device),
    LUT = lab:lc(LAB, 0),
    #{
        title => {user_code, Bit},
        device => Device,
        settings => [
            {user_code, Code},
            {location, q, Pin},
            {location, lut, LUT}
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
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => '0',\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

