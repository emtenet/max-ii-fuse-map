-module(user_code_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    run(epm570_t100).

%%--------------------------------------------------------------------

run(Device) ->
    {_, Zeros} = experiment(Device, <<"00000000">>),
    {_, Ones} = experiment(Device, <<"FFFFFFFF">>),
    io:format("00000000 => ~p fuses~n", [length(Zeros)]),
    io:format("FFFFFFFF => ~p fuses~n", [length(Ones)]),
    Shortest = length(Zeros),
    Shortest = length(Ones) - 32,
    {_, Default} = experiment(Device),
    % want default with least amount of fuses
    Zeros = Default,
    Experiments = [
        experiment(Device, <<"00000001">>),
        experiment(Device, <<"00000002">>),
        experiment(Device, <<"00000004">>),
        experiment(Device, <<"00000008">>),
        experiment(Device, <<"00000010">>),
        experiment(Device, <<"00000020">>),
        experiment(Device, <<"00000040">>),
        experiment(Device, <<"00000080">>),
        experiment(Device, <<"00000100">>),
        experiment(Device, <<"00000200">>),
        experiment(Device, <<"00000400">>),
        experiment(Device, <<"00000800">>),
        experiment(Device, <<"00001000">>),
        experiment(Device, <<"00002000">>),
        experiment(Device, <<"00004000">>),
        experiment(Device, <<"00008000">>),
        experiment(Device, <<"00010000">>),
        experiment(Device, <<"00020000">>),
        experiment(Device, <<"00040000">>),
        experiment(Device, <<"00080000">>),
        experiment(Device, <<"00100000">>),
        experiment(Device, <<"00200000">>),
        experiment(Device, <<"00400000">>),
        experiment(Device, <<"00800000">>),
        experiment(Device, <<"01000000">>),
        experiment(Device, <<"02000000">>),
        experiment(Device, <<"04000000">>),
        experiment(Device, <<"08000000">>),
        experiment(Device, <<"10000000">>),
        experiment(Device, <<"20000000">>),
        experiment(Device, <<"40000000">>),
        experiment(Device, <<"80000000">>)
    ],
    Matrix = matrix:build(Experiments),
    matrix:print(Matrix).

%%--------------------------------------------------------------------

experiment(Device) ->
    Title = <<"user code (default)">>,
    io:format(" => ~s ~s~n", [Device, Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => Device,
        settings => [
            {location, d, pin14},
            {location, q, pin15},
            {location, lut, {lc, 1, 5, 0}}
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
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => d,\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }),
    {ok, POF} = quartus:pof(Cache),
    {Title, pof_file:fuses(POF)}.

%%--------------------------------------------------------------------

experiment(Device, Code) ->
    Title = <<"user code ", Code/binary>>,
    io:format(" => ~s ~s~n", [Device, Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => Device,
        settings => [
            {user_code, Code},
            {location, d, pin14},
            {location, q, pin15},
            {location, lut, {lc, 1, 5, 0}}
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
            "    d : in STD_LOGIC;\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => d,\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }),
    {ok, POF} = quartus:pof(Cache),
    {Title, pof_file:fuses(POF)}.

