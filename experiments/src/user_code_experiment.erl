-module(user_code_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    {_, Zeros} = experiment(<<"00000000">>),
    {_, Ones} = experiment(<<"FFFFFFFF">>),
    io:format("00000000 => ~p fuses~n", [length(Zeros)]),
    io:format("FFFFFFFF => ~p fuses~n", [length(Ones)]),
    Shortest = length(Zeros),
    Shortest = length(Ones) - 32,
    {_, Default} = experiment(),
    % want default with least amount of fuses
    Zeros = Default,
    Experiments = [
        experiment(<<"80000000">>),
        experiment(<<"40000000">>),
        experiment(<<"20000000">>),
        experiment(<<"10000000">>),
        experiment(<<"08000000">>),
        experiment(<<"04000000">>),
        experiment(<<"02000000">>),
        experiment(<<"01000000">>),
        experiment(<<"00800000">>),
        experiment(<<"00400000">>),
        experiment(<<"00200000">>),
        experiment(<<"00100000">>),
        experiment(<<"00080000">>),
        experiment(<<"00040000">>),
        experiment(<<"00020000">>),
        experiment(<<"00010000">>),
        experiment(<<"00008000">>),
        experiment(<<"00004000">>),
        experiment(<<"00002000">>),
        experiment(<<"00001000">>),
        experiment(<<"00000800">>),
        experiment(<<"00000400">>),
        experiment(<<"00000200">>),
        experiment(<<"00000100">>),
        experiment(<<"00000080">>),
        experiment(<<"00000040">>),
        experiment(<<"00000020">>),
        experiment(<<"00000010">>),
        experiment(<<"00000008">>),
        experiment(<<"00000004">>),
        experiment(<<"00000002">>),
        experiment(<<"00000001">>)
    ],
    Matrix = matrix:build(Experiments),
    matrix:print(Matrix).

%%--------------------------------------------------------------------

experiment() ->
    Title = <<"user code (default)">>,
    io:format(" => ~s~n", [Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => epm570_t100,
        settings => <<
            "set_location_assignment PIN_14 -to d\n"
            "set_location_assignment PIN_15 -to q\n"
            "set_location_assignment LC_X1_Y5_N0 -to lut\n"
        >>,
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

experiment(Code) ->
    Title = <<"user code ", Code/binary>>,
    io:format(" => ~s~n", [Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => epm570_t100,
        settings => <<
            "set_global_assignment -name STRATIX_JTAG_USER_CODE ", Code/binary, "\n"
            "set_location_assignment PIN_14 -to d\n"
            "set_location_assignment PIN_15 -to q\n"
            "set_location_assignment LC_X1_Y5_N0 -to lut\n"
        >>,
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

