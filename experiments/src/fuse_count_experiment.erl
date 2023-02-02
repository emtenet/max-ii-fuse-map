-module(fuse_count_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun experiment/1, device:list()).

%%--------------------------------------------------------------------

experiment(Device) ->
    Title = <<"fuse count">>,
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => Device,
        settings => <<>>,
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
    FuseCount = pof_file:fuse_count(POF),
    Density = device:density(Device),
    FuseCount = density:fuse_count(Density),
    io:format("fuse_count(~s) -> ~p.~n", [Device, FuseCount]).

