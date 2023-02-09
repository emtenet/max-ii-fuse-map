-module(minimal_experiment).

-export([run/0]).

-export([compile/1]).
-export([fuses/1]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    {ok, Fuses} = fuses(Density),
    io:format("~s fuse count = ~p~n", [Density, length(Fuses)]).

%%====================================================================
%% run
%%====================================================================

compile(Density) ->
    Device = density:largest_device(Density),
    [{Pin, _} | _] = device:pins(Device),
    experiment:compile(#{
        title => minimal,
        device => Device,
        settings => [
            {location, q, Pin}
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
            "    a_in => '1',\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }).

%%====================================================================
%% fuses
%%====================================================================

fuses(Density) ->
    {ok, Result} = compile(Density),
    experiment:fuses(Result).

