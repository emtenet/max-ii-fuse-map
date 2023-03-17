-module(source).

-export([in_out/3]).
-export([in_out/4]).
-export([in_out/5]).
-export([in_lut_out/4]).
-export([lut_out/3]).
-export([open_drain/3]).

-export([ioc/1]).
-export([pin/1]).

%%====================================================================
%% in_out
%%====================================================================

in_out(Device, In, Out) ->
    in_out(Device, [], In, Out).

%%--------------------------------------------------------------------

in_out(Device, Settings, In, Out) ->
    in_out(Device, {ioc(In), to, ioc(Out)}, Settings, In, Out).

%%--------------------------------------------------------------------

in_out(Device, Title, Settings, In, Out) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(In)},
            {location, o, pin(Out)}
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
            "    i : in STD_LOGIC;\n"
            "    o : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  o <= i;\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% in_lut_out
%%====================================================================

in_lut_out(Device, In, LC, Out) ->
    #{
        title => {ioc(In), via, LC, to, ioc(Out)},
        device => Device,
        settings => [
            {location, i, pin(In)},
            {location, via, LC},
            {location, o, pin(Out)}
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
            "    i : in STD_LOGIC;\n"
            "    o : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  via: LCELL port map (\n"
            "    a_in => i,\n"
            "    a_out => o\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% lut_out
%%====================================================================

lut_out(Device, LC, Out) ->
    #{
        title => {LC, to, ioc(Out)},
        device => Device,
        settings => [
            {location, lut, LC},
            {location, q, pin(Out)}
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
    }.

%%====================================================================
%% open_drain
%%====================================================================

open_drain(Device, In, Out) ->
    #{
        title => {open, drain, ioc(Out)},
        device => Device,
        settings => [
            {location, i, pin(In)},
            {location, o, pin(Out)}
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
            "    i : in STD_LOGIC;\n"
            "    o : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  pad: OPNDRN port map (\n"
            "    a_in => i,\n"
            "    a_out => o\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% helpers
%%====================================================================

pin({{ioc, _, _, _}, Pin}) -> Pin;
pin({Pin, {ioc, _, _, _}}) -> Pin.

%%--------------------------------------------------------------------

ioc({IOC = {ioc, _, _, _}, _}) -> IOC;
ioc({_, IOC = {ioc, _, _, _}}) -> IOC.

