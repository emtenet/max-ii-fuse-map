-module(global_clock_experiment).

-export([run/0]).

% How should we indicate the use (or no use) of global clocks?
%
% Options:
% 
%   auto:
%       set_global_assignment -name AUTO_GLOBAL_CLOCK On
%       set_global_assignment -name AUTO_GLOBAL_CLOCK Off
%
%   setting:
%       set_instance_assignment -name GLOBAL_SIGNAL \"GLOBAL CLOCK\" -to clk
%
%   primitive:
%       VHDL
%
% Observations:
%
%  * AUTO_GLOBAL_CLOCK does not seem to matter
%  * the setting option is simplest

%%====================================================================
%% run
%%====================================================================

run() ->
    {ok, Experiments} = experiment:compile_to_fuses([
        not_global(),
        setting_global(),
        primitive_global()
    ]),
    Matrix = matrix:build(Experiments),
    false = matrix:is_empty(Matrix),
    matrix:print(Matrix).

%%--------------------------------------------------------------------

not_global() ->
    #{
        title => not_global,
        device => epm570_t100,
        settings => [
            %{auto_global_clock, false},
            {location, clk, pin12},
            {location, d, pin14},
            {location, q, pin15},
            {location, ff, {lc, 1, 5, 0}}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;	 \n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

setting_global() ->
    #{
        title => setting_global,
        device => epm570_t100,
        settings => [
            %{auto_global_clock, false},
            {location, clk, pin12},
            {location, d, pin14},
            {location, q, pin15},
            {location, ff, {lc, 1, 5, 0}},
            {global_clock, clk, true}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;	 \n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => clk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

primitive_global() ->
    #{
        title => primitive_global,
        device => epm570_t100,
        settings => [
            %{auto_global_clock, false},
            {location, clk, pin12},
            {location, d, pin14},
            {location, q, pin15},
            {location, ff, {lc, 1, 5, 0}}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    clk : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;	 \n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "  signal gclk : STD_LOGIC;\n"
            "begin\n"
            "  gck1: GLOBAL port map (\n"
            "    a_in => clk,\n"
            "    a_out => gclk\n"
            "  );\n"
            "  ff: DFF port map (\n"
            "    d => d,\n"
            "    clk => gclk,\n"
            "    clrn => '1',\n"
            "    prn => '1',\n"
            "    q => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

