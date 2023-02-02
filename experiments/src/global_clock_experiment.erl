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
    Experiments = [
        not_global(),
        setting_global(),
        primitive_global()
    ],
    Matrix = matrix:build(Experiments),
    false = matrix:is_empty(Matrix),
    matrix:print(Matrix).

%%--------------------------------------------------------------------

not_global() ->
    Title = <<"not global">>,
    io:format(" => ~s~n", [Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => epm570_t100,
        settings => <<
            %"set_global_assignment -name AUTO_GLOBAL_CLOCK Off\n"
            "set_location_assignment PIN_12 -to clk\n"
            "set_location_assignment PIN_14 -to d\n"
            "set_location_assignment LC_X1_Y5_N0 -to ff\n"
            "set_location_assignment PIN_15 -to q\n"
        >>,
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
    }),
    {ok, POF} = quartus:pof(Cache),
    {Title, pof_file:fuses(POF)}.


%%--------------------------------------------------------------------

setting_global() ->
    Title = <<"setting global">>,
    io:format(" => ~s~n", [Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => epm570_t100,
        settings => <<
            %"set_global_assignment -name AUTO_GLOBAL_CLOCK Off\n"
            "set_location_assignment PIN_12 -to clk\n"
            "set_location_assignment PIN_14 -to d\n"
            "set_location_assignment LC_X1_Y5_N0 -to ff\n"
            "set_location_assignment PIN_15 -to q\n"
            "set_instance_assignment -name GLOBAL_SIGNAL \"GLOBAL CLOCK\" -to clk\n"
        >>,
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
    }),
    {ok, POF} = quartus:pof(Cache),
    {Title, pof_file:fuses(POF)}.


%%--------------------------------------------------------------------

primitive_global() ->
    Title = <<"primitive global">>,
    io:format(" => ~s~n", [Title]),
    {ok, Cache} = quartus:cache(#{
        title => Title,
        device => epm570_t100,
        settings => <<
            %"set_global_assignment -name AUTO_GLOBAL_CLOCK Off\n"
            "set_location_assignment PIN_12 -to clk\n"
            "set_location_assignment PIN_14 -to d\n"
            "set_location_assignment LC_X1_Y5_N0 -to ff\n"
            "set_location_assignment PIN_15 -to q\n"
        >>,
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
    }),
    {ok, POF} = quartus:pof(Cache),
    {Title, pof_file:fuses(POF)}.


