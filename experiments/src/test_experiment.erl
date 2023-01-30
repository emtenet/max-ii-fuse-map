-module(test_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Device = <<"EPM570T100C5">>,
    Settings = <<
        "set_location_assignment PIN_12 -to clock\n"
        "set_location_assignment PIN_14 -to input\n"
        "set_location_assignment LC_X1_Y5_N0 -to ff\n"
        "set_location_assignment PIN_15 -to output\n"
    >>,
    VHDL = <<
        "library IEEE;\n"
        "use IEEE.STD_LOGIC_1164.ALL;\n"
        "library altera;\n"
        "use altera.altera_primitives_components.all;\n"
        "\n"
        "entity experiment is\n"
        "  port (\n"
        "    clock : in STD_LOGIC;\n"
        "    input : in STD_LOGIC;	 \n"
        "    output : out STD_LOGIC\n"
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is\n"
        "begin\n"
        "  ff: DFF port map (\n"
        "    d => input,\n"
        "    clk => clock,\n"
        "    clrn => '1',\n"
        "    prn => '1',\n"
        "    q => output\n"
        "  );\n"
        "end behavioral;\n"
    >>,
    {ok, POF} = quartus:compile(Device, Settings, VHDL),
    pof_file:fuses(POF).

%%--------------------------------------------------------------------

