-module(lab_interconnect_limit_experiment).

-export([run/0]).

% Experiment to confirm that each LAB only has 26 interconnects.
%
% The experiment tries to use 28 inputs to a LAB and failes with the
% following message extract in the error:
%
%   LAB legality constraint that was not satisfied:
%   LAB requires more input signals requiring LAB lines than are available.
%   Resources used: 28. Resources available: 26.

-define(DEVICE, epm240_t100).

%%====================================================================
%% run
%%====================================================================

run() ->
    {ok, Experiments} = experiment:compile_to_fuses([
        #{
            title => limit,
            device => ?DEVICE,
            settings => [
                {location, a, {lc, 2, 2, 0}},
                {location, b, {lc, 2, 2, 1}},
                {location, c, {lc, 2, 2, 2}},
                {location, d, {lc, 2, 2, 3}},
                {location, e, {lc, 2, 2, 4}},
                {location, f, {lc, 2, 2, 5}},
                {location, g, {lc, 2, 2, 6}}
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
                "    a0 : in STD_LOGIC;\n"
                "    a1 : in STD_LOGIC;\n"
                "    a2 : in STD_LOGIC;\n"
                "    a3 : in STD_LOGIC;\n"
                "    aq : out STD_LOGIC;\n"
                "    b0 : in STD_LOGIC;\n"
                "    b1 : in STD_LOGIC;\n"
                "    b2 : in STD_LOGIC;\n"
                "    b3 : in STD_LOGIC;\n"
                "    bq : out STD_LOGIC;\n"
                "    c0 : in STD_LOGIC;\n"
                "    c1 : in STD_LOGIC;\n"
                "    c2 : in STD_LOGIC;\n"
                "    c3 : in STD_LOGIC;\n"
                "    cq : out STD_LOGIC;\n"
                "    d0 : in STD_LOGIC;\n"
                "    d1 : in STD_LOGIC;\n"
                "    d2 : in STD_LOGIC;\n"
                "    d3 : in STD_LOGIC;\n"
                "    dq : out STD_LOGIC;\n"
                "    e0 : in STD_LOGIC;\n"
                "    e1 : in STD_LOGIC;\n"
                "    e2 : in STD_LOGIC;\n"
                "    e3 : in STD_LOGIC;\n"
                "    eq : out STD_LOGIC;\n"
                "    f0 : in STD_LOGIC;\n"
                "    f1 : in STD_LOGIC;\n"
                "    f2 : in STD_LOGIC;\n"
                "    f3 : in STD_LOGIC;\n"
                "    fq : out STD_LOGIC;\n"
                "    g0 : in STD_LOGIC;\n"
                "    g1 : in STD_LOGIC;\n"
                "    g2 : in STD_LOGIC;\n"
                "    g3 : in STD_LOGIC;\n"
                "    gq : out STD_LOGIC\n"
                "  );\n"
                "end experiment;\n"
                "\n"
                "architecture behavioral of experiment is\n"
                "begin\n"
                "  a: LCELL port map (\n"
                "    a_in => a0 AND a1 AND a2 AND a3,\n"
                "    a_out => aq\n"
                "  );\n"
                "  b: LCELL port map (\n"
                "    a_in => b0 AND b1 AND b2 AND b3,\n"
                "    a_out => bq\n"
                "  );\n"
                "  c: LCELL port map (\n"
                "    a_in => c0 AND c1 AND c2 AND c3,\n"
                "    a_out => cq\n"
                "  );\n"
                "  d: LCELL port map (\n"
                "    a_in => d0 AND d1 AND d2 AND d3,\n"
                "    a_out => dq\n"
                "  );\n"
                "  e: LCELL port map (\n"
                "    a_in => e0 AND e1 AND e2 AND e3,\n"
                "    a_out => eq\n"
                "  );\n"
                "  f: LCELL port map (\n"
                "    a_in => f0 AND f1 AND f2 AND f3,\n"
                "    a_out => fq\n"
                "  );\n"
                "  g: LCELL port map (\n"
                "    a_in => g0 AND g1 AND g2 AND g3,\n"
                "    a_out => gq\n"
                "  );\n"
                "end behavioral;\n"
             >>
        }
    ]),
    io:format("~p~n", [Experiments]),
    ok.

