-module(source).

-export([in_out/3]).
-export([in_out/4]).
-export([in_out/5]).
-export([in_lut_out/4]).
-export([ins_lut_outs/6]).
-export([lut_out/3]).
-export([open_drain/3]).
-export([out_constant/3]).

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
%% ins_lut_outs
%%====================================================================

ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J, K, L], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, k, pin(K)},
            {location, l, pin(L)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    k : in STD_LOGIC;\n"
            "    l : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j AND k AND l,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J, K], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, k, pin(K)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    k : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j AND k,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => h AND i,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H], [Q, R | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, q, pin(Q)},
            {location, r, pin(R)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G], [Q, R | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, q, pin(Q)},
            {location, r, pin(R)}
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
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g,\n"
            "    a_out => r\n"
            "  );\n",
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
%% out_constant
%%====================================================================

out_constant(Device, {Pin, IOC}, Bit) ->
    #{
        title => {output, IOC, as, Bit},
        device => Device,
        settings => [
            {location, q, Pin}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  q <= '", ($0 + Bit), "';\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% helpers
%%====================================================================

pin(Pin) when is_atom(Pin) -> Pin;
pin({{ioc, _, _, _}, Pin}) -> Pin;
pin({Pin, {ioc, _, _, _}}) -> Pin.

%%--------------------------------------------------------------------

ioc(IOC = {ioc, _, _, _}) -> IOC;
ioc({IOC = {ioc, _, _, _}, _}) -> IOC;
ioc({_, IOC = {ioc, _, _, _}}) -> IOC.

