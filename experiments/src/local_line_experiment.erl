-module(local_line_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [Pin | _] = device:pins(Device),
    [
        lab(Density, Device, Pin, LAB)
        ||
        LAB <- density:labs(Density)
    ],
    ok.

%%--------------------------------------------------------------------

lab(Density, Device, Pin, LAB) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, lab:lc(LAB, 0), lab:lc(LAB, 1), lab:lc(LAB, 5), Pin),
        source(Device, lab:lc(LAB, 1), lab:lc(LAB, 2), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 2), lab:lc(LAB, 3), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 3), lab:lc(LAB, 4), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 4), lab:lc(LAB, 5), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 5), lab:lc(LAB, 6), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 6), lab:lc(LAB, 7), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 7), lab:lc(LAB, 8), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 8), lab:lc(LAB, 9), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 0), lab:lc(LAB, 9), lab:lc(LAB, 5), Pin)
    ]),
    Minimal = {minimal, density:minimal_fuses(Density)},
    Matrix0 = matrix:build(Density, [Minimal | Experiments]),
    %Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    lists:foreach(fun always_local_line/1, Experiments),
    Candidates = [
        candidates(Matrix, [1,0,1,1,1,1,1,1,1,1,0]),
        candidates(Matrix, [1,0,0,1,1,1,1,1,1,1,1]),
        candidates(Matrix, [1,1,0,0,1,1,1,1,1,1,1]),
        candidates(Matrix, [1,1,1,0,0,1,1,1,1,1,1]),
        candidates(Matrix, [1,1,1,1,0,0,1,1,1,1,1]),
        candidates(Matrix, [1,1,1,1,1,0,0,1,1,1,1]),
        candidates(Matrix, [1,1,1,1,1,1,0,0,1,1,1]),
        candidates(Matrix, [1,1,1,1,1,1,1,0,0,1,1]),
        candidates(Matrix, [1,1,1,1,1,1,1,1,0,0,1]),
        candidates(Matrix, [1,1,1,1,1,1,1,1,1,0,0])
    ],
    Fuses = fuses(Candidates),
    %io:format("~p~n", [Fuses]),
    fuse_database:update(Density, [
        {Fuse, {lab:lc(LAB, N), local_line}}
        ||
        {Fuse, N} <- lists:zip(Fuses, lists:seq(0, 9))
    ]),
    ok.

%%--------------------------------------------------------------------

source(Device, A, B, Thru, Pin) ->
    #{
        title => {A, B},
        device => Device,
        settings => [
            {location, a, A},
            {location, b, B},
            {location, thru, Thru},
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
            "architecture behavioral of experiment is\n",
            "  signal a_q : STD_LOGIC;\n"
            "  signal b_q : STD_LOGIC;\n"
            "begin\n"
            "  a: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => a_q\n"
            "  );\n",
            "  b: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => b_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => a_q AND b_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

always_local_line({_, _, #{signals := #{a := A, b := B}}}) ->
    #{dests := [#{route := [{local_line, _, _, 0, _}]}]} = A,
    #{dests := [#{route := [{local_line, _, _, 0, _}]}]} = B,
    ok.

%%--------------------------------------------------------------------

candidates(Matrix, Pattern) ->
    [
        Fuse
        ||
        {Fuse, _} <- matrix:pattern_is(Matrix, Pattern)
    ].

%%--------------------------------------------------------------------

fuses(Candidates) ->
    fuses_expect([
        [ 4,  8, 12, 16, 39, 35, 31, 27, 23],
        [ 4,  8, 12, 16, 42, 38, 34, 27, 23],
        [ 4,  8, 12, 16, 42, 38, 34, 30, 23],
        [ 4,  8, 12, 16, 42, 38, 34, 30, 26],
        [ 4,  8, 12, 19, 42, 38, 34, 30, 26],
        [ 4,  8, 15, 19, 45, 38, 34, 30, 26],
        [ 7, 11, 15, 19, 42, 38, 34, 30, 26],
        [ 7, 11, 15, 19, 45, 41, 34, 30, 26]
    ], Candidates).

%%--------------------------------------------------------------------

fuses_expect([], Candidates) ->
    throw(Candidates);
fuses_expect([Expect | Expects], Candidates = [Head | Tail]) ->
    case fuses_head(Head, Tail, Expect) of
        {ok, Fuses} ->
            Fuses;

        false ->
            fuses_expect(Expects, Candidates)
    end.

%%--------------------------------------------------------------------

fuses_head([], _, _) ->
    false;
fuses_head([C | Cs], Rest, Expect) ->
    case fuses_tail(C, Rest, Expect) of
        true ->
            {ok, [C | lists:map(fun (E) -> E + C end, Expect)]};

        false ->
            fuses_head(Cs, Rest, Expect)
    end.

%%--------------------------------------------------------------------

fuses_tail(_, [], []) ->
    true;
fuses_tail(C, [Head | Tail], [Expect | Expects]) ->
    case lists:member(C + Expect, Head) of
        true ->
            fuses_tail(C, Tail, Expects);

        false ->
            false
    end.

