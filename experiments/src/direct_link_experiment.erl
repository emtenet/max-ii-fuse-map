-module(direct_link_experiment).

-export([run/0]).

-export([source/4]).

% Find LUT out (left / right) fuses that drive the direct-link
% interconnects to neighbouring LABs.
%
% By selecting LCs that have a fast-out to an IOC directly above or
% below we can check that there is a LUT out to the right.
%
% By selecting a "thru" LAB that has LABs to the left and right
% we can create experiments that route via direct-link to the
% left and right.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(
        fun (IOB) -> iob(Density, Device, IOB) end,
        density:iobs(Density)
    ).

%%--------------------------------------------------------------------

iob(Density, Device, {IOB = {iob, X, IOY}, Thru = {lab, X, Y}})
        when (IOY =:= Y + 1) orelse (IOY =:= Y - 1) ->
    LABs = density:labs(Density),
    Left = {lab, X - 1, Y},
    Right = {lab, X + 1, Y},
    case lists:member(Thru, LABs) andalso
         lists:member(Left, LABs) andalso
         lists:member(Right, LABs) of
        true ->
            Pins = pins(Density, Device, IOB, Thru),
            iob(Density, Device, Pins, Thru, Left, Right);

        false ->
            ok
    end;
iob(_, _, _) ->
    ok.

%%--------------------------------------------------------------------

pins(Density, Device, IOB, Thru) ->
    Pins = [
        Pin
        ||
        Pin = {_, IOC} <- device:iocs(Device),
        ioc:in_iob(IOC, IOB)
    ],
    FastOuts = density:fast_outs(Density),
    lists:filtermap(fun (Pin) -> pin(Pin, FastOuts, Thru) end, Pins).

%%--------------------------------------------------------------------

pin({Pin, IOC}, FastOuts, Thru) ->
    case lists:keyfind(IOC, 1, FastOuts) of
        {_, LC, _} ->
            Thru = lc:lab(LC),
            {true, {Pin, IOC, LC}};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

iob(_, _, [], _, _, _) ->
    ok;
iob(Density, Device, Pins, ThruLAB, Left, Right) ->
    io:format(" ==> ~s thru ~p~n", [Density, ThruLAB]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source(Device, LC, Thru, Pin)
        ||
        LC <- lab:lcs(Left) ++ lab:lcs(Right),
        {Pin, _, Thru} <- Pins
    ]),
    Matrix0 = matrix:build_with_map(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, enable}) -> true;
        ({_, invert}) -> true;
        ({_, fast_out}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    FastOuts = fast_outs(Density, Pins, Matrix),
    fuse_database:update(Density, FastOuts),
    LutOuts = lut_outs(Density, Pins, Left, Right, Matrix),
    fuse_database:update(Density, LutOuts),
    ok.

%%--------------------------------------------------------------------

source(Device, LC = {lc, _, _, _}, Thru = {lc, _, _, _}, Pin)
        when is_atom(Pin) ->
    #{
        title => {LC, thru, Thru, Pin},
        device => Device,
        settings => [
            {location, lut, LC},
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
            "  signal lut_q : STD_LOGIC;\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => lut_q\n"
            "  );\n",
            "  thru: LCELL port map (\n"
            "    a_in => lut_q,\n"
            "    a_out => q\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

fast_outs(Density, [A, B, C, D], Matrix) ->
    [
        fast_out(Density, A, [0, 1, 1, 1], Matrix),
        fast_out(Density, B, [1, 0, 1, 1], Matrix),
        fast_out(Density, C, [1, 1, 0, 1], Matrix),
        fast_out(Density, D, [1, 1, 1, 0], Matrix)
    ];
fast_outs(Density, [A, B, C], Matrix) ->
    [
        fast_out(Density, A, [0, 1, 1], Matrix),
        fast_out(Density, B, [1, 0, 1], Matrix),
        fast_out(Density, C, [1, 1, 0], Matrix)
    ].

%%--------------------------------------------------------------------

fast_out(Density, {_Pin, _, LC = {lc, X, Y, N}}, SubPattern, Matrix) ->
    Pattern = lists:flatten(lists:duplicate(20, SubPattern)),
    [{Fuse, _}] = matrix:pattern_is(Matrix, Pattern),
    {X, Y, N, 1, cell, 19} = fuse_map:to_location(Fuse, Density),
    {Fuse, {LC, lut_out, right}}.

%%--------------------------------------------------------------------

lut_outs(Density, [_, _, _, _], Left, Right, Matrix) ->
    lut_outs(Density, [0, 0, 0, 0], [1, 1, 1, 1], Left, Right, Matrix);
lut_outs(Density, [_, _, _], Left, Right, Matrix) ->
    lut_outs(Density, [0, 0, 0], [1, 1, 1], Left, Right, Matrix).

%%--------------------------------------------------------------------

lut_outs(Density, Zeros, Ones, Left, Right, Matrix) ->
    [
        lut_out(Density, lut_pattern(Zeros, Ones, LC, right), Matrix)
        ||
        LC <- lab:lcs(Left)
    ] ++ [
        lut_out(Density, lut_pattern(Zeros, Ones, LC, left), Matrix)
        ||
        LC <- lab:lcs(Right)
    ].

%%--------------------------------------------------------------------

lut_pattern(Zeros, Ones, LC = {lc, _, _, N}, right) ->
    Pattern = lists:flatten([
        lists:duplicate(N, Ones),
        Zeros,
        lists:duplicate(19 - N, Ones)
    ]),
    {LC, right, Pattern};
lut_pattern(Zeros, Ones, LC = {lc, _, _, N}, left) ->
    Pattern = lists:flatten([
        lists:duplicate(10 + N, Ones),
        Zeros,
        lists:duplicate(9 - N, Ones)
    ]),
    {LC, left, Pattern}.

%%--------------------------------------------------------------------

lut_out(Density, {LC, Dir, Pattern}, Matrix) ->
    lut_out(Density, LC, Dir, matrix:pattern_is(Matrix, Pattern)).

%%--------------------------------------------------------------------

lut_out(Density, LC = {lc, X, Y, N}, Dir, [{Fuse, _} | Fuses]) ->
    case fuse_map:to_location(Fuse, Density) of
        {X, Y, N, 1, cell, 19} when Dir =:= right ->
            {Fuse, {LC, lut_out, right}};

        {X, Y, N, 1, cell, 20} when Dir =:= left ->
            {Fuse, {LC, lut_out, left}};

        _ ->
            lut_out(Density, LC, Dir, Fuses)
    end.

