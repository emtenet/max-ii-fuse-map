-module(output_enable_invert_experiment).

-export([run/0]).

% This experiment looks for the output-enable-invert fuse.
%
% This experiment also confirms the output-invert fuse.

%%====================================================================
%% run
%%====================================================================

run() ->
    %Density = epm240,
    %Device = density:largest_device(Density),
    %block(Density, Device, {iob, 1, 3}),
    %block(Density, Device, {iob, 8, 3}),
    %block(Density, Device, {iob, 3, 0}),
    %block(Density, Device, {iob, 3, 5}),
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(
        fun ({IOB, _}) -> block(Density, Device, IOB) end,
        density:iobs(Density)
    ).

%%--------------------------------------------------------------------

block(Density, Device, IOB) ->
    io:format(" => ~s ~w OE~n", [Density, IOB]),
    Pins = pins(Device, IOB),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, In, InBuf, Out, OE, OEBuf)
        ||
        {Out, Ins} <- Pins,
        In <- Ins,
        InBuf <- [buf, inv],
        OE <- Ins, OE =/= In,
        OEBuf <- [buf, inv]
    ]),
    Matrix = matrix:build_with_map(Density, Experiments),
    %matrix:print(Matrix),
    EnableFuses = lists:map(fun (Pin) -> enable_fuse(Pin, Matrix) end, Pins),
    OutputFuses = lists:map(fun (Pin) -> output_fuse(Pin, Matrix) end, Pins),
    fuse_database:update(Density, EnableFuses ++ OutputFuses),
    ok.

%%--------------------------------------------------------------------

pins(Device, IOB) ->
    shuffle(lists:sort([
        {IOC, Pin}
        ||
        {Pin, IOC} <- device:iocs(Device),
        ioc:iob(IOC) =:= IOB
    ])).

%%--------------------------------------------------------------------

shuffle([A, B, C]) ->
    [{A, [B, C]},
     {B, [C, A]},
     {C, [A, B]}
    ];
shuffle([A, B, C, D]) ->
    [{A, [B, C]},
     {B, [C, D]},
     {C, [D, A]},
     {D, [A, B]}
    ];
shuffle([A, B, C, D, E]) ->
    [{A, [B, C]},
     {B, [C, D]},
     {C, [D, E]},
     {D, [E, A]},
     {E, [A, B]}
    ];
shuffle([A, B, C, D, E, F]) ->
    [{A, [B, C]},
     {B, [C, D]},
     {C, [D, E]},
     {D, [E, F]},
     {E, [F, A]},
     {F, [A, B]}
    ];
shuffle([A, B, C, D, E, F, G]) ->
    [{A, [B, C]},
     {B, [C, D]},
     {C, [D, E]},
     {D, [E, F]},
     {E, [F, G]},
     {F, [G, A]},
     {G, [A, B]}
    ].

%%--------------------------------------------------------------------

source(Device, In, InBuf, Out, OE, OEBuf) ->
    InNot = case InBuf of buf -> <<>>; inv -> <<"NOT ">> end,
    OENot = case OEBuf of buf -> <<>>; inv -> <<"NOT ">> end,
    #{
        title => {ioc(In), InBuf, to, ioc(Out), oe, ioc(OE), OEBuf},
        device => Device,
        settings => [
            {location, i, pin(In)},
            {location, o, pin(Out)},
            {location, oe, pin(OE)}
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
            "    o : out STD_LOGIC;\n"
            "    oe : in STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  ioc: ALT_OUTBUF_TRI port map (\n"
            "    i => ", InNot/binary, "i,\n"
            "    o => o,\n"
            "    oe => ", OENot/binary, "oe\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%--------------------------------------------------------------------

ioc({IOC, _}) -> IOC.

%%--------------------------------------------------------------------

pin({_, Pin}) -> Pin.

%%====================================================================
%% fuse
%%====================================================================

enable_fuse({{IOC, _}, _}, Matrix = {matrix, Names, _}) ->
    Pattern = lists:map(fun (Name) ->
        enable_fuse_pattern(IOC, Name)
    end, Names),
    [{Fuse, _}] = matrix:pattern_is(Matrix, Pattern),
    {Fuse, {IOC, enable_invert}}.

%%--------------------------------------------------------------------

enable_fuse_pattern(IOC, {_, _, to, IOC, oe, _, buf}) -> 1;
enable_fuse_pattern(IOC, {_, _, to, IOC, oe, _, inv}) -> 0;
enable_fuse_pattern(IOC, {IOC, _, to, _, oe, _, _}) -> 0;
enable_fuse_pattern(IOC, {_, _, to, _, oe, IOC, _}) -> 0;
enable_fuse_pattern(_, _) -> 1.

%%--------------------------------------------------------------------

output_fuse({{IOC, _}, _}, Matrix = {matrix, Names, _}) ->
    Pattern = lists:map(fun (Name) ->
        output_fuse_pattern(IOC, Name)
    end, Names),
    [{Fuse, _}] = matrix:pattern_is(Matrix, Pattern),
    {Fuse, {IOC, output_invert}}.

%%--------------------------------------------------------------------

output_fuse_pattern(IOC, {_, buf, to, IOC, oe, _, _}) -> 1;
output_fuse_pattern(IOC, {_, inv, to, IOC, oe, _, _}) -> 0;
output_fuse_pattern(IOC, {IOC, _, to, _, oe, _, _}) -> 0;
output_fuse_pattern(IOC, {_, _, to, _, oe, IOC, _}) -> 0;
output_fuse_pattern(_, _) -> 0.

