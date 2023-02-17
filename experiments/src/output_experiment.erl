-module(output_experiment).

-export([run/0]).

% Fuses observed from a minimal experiment outputing a constant
% to each pin.
%
% Outputing a 0 provides an `enable` fuse (guess).
%
% Outputing a 1 provides the same `enable` fuse
% and an addition `invert` fuse (guess).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    enable(Density),
    invert(Density).

%%--------------------------------------------------------------------

enable(Density) ->
    io:format(" => ~s output enable~n", [Density]),
    Device = density:largest_device(Density),
    {ok, Experiments} = experiment:compile_to_fuses([
        source(Device, Pin, 0)
        ||
        Pin <- device:pins(Device)
    ]),
    Matrix = matrix:build(Experiments),
    [] = matrix:single_ones(Matrix),
    Zeros = matrix:single_zeros(Matrix),
    Count = length(Experiments),
    Count = length(Zeros),
    fuse_database:update(Density, [
        {Fuse, {IOC, enable_guess}}
        ||
        {Fuse, {output, IOC, as, 0}} <- Zeros
    ]).

%%--------------------------------------------------------------------

invert(Density) ->
    io:format(" => ~s output invert~n", [Density]),
    Device = density:largest_device(Density),
    {ok, Experiments} = experiment:compile_to_fuses([
        source(Device, Pin, 1)
        ||
        Pin <- device:pins(Device)
    ]),
    Matrix = matrix:build(Experiments),
    Ones = matrix:single_ones(Matrix),
    Zeros = matrix:single_zeros(Matrix),
    Count = length(Experiments),
    Count = length(Ones),
    Count = length(Zeros),
    fuse_database:update(Density, [
        {Fuse, {IOC, enable_guess}}
        ||
        {Fuse, {output, IOC, as, 1}} <- Zeros
    ]),
    fuse_database:update(Density, [
        {Fuse, {IOC, invert_guess}}
        ||
        {Fuse, {output, IOC, as, 1}} <- Ones
    ]).

%%--------------------------------------------------------------------

source(Device, {Pin, IOC}, Bit) ->
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

