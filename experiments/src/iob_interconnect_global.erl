-module(iob_interconnect_global).

-export([run/0]).

% This experiment sends global networks in to the IOB interconnects.
%
% It appears to only be to interconnects 8 and 17 with the from4
% mux being suplimented by an additional one-hot entry.
%
% fuse: {X,Y,line,20,side,10} (left)
% fuse: {X,Y,line,20,side,7} (right)
% name: {{iob,X,Y},{interconnect,8},from4,gclk}
%
% fuse: {X,Y,line,25,side,10} (left)
% fuse: {X,Y,line,25,side,7} (right)
% name: {{iob,X,Y},{interconnect,17},from4,gclk}

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Gs = device:gclk_pins(Device),
    Pins = lists:subtract(device:pins(Device), Gs),
    lists:foreach(fun (Pin) -> block(Density, Device, Pin, Gs) end, Pins).

%%--------------------------------------------------------------------

block(Density, Device, Out, [A, B, C, D]) ->
    io:format(" ==> ~p ~p~n", [Density, Out]),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        experiment(Device, Out, A, B, true),
        experiment(Device, Out, A, C, true),
        experiment(Device, Out, A, D, true),
        experiment(Device, Out, B, C, true),
        experiment(Device, Out, B, D, true),
        experiment(Device, Out, C, D, true),
        experiment(Device, Out, A, B, false),
        experiment(Device, Out, A, C, false),
        experiment(Device, Out, A, D, false),
        experiment(Device, Out, B, C, false),
        experiment(Device, Out, B, D, false),
        experiment(Device, Out, C, D, false)
    ]),
    _ = Experiments,
    %Minimal = {minimal, epm240_minimal:fuses()},
    %Matrix = matrix:build_with_map(epm240, [Minimal | Experiments]),
    %%Matrix = matrix:remove_fuses(Matrix0, fun
    %%    ({{iob, 2, 3}, _}) -> false;
    %%    ({{iob, 2, 3}, _, _}) -> false;
    %%    ({{iob, 2, 3}, _, _, _}) -> false;
    %%    ({{ioc, 2, 3, _}, _}) -> false;
    %%    ({{ioc, 2, 3, _}, _, _}) -> false;
    %%    ({{ioc, 2, 3, _}, _, _, _}) -> false;
    %%    (_) -> true
    %%end),
    %matrix:print(Matrix),
    %lists:foreach(fun routes/1, Experiments),
    %throw(stop),
    ok.

%%--------------------------------------------------------------------

%routes({Experiment, _, #{signals := Signals}}) ->
%    io:format(" ==> ~p~n", [Experiment]),
%    maps:foreach(fun (Signal, #{dests := Dests}) ->
%        lists:foreach(fun (#{port := Port, route := Route}) ->
%            io:format("  ~w -> ~w ~w~n", [Signal, Route, Port])
%        end, Dests)
%    end, Signals).

%%--------------------------------------------------------------------

experiment(Device, Out, In, OE, Global) ->
    #{
        title => {Out, In, OE},
        device => Device,
        settings => [
            {global_clock, i, true},
            {global_clock, oe, Global},
            {location, i, In},
            {location, o, Out},
            {location, oe, OE}
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
            "    i => i,\n"
            "    o => o,\n"
            "    oe => oe\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

