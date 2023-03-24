-module(r4_fuse_name_experiment).

% This experiment is an aid to naming the fuses involved with
% muxing signals into R4 interconnects.
%
% There are multiple muxes involved that do not _yet_ appear to have
% a direct repationship to the R4 interconnect or signals.
%
% Name these muxes as _ports_ connected to their x,y fuse location.
%
% Check that fuse pairs produce:
%  * matching x,y coordinates
%  * matching port numbers
%  * an axis of size 3
%  * an axis of size 4
%
% Check that direct-link fuses are named appropriatelly.

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    density(epm240),
    density(epm570),
    density(epm1270),
    density(epm2210),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    {ok, Blocks} = r4_fuses_database:open(Density),
    maps:foreach(fun (_Block, Indexes) ->
        block(Density, Indexes)
    end, Blocks),
    ok.

%%--------------------------------------------------------------------

block(Density, Indexes) ->
    maps:foreach(fun (_Index, Mux) ->
        index(Density, Mux)
    end, Indexes).

%%--------------------------------------------------------------------

index(Density, Mux) ->
    maps:foreach(fun (Entry, _From) ->
        entry(Density, Entry)
    end, Mux).

%%--------------------------------------------------------------------

entry(Density, {Left, Right}) ->
    {ok, L} = fuse_map:to_name(Left, Density),
    {ok, R} = fuse_map:to_name(Right, Density),
    port(L, R, Left, Right);
entry(Density, Fuse) ->
    {ok, F} = fuse_map:to_name(Fuse, Density),
    port(F, Fuse).

%%--------------------------------------------------------------------

port({R4 = {r4, _, _, port, _}, port3, _}, {R4, port4, _}, _, _) -> ok;
port({R4 = {r4, _, _, port, _}, port4, _}, {R4, port3, _}, _, _) -> ok.

%%--------------------------------------------------------------------

port({{r4, _, _, port, _}, direct_link}, _) -> ok.

