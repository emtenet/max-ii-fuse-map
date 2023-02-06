-module(fuse_bitmap).

-export([print/1]).

-type density() :: density:density().

%%====================================================================
%% print
%%====================================================================

-spec print(density()) -> ok.

print(Density) ->
    FuseCount = density:fuse_count(Density),
    Database = fuse_database:read(Density),
    lines(0, FuseCount, Database).

%%--------------------------------------------------------------------

lines(Start, Stop, _) when Start >= Stop ->
    ok;
lines(Start, Stop, Database) ->
    Line = line(Start, Start + 64, Database, <<>>),
    io:format("~6b: ~s~n", [Start, Line]),
    lines(Start + 64, Stop, Database).

%%--------------------------------------------------------------------

line(Stop, Stop, _, Line) ->
    Line;
line(Fuse, Stop, Database, Line) ->
    C = fuse(Fuse, Database),
    line(Fuse + 1, Stop, Database, <<Line/binary, C>>).

%%--------------------------------------------------------------------

fuse(Fuse, Database) ->
    case fuse_database:name(Fuse, Database) of
        Name when is_integer(Name) -> $.;
        {_, bus_hold} -> $B;
        {_, lut, _} -> $L;
        {_, weak_pull_up} -> $W;
        {user_code, _} -> $U;
        _ -> $?
    end.


