-module(fuse_bitmap).

-export([print/1]).
-export([print_minimal/1]).

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

-spec print_minimal(density()) -> ok.

print_minimal(Density) ->
    FuseCount = density:fuse_count(Density),
    {ok, Database} = minimal_experiment:fuses(Density),
    lines(0, FuseCount, Database).

%%--------------------------------------------------------------------

lines(Start, Stop, _) when Start >= Stop ->
    ok;
lines(Start, Stop, Database) ->
    Line = line(Start, Start + 64, Database, <<>>),
    io:format("~6b:~s~n", [Start, Line]),
    lines(Start + 64, Stop, Database).

%%--------------------------------------------------------------------

line(Stop, Stop, _, Line) ->
    Line;
line(Fuse, Stop, Database, Line) when Fuse rem 8 =:= 0 ->
    C = fuse(Fuse, Database),
    line(Fuse + 1, Stop, Database, <<Line/binary, " ", C>>);
line(Fuse, Stop, Database, Line) ->
    C = fuse(Fuse, Database),
    line(Fuse + 1, Stop, Database, <<Line/binary, C>>).

%%--------------------------------------------------------------------

fuse(Fuse, Fuses) when is_list(Fuses) ->
    case lists:member(Fuse, Fuses) of
        true -> $M;
        false -> $.
    end;
fuse(Fuse, Database) ->
    case fuse_database:name(Fuse, Database) of
        Name when is_integer(Name) -> $.;
        {_IOC, bus_hold} -> $B;
        {_IOC, enable_guess} -> $E;
        {_IOC, invert_guess} -> $I;
        {_IOC, weak_pull_up} -> $W;
        {_LAB, clk1_global0} -> $k;
        {_LAB, clk1_global1} -> $k;
        {_LAB, clk1_global2} -> $k;
        {_LAB, clk1_global3} -> $k;
        {_LAB, clk1_invert} -> $k;
        {_LAB, clk2_global0} -> $k;
        {_LAB, clk2_global1} -> $k;
        {_LAB, clk2_global2} -> $k;
        {_LAB, clk2_global3} -> $k;
        {_LAB, clk2_invert} -> $k;
        {_LAB, clr1_global0} -> $r;
        {_LAB, clr1_global1} -> $r;
        {_LAB, clr1_global2} -> $r;
        {_LAB, clr1_global3} -> $r;
        {_LAB, clr1_invert} -> $r;
        {_LC, clk} -> $K;
        {_LC, clr} -> $R;
        {_LC, data_a, mux} -> $A;
        {_LC, data_b, mux} -> $B;
        {_LC, data_c, mux} -> $C;
        {_LC, data_d, mux} -> $D;
        {_LC, data_d, lut_chain} -> $>;
        {_LC, local_line} -> $L;
        {_LC, lut, _} -> $#;
        {user_code, _} -> $U;
        _ -> $~
    end.


