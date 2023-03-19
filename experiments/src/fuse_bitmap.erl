-module(fuse_bitmap).

-export([print_database/1]).
-export([print_map/1]).
-export([print_minimal/1]).

-type density() :: density:density().

%%====================================================================
%% print
%%====================================================================

-spec print_database(density()) -> ok.

print_database(Density) ->
    FuseCount = density:fuse_count(Density),
    Database = fuse_database:read(Density),
    lines(0, FuseCount, Database).

%%--------------------------------------------------------------------

-spec print_map(density()) -> ok.

print_map(Density) ->
    FuseCount = density:fuse_count(Density),
    lines(0, FuseCount, Density).

%%--------------------------------------------------------------------

-spec print_minimal(density()) -> ok.

print_minimal(Density) ->
    FuseCount = density:fuse_count(Density),
    {ok, Database} = minimal_experiment:fuses(Density),
    lines(0, FuseCount, {Density, Database}).

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

fuse(Fuse, {Density, Fuses}) when is_list(Fuses) ->
    case lists:member(Fuse, Fuses) of
        true ->
            case fuse_map:to_name(Fuse, Density) of
                {ok, _} -> $M;
                {error, _} -> $?
            end;

        false -> $.
    end;
fuse(Fuse, Density) when is_atom(Density) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Name} ->
            fuse(Name);

        {error, _} ->
            $.
    end;
fuse(Fuse, Database) ->
    case fuse_database:name(Fuse, Database) of
        Name when is_integer(Name) ->
            $.;

        Name ->
            fuse(Name)
    end.

%%--------------------------------------------------------------------

fuse({_IOC, bus_hold}) -> $B;
fuse({_IOC, enable}) -> $E;
fuse({_IOC, output_invert}) -> $I;
fuse({_IOC, weak_pull_up}) -> $W;
fuse({_LAB, clk1_global0}) -> $k;
fuse({_LAB, clk1_global1}) -> $k;
fuse({_LAB, clk1_global2}) -> $k;
fuse({_LAB, clk1_global3}) -> $k;
fuse({_LAB, clk1_invert}) -> $k;
fuse({_LAB, clk2_global0}) -> $k;
fuse({_LAB, clk2_global1}) -> $k;
fuse({_LAB, clk2_global2}) -> $k;
fuse({_LAB, clk2_global3}) -> $k;
fuse({_LAB, clk2_invert}) -> $k;
fuse({_LAB, clr1_global0}) -> $r;
fuse({_LAB, clr1_global1}) -> $r;
fuse({_LAB, clr1_global2}) -> $r;
fuse({_LAB, clr1_global3}) -> $r;
fuse({_LAB, clr1_invert}) -> $r;
fuse({_LC, clk}) -> $K;
fuse({_LC, clr}) -> $R;
fuse({_LC, local_line}) -> $L;
fuse({_LC, lut, _}) -> $#;
fuse({user_code, _}) -> $U;
fuse(_) -> $~.


