-module(iob_interconnect_mux_database).

-export([run/0]).

-export([read/1]).
-export([update/2]).

-export_type([entry/0]).

-type density() :: density().
-type iob() :: iob:iob().
-type x() :: max_ii:x().
-type y() :: max_ii:y().

-type entry() :: {iob(), interconnect(), select(), from()}.
-type interconnect() :: {interconnect, 0..17}.
-type select() :: direct_link | {mux4(), mux3()}.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.
-type from() :: le_buffer() | c4() | r4().
-type le_buffer() :: {le_buffer, x(), y(), 0, 0..19}.
-type c4() :: {c4, x(), y(), 0, non_neg_integer()}.
-type r4() :: {r4, x(), y(), 0, non_neg_integer()}.

%%====================================================================
%% run
%%====================================================================

run() ->
    Es = merge(),
    [
        run(I, Mux4, Mux3, Es)
        ||
        I <- lists:seq(0, 17),
        Mux4 <- [mux0, mux1, mux2, mux3],
        Mux3 <- [mux0, mux1, mux2]
    ],
    ok.

%%--------------------------------------------------------------------

run(I, Mux4, Mux3, Es) ->
    Fs = lists:filter(fun
        ({_, {interconnect, If}, {Mux4f, Mux3f}, _}) ->
            (If =:= I) andalso (Mux4f =:= Mux4) andalso (Mux3f =:= Mux3);
        (_) ->
            false
    end, Es),
    case detect(Fs) of
        false when Fs =:= [] ->
            io:format(
                "to_from(X, Y, ~p, ~p, ~p) -> ?; % 0 samples~n",
                [I, Mux4, Mux3]
            );

        {false, false, Y, false} when Y =/= false ->
            table_axis_x_n(I, Mux4, Mux3, Y, Fs);

        {false, X, Y, N} ->
            throw({I, Mux4, Mux3, variable_axis, X, Y, N});

        {Axis, false, false, false} ->
            table_x_y_n(I, Mux4, Mux3, Axis, Fs);

        {Axis, X, false, false} ->
            table_y_n(I, Mux4, Mux3, Axis, X, Fs);

        {Axis, false, Y, false} ->
            table_x_n(I, Mux4, Mux3, Axis, Y, Fs);

        {Axis, false, false, N} ->
            table_x_y(I, Mux4, Mux3, Axis, N, Fs);

        {Axis, false, Y, N} ->
            table_x(I, Mux4, Mux3, Axis, Y, N, Fs);

        {Axis, X, false, N} ->
            table_y(I, Mux4, Mux3, Axis, X, N, Fs);

        {Axis, X, Y, false} ->
            table_n(I, Mux4, Mux3, Axis, X, Y, Fs);

        {Axis, X, Y, N} ->
            table(I, Mux4, Mux3, Axis, X, Y, N)
    end.

%%--------------------------------------------------------------------

detect([]) ->
    false;
detect([{{iob, X, Y}, _, _, {Axis, XX, YY, 0, N}} | Es]) ->
    detect(Es, Axis, XX - X, YY - Y, N).

%%--------------------------------------------------------------------

detect([], Axis, X, Y, N) ->
    {Axis, X, Y, N};
detect([{{iob, X, Y}, _, _, {Axis, XX, YY, 0, N}} | Es], Axis0, X0, Y0, N0) ->
    detect(
        Es,
        detect_same(Axis, Axis0),
        detect_same(XX - X, X0),
        detect_same(YY - Y, Y0),
        detect_same(N, N0)
    ).

%%--------------------------------------------------------------------

detect_same(Same, Same) ->
    Same;
detect_same(_, _) ->
    false.

%%--------------------------------------------------------------------

table(Es, Header, Cell) ->
    {MaxX, MaxY} = table_max(Es, 0, 0),
    table_headers(MaxX, Header),
    table_rows(MaxX, MaxY, Es, Cell),
    ok.

%%--------------------------------------------------------------------

table_max([], MaxX, MaxY) ->
    {MaxX, MaxY};
table_max([{{iob, X, Y}, _, _, _} | Es], MaxX, MaxY) ->
    table_max(Es, max(X, MaxX), max(Y, MaxY)).

%%--------------------------------------------------------------------

table_headers(MaxX, With) ->
    io:format("   ", []),
    table_headers(1, MaxX, With).

%%--------------------------------------------------------------------

table_headers(X, MaxX, _) when X > MaxX ->
    io:format("~n", []);
table_headers(X, MaxX, With) ->
    With(X),
    table_headers(X + 1, MaxX, With).

%%--------------------------------------------------------------------

table_axis_header(Axis) ->
    [atom_to_binary(Axis), <<",">>].

%%--------------------------------------------------------------------

table_offset_header(Name, 0) ->
    io_lib:format("~c,", [Name]);
table_offset_header(Name, Offset) when Offset < 0 ->
    io_lib:format("~c - ~p,", [Name, -Offset]);
table_offset_header(Name, Offset) ->
    io_lib:format("~c + ~p,", [Name, Offset]).

%%--------------------------------------------------------------------

table_rows(_, -1, _, _) ->
    ok;
table_rows(MaxX, Y, Es, With) ->
    io:format("~2b|", [Y]),
    table_col(1, MaxX, Y, Es, With),
    table_rows(MaxX, Y - 1, Es, With).

%%--------------------------------------------------------------------

table_col(X, MaxX, _, _, _) when X > MaxX ->
    io:format("~n", []);
table_col(X, MaxX, Y, Es, With) ->
    table_cell(X, Y, Es, With),
    table_col(X + 1, MaxX, Y, Es, With).

%%--------------------------------------------------------------------

table_cell(_, _, [], With) ->
    With(undefined);
table_cell(X, Y, [{{iob, X, Y}, _, _, From} | _], With) ->
    With(From);
table_cell(X, Y, [_ | Es], With) ->
    table_cell(X, Y, Es, With).

%%--------------------------------------------------------------------

table(I, Mux4, Mux3, Axis, X, Y, N) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ~-6s ~-6s 0, ~p};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($X, X),
        table_offset_header($Y, Y),
        N
        ]).

%%--------------------------------------------------------------------

table_axis_x_n(I, Mux4, Mux3, Y, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {?,           ?,     ~-6s 0, ?};~n", [
        I, Mux4, Mux3,
        table_offset_header($Y, Y)
    ]),
    table(Es, fun table_axis_x_n_header/1, fun table_axis_x_n_cell/1).

%%--------------------------------------------------------------------

table_axis_x_n_header(X) ->
    io:format("   ~3b   ", [X]).

%%--------------------------------------------------------------------

table_axis_x_n_cell(undefined) ->
    io:format("         ", []);
table_axis_x_n_cell({Axis, X, _, 0, N}) ->
    io:format(" ~2s,~2b,~2b", [Axis, X, N]).

%%--------------------------------------------------------------------

table_n(I, Mux4, Mux3, Axis, X, Y, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ~-6s ~-6s 0, ?};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($X, X),
        table_offset_header($Y, Y)
    ]),
    table(Es, fun table_n_header/1, fun table_n_cell/1).

%%--------------------------------------------------------------------

table_n_header(X) ->
    io:format(" ~2b", [X]).

%%--------------------------------------------------------------------

table_n_cell(undefined) ->
    io:format("   ", []);
table_n_cell({_, _, _, 0, N}) ->
    io:format(" ~2b", [N]).

%%--------------------------------------------------------------------

table_x(I, Mux4, Mux3, Axis, Y, N, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ?,     ~-6s 0, ~p};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($Y, Y),
        N
    ]),
    table(Es, fun table_x_header/1, fun table_x_cell/1).

%%--------------------------------------------------------------------

table_x_header(X) ->
    io:format(" ~2b", [X]).

%%--------------------------------------------------------------------

table_x_cell(undefined) ->
    io:format("   ", []);
table_x_cell({_, X, _, 0, _}) ->
    io:format(" ~2b", [X]).

%%--------------------------------------------------------------------

table_x_n(I, Mux4, Mux3, Axis, Y, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ?,     ~-6s 0, ?};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($Y, Y)
    ]),
    table(Es, fun table_x_n_header/1, fun table_x_n_cell/1).

%%--------------------------------------------------------------------

table_x_n_header(X) ->
    io:format(" ~3b  ", [X]).

%%--------------------------------------------------------------------

table_x_n_cell(undefined) ->
    io:format("      ", []);
table_x_n_cell({_, X, _, 0, N}) when N < 10 ->
    io:format(" ~2b,~b ", [X, N]);
table_x_n_cell({_, X, _, 0, N}) ->
    io:format(" ~2b,~2b", [X, N]).

%%--------------------------------------------------------------------

table_x_y(I, Mux4, Mux3, Axis, N, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ?,     ?,     0, ~p}n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        N
    ]),
    table(Es, fun table_x_y_header/1, fun table_x_y_cell/1).

%%--------------------------------------------------------------------

table_x_y_header(X) ->
    io:format("  ~3b ", [X]).

%%--------------------------------------------------------------------

table_x_y_cell(undefined) ->
    io:format("         ", []);
table_x_y_cell({_, X, Y, 0, _}) when Y < 10 ->
    io:format(" ~2b,~b ", [X, Y]);
table_x_y_cell({_, X, Y, 0, _}) ->
    io:format(" ~2b,~2b", [X, Y]).

%%--------------------------------------------------------------------

table_x_y_n(I, Mux4, Mux3, Axis, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ?,     ?,     0, ?};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis)
    ]),
    table(Es, fun table_x_y_n_header/1, fun table_x_y_n_cell/1).

%%--------------------------------------------------------------------

table_x_y_n_header(X) ->
    io:format("   ~3b   ", [X]).

%%--------------------------------------------------------------------

table_x_y_n_cell(undefined) ->
    io:format("         ", []);
table_x_y_n_cell({_, X, Y, 0, N}) when Y < 10 andalso N < 10 ->
    io:format("  ~2b,~b,~b ", [X, Y, N]);
table_x_y_n_cell({_, X, Y, 0, N}) when Y < 10 ->
    io:format("  ~2b,~b,~2b", [X, Y, N]);
table_x_y_n_cell({_, X, Y, 0, N}) when N < 10 ->
    io:format(" ~2b,~2b,~b ", [X, Y, N]);
table_x_y_n_cell({_, X, Y, 0, N}) ->
    io:format(" ~2b,~2b,~2b", [X, Y, N]).

%%--------------------------------------------------------------------

table_y(I, Mux4, Mux3, Axis, X, N, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ~-6s ?,     0, ~p};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($X, X),
        N
    ]),
    table(Es, fun table_y_header/1, fun table_y_cell/1).

%%--------------------------------------------------------------------

table_y_header(X) ->
    io:format(" ~2b", [X]).

%%--------------------------------------------------------------------

table_y_cell(undefined) ->
    io:format("   ", []);
table_y_cell({_, _, Y, 0, _}) ->
    io:format(" ~2b", [Y]).

%%--------------------------------------------------------------------

table_y_n(I, Mux4, Mux3, Axis, X, Es) ->
    io:format("to_from(X, Y, ~p, ~p, ~p) -> {~-12s ~-6s ?,     0, ?};~n", [
        I, Mux4, Mux3,
        table_axis_header(Axis),
        table_offset_header($X, X)
    ]),
    table(Es, fun table_y_n_header/1, fun table_y_n_cell/1).

%%--------------------------------------------------------------------

table_y_n_header(X) ->
    io:format(" ~3b  ", [X]).

%%--------------------------------------------------------------------

table_y_n_cell(undefined) ->
    io:format("      ", []);
table_y_n_cell({_, _, Y, 0, N}) when N < 10 ->
    io:format(" ~2b,~b ", [Y, N]);
table_y_n_cell({_, _, Y, 0, N}) ->
    io:format(" ~2b,~2b", [Y, N]).

%%====================================================================
%% read
%%====================================================================

-spec read(density()) -> [entry()].

read(Density) ->
    File = database_file(Density),
    read_file(File).

%%--------------------------------------------------------------------

read_file(File) ->
    case file:consult(File) of
        {ok, Exists} ->
            Exists;

        {error, enoent} ->
            []
    end.

%%====================================================================
%% update
%%====================================================================

-spec update(density(), [entry()]) -> ok.

update(Density, Adds0) ->
    File = database_file(Density),
    Exists = read_file(File),
    Adds = lists:filtermap(fun (Add) -> update_check(Exists, Add) end, Adds0),
    Saves = lists:umerge(Exists, lists:sort(Adds)),
    Lines = [
        io_lib:format("~w.~n", [Save])
        ||
        Save <- Saves
    ],
    ok = file:write_file(File, Lines).

%%--------------------------------------------------------------------

update_check([], Add) ->
    {true, Add};
update_check([{IOB, IC, Select, Exist} | _], {IOB, IC, Select, Exist}) ->
    false;
update_check([{IOB, IC, Select, Exist} | _], {IOB, IC, Select, Add}) ->
    throw({update, IOB, IC, Select, exist, Exist, add, Add});
update_check([_ | Exists], Add) ->
    update_check(Exists, Add).

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/old/~s.iob-interconnect", [Density])).

%%====================================================================
%% merge
%%====================================================================

merge() ->
    merge(read(epm240),
    merge(read(epm570),
    merge(read(epm1270),
          read(epm2210)))).

%%--------------------------------------------------------------------

merge(Ls, Rs) ->
    merge(Ls, Rs, []).

%%--------------------------------------------------------------------

merge([], Rs, Ms) ->
    lists:reverse(Ms, Rs);
merge(Ls, [], Ms) ->
    lists:reverse(Ms, Ls);
merge([L | Ls], Rs = [R | _], Ms) when L < R ->
    merge(Ls, Rs, [L | Ms]);
merge(Ls = [L | _], [R | Rs], Ms) when R < L ->
    merge(Ls, Rs, [R | Ms]);
merge([M | Ls], [M | Rs], Ms) ->
    merge(Ls, Rs, [M | Ms]);
merge([{IOB, IC, Select, L} | _], [{IOB, IC, Select, R} | _], _) ->
    throw({merge, IOB, IC, Select, left, L, right, R}).

