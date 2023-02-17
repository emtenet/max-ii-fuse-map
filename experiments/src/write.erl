-module(write).

-export([format/2]).

-export_type([format/0]).

-type format() ::
    [format()] |
    binary() |
    integer() |
    data_a | data_b | data_c | data_d |
    {local_line, non_neg_integer()} |
    {list, [format()], format()} |
    tuple().

%%====================================================================
%% format
%%====================================================================

-spec format(binary(), format()) -> binary().

format(To, Binary) when is_binary(Binary) ->
    <<To/binary, Binary/binary>>;
format(To, []) ->
    To;
format(To, [F | Fs]) ->
    format(format(To, F), Fs);
format(To, Number) when is_integer(Number) ->
    Binary = integer_to_binary(Number),
    <<To/binary, Binary/binary>>;
format(To, data_a) ->
    <<To/binary, "DATAA">>;
format(To, data_b) ->
    <<To/binary, "DATAB">>;
format(To, data_c) ->
    <<To/binary, "DATAC">>;
format(To, data_d) ->
    <<To/binary, "DATAD">>;
format(To, {local_line, N}) ->
    C = $0 + (N rem 10),
    <<To/binary, "LOCAL_LINE", C>>;
format(To, {list, Ts, Sep}) ->
    list_sep(To, Ts, Sep);
format(To, Tuple) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    Module:write(To, Tuple).

%%--------------------------------------------------------------------

list_sep(To, [], _) ->
    To;
list_sep(To, [T], _) ->
    format(To, T);
list_sep(To, [T | Ts], Sep) ->
    list_sep(format(format(To, T), Sep), Ts, Sep).

