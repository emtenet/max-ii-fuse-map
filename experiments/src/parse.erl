-module(parse).

-export([format/2]).
-export([lc/1]).
-export([number/1]).

-type format() ::
    [format()] |
    binary() |
    atom() |
    {list, format(), format()}.

-type lc() :: lc:lc().

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% format
%%====================================================================

-spec format(binary(), [format()])
    -> {ok, [term()], binary()} |
       {error, binary()}.

format(Line, Fs) when is_list(Fs) ->
    list(Line, Fs, []);
format(Line, Binary) when is_binary(Binary) ->
    Size = byte_size(Binary),
    case Line of
        <<Binary:Size/binary, Rest/binary>> ->
            {ok, Binary, Rest};

        _ ->
            {error, Line}
    end;
format(Line, data) ->
    case Line of
        <<"DATAA", Rest/binary>> ->
            {ok, data_a, Rest};

        <<"DATAB", Rest/binary>> ->
            {ok, data_b, Rest};

        <<"DATAC", Rest/binary>> ->
            {ok, data_c, Rest};

        <<"DATAD", Rest/binary>> ->
            {ok, data_d, Rest};

        _ ->
            {error, Line}
    end;
format(Line, local_line) ->
    case Line of
        <<"LOCAL_LINE", C, Rest/binary>> when ?IS_DIGIT(C) ->
            {ok, {local_line, C - $0}, Rest};

        _ ->
            {error, Line}
    end;
format(Line, number) ->
    number(Line);
format(Line, Module) when is_atom(Module) ->
    Module:parse(Line);
format(Line, {list, Of, Sep}) ->
    list_sep(Line, Of, Sep).

%%====================================================================
%% lc
%%====================================================================

-spec lc(binary()) -> {ok, lc(), binary()}.

lc(<<"LC_X", Line/binary>>) ->
    {ok, [X, Y, N], Rest} = format(Line, [
        number,
        <<"_Y">>,
        number,
        <<"_N">>,
        number
    ]),
    {ok, lc:from(X, Y, N), Rest}.

%%====================================================================
%% list
%%====================================================================

list(Line, [], Ts) ->
    {ok, lists:reverse(Ts), Line};
list(Line, [Binary | Fs], Ts) when is_binary(Binary) ->
    Size = byte_size(Binary),
    case Line of
        <<Binary:Size/binary, Rest/binary>> ->
            list(Rest, Fs, Ts);

        _ ->
            {error, Line}
    end;
list(Line, [F | Fs], Ts) ->
    case format(Line, F) of
        {ok, T, Rest} ->
            list(Rest, Fs, [T | Ts]);

        Error = {error, _} ->
            Error
    end;
list(Line, _, _) ->
    {error, Line}.

%%====================================================================
%% list_sep
%%====================================================================

list_sep(Line, Of, Sep) ->
    case format(Line, Of) of
        {ok, T, Rest} ->
            list_sep_rest(Rest, Of, Sep, [T]);

        {error, Line} ->
            {ok, [], Line};

        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------

list_sep_rest(Line, Of, Sep, Ts) ->
    case format(Line, Sep) of
        {ok, _, Rest0} ->
            case format(Rest0, Of) of
                {ok, T, Rest} ->
                    list_sep_rest(Rest, Of, Sep, [T | Ts]);

                Error = {error, _} ->
                    Error
            end;

        {error, Line} ->
            {ok, lists:reverse(Ts), Line};

        Error = {error, _} ->
            Error
    end.

%%====================================================================
%% number
%%====================================================================

-spec number(binary())
    -> {ok, non_neg_integer(), binary()} |
       {error, binary()}.

number(<<C, Rest/binary>>) when ?IS_DIGIT(C) ->
    number(Rest, C - $0);
number(Line) ->
    {error, Line}.

%%--------------------------------------------------------------------

number(<<C, Rest/binary>>, N) when ?IS_DIGIT(C) ->
    number(Rest, (10 * N) + (C - $0));
number(Rest, N) ->
    {ok, N, Rest}.

