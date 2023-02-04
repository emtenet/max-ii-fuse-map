-module(rcf_file).

-export([read/1]).
-export([decode/1]).

-export_type([rcf/0]).

-type rcf() :: #{
    device => binary(),
    signals := #{name() => signal()},
    iocs := #{ioc() => signal()},
    lcs := #{lc() => signal()}
}.

-type name() :: atom() | binary().

-type signal() :: #{
    name := name(),
    dests := [dest()],
    ioc => ioc(),
    lc => lc()
}.

-type dest() :: #{
    name := name(),
    port => dest_port(),
    route_port => route_port(),
    ioc => ioc(),
    lc => lc()
}.

-type dest_port() :: clk | data_a | data_b | data_c | data_d | data_in | s_clk.

-type route_port() :: data_a | data_b | data_c | data_d.

-type ioc() :: ioc:ioc().
-type lc() :: lc:lc().

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% read
%%====================================================================

-spec read(file:name_all()) -> {ok, rcf()}.

read(File) ->
    {ok, Data} = file:read_file(File),
    decode(Data).

%%====================================================================
%% decode
%%====================================================================

-spec decode(binary()) -> {ok, rcf()}.

decode(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    decode_lines(Lines, #{
        signals => #{},
        iocs => #{},
        lcs => #{}
    }).

%%--------------------------------------------------------------------

decode_lines([], RCF) ->
    {ok, RCF};
decode_lines([Line | Lines], RCF0) ->
    case decode(Line, RCF0) of
        skip ->
            decode_lines(Lines, RCF0);

        global ->
            decode_global_lines(Lines, RCF0, #{});

        {signal, Signal} ->
            decode_signal_lines(Lines, RCF0, Signal, [], #{})
    end.

%%--------------------------------------------------------------------

decode(<<>>, _) ->
    skip;
decode(<<"#", _/binary>>, _) ->
    skip;
decode(<<"section global_data {">>, _) ->
    global;
decode(<<"signal_name = ", Line/binary>>, _) ->
    [Signal, Comment] = binary:split(Line, <<" {\t#">>),
    case Comment of
        <<"LC_", _/binary>> ->
            {signal, #{
                name => decode_name(Signal),
                lc => lc:parse(Comment),
                dests => []
            }};

        <<"IOC_", _/binary>> ->
            {signal, #{
                name => decode_name(Signal),
                ioc => ioc:parse(Comment),
                dests => []
            }}
    end.

%%--------------------------------------------------------------------

decode_global_lines([<<"}">> | Lines], RCF, Global) ->
    decode_lines(Lines, RCF#{global => Global});
decode_global_lines([Line | Lines], RCF, Global0) ->
    case decode_global(Line, Global0) of
        skip ->
            decode_global_lines(Lines, RCF, Global0);

        {ok, Global} ->
            decode_global_lines(Lines, RCF, Global)
    end.

%%--------------------------------------------------------------------

decode_global(<<"\trcf_written_by = \"", _/binary>>, _) ->
    skip;
decode_global(<<"\tdevice = ", Line/binary>>, Global) ->
    Size = byte_size(Line) - 1,
    <<Device:Size/binary, ";">> = Line,
    {ok, Global#{device => Device}}.

%%--------------------------------------------------------------------

decode_name(Name) ->
    try
        binary_to_existing_atom(Name)
    catch
        error:badarg ->
            Name
    end.

%%--------------------------------------------------------------------

decode_coord(<<"X", X, "Y", Y, "S", S, "I", I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {X - $0, Y - $0, S - $0, I - $0};
decode_coord(<<"X", X, "Y", Y, "S", S, "I", I10, I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {X - $0, Y - $0, S - $0, (10 * (I10 - $0)) + I - $0};
decode_coord(<<"X", X, "Y1", Y, "S", S, "I", I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {X - $0, 10 + Y - $0, S - $0, I - $0};
decode_coord(<<"X", X, "Y1", Y, "S", S, "I", I10, I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {X - $0, 10 + Y - $0, S - $0, (10 * (I10 - $0)) + I - $0};
decode_coord(<<"X1", X, "Y", Y, "S", S, "I", I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {10 + X - $0, Y - $0, S - $0, I - $0};
decode_coord(<<"X1", X, "Y", Y, "S", S, "I", I10, I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {10 + X - $0, Y - $0, S - $0, (10 * (I10 - $0)) + I - $0};
decode_coord(<<"X1", X, "Y1", Y, "S", S, "I", I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {10 + X - $0, 10 + Y - $0, S - $0, I - $0};
decode_coord(<<"X1", X, "Y1", Y, "S", S, "I", I10, I, ";">>)
        when ?IS_DIGIT(X) andalso ?IS_DIGIT(Y) andalso
             ?IS_DIGIT(S) andalso ?IS_DIGIT(I) ->
    {10 + X - $0, 10 + Y - $0, S - $0, (10 * (I10 - $0)) + I - $0}.

%%--------------------------------------------------------------------

decode_signal_lines([<<"}">> | Lines], RCF, Signal, Stack, _) ->
    [] = Stack,
    case Signal of
        #{name := Name, lc := LC}  ->
            #{signals := Signals, lcs := LCS} = RCF,
                decode_lines(Lines, RCF#{
                    signals => Signals#{Name => Signal},
                    lcs => LCS#{LC => Signal}
                });

        #{name := Name, ioc := IOC} ->
            #{signals := Signals, iocs := IOCS} = RCF,
                decode_lines(Lines, RCF#{
                    signals => Signals#{Name => Signal},
                    iocs => IOCS#{IOC => Signal}
                })
    end;
decode_signal_lines([<<>> | Lines], RCF, Signal, [], Labels) ->
    decode_signal_lines(Lines, RCF, Signal, [], Labels);
decode_signal_lines([Line0 | Lines], RCF, Signal0, Stack0, Labels0) ->
    <<"\t", Line/binary>> = Line0,
    case decode_signal(Line) of
        {branch_point, Label} ->
            [] = Stack0,
            #{Label := Stack} = Labels0,
            decode_signal_lines(Lines, RCF, Signal0, Stack, Labels0);

        {push, Top} ->
            Stack = [Top | Stack0],
            decode_signal_lines(Lines, RCF, Signal0, Stack, Labels0);

        {push, Label, Top} ->
            Stack = [Top | Stack0],
            Labels = Labels0#{Label => Stack},
            decode_signal_lines(Lines, RCF, Signal0, Stack, Labels);

        {dest, Dest} ->
            #{dests := Dests} = Signal0,
            Signal = Signal0#{
                dests => [Dest#{route => Stack0} | Dests]
            },
            decode_signal_lines(Lines, RCF, Signal, [], Labels0)
    end.

%%--------------------------------------------------------------------

decode_signal(<<"branch_point = ", Line/binary>>) ->
    Size = byte_size(Line) - 1,
    <<Label:Size/binary, ";">> = Line,
    {branch_point, Label};
decode_signal(<<"label = ", Line/binary>>) ->
    [Label, Rest] = binary:split(Line, <<", ">>),
    {push, Route} = decode_signal(Rest),
    {push, Label, Route};
decode_signal(<<"C4:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {c4, X, Y, S, I}};
decode_signal(<<"CLK_BUFFER:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {clk_buffer, X, Y, S, I}};
decode_signal(<<"GLOBAL_CLK_H:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {global_clk_h, X, Y, S, I}};
decode_signal(<<"IO_BYPASS_OUT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_bypass_out, X, Y, S, I}};
decode_signal(<<"IO_DATAOUT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_data_out, X, Y, S, I}};
decode_signal(<<"LAB_CLK:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lab_clk, X, Y, S, I}};
decode_signal(<<"LAB_CONTROL_MUX:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lab_control_mux, X, Y, S, I}};
decode_signal(<<"LE_BUFFER:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {le_buffer, X, Y, S, I}};
decode_signal(<<"LOCAL_INTERCONNECT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {local_interconnect, X, Y, S, I}};
decode_signal(<<"LOCAL_LINE:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {local_line, X, Y, S, I}};
decode_signal(<<"LUT_CHAIN:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lut_chain, X, Y, S, I}};
decode_signal(<<"R4:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {r4, X, Y, S, I}};
decode_signal(<<"dest = ( ", Line/binary>>) ->
    [Name, Rest] = binary:split(Line, <<", ">>),
    decode_dest(decode_name(Name), Rest).

%%--------------------------------------------------------------------

decode_dest(Name, <<"CLK );\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => clk,
        lc => lc:parse(LC)
    }};
decode_dest(Name, <<"DATAA ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_a, Line);
decode_dest(Name, <<"DATAB ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_b, Line);
decode_dest(Name, <<"DATAC ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_c, Line);
decode_dest(Name, <<"DATAD ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_d, Line);
decode_dest(Name, <<"DATAIN );\t#", IOC/binary>>) ->
    {dest, #{
        name => Name,
        port => data_in,
        ioc => ioc:parse(IOC)
    }};
decode_dest(Name, <<"SCLR );\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => s_clr,
        lc => lc:parse(LC)
    }}.

%%--------------------------------------------------------------------

decode_dest_route(Name, Port, <<"DATAA;\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_a,
        lc => lc:parse(LC)
    }};
decode_dest_route(Name, Port, <<"DATAB;\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_b,
        lc => lc:parse(LC)
    }};
decode_dest_route(Name, Port, <<"DATAC;\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_c,
        lc => lc:parse(LC)
    }};
decode_dest_route(Name, Port, <<"DATAD;\t#", LC/binary>>) ->
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_d,
        lc => lc:parse(LC)
    }}.

%%====================================================================
%% test
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

led_test() ->
    {ok, _} = read("tests/led.rcf").

-endif.

