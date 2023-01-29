-module(pof_file).

-export([read/1]).
-export([decode/1]).
-export([fuses/1]).

%%====================================================================
%% read
%%====================================================================

read(File) ->
    {ok, Data} = file:decode_file(File),
    decode(Data).

%%====================================================================
%% decode
%%====================================================================

decode(<<"POF", 0, 0, 0, 1, 0, Count:32/little-unsigned, Data/binary>>) ->
    decode_parts(Count, Data, #{}).

%%--------------------------------------------------------------------

decode_parts(0, <<>>, Parts) ->
    Parts;
decode_parts(N, <<Id:16/little, Size:32/little, Data/binary>>, Parts) ->
    <<Part:Size/binary, Rest/binary>> = Data,
    decode_parts(N - 1, Rest, decode_part(Id, Part, Parts)).

%%--------------------------------------------------------------------

decode_part(1, Data, Parts) ->
    decode_string(compiler, Data, Parts);
decode_part(2, Data, Parts) ->
    decode_string(device, Data, Parts);
decode_part(3, Data, Parts) ->
    decode_string(name, Data, Parts);
decode_part(17, Data, Parts) ->
    decode_block(cfm, <<0,0,0,0,0,0,0,176,1,0,1,0>>, Data, Parts);
decode_part(24, Data, Parts) ->
    decode_block(ufm, <<0,0,0,0,0,0,0,32,0,0,1,0>>, Data, Parts);
decode_part(5, Data, Parts) ->
    decode_skip(<<0,0>>, Data, Parts);
decode_part(8, _Data, Parts) ->
    % checksum ?
    Parts.

%%--------------------------------------------------------------------

decode_string(Name, Data, Parts) ->
    Size = byte_size(Data) - 1,
    <<String:Size/binary, 0>> = Data,
    Parts#{Name => String}.

%%--------------------------------------------------------------------

decode_block(Name, Expect, <<Header:12/binary, Data/binary>>, Parts) ->
    Expect = Header,
    Parts#{Name => #{
        data => Data,
        size => 8 * byte_size(Data)
    }}.

%%--------------------------------------------------------------------

decode_skip(Expect, Data, Parts) ->
    Expect = Data,
    Parts.

%%====================================================================
%% fuses
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fuses_test() ->
    CFM = <<16#fe, 16#ff, 16#ff, 16#ff, 16#fd, 16#3f, 16#9f, 16#e7>>,
    POF = #{cfm => #{data => CFM}},
    Fuses = [0, 33, 46, 47, 53, 54, 59, 60],
    ?assertEqual(Fuses, fuses(POF)).

-endif.

%%--------------------------------------------------------------------

fuses(#{cfm := #{data := Bytes}}) ->
    fuses_bytes(0, Bytes, []).

%%--------------------------------------------------------------------

fuses_bytes(_, <<>>, Fuses) ->
    lists:reverse(Fuses);
fuses_bytes(Fuse, <<255, Bytes/binary>>, Fuses) ->
    fuses_bytes(Fuse + 8, Bytes, Fuses);
fuses_bytes(Fuse, <<Byte, Bytes/binary>>, Fuses) ->
    fuses_bytes(Fuse + 8, Bytes, fuses_byte(8, Fuse, Byte, Fuses)).

%%--------------------------------------------------------------------

fuses_byte(0, _, _, Fuses) ->
    Fuses;
fuses_byte(N, Fuse, Byte, Fuses) when Byte band 1 =:= 0 ->
    fuses_byte(N - 1, Fuse + 1, Byte bsr 1, [Fuse | Fuses]);
fuses_byte(N, Fuse, Byte, Fuses) ->
    fuses_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses).

