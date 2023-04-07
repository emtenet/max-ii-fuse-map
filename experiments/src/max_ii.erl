-module(max_ii).

-export([n_list/0]).

-export_type([x/0]).
-export_type([y/0]).
-export_type([n/0]).
-export_type([i/0]).

-export_type([mux3/0]).
-export_type([mux4/0]).

-export_type([le_buffer/0]).

-export_type([c4/0]).
-export_type([c4_block/0]).
-export_type([c4_index/0]).
-export_type([c4_select/0]).

-export_type([r4/0]).
-export_type([r4_block/0]).
-export_type([r4_index/0]).
-export_type([r4_select/0]).

-type x() :: 0..21.
-type y() :: 0..13.
-type n() :: 0..9.
-type i() :: 0..3.

-type mux3() :: mux0 | mux1 | mux2.
-type mux4() :: mux0 | mux1 | mux2 | mux3.

-type le_buffer() :: {le_buffer, x(), y(), 0, 0..19}.

-type c4() :: {c4, x(), y(), 0, 0..63}.
-type c4_block() :: {c4, max_ii:x(), max_ii:y()}.
-type c4_index() :: {mux, 0..13}.
-type c4_select() ::
    direct_link |
    direct_in_0 | direct_in_1 | direct_in_2 | direct_in_3 |
    {mux4(), mux3()}.

-type r4() :: {r4, x(), y(), 0, 0..63}.
-type r4_block() :: {r4, max_ii:x(), max_ii:y()}.
-type r4_index() :: {mux, 0..15}.
-type r4_select() :: direct_link | {mux4(), mux3()}.

%%====================================================================
%% n_list
%%====================================================================

-spec n_list() -> [n()].

n_list() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].

