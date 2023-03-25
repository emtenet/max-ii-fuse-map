-module(max_ii).

-export([n_list/0]).

-export_type([x/0]).
-export_type([y/0]).
-export_type([n/0]).
-export_type([i/0]).

-export_type([le_buffer/0]).
-export_type([le_buffer_index/0]).

-export_type([c4/0]).
-export_type([c4_index/0]).

-export_type([r4/0]).
-export_type([r4_index/0]).

-type x() :: 0..21.
-type y() :: 0..13.
-type n() :: 0..9.
-type i() :: 0..3.

-type le_buffer() :: {le_buffer, x(), y(), 0, le_buffer_index()}.
-type le_buffer_index() :: 0..19.

-type c4() :: {c4, x(), y(), 0, c4_index()}.
-type c4_index() :: 0..63.

-type r4() :: {r4, x(), y(), 0, r4_index()}.
-type r4_index() :: 0..63.

%%====================================================================
%% n_list
%%====================================================================

-spec n_list() -> [n()].

n_list() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].

