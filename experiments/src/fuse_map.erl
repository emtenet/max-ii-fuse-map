-module(fuse_map).

-export([run/0]).
-export([run/2]).

-export([from_name/2]).
-export([to_location/2]).
-export([to_name/2]).

-export_type([location/0]).
-export_type([row/0]).
-export_type([col/0]).
-export_type([side/0]).
-export_type([type/0]).
-export_type([index/0]).
-export_type([sector/0]).

-type density() :: density:density().
-type fuse() :: fuse:fuse().
-type name() :: fuse:name().

-type location() ::
    {fuse(), header} |
    {fuse(), footer} |
    {row(), col(), strip} |
    {side(), index(), strip, row(), col()} |
    {y(), column, index(), type(), sector()} |
    {x(), skip, index(), type(), sector()} |
    {x(), head, index(), type(), sector()} |
    {x(), tail, index(), type(), sector()} |
    {x(), y(), line, index(), type(), sector()} |
    {x(), y(), n(), i(), type(), sector()}.
-type row() :: non_neg_integer().
-type col() :: non_neg_integer().
-type side() :: left | top | right | bottom.
-type type() :: cell | side.
-type index() :: non_neg_integer().
-type sector() :: non_neg_integer().
-type x() :: max_ii:x().
-type y() :: max_ii:y().
-type n() :: max_ii:n().
-type i() :: max_ii:i().

-record(with, {
    density :: density(),
    strip_width :: 32 | 64,
    offset_x :: 0 | 1,
    side_x :: 7 | 13 | 17 | 21,
    grow_x :: 0 | 9 | 11 | 13,
    short_y :: 4 | 7 | 10,
    long_y :: 4 | 7 | 10 | 13,
    top_y :: 5 | 8 | 11 | 14,
    short_sector :: 256 | 384 | 512,
    long_sector :: 256 | 384 | 512 | 704,
    left_base :: non_neg_integer(),
    short_base :: non_neg_integer(),
    grow_base :: non_neg_integer(),
    long_base :: non_neg_integer(),
    right_base :: non_neg_integer(),
    end_base :: non_neg_integer(),
    sector_skip :: non_neg_integer(),
    left_strip :: non_neg_integer(),
    top_strip :: non_neg_integer(),
    right_strip :: non_neg_integer(),
    bottom_strip :: non_neg_integer(),
    end_strip :: non_neg_integer()
}).

-define(HEAD_WIDTH, 11).
-define(LINE_WIDTH, 46).

-define(SIDE_SECTORS, 13).
-define(COLUMN_SECTORS, 28).
-define(SHORT_SECTORS, 20).
-define(LONG_SECTORS, (?COLUMN_SECTORS - ?SHORT_SECTORS)).

-define(IOB_SIDES(),
    ?IOB_SIDE( 7, 0, 2, {{interconnect, 0}, from3, mux0});
    ?IOB_SIDE( 7, 0, 3, {{interconnect, 0}, from3, mux1});
    ?IOB_SIDE( 7, 1, 0, {{interconnect, 1}, from3, mux0});
    ?IOB_SIDE( 7, 1, 1, {{interconnect, 1}, from3, mux1});
    ?IOB_SIDE( 7, 1, 2, {{interconnect, 2}, from3, mux0});
    ?IOB_SIDE( 7, 1, 3, {{interconnect, 2}, from3, mux1});
    ?IOB_SIDE( 7, 2, 0, {{interconnect, 3}, from3, mux0});
    ?IOB_SIDE( 7, 2, 1, {{interconnect, 3}, from3, mux1});
    ?IOB_SIDE( 7, 2, 2, {{interconnect, 4}, from3, mux0});
    ?IOB_SIDE( 7, 2, 3, {{interconnect, 4}, from3, mux1});
    ?IOB_SIDE( 7, 3, 0, {{interconnect, 5}, from3, mux0});
    ?IOB_SIDE( 7, 3, 1, {{interconnect, 5}, from3, mux1});
    ?IOB_SIDE( 7, 3, 2, {{interconnect, 6}, from3, mux0});
    ?IOB_SIDE( 7, 3, 3, {{interconnect, 6}, from3, mux1});
    ?IOB_SIDE( 7, 4, 0, {{interconnect, 7}, from3, mux0});
    ?IOB_SIDE( 7, 4, 1, {{interconnect, 7}, from3, mux1});
    ?IOB_SIDE( 7, 4, 2, {{interconnect, 8}, from3, mux0});
    ?IOB_SIDE( 7, 4, 3, {{interconnect, 8}, from3, mux1});
    ?IOB_SIDE( 7, 5, 0, {{interconnect,16}, from3, mux0});
    ?IOB_SIDE( 7, 5, 1, {{interconnect,16}, from3, mux1});
    ?IOB_SIDE( 7, 5, 2, {{interconnect,17}, from3, mux0});
    ?IOB_SIDE( 7, 5, 3, {{interconnect,17}, from3, mux1});
    ?IOB_SIDE( 7, 6, 0, {{interconnect,14}, from3, mux0});
    ?IOB_SIDE( 7, 6, 1, {{interconnect,14}, from3, mux1});
    ?IOB_SIDE( 7, 6, 2, {{interconnect,15}, from3, mux0});
    ?IOB_SIDE( 7, 6, 3, {{interconnect,15}, from3, mux1});
    ?IOB_SIDE( 7, 7, 0, {{interconnect,12}, from3, mux0});
    ?IOB_SIDE( 7, 7, 1, {{interconnect,12}, from3, mux1});
    ?IOB_SIDE( 7, 7, 2, {{interconnect,13}, from3, mux0});
    ?IOB_SIDE( 7, 7, 3, {{interconnect,13}, from3, mux1});
    ?IOB_SIDE( 7, 8, 0, {{interconnect,10}, from3, mux0});
    ?IOB_SIDE( 7, 8, 1, {{interconnect,10}, from3, mux1});
    ?IOB_SIDE( 7, 8, 2, {{interconnect,11}, from3, mux0});
    ?IOB_SIDE( 7, 8, 3, {{interconnect,11}, from3, mux1});
    ?IOB_SIDE( 7, 9, 2, {{interconnect, 9}, from3, mux0});
    ?IOB_SIDE( 7, 9, 3, {{interconnect, 9}, from3, mux1});
    ?IOB_SIDE( 8, 0, 2, {{interconnect, 0}, from3, mux2});
    ?IOB_SIDE( 8, 0, 3, {{interconnect, 0}, direct_link});
    ?IOB_SIDE( 8, 1, 0, {{interconnect, 1}, from3, mux2});
    ?IOB_SIDE( 8, 1, 1, {{interconnect, 1}, direct_link});
    ?IOB_SIDE( 8, 1, 2, {{interconnect, 2}, from3, mux2});
    ?IOB_SIDE( 8, 1, 3, {{interconnect, 2}, direct_link});
    ?IOB_SIDE( 8, 2, 0, {{interconnect, 3}, from3, mux2});
    ?IOB_SIDE( 8, 2, 1, {{interconnect, 3}, direct_link});
    ?IOB_SIDE( 8, 2, 2, {{interconnect, 4}, from3, mux2});
    ?IOB_SIDE( 8, 2, 3, {{interconnect, 4}, direct_link});
    ?IOB_SIDE( 8, 3, 0, {{interconnect, 5}, from3, mux2});
    ?IOB_SIDE( 8, 3, 1, {{interconnect, 5}, direct_link});
    ?IOB_SIDE( 8, 3, 2, {{interconnect, 6}, from3, mux2});
    ?IOB_SIDE( 8, 3, 3, {{interconnect, 6}, direct_link});
    ?IOB_SIDE( 8, 4, 0, {{interconnect, 7}, from3, mux2});
    ?IOB_SIDE( 8, 4, 1, {{interconnect, 7}, direct_link});
    ?IOB_SIDE( 8, 4, 2, {{interconnect, 8}, from3, mux2});
    ?IOB_SIDE( 8, 4, 3, {{interconnect, 8}, direct_link});
    ?IOB_SIDE( 8, 5, 0, {{interconnect,16}, from3, mux2});
    ?IOB_SIDE( 8, 5, 1, {{interconnect,16}, direct_link});
    ?IOB_SIDE( 8, 5, 2, {{interconnect,17}, from3, mux2});
    ?IOB_SIDE( 8, 5, 3, {{interconnect,17}, direct_link});
    ?IOB_SIDE( 8, 6, 0, {{interconnect,14}, from3, mux2});
    ?IOB_SIDE( 8, 6, 1, {{interconnect,14}, direct_link});
    ?IOB_SIDE( 8, 6, 2, {{interconnect,15}, from3, mux2});
    ?IOB_SIDE( 8, 6, 3, {{interconnect,15}, direct_link});
    ?IOB_SIDE( 8, 7, 0, {{interconnect,12}, from3, mux2});
    ?IOB_SIDE( 8, 7, 1, {{interconnect,12}, direct_link});
    ?IOB_SIDE( 8, 7, 2, {{interconnect,13}, from3, mux2});
    ?IOB_SIDE( 8, 7, 3, {{interconnect,13}, direct_link});
    ?IOB_SIDE( 8, 8, 0, {{interconnect,10}, from3, mux2});
    ?IOB_SIDE( 8, 8, 1, {{interconnect,10}, direct_link});
    ?IOB_SIDE( 8, 8, 2, {{interconnect,11}, from3, mux2});
    ?IOB_SIDE( 8, 8, 3, {{interconnect,11}, direct_link});
    ?IOB_SIDE( 8, 9, 2, {{interconnect, 9}, from3, mux2});
    ?IOB_SIDE( 8, 9, 3, {{interconnect, 9}, direct_link});
    ?IOB_SIDE( 9, 0, 2, {{interconnect, 0}, from4, mux0});
    ?IOB_SIDE( 9, 0, 3, {{interconnect, 0}, from4, mux1});
    ?IOB_SIDE( 9, 1, 0, {{interconnect, 1}, from4, mux0});
    ?IOB_SIDE( 9, 1, 1, {{interconnect, 1}, from4, mux1});
    ?IOB_SIDE( 9, 1, 2, {{interconnect, 2}, from4, mux0});
    ?IOB_SIDE( 9, 1, 3, {{interconnect, 2}, from4, mux1});
    ?IOB_SIDE( 9, 2, 0, {{interconnect, 3}, from4, mux0});
    ?IOB_SIDE( 9, 2, 1, {{interconnect, 3}, from4, mux1});
    ?IOB_SIDE( 9, 2, 2, {{interconnect, 4}, from4, mux0});
    ?IOB_SIDE( 9, 2, 3, {{interconnect, 4}, from4, mux1});
    ?IOB_SIDE( 9, 3, 0, {{interconnect, 5}, from4, mux0});
    ?IOB_SIDE( 9, 3, 1, {{interconnect, 5}, from4, mux1});
    ?IOB_SIDE( 9, 3, 2, {{interconnect, 6}, from4, mux0});
    ?IOB_SIDE( 9, 3, 3, {{interconnect, 6}, from4, mux1});
    ?IOB_SIDE( 9, 4, 0, {{interconnect, 7}, from4, mux0});
    ?IOB_SIDE( 9, 4, 1, {{interconnect, 7}, from4, mux1});
    ?IOB_SIDE( 9, 4, 2, {{interconnect, 8}, from4, mux0});
    ?IOB_SIDE( 9, 4, 3, {{interconnect, 8}, from4, mux1});
    ?IOB_SIDE( 9, 5, 0, {{interconnect,16}, from4, mux0});
    ?IOB_SIDE( 9, 5, 1, {{interconnect,16}, from4, mux1});
    ?IOB_SIDE( 9, 5, 2, {{interconnect,17}, from4, mux0});
    ?IOB_SIDE( 9, 5, 3, {{interconnect,17}, from4, mux1});
    ?IOB_SIDE( 9, 6, 0, {{interconnect,14}, from4, mux0});
    ?IOB_SIDE( 9, 6, 1, {{interconnect,14}, from4, mux1});
    ?IOB_SIDE( 9, 6, 2, {{interconnect,15}, from4, mux0});
    ?IOB_SIDE( 9, 6, 3, {{interconnect,15}, from4, mux1});
    ?IOB_SIDE( 9, 7, 0, {{interconnect,12}, from4, mux0});
    ?IOB_SIDE( 9, 7, 1, {{interconnect,12}, from4, mux1});
    ?IOB_SIDE( 9, 7, 2, {{interconnect,13}, from4, mux0});
    ?IOB_SIDE( 9, 7, 3, {{interconnect,13}, from4, mux1});
    ?IOB_SIDE( 9, 8, 0, {{interconnect,10}, from4, mux0});
    ?IOB_SIDE( 9, 8, 1, {{interconnect,10}, from4, mux1});
    ?IOB_SIDE( 9, 8, 2, {{interconnect,11}, from4, mux0});
    ?IOB_SIDE( 9, 8, 3, {{interconnect,11}, from4, mux1});
    ?IOB_SIDE( 9, 9, 2, {{interconnect, 9}, from4, mux0});
    ?IOB_SIDE( 9, 9, 3, {{interconnect, 9}, from4, mux1});
    ?IOB_SIDE(10, 0, 2, {{interconnect, 0}, from4, mux2});
    ?IOB_SIDE(10, 0, 3, {{interconnect, 0}, from4, mux3});
    ?IOB_SIDE(10, 1, 0, {{interconnect, 1}, from4, mux2});
    ?IOB_SIDE(10, 1, 1, {{interconnect, 1}, from4, mux3});
    ?IOB_SIDE(10, 1, 2, {{interconnect, 2}, from4, mux2});
    ?IOB_SIDE(10, 1, 3, {{interconnect, 2}, from4, mux3});
    ?IOB_SIDE(10, 2, 0, {{interconnect, 3}, from4, mux2});
    ?IOB_SIDE(10, 2, 1, {{interconnect, 3}, from4, mux3});
    ?IOB_SIDE(10, 2, 2, {{interconnect, 4}, from4, mux2});
    ?IOB_SIDE(10, 2, 3, {{interconnect, 4}, from4, mux3});
    ?IOB_SIDE(10, 3, 0, {{interconnect, 5}, from4, mux2});
    ?IOB_SIDE(10, 3, 1, {{interconnect, 5}, from4, mux3});
    ?IOB_SIDE(10, 3, 2, {{interconnect, 6}, from4, mux2});
    ?IOB_SIDE(10, 3, 3, {{interconnect, 6}, from4, mux3});
    ?IOB_SIDE(10, 4, 0, {{interconnect, 7}, from4, mux2});
    ?IOB_SIDE(10, 4, 1, {{interconnect, 7}, from4, mux3});
    ?IOB_SIDE(10, 4, 2, {{interconnect, 8}, from4, mux2});
    ?IOB_SIDE(10, 4, 3, {{interconnect, 8}, from4, mux3});
    ?IOB_SIDE(10, 5, 0, {{interconnect,16}, from4, mux2});
    ?IOB_SIDE(10, 5, 1, {{interconnect,16}, from4, mux3});
    ?IOB_SIDE(10, 5, 2, {{interconnect,17}, from4, mux2});
    ?IOB_SIDE(10, 5, 3, {{interconnect,17}, from4, mux3});
    ?IOB_SIDE(10, 6, 0, {{interconnect,14}, from4, mux2});
    ?IOB_SIDE(10, 6, 1, {{interconnect,14}, from4, mux3});
    ?IOB_SIDE(10, 6, 2, {{interconnect,15}, from4, mux2});
    ?IOB_SIDE(10, 6, 3, {{interconnect,15}, from4, mux3});
    ?IOB_SIDE(10, 7, 0, {{interconnect,12}, from4, mux2});
    ?IOB_SIDE(10, 7, 1, {{interconnect,12}, from4, mux3});
    ?IOB_SIDE(10, 7, 2, {{interconnect,13}, from4, mux2});
    ?IOB_SIDE(10, 7, 3, {{interconnect,13}, from4, mux3});
    ?IOB_SIDE(10, 8, 0, {{interconnect,10}, from4, mux2});
    ?IOB_SIDE(10, 8, 1, {{interconnect,10}, from4, mux3});
    ?IOB_SIDE(10, 8, 2, {{interconnect,11}, from4, mux2});
    ?IOB_SIDE(10, 8, 3, {{interconnect,11}, from4, mux3});
    ?IOB_SIDE(10, 9, 2, {{interconnect, 9}, from4, mux2});
    ?IOB_SIDE(10, 9, 3, {{interconnect, 9}, from4, mux3});
).

-define(IOB_HEADS(),
    ?IOB_HEAD( 2,  1, {{interconnect, 5}, from4, mux0});
    ?IOB_HEAD( 2,  2, {{interconnect, 5}, from4, mux1});
    ?IOB_HEAD( 2,  3, {{interconnect, 6}, from4, mux0});
    ?IOB_HEAD( 2,  4, {{interconnect, 6}, from4, mux1});
    ?IOB_HEAD( 2,  5, {{interconnect, 7}, from4, mux0});
    ?IOB_HEAD( 2,  6, {{interconnect, 7}, from4, mux1});
    ?IOB_HEAD( 2,  7, {{interconnect, 8}, from4, mux0});
    ?IOB_HEAD( 2,  8, {{interconnect, 8}, from4, mux1});
    ?IOB_HEAD( 2,  9, {{interconnect, 9}, from4, mux0});
    ?IOB_HEAD( 2, 10, {{interconnect, 9}, from4, mux1});
    ?IOB_HEAD( 3,  1, {{interconnect, 5}, from4, mux2});
    ?IOB_HEAD( 3,  2, {{interconnect, 5}, from4, mux3});
    ?IOB_HEAD( 3,  3, {{interconnect, 6}, from4, mux2});
    ?IOB_HEAD( 3,  4, {{interconnect, 6}, from4, mux3});
    ?IOB_HEAD( 3,  5, {{interconnect, 7}, from4, mux2});
    ?IOB_HEAD( 3,  6, {{interconnect, 7}, from4, mux3});
    ?IOB_HEAD( 3,  7, {{interconnect, 8}, from4, mux2});
    ?IOB_HEAD( 3,  8, {{interconnect, 8}, from4, mux3});
    ?IOB_HEAD( 3,  9, {{interconnect, 9}, from4, mux2});
    ?IOB_HEAD( 3, 10, {{interconnect, 9}, from4, mux3});
    ?IOB_HEAD( 4,  1, {{interconnect, 5}, from3, mux0});
    ?IOB_HEAD( 4,  3, {{interconnect, 6}, from3, mux0});
    ?IOB_HEAD( 4,  5, {{interconnect, 7}, from3, mux0});
    ?IOB_HEAD( 4,  7, {{interconnect, 8}, from3, mux0});
    ?IOB_HEAD( 4,  9, {{interconnect, 9}, from3, mux0});
    ?IOB_HEAD( 5,  1, {{interconnect, 5}, from3, mux1});
    ?IOB_HEAD( 5,  2, {{interconnect, 5}, from3, mux2});
    ?IOB_HEAD( 5,  3, {{interconnect, 6}, from3, mux1});
    ?IOB_HEAD( 5,  4, {{interconnect, 6}, from3, mux2});
    ?IOB_HEAD( 5,  5, {{interconnect, 7}, from3, mux1});
    ?IOB_HEAD( 5,  6, {{interconnect, 7}, from3, mux2});
    ?IOB_HEAD( 5,  7, {{interconnect, 8}, from3, mux1});
    ?IOB_HEAD( 5,  8, {{interconnect, 8}, from3, mux2});
    ?IOB_HEAD( 5,  9, {{interconnect, 9}, from3, mux1});
    ?IOB_HEAD( 5, 10, {{interconnect, 9}, from3, mux2});
    ?IOB_HEAD(18,  1, {{interconnect, 0}, from3, mux1});
    ?IOB_HEAD(18,  2, {{interconnect, 0}, from3, mux2});
    ?IOB_HEAD(18,  3, {{interconnect, 1}, from3, mux1});
    ?IOB_HEAD(18,  4, {{interconnect, 1}, from3, mux2});
    ?IOB_HEAD(18,  5, {{interconnect, 2}, from3, mux1});
    ?IOB_HEAD(18,  6, {{interconnect, 2}, from3, mux2});
    ?IOB_HEAD(18,  7, {{interconnect, 3}, from3, mux1});
    ?IOB_HEAD(18,  8, {{interconnect, 3}, from3, mux2});
    ?IOB_HEAD(18,  9, {{interconnect, 4}, from3, mux1});
    ?IOB_HEAD(18, 10, {{interconnect, 4}, from3, mux2});
    ?IOB_HEAD(19,  1, {{interconnect, 0}, from3, mux0});
    ?IOB_HEAD(19,  3, {{interconnect, 1}, from3, mux0});
    ?IOB_HEAD(19,  5, {{interconnect, 2}, from3, mux0});
    ?IOB_HEAD(19,  7, {{interconnect, 3}, from3, mux0});
    ?IOB_HEAD(19,  9, {{interconnect, 4}, from3, mux0});
    ?IOB_HEAD(20,  1, {{interconnect, 0}, from4, mux2});
    ?IOB_HEAD(20,  2, {{interconnect, 0}, from4, mux3});
    ?IOB_HEAD(20,  3, {{interconnect, 1}, from4, mux2});
    ?IOB_HEAD(20,  4, {{interconnect, 1}, from4, mux3});
    ?IOB_HEAD(20,  5, {{interconnect, 2}, from4, mux2});
    ?IOB_HEAD(20,  6, {{interconnect, 2}, from4, mux3});
    ?IOB_HEAD(20,  7, {{interconnect, 3}, from4, mux2});
    ?IOB_HEAD(20,  8, {{interconnect, 3}, from4, mux3});
    ?IOB_HEAD(20,  9, {{interconnect, 4}, from4, mux2});
    ?IOB_HEAD(20, 10, {{interconnect, 4}, from4, mux3});
    ?IOB_HEAD(21,  1, {{interconnect, 0}, from4, mux0});
    ?IOB_HEAD(21,  2, {{interconnect, 0}, from4, mux1});
    ?IOB_HEAD(21,  3, {{interconnect, 1}, from4, mux0});
    ?IOB_HEAD(21,  4, {{interconnect, 1}, from4, mux1});
    ?IOB_HEAD(21,  5, {{interconnect, 2}, from4, mux0});
    ?IOB_HEAD(21,  6, {{interconnect, 2}, from4, mux1});
    ?IOB_HEAD(21,  7, {{interconnect, 3}, from4, mux0});
    ?IOB_HEAD(21,  8, {{interconnect, 3}, from4, mux1});
    ?IOB_HEAD(21,  9, {{interconnect, 4}, from4, mux0});
    ?IOB_HEAD(21, 10, {{interconnect, 4}, from4, mux1});
).

-define(IOB_TAILS(),
    ?IOB_TAIL( 2,  1, {{interconnect, 5}, from4, mux0});
    ?IOB_TAIL( 2,  2, {{interconnect, 5}, from4, mux1});
    ?IOB_TAIL( 2,  3, {{interconnect, 6}, from4, mux0});
    ?IOB_TAIL( 2,  4, {{interconnect, 6}, from4, mux1});
    ?IOB_TAIL( 2,  5, {{interconnect, 7}, from4, mux0});
    ?IOB_TAIL( 2,  6, {{interconnect, 7}, from4, mux1});
    ?IOB_TAIL( 2,  7, {{interconnect, 8}, from4, mux0});
    ?IOB_TAIL( 2,  8, {{interconnect, 8}, from4, mux1});
    ?IOB_TAIL( 2,  9, {{interconnect, 9}, from4, mux0});
    ?IOB_TAIL( 2, 10, {{interconnect, 9}, from4, mux1});
    ?IOB_TAIL( 3,  1, {{interconnect, 5}, from4, mux2});
    ?IOB_TAIL( 3,  2, {{interconnect, 5}, from4, mux3});
    ?IOB_TAIL( 3,  3, {{interconnect, 6}, from4, mux2});
    ?IOB_TAIL( 3,  4, {{interconnect, 6}, from4, mux3});
    ?IOB_TAIL( 3,  5, {{interconnect, 7}, from4, mux2});
    ?IOB_TAIL( 3,  6, {{interconnect, 7}, from4, mux3});
    ?IOB_TAIL( 3,  7, {{interconnect, 8}, from4, mux2});
    ?IOB_TAIL( 3,  8, {{interconnect, 8}, from4, mux3});
    ?IOB_TAIL( 3,  9, {{interconnect, 9}, from4, mux2});
    ?IOB_TAIL( 3, 10, {{interconnect, 9}, from4, mux3});
    ?IOB_TAIL( 4,  1, {{interconnect, 5}, from3, mux0});
    ?IOB_TAIL( 4,  3, {{interconnect, 6}, from3, mux0});
    ?IOB_TAIL( 4,  5, {{interconnect, 7}, from3, mux0});
    ?IOB_TAIL( 4,  7, {{interconnect, 8}, from3, mux0});
    ?IOB_TAIL( 4,  9, {{interconnect, 9}, from3, mux0});
    ?IOB_TAIL( 5,  1, {{interconnect, 5}, from3, mux1});
    ?IOB_TAIL( 5,  2, {{interconnect, 5}, from3, mux2});
    ?IOB_TAIL( 5,  3, {{interconnect, 6}, from3, mux1});
    ?IOB_TAIL( 5,  4, {{interconnect, 6}, from3, mux2});
    ?IOB_TAIL( 5,  5, {{interconnect, 7}, from3, mux1});
    ?IOB_TAIL( 5,  6, {{interconnect, 7}, from3, mux2});
    ?IOB_TAIL( 5,  7, {{interconnect, 8}, from3, mux1});
    ?IOB_TAIL( 5,  8, {{interconnect, 8}, from3, mux2});
    ?IOB_TAIL( 5,  9, {{interconnect, 9}, from3, mux1});
    ?IOB_TAIL( 5, 10, {{interconnect, 9}, from3, mux2});
    ?IOB_TAIL(18,  1, {{interconnect, 0}, from3, mux1});
    ?IOB_TAIL(18,  2, {{interconnect, 0}, from3, mux2});
    ?IOB_TAIL(18,  3, {{interconnect, 1}, from3, mux1});
    ?IOB_TAIL(18,  4, {{interconnect, 1}, from3, mux2});
    ?IOB_TAIL(18,  5, {{interconnect, 2}, from3, mux1});
    ?IOB_TAIL(18,  6, {{interconnect, 2}, from3, mux2});
    ?IOB_TAIL(18,  7, {{interconnect, 3}, from3, mux1});
    ?IOB_TAIL(18,  8, {{interconnect, 3}, from3, mux2});
    ?IOB_TAIL(18,  9, {{interconnect, 4}, from3, mux1});
    ?IOB_TAIL(18, 10, {{interconnect, 4}, from3, mux2});
    ?IOB_TAIL(19,  1, {{interconnect, 0}, from3, mux0});
    ?IOB_TAIL(19,  3, {{interconnect, 1}, from3, mux0});
    ?IOB_TAIL(19,  5, {{interconnect, 2}, from3, mux0});
    ?IOB_TAIL(19,  7, {{interconnect, 3}, from3, mux0});
    ?IOB_TAIL(19,  9, {{interconnect, 4}, from3, mux0});
    ?IOB_TAIL(20,  1, {{interconnect, 0}, from4, mux2});
    ?IOB_TAIL(20,  2, {{interconnect, 0}, from4, mux3});
    ?IOB_TAIL(20,  3, {{interconnect, 1}, from4, mux2});
    ?IOB_TAIL(20,  4, {{interconnect, 1}, from4, mux3});
    ?IOB_TAIL(20,  5, {{interconnect, 2}, from4, mux2});
    ?IOB_TAIL(20,  6, {{interconnect, 2}, from4, mux3});
    ?IOB_TAIL(20,  7, {{interconnect, 3}, from4, mux2});
    ?IOB_TAIL(20,  8, {{interconnect, 3}, from4, mux3});
    ?IOB_TAIL(20,  9, {{interconnect, 4}, from4, mux2});
    ?IOB_TAIL(20, 10, {{interconnect, 4}, from4, mux3});
    ?IOB_TAIL(21,  1, {{interconnect, 0}, from4, mux0});
    ?IOB_TAIL(21,  2, {{interconnect, 0}, from4, mux1});
    ?IOB_TAIL(21,  3, {{interconnect, 1}, from4, mux0});
    ?IOB_TAIL(21,  4, {{interconnect, 1}, from4, mux1});
    ?IOB_TAIL(21,  5, {{interconnect, 2}, from4, mux0});
    ?IOB_TAIL(21,  6, {{interconnect, 2}, from4, mux1});
    ?IOB_TAIL(21,  7, {{interconnect, 3}, from4, mux0});
    ?IOB_TAIL(21,  8, {{interconnect, 3}, from4, mux1});
    ?IOB_TAIL(21,  9, {{interconnect, 4}, from4, mux0});
    ?IOB_TAIL(21, 10, {{interconnect, 4}, from4, mux1});
).

-define(IOC_ZEROS(),
    ?IOC_ZERO( 0, output);
    ?IOC_ZERO( 1, schmitt_trigger);
).

-define(IOC_SIDES(),
    ?IOC_SIDE( 1, 1, fast_out);
    ?IOC_SIDE( 2, 0, {output3, mux1});
    ?IOC_SIDE( 2, 1, invert);
    ?IOC_SIDE( 3, 0, {output3, mux0});
    ?IOC_SIDE( 3, 1, {output3, mux2});
    ?IOC_SIDE( 4, 0, {output6, mux0});
    ?IOC_SIDE( 4, 1, {output6, mux1});
    ?IOC_SIDE( 5, 0, {output6, mux2});
    ?IOC_SIDE( 5, 1, {output6, mux3});
    ?IOC_SIDE( 6, 0, {output6, mux4});
    ?IOC_SIDE( 6, 1, {output6, mux5});
).

-define(IOC_LEFTS(),
    ?IOC_LEFT(1, 9, 3, 6, input_delay);
    ?IOC_LEFT(2, 1, 0, 0, input_delay);
    ?IOC_LEFT(2, 1, 1, 1, input_delay);
    ?IOC_LEFT(2, 1, 2, 2, input_delay);
).

-define(IOC_LEFT_LINES(),
    ?IOC_LEFT_LINE( 4, 21, 3, input_delay);
    ?IOC_LEFT_LINE( 4, 22, 4, input_delay);
    ?IOC_LEFT_LINE( 4, 23, 5, input_delay);
).

-define(IOC_RIGHTS(),
    ?IOC_RIGHT(3, 9, 2, 5, input_delay);
    ?IOC_RIGHT(4, 1, 1, 0, input_delay);
    ?IOC_RIGHT(4, 1, 2, 1, input_delay);
    ?IOC_RIGHT(4, 1, 3, 2, input_delay);
).

-define(IOC_RIGHT_LINES(),
    ?IOC_RIGHT_LINE( 2, 20, 3, input_delay);
    ?IOC_RIGHT_LINE( 2, 21, 4, input_delay);
).

-define(IOC_HEADS(),
    ?IOC_HEAD( 6, 0, 3, output);
    ?IOC_HEAD( 7, 0, 3, schmitt_trigger);
    ?IOC_HEAD( 8, 0, 2, output);
    ?IOC_HEAD( 9, 0, 2, schmitt_trigger);
    ?IOC_HEAD(15, 0, 1, output);
    ?IOC_HEAD(16, 0, 1, schmitt_trigger);
    ?IOC_HEAD(19, 2, 0, input_delay);
    ?IOC_HEAD(19, 4, 1, input_delay);
    ?IOC_HEAD(19, 6, 2, input_delay);
    ?IOC_HEAD(19, 8, 3, input_delay);
    ?IOC_HEAD(24, 0, 0, output);
    ?IOC_HEAD(25, 0, 0, schmitt_trigger);

    ?IOC_HEAD( 6, 1, 2, {output4, mux2});
    ?IOC_HEAD( 6, 2, 2, {output4, mux3});
    ?IOC_HEAD( 6, 3, 3, {output4, mux2});
    ?IOC_HEAD( 6, 4, 3, {output4, mux3});
    ?IOC_HEAD( 7, 1, 2, {output4, mux0});
    ?IOC_HEAD( 7, 2, 2, {output4, mux1});
    ?IOC_HEAD( 7, 3, 3, {output4, mux0});
    ?IOC_HEAD( 7, 4, 3, {output4, mux1});
    ?IOC_HEAD( 8, 1, 2, {output3, mux0});
    ?IOC_HEAD( 8, 2, 2, {output3, mux2});
    ?IOC_HEAD( 8, 3, 3, {output3, mux0});
    ?IOC_HEAD( 8, 4, 3, {output3, mux2});
    ?IOC_HEAD( 9, 1, 2, {output3, mux1});
    ?IOC_HEAD( 9, 2, 2, invert);
    ?IOC_HEAD( 9, 3, 3, {output3, mux1});
    ?IOC_HEAD( 9, 4, 3, invert);
    ?IOC_HEAD(10, 2, 2, fast_out);
    ?IOC_HEAD(10, 4, 3, fast_out);
    ?IOC_HEAD(12, 2, 0, fast_out);
    ?IOC_HEAD(12, 4, 1, fast_out);
    ?IOC_HEAD(13, 1, 0, {output3, mux1});
    ?IOC_HEAD(13, 2, 0, invert);
    ?IOC_HEAD(13, 3, 1, {output3, mux1});
    ?IOC_HEAD(13, 4, 1, invert);
    ?IOC_HEAD(15, 1, 0, {output3, mux0});
    ?IOC_HEAD(15, 2, 0, {output3, mux2});
    ?IOC_HEAD(15, 3, 1, {output3, mux0});
    ?IOC_HEAD(15, 4, 1, {output3, mux2});
    ?IOC_HEAD(16, 1, 0, {output4, mux0});
    ?IOC_HEAD(16, 2, 0, {output4, mux1});
    ?IOC_HEAD(16, 3, 1, {output4, mux0});
    ?IOC_HEAD(16, 4, 1, {output4, mux1});
    ?IOC_HEAD(17, 1, 0, {output4, mux2});
    ?IOC_HEAD(17, 2, 0, {output4, mux3});
    ?IOC_HEAD(17, 3, 1, {output4, mux2});
    ?IOC_HEAD(17, 4, 1, {output4, mux3});
).

-define(IOC_TAILS(),
    ?IOC_TAIL( 6, 0, 3, output);
    ?IOC_TAIL( 7, 0, 3, schmitt_trigger);
    ?IOC_TAIL( 8, 0, 2, output);
    ?IOC_TAIL( 9, 0, 2, schmitt_trigger);
    ?IOC_TAIL(15, 0, 1, output);
    ?IOC_TAIL(16, 0, 1, schmitt_trigger);
    ?IOC_TAIL(19, 2, 0, input_delay);
    ?IOC_TAIL(19, 4, 1, input_delay);
    ?IOC_TAIL(19, 6, 2, input_delay);
    ?IOC_TAIL(19, 8, 3, input_delay);
    ?IOC_TAIL(24, 0, 0, output);
    ?IOC_TAIL(25, 0, 0, schmitt_trigger);

    ?IOC_TAIL( 6, 1, 2, {output4, mux2});
    ?IOC_TAIL( 6, 2, 2, {output4, mux3});
    ?IOC_TAIL( 6, 3, 3, {output4, mux2});
    ?IOC_TAIL( 6, 4, 3, {output4, mux3});
    ?IOC_TAIL( 7, 1, 2, {output4, mux0});
    ?IOC_TAIL( 7, 2, 2, {output4, mux1});
    ?IOC_TAIL( 7, 3, 3, {output4, mux0});
    ?IOC_TAIL( 7, 4, 3, {output4, mux1});
    ?IOC_TAIL( 8, 1, 2, {output3, mux0});
    ?IOC_TAIL( 8, 2, 2, {output3, mux2});
    ?IOC_TAIL( 8, 3, 3, {output3, mux0});
    ?IOC_TAIL( 8, 4, 3, {output3, mux2});
    ?IOC_TAIL( 9, 1, 2, {output3, mux1});
    ?IOC_TAIL( 9, 2, 2, invert);
    ?IOC_TAIL( 9, 3, 3, {output3, mux1});
    ?IOC_TAIL( 9, 4, 3, invert);
    ?IOC_TAIL(10, 2, 2, fast_out);
    ?IOC_TAIL(10, 4, 3, fast_out);
    ?IOC_TAIL(12, 2, 0, fast_out);
    ?IOC_TAIL(12, 4, 1, fast_out);
    ?IOC_TAIL(13, 1, 0, {output3, mux1});
    ?IOC_TAIL(13, 2, 0, invert);
    ?IOC_TAIL(13, 3, 1, {output3, mux1});
    ?IOC_TAIL(13, 4, 1, invert);
    ?IOC_TAIL(15, 1, 0, {output3, mux0});
    ?IOC_TAIL(15, 2, 0, {output3, mux2});
    ?IOC_TAIL(15, 3, 1, {output3, mux0});
    ?IOC_TAIL(15, 4, 1, {output3, mux2});
    ?IOC_TAIL(16, 1, 0, {output4, mux0});
    ?IOC_TAIL(16, 2, 0, {output4, mux1});
    ?IOC_TAIL(16, 3, 1, {output4, mux0});
    ?IOC_TAIL(16, 4, 1, {output4, mux1});
    ?IOC_TAIL(17, 1, 0, {output4, mux2});
    ?IOC_TAIL(17, 2, 0, {output4, mux3});
    ?IOC_TAIL(17, 3, 1, {output4, mux2});
    ?IOC_TAIL(17, 4, 1, {output4, mux3});
).

-define(IOC_STRIPS(),
    ?IOC_STRIP(0, 2, open_drain);
    ?IOC_STRIP(1, 2, bus_hold);
    ?IOC_STRIP(2, 2, enable);
    ?IOC_STRIP(3, 2, weak_pull_up);
    ?IOC_STRIP(4, 2, current_strength_0);
    ?IOC_STRIP(5, 2, current_strength_1);
).

-define(LAB_CELLS(),
    ?LAB_CELL(2, 0, 0, {{interconnect, 5}, from4, mux0});
    ?LAB_CELL(2, 0, 1, {{interconnect, 5}, from4, mux1});
    ?LAB_CELL(2, 0, 2, {{interconnect, 6}, from4, mux0});
    ?LAB_CELL(2, 0, 3, {{interconnect, 6}, from4, mux1});
    ?LAB_CELL(2, 1, 0, {{interconnect, 7}, from4, mux0});
    ?LAB_CELL(2, 1, 1, {{interconnect, 7}, from4, mux1});
    ?LAB_CELL(2, 1, 2, {{interconnect, 8}, from4, mux0});
    ?LAB_CELL(2, 1, 3, {{interconnect, 8}, from4, mux1});
    ?LAB_CELL(2, 2, 0, {{interconnect, 9}, from4, mux0});
    ?LAB_CELL(2, 2, 1, {{interconnect, 9}, from4, mux1});
    ?LAB_CELL(2, 2, 2, {{interconnect, 10}, from4, mux0});
    ?LAB_CELL(2, 2, 3, {{interconnect, 10}, from4, mux1});
    ?LAB_CELL(2, 3, 0, {{interconnect, 11}, from4, mux0});
    ?LAB_CELL(2, 3, 1, {{interconnect, 11}, from4, mux1});
    ?LAB_CELL(2, 3, 2, {{interconnect, 12}, from4, mux0});
    ?LAB_CELL(2, 3, 3, {{interconnect, 12}, from4, mux1});
    ?LAB_CELL(2, 5, 0, {{interconnect, 18}, from4, mux0});
    ?LAB_CELL(2, 5, 1, {{interconnect, 18}, from4, mux1});
    ?LAB_CELL(2, 5, 2, {{interconnect, 19}, from4, mux0});
    ?LAB_CELL(2, 5, 3, {{interconnect, 19}, from4, mux1});
    ?LAB_CELL(2, 6, 0, {{interconnect, 20}, from4, mux0});
    ?LAB_CELL(2, 6, 1, {{interconnect, 20}, from4, mux1});
    ?LAB_CELL(2, 6, 2, {{interconnect, 21}, from4, mux0});
    ?LAB_CELL(2, 6, 3, {{interconnect, 21}, from4, mux1});
    ?LAB_CELL(2, 7, 0, {{interconnect, 22}, from4, mux0});
    ?LAB_CELL(2, 7, 1, {{interconnect, 22}, from4, mux1});
    ?LAB_CELL(2, 7, 2, {{interconnect, 23}, from4, mux0});
    ?LAB_CELL(2, 7, 3, {{interconnect, 23}, from4, mux1});
    ?LAB_CELL(2, 8, 0, {{interconnect, 24}, from4, mux0});
    ?LAB_CELL(2, 8, 1, {{interconnect, 24}, from4, mux1});
    ?LAB_CELL(2, 8, 2, {{interconnect, 25}, from4, mux0});
    ?LAB_CELL(2, 8, 3, {{interconnect, 25}, from4, mux1});
    ?LAB_CELL(3, 0, 0, {{interconnect, 5}, from4, mux2});
    ?LAB_CELL(3, 0, 1, {{interconnect, 5}, from4, mux3});
    ?LAB_CELL(3, 0, 2, {{interconnect, 6}, from4, mux2});
    ?LAB_CELL(3, 0, 3, {{interconnect, 6}, from4, mux3});
    ?LAB_CELL(3, 1, 0, {{interconnect, 7}, from4, mux2});
    ?LAB_CELL(3, 1, 1, {{interconnect, 7}, from4, mux3});
    ?LAB_CELL(3, 1, 2, {{interconnect, 8}, from4, mux2});
    ?LAB_CELL(3, 1, 3, {{interconnect, 8}, from4, mux3});
    ?LAB_CELL(3, 2, 0, {{interconnect, 9}, from4, mux2});
    ?LAB_CELL(3, 2, 1, {{interconnect, 9}, from4, mux3});
    ?LAB_CELL(3, 2, 2, {{interconnect, 10}, from4, mux2});
    ?LAB_CELL(3, 2, 3, {{interconnect, 10}, from4, mux3});
    ?LAB_CELL(3, 3, 0, {{interconnect, 11}, from4, mux2});
    ?LAB_CELL(3, 3, 1, {{interconnect, 11}, from4, mux3});
    ?LAB_CELL(3, 3, 2, {{interconnect, 12}, from4, mux2});
    ?LAB_CELL(3, 3, 3, {{interconnect, 12}, from4, mux3});
    ?LAB_CELL(3, 4, 2, clk1_global0);
    ?LAB_CELL(3, 4, 3, clk1_global1);
    ?LAB_CELL(3, 5, 0, {{interconnect, 18}, from4, mux2});
    ?LAB_CELL(3, 5, 1, {{interconnect, 18}, from4, mux3});
    ?LAB_CELL(3, 5, 2, {{interconnect, 19}, from4, mux2});
    ?LAB_CELL(3, 5, 3, {{interconnect, 19}, from4, mux3});
    ?LAB_CELL(3, 6, 0, {{interconnect, 20}, from4, mux2});
    ?LAB_CELL(3, 6, 1, {{interconnect, 20}, from4, mux3});
    ?LAB_CELL(3, 6, 2, {{interconnect, 21}, from4, mux2});
    ?LAB_CELL(3, 6, 3, {{interconnect, 21}, from4, mux3});
    ?LAB_CELL(3, 7, 0, {{interconnect, 22}, from4, mux2});
    ?LAB_CELL(3, 7, 1, {{interconnect, 22}, from4, mux3});
    ?LAB_CELL(3, 7, 2, {{interconnect, 23}, from4, mux2});
    ?LAB_CELL(3, 7, 3, {{interconnect, 23}, from4, mux3});
    ?LAB_CELL(3, 8, 0, {{interconnect, 24}, from4, mux2});
    ?LAB_CELL(3, 8, 1, {{interconnect, 24}, from4, mux3});
    ?LAB_CELL(3, 8, 2, {{interconnect, 25}, from4, mux2});
    ?LAB_CELL(3, 8, 3, {{interconnect, 25}, from4, mux3});
    ?LAB_CELL(3, 9, 2, clk1_global2);
    ?LAB_CELL(3, 9, 3, clk1_global3);
    ?LAB_CELL(4, 0, 0, {{interconnect, 5}, from3, mux0});
    ?LAB_CELL(4, 0, 2, {{interconnect, 6}, from3, mux0});
    ?LAB_CELL(4, 0, 3, {{interconnect, 6}, direct_link});
    ?LAB_CELL(4, 1, 0, {{interconnect, 7}, from3, mux0});
    ?LAB_CELL(4, 1, 2, {{interconnect, 8}, from3, mux0});
    ?LAB_CELL(4, 1, 3, {{interconnect, 8}, direct_link});
    ?LAB_CELL(4, 2, 0, {{interconnect, 9}, from3, mux0});
    ?LAB_CELL(4, 2, 2, {{interconnect, 10}, from3, mux0});
    ?LAB_CELL(4, 2, 3, {{interconnect, 10}, direct_link});
    ?LAB_CELL(4, 3, 0, {{interconnect, 11}, from3, mux0});
    ?LAB_CELL(4, 3, 2, {{interconnect, 12}, from3, mux0});
    ?LAB_CELL(4, 3, 3, {{interconnect, 12}, direct_link});
    ?LAB_CELL(4, 4, 2, clk2_global0);
    ?LAB_CELL(4, 4, 3, clk2_global1);
    ?LAB_CELL(4, 5, 0, {{interconnect, 18}, from3, mux0});
    ?LAB_CELL(4, 5, 2, {{interconnect, 19}, from3, mux0});
    ?LAB_CELL(4, 5, 3, {{interconnect, 19}, direct_link});
    ?LAB_CELL(4, 6, 0, {{interconnect, 20}, from3, mux0});
    ?LAB_CELL(4, 6, 2, {{interconnect, 21}, from3, mux0});
    ?LAB_CELL(4, 6, 3, {{interconnect, 21}, direct_link});
    ?LAB_CELL(4, 7, 0, {{interconnect, 22}, from3, mux0});
    ?LAB_CELL(4, 7, 2, {{interconnect, 23}, from3, mux0});
    ?LAB_CELL(4, 7, 3, {{interconnect, 23}, direct_link});
    ?LAB_CELL(4, 8, 0, {{interconnect, 24}, from3, mux0});
    ?LAB_CELL(4, 8, 2, {{interconnect, 25}, from3, mux0});
    ?LAB_CELL(4, 8, 3, {{interconnect, 25}, direct_link});
    ?LAB_CELL(4, 9, 2, clk2_global2);
    ?LAB_CELL(4, 9, 3, clk2_global3);
    ?LAB_CELL(5, 0, 0, {{interconnect, 5}, from3, mux1});
    ?LAB_CELL(5, 0, 1, {{interconnect, 5}, from3, mux2});
    ?LAB_CELL(5, 0, 2, {{interconnect, 6}, from3, mux1});
    ?LAB_CELL(5, 0, 3, {{interconnect, 6}, from3, mux2});
    ?LAB_CELL(5, 1, 0, {{interconnect, 7}, from3, mux1});
    ?LAB_CELL(5, 1, 1, {{interconnect, 7}, from3, mux2});
    ?LAB_CELL(5, 1, 2, {{interconnect, 8}, from3, mux1});
    ?LAB_CELL(5, 1, 3, {{interconnect, 8}, from3, mux2});
    ?LAB_CELL(5, 2, 0, {{interconnect, 9}, from3, mux1});
    ?LAB_CELL(5, 2, 1, {{interconnect, 9}, from3, mux2});
    ?LAB_CELL(5, 2, 2, {{interconnect, 10}, from3, mux1});
    ?LAB_CELL(5, 2, 3, {{interconnect, 10}, from3, mux2});
    ?LAB_CELL(5, 3, 0, {{interconnect, 11}, from3, mux1});
    ?LAB_CELL(5, 3, 1, {{interconnect, 11}, from3, mux2});
    ?LAB_CELL(5, 3, 2, {{interconnect, 12}, from3, mux1});
    ?LAB_CELL(5, 3, 3, {{interconnect, 12}, from3, mux2});
    ?LAB_CELL(5, 4, 2, clr1_global0);
    ?LAB_CELL(5, 4, 3, clr1_global1);
    ?LAB_CELL(5, 5, 0, {{interconnect, 18}, from3, mux1});
    ?LAB_CELL(5, 5, 1, {{interconnect, 18}, from3, mux2});
    ?LAB_CELL(5, 5, 2, {{interconnect, 19}, from3, mux1});
    ?LAB_CELL(5, 5, 3, {{interconnect, 19}, from3, mux2});
    ?LAB_CELL(5, 6, 0, {{interconnect, 20}, from3, mux1});
    ?LAB_CELL(5, 6, 1, {{interconnect, 20}, from3, mux2});
    ?LAB_CELL(5, 6, 2, {{interconnect, 21}, from3, mux1});
    ?LAB_CELL(5, 6, 3, {{interconnect, 21}, from3, mux2});
    ?LAB_CELL(5, 7, 0, {{interconnect, 22}, from3, mux1});
    ?LAB_CELL(5, 7, 1, {{interconnect, 22}, from3, mux2});
    ?LAB_CELL(5, 7, 2, {{interconnect, 23}, from3, mux1});
    ?LAB_CELL(5, 7, 3, {{interconnect, 23}, from3, mux2});
    ?LAB_CELL(5, 8, 0, {{interconnect, 24}, from3, mux1});
    ?LAB_CELL(5, 8, 1, {{interconnect, 24}, from3, mux2});
    ?LAB_CELL(5, 8, 2, {{interconnect, 25}, from3, mux1});
    ?LAB_CELL(5, 8, 3, {{interconnect, 25}, from3, mux2});
    ?LAB_CELL(5, 9, 2, clr1_global2);
    ?LAB_CELL(5, 9, 3, clr1_global3);
    ?LAB_CELL(22, 0, 2, {{interconnect, 0}, from3, mux1});
    ?LAB_CELL(22, 0, 3, {{interconnect, 0}, from3, mux2});
    ?LAB_CELL(22, 1, 2, {{interconnect, 1}, from3, mux1});
    ?LAB_CELL(22, 1, 3, {{interconnect, 1}, from3, mux2});
    ?LAB_CELL(22, 2, 2, {{interconnect, 2}, from3, mux1});
    ?LAB_CELL(22, 2, 3, {{interconnect, 2}, from3, mux2});
    ?LAB_CELL(22, 3, 2, {{interconnect, 3}, from3, mux1});
    ?LAB_CELL(22, 3, 3, {{interconnect, 3}, from3, mux2});
    ?LAB_CELL(22, 4, 2, {{interconnect, 4}, from3, mux1});
    ?LAB_CELL(22, 4, 3, {{interconnect, 4}, from3, mux2});
    ?LAB_CELL(22, 5, 2, {{interconnect, 13}, from3, mux1});
    ?LAB_CELL(22, 5, 3, {{interconnect, 13}, from3, mux2});
    ?LAB_CELL(22, 6, 2, {{interconnect, 14}, from3, mux1});
    ?LAB_CELL(22, 6, 3, {{interconnect, 14}, from3, mux2});
    ?LAB_CELL(22, 7, 2, {{interconnect, 15}, from3, mux1});
    ?LAB_CELL(22, 7, 3, {{interconnect, 15}, from3, mux2});
    ?LAB_CELL(22, 8, 2, {{interconnect, 16}, from3, mux1});
    ?LAB_CELL(22, 8, 3, {{interconnect, 16}, from3, mux2});
    ?LAB_CELL(22, 9, 2, {{interconnect, 17}, from3, mux1});
    ?LAB_CELL(22, 9, 3, {{interconnect, 17}, from3, mux2});
    ?LAB_CELL(23, 0, 2, {{interconnect, 0}, from3, mux0});
    ?LAB_CELL(23, 0, 3, {{interconnect, 0}, direct_link});
    ?LAB_CELL(23, 1, 2, {{interconnect, 1}, from3, mux0});
    ?LAB_CELL(23, 1, 3, {{interconnect, 1}, direct_link});
    ?LAB_CELL(23, 2, 2, {{interconnect, 2}, from3, mux0});
    ?LAB_CELL(23, 2, 3, {{interconnect, 2}, direct_link});
    ?LAB_CELL(23, 3, 2, {{interconnect, 3}, from3, mux0});
    ?LAB_CELL(23, 3, 3, {{interconnect, 3}, direct_link});
    ?LAB_CELL(23, 4, 2, {{interconnect, 4}, from3, mux0});
    ?LAB_CELL(23, 4, 3, {{interconnect, 4}, direct_link});
    ?LAB_CELL(23, 5, 2, {{interconnect, 13}, from3, mux0});
    ?LAB_CELL(23, 5, 3, {{interconnect, 13}, direct_link});
    ?LAB_CELL(23, 6, 2, {{interconnect, 14}, from3, mux0});
    ?LAB_CELL(23, 6, 3, {{interconnect, 14}, direct_link});
    ?LAB_CELL(23, 7, 2, {{interconnect, 15}, from3, mux0});
    ?LAB_CELL(23, 7, 3, {{interconnect, 15}, direct_link});
    ?LAB_CELL(23, 8, 2, {{interconnect, 16}, from3, mux0});
    ?LAB_CELL(23, 8, 3, {{interconnect, 16}, direct_link});
    ?LAB_CELL(23, 9, 2, {{interconnect, 17}, from3, mux0});
    ?LAB_CELL(23, 9, 3, {{interconnect, 17}, direct_link});
    ?LAB_CELL(24, 0, 2, {{interconnect, 0}, from4, mux2});
    ?LAB_CELL(24, 0, 3, {{interconnect, 0}, from4, mux3});
    ?LAB_CELL(24, 1, 2, {{interconnect, 1}, from4, mux2});
    ?LAB_CELL(24, 1, 3, {{interconnect, 1}, from4, mux3});
    ?LAB_CELL(24, 2, 2, {{interconnect, 2}, from4, mux2});
    ?LAB_CELL(24, 2, 3, {{interconnect, 2}, from4, mux3});
    ?LAB_CELL(24, 3, 2, {{interconnect, 3}, from4, mux2});
    ?LAB_CELL(24, 3, 3, {{interconnect, 3}, from4, mux3});
    ?LAB_CELL(24, 4, 2, {{interconnect, 4}, from4, mux2});
    ?LAB_CELL(24, 4, 3, {{interconnect, 4}, from4, mux3});
    ?LAB_CELL(24, 5, 2, {{interconnect, 13}, from4, mux2});
    ?LAB_CELL(24, 5, 3, {{interconnect, 13}, from4, mux3});
    ?LAB_CELL(24, 6, 2, {{interconnect, 14}, from4, mux2});
    ?LAB_CELL(24, 6, 3, {{interconnect, 14}, from4, mux3});
    ?LAB_CELL(24, 7, 2, {{interconnect, 15}, from4, mux2});
    ?LAB_CELL(24, 7, 3, {{interconnect, 15}, from4, mux3});
    ?LAB_CELL(24, 8, 2, {{interconnect, 16}, from4, mux2});
    ?LAB_CELL(24, 8, 3, {{interconnect, 16}, from4, mux3});
    ?LAB_CELL(24, 9, 2, {{interconnect, 17}, from4, mux2});
    ?LAB_CELL(24, 9, 3, {{interconnect, 17}, from4, mux3});
    ?LAB_CELL(25, 0, 2, {{interconnect, 0}, from4, mux0});
    ?LAB_CELL(25, 0, 3, {{interconnect, 0}, from4, mux1});
    ?LAB_CELL(25, 1, 2, {{interconnect, 1}, from4, mux0});
    ?LAB_CELL(25, 1, 3, {{interconnect, 1}, from4, mux1});
    ?LAB_CELL(25, 2, 2, {{interconnect, 2}, from4, mux0});
    ?LAB_CELL(25, 2, 3, {{interconnect, 2}, from4, mux1});
    ?LAB_CELL(25, 3, 2, {{interconnect, 3}, from4, mux0});
    ?LAB_CELL(25, 3, 3, {{interconnect, 3}, from4, mux1});
    ?LAB_CELL(25, 4, 2, {{interconnect, 4}, from4, mux0});
    ?LAB_CELL(25, 4, 3, {{interconnect, 4}, from4, mux1});
    ?LAB_CELL(25, 5, 2, {{interconnect, 13}, from4, mux0});
    ?LAB_CELL(25, 5, 3, {{interconnect, 13}, from4, mux1});
    ?LAB_CELL(25, 6, 2, {{interconnect, 14}, from4, mux0});
    ?LAB_CELL(25, 6, 3, {{interconnect, 14}, from4, mux1});
    ?LAB_CELL(25, 7, 2, {{interconnect, 15}, from4, mux0});
    ?LAB_CELL(25, 7, 3, {{interconnect, 15}, from4, mux1});
    ?LAB_CELL(25, 8, 2, {{interconnect, 16}, from4, mux0});
    ?LAB_CELL(25, 8, 3, {{interconnect, 16}, from4, mux1});
    ?LAB_CELL(25, 9, 2, {{interconnect, 17}, from4, mux0});
    ?LAB_CELL(25, 9, 3, {{interconnect, 17}, from4, mux1});
).

-define(LAB_LINES(),
    ?LAB_LINE(15, 21, clk1_invert);
    ?LAB_LINE(19, 22, clk2_invert);
    ?LAB_LINE(21, 23, clr1_invert);
).

-define(LC_CELLS(),
    ?LC_CELL( 6, 0, {data_a6, mux0});
    ?LC_CELL( 6, 1, {data_a6, mux1});
    ?LC_CELL( 6, 2, {data_c6, mux0});
    ?LC_CELL( 6, 3, {data_c6, mux1});
    ?LC_CELL( 7, 0, {data_a6, mux2});
    ?LC_CELL( 7, 1, {data_a6, mux3});
    ?LC_CELL( 7, 2, {data_c6, mux2});
    ?LC_CELL( 7, 3, {data_c6, mux3});
    ?LC_CELL( 8, 0, {data_b6, mux0});
    ?LC_CELL( 8, 1, {data_b6, mux1});
    ?LC_CELL( 8, 2, {data_c6, mux4});
    ?LC_CELL( 8, 3, {data_c6, mux5});
    ?LC_CELL( 9, 0, {data_b6, mux2});
    ?LC_CELL( 9, 1, {data_b6, mux3});
    ?LC_CELL( 9, 2, {data_d6, mux0});
    ?LC_CELL( 9, 3, {data_d6, mux1});
    ?LC_CELL(10, 0, {data_b6, mux4});
    ?LC_CELL(10, 1, {data_b6, mux5});
    ?LC_CELL(10, 2, {data_d6, mux2});
    ?LC_CELL(10, 3, {data_d6, mux3});
    ?LC_CELL(11, 0, {data_a6, mux4});
    ?LC_CELL(11, 1, {data_a6, mux5});
    ?LC_CELL(11, 2, {data_d6, mux4});
    ?LC_CELL(11, 3, {data_d6, mux5});
    ?LC_CELL(12, 0, {data_a3, mux0});
    ?LC_CELL(12, 1, {data_b3, mux0});
    ?LC_CELL(12, 2, {data_c3, mux0});
    ?LC_CELL(12, 3, {data_d3, mux0});
    ?LC_CELL(13, 0, {data_a3, mux1});
    ?LC_CELL(13, 1, {data_b3, mux1});
    ?LC_CELL(13, 2, {data_c3, mux1});
    ?LC_CELL(13, 3, {data_d3, mux1});
    ?LC_CELL(14, 0, {data_a3, mux2});
    ?LC_CELL(14, 1, {data_b3, mux2});
    ?LC_CELL(14, 2, {data_c3, mux2});
    ?LC_CELL(14, 3, {data_d3, mux2});
    ?LC_CELL(15, 0, {lut, a1b1c0d1});
    ?LC_CELL(15, 1, {lut, a1b0c0d1});
    ?LC_CELL(15, 2, {lut, a1b1c0d0});
    ?LC_CELL(15, 3, {lut, a1b0c0d0});
    ?LC_CELL(16, 0, {lut, a0b1c0d1});
    ?LC_CELL(16, 1, {lut, a0b0c0d1});
    ?LC_CELL(16, 2, {lut, a0b1c0d0});
    ?LC_CELL(16, 3, {lut, a0b0c0d0});
    ?LC_CELL(17, 0, {lut, a1b1c1d1});
    ?LC_CELL(17, 1, {lut, a1b0c1d1});
    ?LC_CELL(17, 2, {lut, a1b0c1d0});
    ?LC_CELL(17, 3, {lut, a1b1c1d0});
    ?LC_CELL(18, 0, {lut, a0b1c1d1});
    ?LC_CELL(18, 1, {lut, a0b0c1d1});
    ?LC_CELL(18, 2, {lut, a0b0c1d0});
    ?LC_CELL(18, 3, {lut, a0b1c1d0});
    ?LC_CELL(19, 1, {lut_out, right});
    ?LC_CELL(19, 2, clk);
    ?LC_CELL(20, 1, {lut_out, left});
    ?LC_CELL(20, 3, clr);
    ?LC_CELL(21, 3, local_line);
).

-define(EPM240_USER_CODES(),
    ?USER_CODE(0, 511);
    ?USER_CODE(1, 767);
    ?USER_CODE(2, 1023);
    ?USER_CODE(3, 1279);
    ?USER_CODE(4, 1535);
    ?USER_CODE(5, 1791);
    ?USER_CODE(6, 2047);
    ?USER_CODE(7, 2303);
    ?USER_CODE(8, 2559);
    ?USER_CODE(9, 2815);
    ?USER_CODE(10, 515);
    ?USER_CODE(11, 771);
    ?USER_CODE(12, 1027);
    ?USER_CODE(13, 1283);
    ?USER_CODE(14, 1539);
    ?USER_CODE(15, 1795);
    ?USER_CODE(16, 2051);
    ?USER_CODE(17, 2307);
    ?USER_CODE(18, 2563);
    ?USER_CODE(19, 2819);
    ?USER_CODE(20, 516);
    ?USER_CODE(21, 772);
    ?USER_CODE(22, 1028);
    ?USER_CODE(23, 1284);
    ?USER_CODE(24, 1540);
    ?USER_CODE(25, 1796);
    ?USER_CODE(26, 2052);
    ?USER_CODE(27, 2308);
    ?USER_CODE(28, 2564);
    ?USER_CODE(29, 2820);
    ?USER_CODE(30, 2565);
    ?USER_CODE(31, 2821);
).

-define(EPM240_STRIPS(),
    ?STRIP(1, 1, 3, left, 0);
    ?STRIP(1, 1, 2, left, 1);
    ?STRIP(1, 1, 1, left, 2);
    ?STRIP(1, 1, 0, left, 3);
    ?STRIP(1, 2, 3, left, 4);
    ?STRIP(1, 2, 2, left, 5);
    ?STRIP(1, 2, 1, left, 6);
    ?STRIP(1, 2, 0, left, 7);
    ?STRIP(1, 3, 3, left, 8);
    ?STRIP(1, 3, 2, left, 9);
    ?STRIP(1, 3, 1, left, 10);
    ?STRIP(1, 3, 0, left, 11);
    ?STRIP(1, 4, 3, left, 12);
    ?STRIP(1, 4, 2, left, 13);
    ?STRIP(1, 4, 1, left, 14);
    ?STRIP(1, 4, 0, left, 15);
    ?STRIP(2, 5, 3, top, 0);
    ?STRIP(2, 5, 2, top, 1);
    ?STRIP(2, 5, 1, top, 2);
    ?STRIP(2, 5, 0, top, 3);
    ?STRIP(3, 5, 3, top, 4);
    ?STRIP(3, 5, 2, top, 5);
    ?STRIP(3, 5, 1, top, 6);
    ?STRIP(3, 5, 0, top, 7);
    ?STRIP(4, 5, 2, top, 8);
    ?STRIP(4, 5, 1, top, 9);
    ?STRIP(4, 5, 0, top, 10);
    ?STRIP(5, 5, 3, top, 11);
    ?STRIP(5, 5, 2, top, 12);
    ?STRIP(5, 5, 1, top, 13);
    ?STRIP(5, 5, 0, top, 14);
    ?STRIP(6, 5, 3, top, 15);
    ?STRIP(6, 5, 2, top, 16);
    ?STRIP(6, 5, 1, top, 17);
    ?STRIP(6, 5, 0, top, 18);
    ?STRIP(7, 5, 3, top, 19);
    ?STRIP(7, 5, 2, top, 20);
    ?STRIP(7, 5, 1, top, 21);
    ?STRIP(7, 5, 0, top, 22);
    ?STRIP(8, 4, 0, right, 0);
    ?STRIP(8, 4, 1, right, 1);
    ?STRIP(8, 4, 2, right, 2);
    ?STRIP(8, 4, 3, right, 3);
    ?STRIP(8, 4, 4, right, 4);
    ?STRIP(8, 3, 0, right, 5);
    ?STRIP(8, 3, 1, right, 6);
    ?STRIP(8, 3, 2, right, 7);
    ?STRIP(8, 3, 3, right, 8);
    ?STRIP(8, 3, 4, right, 9);
    ?STRIP(8, 2, 0, right, 10);
    ?STRIP(8, 2, 1, right, 11);
    ?STRIP(8, 2, 2, right, 12);
    ?STRIP(8, 2, 3, right, 13);
    ?STRIP(8, 1, 0, right, 14);
    ?STRIP(8, 1, 1, right, 15);
    ?STRIP(8, 1, 2, right, 16);
    ?STRIP(8, 1, 3, right, 17);
    ?STRIP(8, 1, 4, right, 18);
    ?STRIP(7, 0, 0, bottom, 0);
    ?STRIP(7, 0, 1, bottom, 1);
    ?STRIP(7, 0, 2, bottom, 2);
    ?STRIP(6, 0, 0, bottom, 3);
    ?STRIP(6, 0, 1, bottom, 4);
    ?STRIP(6, 0, 2, bottom, 5);
    ?STRIP(6, 0, 3, bottom, 6);
    ?STRIP(5, 0, 0, bottom, 7);
    ?STRIP(5, 0, 1, bottom, 8);
    ?STRIP(5, 0, 2, bottom, 9);
    ?STRIP(5, 0, 3, bottom, 10);
    ?STRIP(4, 0, 0, bottom, 11);
    ?STRIP(4, 0, 1, bottom, 12);
    ?STRIP(4, 0, 2, bottom, 13);
    ?STRIP(3, 0, 0, bottom, 14);
    ?STRIP(3, 0, 1, bottom, 15);
    ?STRIP(3, 0, 2, bottom, 16);
    ?STRIP(3, 0, 3, bottom, 17);
    ?STRIP(2, 0, 0, bottom, 18);
    ?STRIP(2, 0, 1, bottom, 19);
    ?STRIP(2, 0, 2, bottom, 20);
    ?STRIP(2, 0, 3, bottom, 21);
).

-define(EPM570_USER_CODES(),
    ?USER_CODE(0, 65786);
    ?USER_CODE(1, 65530);
    ?USER_CODE(2, 65274);
    ?USER_CODE(3, 65018);
    ?USER_CODE(4, 64762);
    ?USER_CODE(5, 64506);
    ?USER_CODE(6, 65785);
    ?USER_CODE(7, 65529);
    ?USER_CODE(8, 65273);
    ?USER_CODE(9, 65017);
    ?USER_CODE(10, 64761);
    ?USER_CODE(11, 64505);
    ?USER_CODE(12, 64249);
    ?USER_CODE(13, 63993);
    ?USER_CODE(14, 63737);
    ?USER_CODE(15, 63481);
    ?USER_CODE(16, 63225);
    ?USER_CODE(17, 62969);
    ?USER_CODE(18, 62713);
    ?USER_CODE(19, 62457);
    ?USER_CODE(20, 62201);
    ?USER_CODE(21, 61945);
    ?USER_CODE(22, 61689);
    ?USER_CODE(23, 61433);
    ?USER_CODE(24, 61177);
    ?USER_CODE(25, 60921);
    ?USER_CODE(26, 60665);
    ?USER_CODE(27, 60409);
    ?USER_CODE(28, 64250);
    ?USER_CODE(29, 63994);
    ?USER_CODE(30, 63738);
    ?USER_CODE(31, 63482);
).

-define(EPM570_STRIPS(),
    ?STRIP(1, 3, 0, left, 0);
    ?STRIP(1, 3, 1, left, 1);
    ?STRIP(1, 3, 2, left, 2);
    ?STRIP(1, 3, 3, left, 3);
    ?STRIP(0, 4, 6, left, 4);
    ?STRIP(0, 4, 5, left, 5);
    ?STRIP(0, 4, 4, left, 6);
    ?STRIP(0, 4, 3, left, 7);
    ?STRIP(0, 4, 2, left, 8);
    ?STRIP(0, 4, 1, left, 9);
    ?STRIP(0, 4, 0, left, 10);
    ?STRIP(0, 5, 6, left, 11);
    ?STRIP(0, 5, 5, left, 12);
    ?STRIP(0, 5, 4, left, 13);
    ?STRIP(0, 5, 3, left, 14);
    ?STRIP(0, 5, 2, left, 15);
    ?STRIP(0, 5, 1, left, 16);
    ?STRIP(0, 5, 0, left, 17);
    ?STRIP(0, 6, 6, left, 18);
    ?STRIP(0, 6, 5, left, 19);
    ?STRIP(0, 6, 4, left, 20);
    ?STRIP(0, 6, 3, left, 21);
    ?STRIP(0, 6, 2, left, 22);
    ?STRIP(0, 6, 1, left, 23);
    ?STRIP(0, 6, 0, left, 24);
    ?STRIP(0, 7, 6, left, 25);
    ?STRIP(0, 7, 5, left, 26);
    ?STRIP(0, 7, 4, left, 27);
    ?STRIP(0, 7, 3, left, 28);
    ?STRIP(0, 7, 2, left, 29);
    ?STRIP(0, 7, 1, left, 30);
    ?STRIP(0, 7, 0, left, 31);
    ?STRIP(1, 8, 3, top, 0);
    ?STRIP(1, 8, 2, top, 1);
    ?STRIP(1, 8, 1, top, 2);
    ?STRIP(1, 8, 0, top, 3);
    ?STRIP(2, 8, 3, top, 4);
    ?STRIP(2, 8, 2, top, 5);
    ?STRIP(2, 8, 1, top, 6);
    ?STRIP(2, 8, 0, top, 7);
    ?STRIP(3, 8, 3, top, 8);
    ?STRIP(3, 8, 2, top, 9);
    ?STRIP(3, 8, 1, top, 10);
    ?STRIP(3, 8, 0, top, 11);
    ?STRIP(4, 8, 3, top, 12);
    ?STRIP(4, 8, 2, top, 13);
    ?STRIP(4, 8, 1, top, 14);
    ?STRIP(4, 8, 0, top, 15);
    ?STRIP(5, 8, 2, top, 16);
    ?STRIP(5, 8, 1, top, 17);
    ?STRIP(5, 8, 0, top, 18);
    ?STRIP(6, 8, 3, top, 19);
    ?STRIP(6, 8, 2, top, 20);
    ?STRIP(6, 8, 1, top, 21);
    ?STRIP(6, 8, 0, top, 22);
    ?STRIP(7, 8, 3, top, 23);
    ?STRIP(7, 8, 2, top, 24);
    ?STRIP(7, 8, 1, top, 25);
    ?STRIP(7, 8, 0, top, 26);
    ?STRIP(8, 8, 3, top, 27);
    ?STRIP(8, 8, 2, top, 28);
    ?STRIP(8, 8, 1, top, 29);
    ?STRIP(8, 8, 0, top, 30);
    ?STRIP(9, 8, 2, top, 31);
    ?STRIP(9, 8, 1, top, 32);
    ?STRIP(9, 8, 0, top, 33);
    ?STRIP(10, 8, 3, top, 34);
    ?STRIP(10, 8, 2, top, 35);
    ?STRIP(10, 8, 1, top, 36);
    ?STRIP(10, 8, 0, top, 37);
    ?STRIP(11, 8, 3, top, 38);
    ?STRIP(11, 8, 2, top, 39);
    ?STRIP(11, 8, 1, top, 40);
    ?STRIP(11, 8, 0, top, 41);
    ?STRIP(12, 8, 3, top, 42);
    ?STRIP(12, 8, 2, top, 43);
    ?STRIP(12, 8, 1, top, 44);
    ?STRIP(12, 8, 0, top, 45);
    ?STRIP(13, 7, 0, right, 0);
    ?STRIP(13, 7, 1, right, 1);
    ?STRIP(13, 7, 2, right, 2);
    ?STRIP(13, 7, 3, right, 3);
    ?STRIP(13, 7, 4, right, 4);
    ?STRIP(13, 7, 5, right, 5);
    ?STRIP(13, 6, 0, right, 6);
    ?STRIP(13, 6, 1, right, 7);
    ?STRIP(13, 6, 2, right, 8);
    ?STRIP(13, 6, 3, right, 9);
    ?STRIP(13, 6, 4, right, 10);
    ?STRIP(13, 6, 5, right, 11);
    ?STRIP(13, 5, 0, right, 12);
    ?STRIP(13, 5, 1, right, 13);
    ?STRIP(13, 5, 2, right, 14);
    ?STRIP(13, 5, 3, right, 15);
    ?STRIP(13, 5, 4, right, 16);
    ?STRIP(13, 5, 5, right, 17);
    ?STRIP(13, 4, 0, right, 18);
    ?STRIP(13, 4, 1, right, 19);
    ?STRIP(13, 4, 2, right, 20);
    ?STRIP(13, 4, 3, right, 21);
    ?STRIP(13, 4, 4, right, 22);
    ?STRIP(13, 4, 5, right, 23);
    ?STRIP(13, 3, 0, right, 24);
    ?STRIP(13, 3, 1, right, 25);
    ?STRIP(13, 3, 2, right, 26);
    ?STRIP(13, 3, 3, right, 27);
    ?STRIP(13, 3, 4, right, 28);
    ?STRIP(13, 3, 5, right, 29);
    ?STRIP(13, 2, 0, right, 30);
    ?STRIP(13, 2, 1, right, 31);
    ?STRIP(13, 2, 2, right, 32);
    ?STRIP(13, 2, 3, right, 33);
    ?STRIP(13, 2, 4, right, 34);
    ?STRIP(13, 2, 5, right, 35);
    ?STRIP(13, 1, 0, right, 36);
    ?STRIP(13, 1, 1, right, 37);
    ?STRIP(13, 1, 2, right, 38);
    ?STRIP(13, 1, 3, right, 39);
    ?STRIP(13, 1, 4, right, 40);
    ?STRIP(13, 1, 5, right, 41);
    ?STRIP(12, 0, 0, bottom, 0);
    ?STRIP(12, 0, 1, bottom, 1);
    ?STRIP(12, 0, 2, bottom, 2);
    ?STRIP(12, 0, 3, bottom, 3);
    ?STRIP(11, 0, 0, bottom, 4);
    ?STRIP(11, 0, 1, bottom, 5);
    ?STRIP(11, 0, 2, bottom, 6);
    ?STRIP(11, 0, 3, bottom, 7);
    ?STRIP(10, 0, 0, bottom, 8);
    ?STRIP(10, 0, 1, bottom, 9);
    ?STRIP(10, 0, 2, bottom, 10);
    ?STRIP(10, 0, 3, bottom, 11);
    ?STRIP(8, 3, 0, bottom, 12);
    ?STRIP(8, 3, 1, bottom, 13);
    ?STRIP(8, 3, 2, bottom, 14);
    ?STRIP(8, 3, 3, bottom, 15);
    ?STRIP(7, 3, 0, bottom, 16);
    ?STRIP(7, 3, 1, bottom, 17);
    ?STRIP(7, 3, 2, bottom, 18);
    ?STRIP(7, 3, 3, bottom, 19);
    ?STRIP(6, 3, 0, bottom, 20);
    ?STRIP(6, 3, 1, bottom, 21);
    ?STRIP(6, 3, 2, bottom, 22);
    ?STRIP(6, 3, 3, bottom, 23);
    ?STRIP(5, 3, 0, bottom, 24);
    ?STRIP(5, 3, 1, bottom, 25);
    ?STRIP(5, 3, 2, bottom, 26);
    ?STRIP(5, 3, 3, bottom, 27);
    ?STRIP(4, 3, 0, bottom, 28);
    ?STRIP(4, 3, 1, bottom, 29);
    ?STRIP(4, 3, 2, bottom, 30);
    ?STRIP(4, 3, 3, bottom, 31);
    ?STRIP(3, 3, 0, bottom, 32);
    ?STRIP(3, 3, 1, bottom, 33);
    ?STRIP(3, 3, 2, bottom, 34);
    ?STRIP(3, 3, 3, bottom, 35);
    ?STRIP(2, 3, 0, bottom, 36);
    ?STRIP(2, 3, 1, bottom, 37);
    ?STRIP(2, 3, 2, bottom, 38);
    ?STRIP(2, 3, 3, bottom, 39);
).

-define(EPM1270_USER_CODES(),
    ?USER_CODE(0, 120225);
    ?USER_CODE(1, 119841);
    ?USER_CODE(2, 119457);
    ?USER_CODE(3, 119073);
    ?USER_CODE(4, 118689);
    ?USER_CODE(5, 118305);
    ?USER_CODE(6, 120224);
    ?USER_CODE(7, 119840);
    ?USER_CODE(8, 119456);
    ?USER_CODE(9, 119072);
    ?USER_CODE(10, 118688);
    ?USER_CODE(11, 118304);
    ?USER_CODE(12, 117920);
    ?USER_CODE(13, 117536);
    ?USER_CODE(14, 117152);
    ?USER_CODE(15, 116768);
    ?USER_CODE(16, 116384);
    ?USER_CODE(17, 116000);
    ?USER_CODE(18, 115616);
    ?USER_CODE(19, 115232);
    ?USER_CODE(20, 114848);
    ?USER_CODE(21, 114464);
    ?USER_CODE(22, 114080);
    ?USER_CODE(23, 113696);
    ?USER_CODE(24, 113312);
    ?USER_CODE(25, 112928);
    ?USER_CODE(26, 112544);
    ?USER_CODE(27, 112160);
    ?USER_CODE(28, 117921);
    ?USER_CODE(29, 117537);
    ?USER_CODE(30, 117153);
    ?USER_CODE(31, 116769);
).

-define(EPM1270_STRIPS(),
    ?STRIP(1, 3, 2, left, 0);
    ?STRIP(1, 3, 3, left, 1);
    ?STRIP(0, 4, 6, left, 2);
    ?STRIP(0, 4, 5, left, 3);
    ?STRIP(0, 4, 4, left, 4);
    ?STRIP(0, 4, 3, left, 5);
    ?STRIP(0, 4, 2, left, 6);
    ?STRIP(0, 4, 1, left, 7);
    ?STRIP(0, 4, 0, left, 8);
    ?STRIP(0, 5, 6, left, 9);
    ?STRIP(0, 5, 5, left, 10);
    ?STRIP(0, 5, 4, left, 11);
    ?STRIP(0, 5, 3, left, 12);
    ?STRIP(0, 5, 2, left, 13);
    ?STRIP(0, 5, 1, left, 14);
    ?STRIP(0, 5, 0, left, 15);
    ?STRIP(0, 6, 6, left, 16);
    ?STRIP(0, 6, 5, left, 17);
    ?STRIP(0, 6, 4, left, 18);
    ?STRIP(0, 6, 3, left, 19);
    ?STRIP(0, 6, 2, left, 20);
    ?STRIP(0, 6, 1, left, 21);
    ?STRIP(0, 6, 0, left, 22);
    ?STRIP(0, 7, 6, left, 23);
    ?STRIP(0, 7, 5, left, 24);
    ?STRIP(0, 7, 4, left, 25);
    ?STRIP(0, 7, 3, left, 26);
    ?STRIP(0, 7, 2, left, 27);
    ?STRIP(0, 7, 1, left, 28);
    ?STRIP(0, 7, 0, left, 29);
    ?STRIP(0, 8, 6, left, 30);
    ?STRIP(0, 8, 5, left, 31);
    ?STRIP(0, 8, 4, left, 32);
    ?STRIP(0, 8, 3, left, 33);
    ?STRIP(0, 8, 2, left, 34);
    ?STRIP(0, 8, 1, left, 35);
    ?STRIP(0, 8, 0, left, 36);
    ?STRIP(0, 9, 6, left, 37);
    ?STRIP(0, 9, 5, left, 38);
    ?STRIP(0, 9, 4, left, 39);
    ?STRIP(0, 9, 3, left, 40);
    ?STRIP(0, 9, 2, left, 41);
    ?STRIP(0, 9, 1, left, 42);
    ?STRIP(0, 9, 0, left, 43);
    ?STRIP(0, 10, 6, left, 44);
    ?STRIP(0, 10, 5, left, 45);
    ?STRIP(0, 10, 4, left, 46);
    ?STRIP(0, 10, 3, left, 47);
    ?STRIP(0, 10, 2, left, 48);
    ?STRIP(0, 10, 1, left, 49);
    ?STRIP(0, 10, 0, left, 50);
    ?STRIP(1, 11, 2, top, 0);
    ?STRIP(1, 11, 1, top, 1);
    ?STRIP(1, 11, 0, top, 2);
    ?STRIP(2, 11, 3, top, 3);
    ?STRIP(2, 11, 2, top, 4);
    ?STRIP(2, 11, 1, top, 5);
    ?STRIP(2, 11, 0, top, 6);
    ?STRIP(3, 11, 2, top, 7);
    ?STRIP(3, 11, 1, top, 8);
    ?STRIP(3, 11, 0, top, 9);
    ?STRIP(4, 11, 2, top, 10);
    ?STRIP(4, 11, 1, top, 11);
    ?STRIP(4, 11, 0, top, 12);
    ?STRIP(5, 11, 2, top, 13);
    ?STRIP(5, 11, 1, top, 14);
    ?STRIP(5, 11, 0, top, 15);
    ?STRIP(6, 11, 3, top, 16);
    ?STRIP(6, 11, 2, top, 17);
    ?STRIP(6, 11, 1, top, 18);
    ?STRIP(6, 11, 0, top, 19);
    ?STRIP(7, 11, 2, top, 20);
    ?STRIP(7, 11, 1, top, 21);
    ?STRIP(7, 11, 0, top, 22);
    ?STRIP(8, 11, 2, top, 23);
    ?STRIP(8, 11, 1, top, 24);
    ?STRIP(8, 11, 0, top, 25);
    ?STRIP(9, 11, 3, top, 26);
    ?STRIP(9, 11, 2, top, 27);
    ?STRIP(9, 11, 1, top, 28);
    ?STRIP(9, 11, 0, top, 29);
    ?STRIP(10, 11, 2, top, 30);
    ?STRIP(10, 11, 1, top, 31);
    ?STRIP(10, 11, 0, top, 32);
    ?STRIP(11, 11, 3, top, 33);
    ?STRIP(11, 11, 2, top, 34);
    ?STRIP(11, 11, 1, top, 35);
    ?STRIP(11, 11, 0, top, 36);
    ?STRIP(12, 11, 2, top, 37);
    ?STRIP(12, 11, 1, top, 38);
    ?STRIP(12, 11, 0, top, 39);
    ?STRIP(13, 11, 2, top, 40);
    ?STRIP(13, 11, 1, top, 41);
    ?STRIP(13, 11, 0, top, 42);
    ?STRIP(14, 11, 2, top, 43);
    ?STRIP(14, 11, 1, top, 44);
    ?STRIP(14, 11, 0, top, 45);
    ?STRIP(15, 11, 3, top, 46);
    ?STRIP(15, 11, 2, top, 47);
    ?STRIP(15, 11, 1, top, 48);
    ?STRIP(15, 11, 0, top, 49);
    ?STRIP(16, 11, 2, top, 50);
    ?STRIP(16, 11, 1, top, 51);
    ?STRIP(16, 11, 0, top, 52);
    ?STRIP(17, 10, 0, right, 0);
    ?STRIP(17, 10, 1, right, 1);
    ?STRIP(17, 10, 2, right, 2);
    ?STRIP(17, 10, 3, right, 3);
    ?STRIP(17, 10, 4, right, 4);
    ?STRIP(17, 9, 0, right, 5);
    ?STRIP(17, 9, 1, right, 6);
    ?STRIP(17, 9, 2, right, 7);
    ?STRIP(17, 9, 3, right, 8);
    ?STRIP(17, 9, 4, right, 9);
    ?STRIP(17, 8, 0, right, 10);
    ?STRIP(17, 8, 1, right, 11);
    ?STRIP(17, 8, 2, right, 12);
    ?STRIP(17, 8, 3, right, 13);
    ?STRIP(17, 8, 4, right, 14);
    ?STRIP(17, 8, 5, right, 15);
    ?STRIP(17, 7, 0, right, 16);
    ?STRIP(17, 7, 1, right, 17);
    ?STRIP(17, 7, 2, right, 18);
    ?STRIP(17, 7, 3, right, 19);
    ?STRIP(17, 7, 4, right, 20);
    ?STRIP(17, 6, 0, right, 21);
    ?STRIP(17, 6, 1, right, 22);
    ?STRIP(17, 6, 2, right, 23);
    ?STRIP(17, 6, 3, right, 24);
    ?STRIP(17, 6, 4, right, 25);
    ?STRIP(17, 6, 5, right, 26);
    ?STRIP(17, 5, 0, right, 27);
    ?STRIP(17, 5, 1, right, 28);
    ?STRIP(17, 5, 2, right, 29);
    ?STRIP(17, 5, 3, right, 30);
    ?STRIP(17, 5, 4, right, 31);
    ?STRIP(17, 5, 5, right, 32);
    ?STRIP(17, 4, 0, right, 33);
    ?STRIP(17, 4, 1, right, 34);
    ?STRIP(17, 4, 2, right, 35);
    ?STRIP(17, 4, 3, right, 36);
    ?STRIP(17, 4, 4, right, 37);
    ?STRIP(17, 3, 0, right, 38);
    ?STRIP(17, 3, 1, right, 39);
    ?STRIP(17, 3, 2, right, 40);
    ?STRIP(17, 3, 3, right, 41);
    ?STRIP(17, 3, 4, right, 42);
    ?STRIP(17, 3, 5, right, 43);
    ?STRIP(17, 2, 0, right, 44);
    ?STRIP(17, 2, 1, right, 45);
    ?STRIP(17, 2, 2, right, 46);
    ?STRIP(17, 2, 3, right, 47);
    ?STRIP(17, 2, 4, right, 48);
    ?STRIP(17, 1, 0, right, 49);
    ?STRIP(17, 1, 1, right, 50);
    ?STRIP(17, 1, 2, right, 51);
    ?STRIP(17, 1, 3, right, 52);
    ?STRIP(17, 1, 4, right, 53);
    ?STRIP(17, 1, 5, right, 54);
    ?STRIP(16, 0, 0, bottom, 0);
    ?STRIP(16, 0, 1, bottom, 1);
    ?STRIP(16, 0, 2, bottom, 2);
    ?STRIP(16, 0, 3, bottom, 3);
    ?STRIP(15, 0, 0, bottom, 4);
    ?STRIP(15, 0, 1, bottom, 5);
    ?STRIP(15, 0, 2, bottom, 6);
    ?STRIP(14, 0, 0, bottom, 7);
    ?STRIP(14, 0, 1, bottom, 8);
    ?STRIP(14, 0, 2, bottom, 9);
    ?STRIP(14, 0, 3, bottom, 10);
    ?STRIP(13, 0, 0, bottom, 11);
    ?STRIP(13, 0, 1, bottom, 12);
    ?STRIP(13, 0, 2, bottom, 13);
    ?STRIP(13, 0, 3, bottom, 14);
    ?STRIP(12, 0, 0, bottom, 15);
    ?STRIP(12, 0, 1, bottom, 16);
    ?STRIP(12, 0, 2, bottom, 17);
    ?STRIP(10, 3, 0, bottom, 18);
    ?STRIP(10, 3, 1, bottom, 19);
    ?STRIP(10, 3, 2, bottom, 20);
    ?STRIP(10, 3, 3, bottom, 21);
    ?STRIP(9, 3, 0, bottom, 22);
    ?STRIP(9, 3, 1, bottom, 23);
    ?STRIP(9, 3, 2, bottom, 24);
    ?STRIP(8, 3, 0, bottom, 25);
    ?STRIP(8, 3, 1, bottom, 26);
    ?STRIP(8, 3, 2, bottom, 27);
    ?STRIP(8, 3, 3, bottom, 28);
    ?STRIP(7, 3, 0, bottom, 29);
    ?STRIP(7, 3, 1, bottom, 30);
    ?STRIP(7, 3, 2, bottom, 31);
    ?STRIP(7, 3, 3, bottom, 32);
    ?STRIP(6, 3, 0, bottom, 33);
    ?STRIP(6, 3, 1, bottom, 34);
    ?STRIP(6, 3, 2, bottom, 35);
    ?STRIP(5, 3, 0, bottom, 36);
    ?STRIP(5, 3, 1, bottom, 37);
    ?STRIP(5, 3, 2, bottom, 38);
    ?STRIP(5, 3, 3, bottom, 39);
    ?STRIP(4, 3, 0, bottom, 40);
    ?STRIP(4, 3, 1, bottom, 41);
    ?STRIP(4, 3, 2, bottom, 42);
    ?STRIP(4, 3, 3, bottom, 43);
    ?STRIP(3, 3, 0, bottom, 44);
    ?STRIP(3, 3, 1, bottom, 45);
    ?STRIP(3, 3, 2, bottom, 46);
    ?STRIP(2, 3, 0, bottom, 47);
    ?STRIP(2, 3, 1, bottom, 48);
    ?STRIP(2, 3, 2, bottom, 49);
    ?STRIP(2, 3, 3, bottom, 50);
    ?STRIP(1, 3, 0, bottom, 51);
    ?STRIP(1, 3, 1, bottom, 52);
).

-define(EPM2210_USER_CODES(),
    ?USER_CODE(0, 188977);
    ?USER_CODE(1, 188465);
    ?USER_CODE(2, 187953);
    ?USER_CODE(3, 187441);
    ?USER_CODE(4, 186929);
    ?USER_CODE(5, 186417);
    ?USER_CODE(6, 188976);
    ?USER_CODE(7, 188464);
    ?USER_CODE(8, 187952);
    ?USER_CODE(9, 187440);
    ?USER_CODE(10, 186928);
    ?USER_CODE(11, 186416);
    ?USER_CODE(12, 185904);
    ?USER_CODE(13, 185392);
    ?USER_CODE(14, 184880);
    ?USER_CODE(15, 184368);
    ?USER_CODE(16, 183856);
    ?USER_CODE(17, 183344);
    ?USER_CODE(18, 182832);
    ?USER_CODE(19, 182320);
    ?USER_CODE(20, 181808);
    ?USER_CODE(21, 181296);
    ?USER_CODE(22, 180784);
    ?USER_CODE(23, 180272);
    ?USER_CODE(24, 179760);
    ?USER_CODE(25, 179248);
    ?USER_CODE(26, 178736);
    ?USER_CODE(27, 178224);
    ?USER_CODE(28, 185905);
    ?USER_CODE(29, 185393);
    ?USER_CODE(30, 184881);
    ?USER_CODE(31, 184369);
).

-define(EPM2210_STRIPS(),
    ?STRIP(0, 4, 6, left, 0);
    ?STRIP(0, 4, 5, left, 1);
    ?STRIP(0, 4, 4, left, 2);
    ?STRIP(0, 4, 3, left, 3);
    ?STRIP(0, 4, 2, left, 4);
    ?STRIP(0, 4, 1, left, 5);
    ?STRIP(0, 4, 0, left, 6);
    ?STRIP(0, 5, 6, left, 7);
    ?STRIP(0, 5, 5, left, 8);
    ?STRIP(0, 5, 4, left, 9);
    ?STRIP(0, 5, 3, left, 10);
    ?STRIP(0, 5, 2, left, 11);
    ?STRIP(0, 5, 1, left, 12);
    ?STRIP(0, 5, 0, left, 13);
    ?STRIP(0, 6, 5, left, 14);
    ?STRIP(0, 6, 4, left, 15);
    ?STRIP(0, 6, 3, left, 16);
    ?STRIP(0, 6, 2, left, 17);
    ?STRIP(0, 6, 1, left, 18);
    ?STRIP(0, 6, 0, left, 19);
    ?STRIP(0, 7, 6, left, 20);
    ?STRIP(0, 7, 5, left, 21);
    ?STRIP(0, 7, 4, left, 22);
    ?STRIP(0, 7, 3, left, 23);
    ?STRIP(0, 7, 2, left, 24);
    ?STRIP(0, 7, 1, left, 25);
    ?STRIP(0, 7, 0, left, 26);
    ?STRIP(0, 8, 6, left, 27);
    ?STRIP(0, 8, 5, left, 28);
    ?STRIP(0, 8, 4, left, 29);
    ?STRIP(0, 8, 3, left, 30);
    ?STRIP(0, 8, 2, left, 31);
    ?STRIP(0, 8, 1, left, 32);
    ?STRIP(0, 8, 0, left, 33);
    ?STRIP(0, 9, 6, left, 34);
    ?STRIP(0, 9, 5, left, 35);
    ?STRIP(0, 9, 4, left, 36);
    ?STRIP(0, 9, 3, left, 37);
    ?STRIP(0, 9, 2, left, 38);
    ?STRIP(0, 9, 1, left, 39);
    ?STRIP(0, 9, 0, left, 40);
    ?STRIP(0, 10, 6, left, 41);
    ?STRIP(0, 10, 5, left, 42);
    ?STRIP(0, 10, 4, left, 43);
    ?STRIP(0, 10, 3, left, 44);
    ?STRIP(0, 10, 2, left, 45);
    ?STRIP(0, 10, 1, left, 46);
    ?STRIP(0, 10, 0, left, 47);
    ?STRIP(0, 11, 5, left, 48);
    ?STRIP(0, 11, 4, left, 49);
    ?STRIP(0, 11, 3, left, 50);
    ?STRIP(0, 11, 2, left, 51);
    ?STRIP(0, 11, 1, left, 52);
    ?STRIP(0, 11, 0, left, 53);
    ?STRIP(0, 12, 6, left, 54);
    ?STRIP(0, 12, 5, left, 55);
    ?STRIP(0, 12, 4, left, 56);
    ?STRIP(0, 12, 3, left, 57);
    ?STRIP(0, 12, 2, left, 58);
    ?STRIP(0, 12, 1, left, 59);
    ?STRIP(0, 12, 0, left, 60);
    ?STRIP(0, 13, 6, left, 61);
    ?STRIP(0, 13, 5, left, 62);
    ?STRIP(0, 13, 4, left, 63);
    ?STRIP(0, 13, 3, left, 64);
    ?STRIP(0, 13, 2, left, 65);
    ?STRIP(0, 13, 1, left, 66);
    ?STRIP(0, 13, 0, left, 67);
    ?STRIP(1, 14, 3, top, 0);
    ?STRIP(1, 14, 2, top, 1);
    ?STRIP(1, 14, 1, top, 2);
    ?STRIP(1, 14, 0, top, 3);
    ?STRIP(2, 14, 2, top, 4);
    ?STRIP(2, 14, 1, top, 5);
    ?STRIP(2, 14, 0, top, 6);
    ?STRIP(3, 14, 3, top, 7);
    ?STRIP(3, 14, 2, top, 8);
    ?STRIP(3, 14, 1, top, 9);
    ?STRIP(3, 14, 0, top, 10);
    ?STRIP(4, 14, 2, top, 11);
    ?STRIP(4, 14, 1, top, 12);
    ?STRIP(4, 14, 0, top, 13);
    ?STRIP(5, 14, 2, top, 14);
    ?STRIP(5, 14, 1, top, 15);
    ?STRIP(5, 14, 0, top, 16);
    ?STRIP(6, 14, 2, top, 17);
    ?STRIP(6, 14, 1, top, 18);
    ?STRIP(6, 14, 0, top, 19);
    ?STRIP(7, 14, 2, top, 20);
    ?STRIP(7, 14, 1, top, 21);
    ?STRIP(7, 14, 0, top, 22);
    ?STRIP(8, 14, 2, top, 23);
    ?STRIP(8, 14, 1, top, 24);
    ?STRIP(8, 14, 0, top, 25);
    ?STRIP(9, 14, 3, top, 26);
    ?STRIP(9, 14, 2, top, 27);
    ?STRIP(9, 14, 1, top, 28);
    ?STRIP(9, 14, 0, top, 29);
    ?STRIP(10,14, 2, top, 30);
    ?STRIP(10,14, 1, top, 31);
    ?STRIP(10,14, 0, top, 32);
    ?STRIP(11,14, 3, top, 33);
    ?STRIP(11,14, 2, top, 34);
    ?STRIP(11,14, 1, top, 35);
    ?STRIP(11,14, 0, top, 36);
    ?STRIP(12,14, 2, top, 37);
    ?STRIP(12,14, 1, top, 38);
    ?STRIP(12,14, 0, top, 39);
    ?STRIP(13,14, 2, top, 40);
    ?STRIP(13,14, 1, top, 41);
    ?STRIP(13,14, 0, top, 42);
    ?STRIP(14,14, 2, top, 43);
    ?STRIP(14,14, 1, top, 44);
    ?STRIP(14,14, 0, top, 45);
    ?STRIP(15,14, 2, top, 46);
    ?STRIP(15,14, 1, top, 47);
    ?STRIP(15,14, 0, top, 48);
    ?STRIP(16,14, 2, top, 49);
    ?STRIP(16,14, 1, top, 50);
    ?STRIP(16,14, 0, top, 51);
    ?STRIP(17,14, 3, top, 52);
    ?STRIP(17,14, 2, top, 53);
    ?STRIP(17,14, 1, top, 54);
    ?STRIP(17,14, 0, top, 55);
    ?STRIP(18,14, 2, top, 56);
    ?STRIP(18,14, 1, top, 57);
    ?STRIP(18,14, 0, top, 58);
    ?STRIP(19,14, 3, top, 59);
    ?STRIP(19,14, 2, top, 60);
    ?STRIP(19,14, 1, top, 61);
    ?STRIP(19,14, 0, top, 62);
    ?STRIP(20,14, 2, top, 63);
    ?STRIP(20,14, 1, top, 64);
    ?STRIP(20,14, 0, top, 65);
    ?STRIP(21,13, 0, right, 0);
    ?STRIP(21,13, 1, right, 1);
    ?STRIP(21,13, 2, right, 2);
    ?STRIP(21,13, 3, right, 3);
    ?STRIP(21,13, 4, right, 4);
    ?STRIP(21,13, 5, right, 5);
    ?STRIP(21,12, 0, right, 6);
    ?STRIP(21,12, 1, right, 7);
    ?STRIP(21,12, 2, right, 8);
    ?STRIP(21,12, 3, right, 9);
    ?STRIP(21,12, 4, right, 10);
    ?STRIP(21,11, 0, right, 11);
    ?STRIP(21,11, 1, right, 12);
    ?STRIP(21,11, 2, right, 13);
    ?STRIP(21,11, 3, right, 14);
    ?STRIP(21,11, 4, right, 15);
    ?STRIP(21,11, 5, right, 16);
    ?STRIP(21,10, 0, right, 17);
    ?STRIP(21,10, 1, right, 18);
    ?STRIP(21,10, 2, right, 19);
    ?STRIP(21,10, 3, right, 20);
    ?STRIP(21,10, 4, right, 21);
    ?STRIP(21, 9, 0, right, 22);
    ?STRIP(21, 9, 1, right, 23);
    ?STRIP(21, 9, 2, right, 24);
    ?STRIP(21, 9, 3, right, 25);
    ?STRIP(21, 9, 4, right, 26);
    ?STRIP(21, 8, 0, right, 27);
    ?STRIP(21, 8, 1, right, 28);
    ?STRIP(21, 8, 2, right, 29);
    ?STRIP(21, 8, 3, right, 30);
    ?STRIP(21, 8, 4, right, 31);
    ?STRIP(21, 8, 5, right, 32);
    ?STRIP(21, 7, 0, right, 33);
    ?STRIP(21, 7, 1, right, 34);
    ?STRIP(21, 7, 2, right, 35);
    ?STRIP(21, 7, 3, right, 36);
    ?STRIP(21, 7, 4, right, 37);
    ?STRIP(21, 7, 5, right, 38);
    ?STRIP(21, 6, 0, right, 39);
    ?STRIP(21, 6, 1, right, 40);
    ?STRIP(21, 6, 2, right, 41);
    ?STRIP(21, 6, 3, right, 42);
    ?STRIP(21, 6, 4, right, 43);
    ?STRIP(21, 5, 0, right, 44);
    ?STRIP(21, 5, 1, right, 45);
    ?STRIP(21, 5, 2, right, 46);
    ?STRIP(21, 5, 3, right, 47);
    ?STRIP(21, 5, 4, right, 48);
    ?STRIP(21, 5, 5, right, 49);
    ?STRIP(21, 4, 0, right, 50);
    ?STRIP(21, 4, 1, right, 51);
    ?STRIP(21, 4, 2, right, 52);
    ?STRIP(21, 4, 3, right, 53);
    ?STRIP(21, 4, 4, right, 54);
    ?STRIP(21, 3, 0, right, 55);
    ?STRIP(21, 3, 1, right, 56);
    ?STRIP(21, 3, 2, right, 57);
    ?STRIP(21, 3, 3, right, 58);
    ?STRIP(21, 3, 4, right, 59);
    ?STRIP(21, 3, 5, right, 60);
    ?STRIP(21, 2, 0, right, 61);
    ?STRIP(21, 2, 1, right, 62);
    ?STRIP(21, 2, 2, right, 63);
    ?STRIP(21, 2, 3, right, 64);
    ?STRIP(21, 2, 4, right, 65);
    ?STRIP(21, 1, 0, right, 66);
    ?STRIP(21, 1, 1, right, 67);
    ?STRIP(21, 1, 2, right, 68);
    ?STRIP(21, 1, 3, right, 69);
    ?STRIP(21, 1, 4, right, 70);
    ?STRIP(21, 1, 5, right, 71);
    ?STRIP(20, 0, 0, bottom, 0);
    ?STRIP(20, 0, 1, bottom, 1);
    ?STRIP(20, 0, 2, bottom, 2);
    ?STRIP(19, 0, 0, bottom, 3);
    ?STRIP(19, 0, 1, bottom, 4);
    ?STRIP(19, 0, 2, bottom, 5);
    ?STRIP(19, 0, 3, bottom, 6);
    ?STRIP(18, 0, 0, bottom, 7);
    ?STRIP(18, 0, 1, bottom, 8);
    ?STRIP(18, 0, 2, bottom, 9);
    ?STRIP(17, 0, 0, bottom, 10);
    ?STRIP(17, 0, 1, bottom, 11);
    ?STRIP(17, 0, 2, bottom, 12);
    ?STRIP(17, 0, 3, bottom, 13);
    ?STRIP(16, 0, 0, bottom, 14);
    ?STRIP(16, 0, 1, bottom, 15);
    ?STRIP(16, 0, 2, bottom, 16);
    ?STRIP(15, 0, 0, bottom, 17);
    ?STRIP(15, 0, 1, bottom, 18);
    ?STRIP(15, 0, 2, bottom, 19);
    ?STRIP(15, 0, 3, bottom, 20);
    ?STRIP(14, 0, 0, bottom, 21);
    ?STRIP(14, 0, 1, bottom, 22);
    ?STRIP(14, 0, 2, bottom, 23);
    ?STRIP(12, 3, 0, bottom, 24);
    ?STRIP(12, 3, 1, bottom, 25);
    ?STRIP(12, 3, 2, bottom, 26);
    ?STRIP(12, 3, 3, bottom, 27);
    ?STRIP(11, 3, 0, bottom, 28);
    ?STRIP(11, 3, 1, bottom, 29);
    ?STRIP(11, 3, 2, bottom, 30);
    ?STRIP(10, 3, 0, bottom, 31);
    ?STRIP(10, 3, 1, bottom, 32);
    ?STRIP(10, 3, 2, bottom, 33);
    ?STRIP(10, 3, 3, bottom, 34);
    ?STRIP(9, 3, 0, bottom, 35);
    ?STRIP(9, 3, 1, bottom, 36);
    ?STRIP(9, 3, 2, bottom, 37);
    ?STRIP(8, 3, 0, bottom, 38);
    ?STRIP(8, 3, 1, bottom, 39);
    ?STRIP(8, 3, 2, bottom, 40);
    ?STRIP(8, 3, 3, bottom, 41);
    ?STRIP(7, 3, 0, bottom, 42);
    ?STRIP(7, 3, 1, bottom, 43);
    ?STRIP(7, 3, 2, bottom, 44);
    ?STRIP(6, 3, 0, bottom, 45);
    ?STRIP(6, 3, 1, bottom, 46);
    ?STRIP(6, 3, 2, bottom, 47);
    ?STRIP(6, 3, 3, bottom, 48);
    ?STRIP(5, 3, 0, bottom, 49);
    ?STRIP(5, 3, 1, bottom, 50);
    ?STRIP(5, 3, 2, bottom, 51);
    ?STRIP(4, 3, 0, bottom, 52);
    ?STRIP(4, 3, 1, bottom, 53);
    ?STRIP(4, 3, 2, bottom, 54);
    ?STRIP(4, 3, 3, bottom, 55);
    ?STRIP(3, 3, 0, bottom, 56);
    ?STRIP(3, 3, 1, bottom, 57);
    ?STRIP(3, 3, 2, bottom, 58);
    ?STRIP(2, 3, 0, bottom, 59);
    ?STRIP(2, 3, 1, bottom, 60);
    ?STRIP(2, 3, 2, bottom, 61);
    ?STRIP(2, 3, 3, bottom, 62);
    ?STRIP(1, 3, 0, bottom, 63);
    ?STRIP(1, 3, 1, bottom, 64);
    ?STRIP(1, 3, 2, bottom, 65);
).

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Count = density:fuse_count(Density),
    Db = fuse_database:read(Density),
    run(0, Count, Density, Db).

%%--------------------------------------------------------------------

run(Stop, Stop, _, _) ->
    ok;
run(Fuse, Stop, Density, Db) ->
    run(Fuse, Density, Db),
    run(Fuse + 1, Stop, Density, Db).

%%====================================================================
%% run
%%====================================================================

-spec run(fuse(), density()) -> ok.

run(Fuse, Density) ->
    Db = fuse_database:read(Density),
    run(Fuse, Density, Db).

%%--------------------------------------------------------------------

run(Fuse, Density, Db) ->
    case fuse_database:find(Fuse, Db) of
        {ok, DbName} ->
            case to_name(Fuse, Density) of
                {ok, MapName} when MapName =:= DbName ->
                    case from_name(DbName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({
                                fuse, Density, Fuse,
                                db, DbName,
                                from_name, different, MapFuse
                            });

                        {error, Error} ->
                            throw({
                                fuse, Density, Fuse,
                                to, DbName,
                                from_name, error, Error
                            })
                    end;

                {ok, MapName} ->
                    throw({
                        fuse, Density, Fuse,
                        db, DbName,
                        to_name, different, MapName
                    });

                {error, Error} ->
                    throw({
                        fuse, Density, Fuse,
                        db, DbName,
                        to_name, error, Error
                    })
            end;

        false ->
            case to_name(Fuse, Density) of
                {ok, MapName} ->
                    case from_name(MapName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({
                                fuse, Density, Fuse,
                                to, MapName,
                                from_name, different, MapFuse
                            });

                        {error, Error} ->
                            throw({
                                fuse, Density, Fuse,
                                to, MapName,
                                from_name, error, Error
                            })
                    end;

                {error, _} ->
                    ok
            end
    end.

%%====================================================================
%% from_name
%%====================================================================

-spec from_name(name(), density()) -> {ok, fuse()} | {error, term()}.

from_name(Name, epm240) ->
    from_epm240(Name);
from_name(Name, epm570) ->
    from_epm570(Name);
from_name(Name, epm1270) ->
    from_epm1270(Name);
from_name(Name, epm2210) ->
    from_epm2210(Name).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm240({user_code, Bit}) -> {ok, Fuse}
).

?EPM240_USER_CODES()
from_epm240(Name) ->
    from_density(Name, epm240()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm570({user_code, Bit}) -> {ok, Fuse}
).

?EPM570_USER_CODES()
from_epm570(Name) ->
    from_density(Name, epm570()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm1270({user_code, Bit}) -> {ok, Fuse}
).

?EPM1270_USER_CODES()
from_epm1270(Name) ->
    from_density(Name, epm1270()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    from_epm2210({user_code, Bit}) -> {ok, Fuse}
).

?EPM2210_USER_CODES()
from_epm2210(Name) ->
    from_density(Name, epm2210()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

from_density({{iob, X0, Y}, Name}, With = #with{}) ->
    case from_density_iob(X0, Y, With) of
        {side, X} ->
            from_iob_side(X, Y, Name, With);

        {cell, X} ->
            from_iob(X, Y, Name, With)
    end;
from_density({{iob, X0, Y}, Name, Value}, With = #with{}) ->
    case from_density_iob(X0, Y, With) of
        {side, X} ->
            from_iob_side(X, Y, {Name, Value}, With);

        {cell, X} ->
            from_iob(X, Y, {Name, Value}, With)
    end;
from_density({{iob, X0, Y}, Name, Key, Value}, With = #with{}) ->
    case from_density_iob(X0, Y, With) of
        {side, X} ->
            from_iob_side(X, Y, {Name, Key, Value}, With);

        {cell, X} ->
            from_iob(X, Y, {Name, Key, Value}, With)
    end;
from_density({{ioc, X0, Y, N}, Name}, With = #with{}) ->
    case from_density_iob(X0, Y, With) of
        {side, X} ->
            from_ioc_side(X, Y, N, Name, With);

        {cell, X} ->
            from_ioc(X, Y, N, Name, With)
    end;
from_density({{ioc, X0, Y, N}, Name, Value}, With = #with{}) ->
    case from_density_iob(X0, Y, With) of
        {side, X} ->
            from_ioc_side(X, Y, N, {Name, Value}, With);

        {cell, X} ->
            from_ioc(X, Y, N, {Name, Value}, With)
    end;
from_density({{lab, X0, Y}, Name}, With = #with{}) ->
    X = from_density_lab(X0, Y, With),
    from_lab(X, Y, Name, With);
from_density({{lab, X0, Y}, Name, Value}, With = #with{}) ->
    X = from_density_lab(X0, Y, With),
    from_lab(X, Y, {Name, Value}, With);
from_density({{lab, X0, Y}, Name, Key, Value}, With = #with{}) ->
    X = from_density_lab(X0, Y, With),
    from_lab(X, Y, {Name, Key, Value}, With);
from_density({{lc, X0, Y, N}, Name}, With = #with{}) ->
    X = from_density_lab(X0, Y, With),
    from_lc(X, Y, N, Name, With);
from_density({{lc, X0, Y, N}, Name, Value}, With = #with{}) ->
    X = from_density_lab(X0, Y, With),
    from_lc(X, Y, N, {Name, Value}, With);
from_density(_Name, _With = #with{}) ->
    {error, density}.

%%--------------------------------------------------------------------

from_density_iob(X0, Y, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X =:= 0 andalso X < With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            {side, X};

        X when X =:= 0 andalso X =:= With#with.grow_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            {side, X};

        X when X > 0 andalso X =< With#with.grow_x andalso
               (Y =:= 3 orelse Y =:= With#with.top_y) ->
            {cell, X};

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               (Y =:= 0 orelse Y =:= With#with.top_y) ->
            {cell, X};

        X when X =:= With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            {side, X};

        X ->
            throw({from_density, {iob, X, Y}, With})
    end.

%%--------------------------------------------------------------------

from_density_lab(X0, Y, With = #with{}) ->
    case X0 - With#with.offset_x of
        X when X > 0 andalso X =< With#with.grow_x andalso
               Y > 3 andalso Y =< With#with.top_y ->
            X;

        X when X > With#with.grow_x andalso X =< With#with.side_x andalso
               Y > 0 andalso Y =< With#with.top_y ->
            X;

        X ->
            throw({from_density, {lab, X, Y}, With})
    end.

%%--------------------------------------------------------------------

-define(IOB_SIDE(Sector, N, Index, Name),
    from_iob_side(X, Y, Name, With) ->
        from_side(X, Sector, Y, N, Index, With)
).

?IOB_SIDES()
from_iob_side(X, Y, Name, _With) ->
    {error, {iob_side, X, Y, Name}}.

-undef(IOB_SIDE).

%%--------------------------------------------------------------------

-define(IOB_HEAD(Sector, Index, Name),
    from_iob(X, Y, Name, With = #with{top_y = Y}) ->
        from_head(X, Sector, Index, With)
).
-define(IOB_TAIL(Sector, Index, Name),
    from_iob(X, 3, Name, With) when X < With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.short_y);
    from_iob(X, 0, Name, With) when X > With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.long_y)
).

?IOB_HEADS()
?IOB_TAILS()
from_iob(X, Y, Name, _With) ->
    {error, {iob, X, Y, Name}}.

-undef(IOB_HEAD).
-undef(IOB_TAIL).

%%--------------------------------------------------------------------

-define(IOC_ZERO(I, Name),
    from_ioc_side(X, Y, N, Name, With) ->
        from_zero(X, Y, N, I, With)
).
-define(IOC_SIDE(Sector, Index, Name),
    from_ioc_side(X, Y, N, Name, With) ->
        from_side(X, Sector, Y, N + 2, Index, With)
).
-define(IOC_LEFT(Sector, U, V, N, Name),
    from_ioc_side(X, Y, N, Name, With) when X =:= 0 ->
        from_side(X, Sector, Y, U, V, With)
).
-define(IOC_LEFT_LINE(Sector, Index, N, Name),
    from_ioc_side(X, Y, N, Name, With) when X =:= 0 ->
        from_side_line(X, Sector, Y, Index, With)
).
-define(IOC_RIGHT(Sector, U, V, N, Name),
    from_ioc_side(X, Y, N, Name, With) when X =/= 0 ->
        from_side(X, Sector, Y, U, V, With)
).
-define(IOC_RIGHT_LINE(Sector, Index, N, Name),
    from_ioc_side(X, Y, N, Name, With) when X =/= 0 ->
        from_side_line(X, Sector, Y, Index, With)
).
-define(IOC_STRIP(R, C, Name),
    from_ioc_side(X, Y, N, Name, With) ->
        from_ioc_strip(X, Y, N, R, C, With)
).

?IOC_ZEROS()
?IOC_SIDES()
?IOC_LEFTS()
?IOC_LEFT_LINES()
?IOC_RIGHTS()
?IOC_RIGHT_LINES()
?IOC_STRIPS()
from_ioc_side(X, Y, N, Name, _With) ->
    {error, {ioc_side, X, Y, N, Name}}.

-undef(IOC_ZERO).
-undef(IOC_SIDE).
-undef(IOC_LEFT).
-undef(IOC_LEFT_LINE).
-undef(IOC_RIGHT).
-undef(IOC_RIGHT_LINE).
-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(IOC_HEAD(Sector, Index, N, Name),
    from_ioc(X, Y, N, Name, With = #with{top_y = Y}) ->
        from_head(X, Sector, Index, With)
).
-define(IOC_TAIL(Sector, Index, N, Name),
    from_ioc(X, 3, N, Name, With) when X < With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.short_y);
    from_ioc(X, 0, N, Name, With) when X > With#with.grow_x ->
        from_tail(X, Sector, Index, With, With#with.long_y)
).
-define(IOC_STRIP(R, C, Name),
    from_ioc(X, Y, N, Name, With) ->
        from_ioc_strip(X, Y, N, R, C, With)
).

?IOC_HEADS()
?IOC_TAILS()
?IOC_STRIPS()
from_ioc(X, Y, N, Name, _With) ->
    {error, {ioc, X, Y, N, Name}}.

-undef(IOC_HEAD).
-undef(IOC_TAIL).
-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(LAB_CELL(Sector, N, I, Name),
    from_lab(X, Y, Name, With) ->
        from_cell(X, Sector, Y, N, I, With)
).
-define(LAB_LINE(Sector, Index, Name),
    from_lab(X, Y, Name, With) ->
        from_line(X, Sector, Y, Index, With)
).

?LAB_CELLS()
?LAB_LINES()
from_lab(X, Y, Name, _With) ->
    {error, {lab, X, Y, Name}}.

-undef(LAB_CELL).
-undef(LAB_LINE).

%%--------------------------------------------------------------------

-define(LC_CELL(Sector, I, Name),
    from_lc(X, Y, N, Name, With) ->
        from_cell(X, Sector, Y, N, I, With)
).

?LC_CELLS()
from_lc(X, Y, N, Name, _With) ->
    {error, {lc, X, Y, N, Name}}.

-undef(LC_CELL).

%%--------------------------------------------------------------------

from_ioc_strip(X, Y, N, R, C, With = #with{density = epm240}) ->
    from_epm240_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm570}) ->
    from_epm570_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm1270}) ->
    from_epm1270_strip(X + With#with.offset_x, Y, N, R, C, With);
from_ioc_strip(X, Y, N, R, C, With = #with{density = epm2210}) ->
    from_epm2210_strip(X + With#with.offset_x, Y, N, R, C, With).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm240_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM240_STRIPS()
from_epm240_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm570_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM570_STRIPS()
from_epm570_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm1270_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM1270_STRIPS()
from_epm1270_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    from_epm2210_strip(X, Y, N, R, C, With) ->
        from_side_strip(Side, Index, R, C, With)
).

?EPM2210_STRIPS()
from_epm2210_strip(X, Y, N, R, C, _With) ->
    {error, {strip, X, Y, N, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

from_side_strip(left, Index, R, C, With = #with{left_strip = Base}) ->
    from_base_strip(Base, Index, R, C, With);
from_side_strip(top, Index, R, C, With = #with{top_strip = Base}) ->
    from_base_strip(Base, Index, R, C, With);
from_side_strip(right, Index, R, C, With = #with{right_strip = Base})
        when With#with.density =:= epm1270 orelse
             With#with.density =:= epm2210 ->
    from_base_strip7(Base, Index, 5 - R, C, With);
from_side_strip(right, Index, R, C, With = #with{right_strip = Base}) ->
    from_base_strip(Base, Index, 5 - R, C, With);
from_side_strip(bottom, Index, R, C, With = #with{bottom_strip = Base}) ->
    from_base_strip(Base, Index, 5 - R, C, With).

%%--------------------------------------------------------------------

from_base_strip(Base, Index, R, C, #with{strip_width = Width}) ->
    {ok, ((Base + (Index * 6) + R) * Width) + C}.

%%--------------------------------------------------------------------

from_base_strip7(Base, Index, R, C, #with{strip_width = Width}) ->
    {ok, ((Base + (Index * 7) + R) * Width) + C}.

%%--------------------------------------------------------------------

from_zero(X, Y, 0, I, With) -> from_side_line(X, 0, Y, 0 + I, With);
from_zero(X, Y, 1, I, With) -> from_side_line(X, 0, Y, 7 + I, With);
from_zero(X, Y, 2, I, With) -> from_side_line(X, 0, Y, 13 + I, With);
from_zero(X, Y, 3, I, With) -> from_side_line(X, 0, Y, 20 + I, With);
from_zero(X, Y, 4, I, With) -> from_side_line(X, 0, Y, 27 + I, With);
from_zero(X, Y, 5, I, With) -> from_side_line(X, 0, Y, 33 + I, With);
from_zero(X, Y, 6, I, With) -> from_side_line(X, 0, Y, 40 + I, With).

%%--------------------------------------------------------------------

from_side(X, Sector, Y, N, I, With) when N < 5 andalso X < 2 ->
    from_line(X, Sector, Y, (N * 4) + I, With);
from_side(X, Sector, Y, N, I, With) when X < 2 ->
    from_line(X, Sector, Y, 9 + (N * 4) - I, With);
from_side(X, Sector, Y, N, I, With) when N < 5 ->
    from_line(X, ?SIDE_SECTORS - 1 - Sector, Y, (N * 4) + I, With);
from_side(X, Sector, Y, N, I, With) ->
    from_line(X, ?SIDE_SECTORS - 1 - Sector, Y, 9 + (N * 4) - I, With).

%%--------------------------------------------------------------------

from_side_line(X, Sector, Y, I, With) when X < 2 ->
    from_line(X, Sector, Y, I, With);
from_side_line(X, Sector, Y, I, With) ->
    from_line(X, ?SIDE_SECTORS - 1 - Sector, Y, I, With).

%%--------------------------------------------------------------------

from_head(X, Sector, Offset, With) ->
    from_sector_skip(
        X,
        Sector,
        Offset,
        With
    ).

%%--------------------------------------------------------------------

from_tail(X, Sector, Offset, With, Lines) ->
    End = ?HEAD_WIDTH + (Lines * ?LINE_WIDTH) + 10,
    from_sector_skip(
        X,
        Sector,
        End - Offset,
        With
    ).

%%--------------------------------------------------------------------

from_cell(X, Sector, Y, N, I, With) when N < 5 ->
    from_line(X, Sector, Y, (N * 4) + I, With);
from_cell(X, Sector, Y, N, I, With) ->
    from_line(X, Sector, Y, 65 - (N * 4) - I, With).

%%--------------------------------------------------------------------

from_line(X, Sector, Y, Offset, With = #with{long_y = TopY}) ->
    from_sector_skip(
        X,
        Sector,
        ?HEAD_WIDTH + ((TopY - Y) * ?LINE_WIDTH) + Offset,
        With
    ).

%%--------------------------------------------------------------------

from_sector_skip(X, Sector, Offset, With = #with{})
        when Offset >= With#with.sector_skip  ->
    from_sector_pad(X, Sector, Offset + 1, With);
from_sector_skip(X, Sector, Offset, With) ->
    from_sector_pad(X, Sector, Offset, With).

%%--------------------------------------------------------------------

from_sector_pad(X, Sector, Offset, With = #with{}) ->
    Line = 1 + (Offset div (With#with.strip_width - 3)),
    from_sector(X, Sector, Offset + (Line * 3), With).

%%--------------------------------------------------------------------

from_sector(0, Sector, Offset, With = #with{}) ->
    {ok,
        With#with.left_base +
        (Sector * With#with.short_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X < With#with.grow_x orelse
             (X =:= With#with.grow_x andalso Sector < ?SHORT_SECTORS) ->
    {ok,
        With#with.short_base +
        ((X - 1) * ?COLUMN_SECTORS * With#with.short_sector) +
        (Sector * With#with.short_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X =:= With#with.grow_x ->
    {ok,
        With#with.grow_base +
        (?SHORT_SECTORS * With#with.short_sector) +
        ((Sector - ?SHORT_SECTORS) * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X < With#with.side_x ->
    {ok,
        With#with.long_base +
        ((X - 1 - With#with.grow_x) * ?COLUMN_SECTORS * With#with.long_sector) +
        (Sector * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, With = #with{})
        when X =:= With#with.side_x ->
    {ok,
        With#with.right_base +
        (Sector * With#with.long_sector) +
        Offset
    };
from_sector(X, Sector, Offset, _With) ->
    {error, {sector, X, Sector, Offset}}.

%%====================================================================
%% to_location
%%====================================================================

-spec to_location(fuse(), density()) -> location().

to_location(Fuse, epm240) ->
    to_density(Fuse, epm240());
to_location(Fuse, epm570) ->
    to_density(Fuse, epm570());
to_location(Fuse, epm1270) ->
    to_density(Fuse, epm1270());
to_location(Fuse, epm2210) ->
    to_density(Fuse, epm2210()).

%%--------------------------------------------------------------------

to_density(Fuse, With = #with{})
        when (Fuse rem With#with.strip_width) < 3 ->
    Row = Fuse div With#with.strip_width,
    Col = Fuse rem With#with.strip_width,
    to_strip(Row, Col, With);
to_density(Fuse, With = #with{}) ->
    OffsetX = With#with.offset_x,
    ShortSector = With#with.short_sector,
    LongSector = With#with.long_sector,
    ShortColumn = ?COLUMN_SECTORS * ShortSector,
    LongColumn = ?COLUMN_SECTORS * LongSector,
    if
        Fuse < With#with.left_base ->
            {Fuse, header};

        Fuse < With#with.short_base ->
            SectorOffset = Fuse - With#with.left_base,
            to_column(
                side,
                SectorOffset div ShortSector,
                OffsetX,
                SectorOffset rem ShortSector,
                With,
                With#with.short_y
            );

        Fuse < With#with.grow_base ->
            ColumnOffset = Fuse - With#with.short_base,
            SectorOffset = ColumnOffset rem ShortColumn,
            to_column(
                cell,
                SectorOffset div ShortSector,
                OffsetX + 1 + (ColumnOffset div ShortColumn),
                SectorOffset rem ShortSector,
                With,
                With#with.short_y
            );

        Fuse < With#with.long_base ->
            GrowOffset = ?SHORT_SECTORS * ShortSector,
            case Fuse - With#with.grow_base of
                SectorOffset when SectorOffset < GrowOffset ->
                    to_column(
                        cell,
                        SectorOffset div ShortSector,
                        OffsetX + With#with.grow_x,
                        SectorOffset rem ShortSector,
                        With,
                        With#with.short_y
                    );

                SectorOffset0 ->
                    SectorOffset = SectorOffset0 - GrowOffset,
                    to_column(
                        cell,
                        (SectorOffset div LongSector) + ?SHORT_SECTORS,
                        OffsetX + With#with.grow_x,
                        SectorOffset rem LongSector,
                        With,
                        With#with.long_y
                    )
            end;

        Fuse < With#with.right_base ->
            ColumnOffset = Fuse - With#with.long_base,
            SectorOffset = ColumnOffset rem LongColumn,
            to_column(
                cell,
                SectorOffset div LongSector,
                OffsetX + With#with.grow_x + 1 + (ColumnOffset div LongColumn),
                SectorOffset rem LongSector,
                With,
                With#with.long_y
            );

        Fuse < With#with.end_base ->
            SectorOffset = Fuse - With#with.right_base,
            to_column(
                side,
                ?SIDE_SECTORS - 1 - (SectorOffset div LongSector),
                OffsetX + With#with.side_x,
                SectorOffset rem LongSector,
                With,
                With#with.long_y
            );

        true ->
            {Fuse, footer}
    end.

%%--------------------------------------------------------------------

to_strip(Row, Col, With = #with{}) when Row < With#with.left_strip ->
    {Row, Col, strip};
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.top_strip ->
    Row = Row0 - With#with.left_strip,
    {left, Row div 6, strip, Row rem 6, Col};
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.right_strip ->
    Row = Row0 - With#with.top_strip,
    {top, Row div 6, strip, Row rem 6, Col};
to_strip(Row0, Col, With = #with{})
        when Row0 < With#with.bottom_strip andalso
             (With#with.density =:= epm1270 orelse
              With#with.density =:= epm2210) ->
    Row = Row0 - With#with.right_strip,
    {right, Row div 7, strip, 5 - (Row rem 7), Col};
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.bottom_strip ->
    Row = Row0 - With#with.right_strip,
    {right, Row div 6, strip, 5 - (Row rem 6), Col};
to_strip(Row0, Col, With = #with{}) when Row0 < With#with.end_strip ->
    Row = Row0 - With#with.bottom_strip,
    {bottom, Row div 6, strip, 5 - (Row rem 6), Col};
to_strip(Row, Col, _With) ->
    {Row, Col, strip}.

%%--------------------------------------------------------------------

to_column(Type, Sector, X, Offset0, With = #with{}, Lines) ->
    Skip = With#with.sector_skip,
    Top = With#with.long_y,
    End = ?HEAD_WIDTH + 1 + (Lines * ?LINE_WIDTH),
    Padding = 3 * (1 + (Offset0 div With#with.strip_width)),
    case Offset0 - Padding of
        Offset when Offset < ?HEAD_WIDTH ->
            {X, head, Offset, Type, Sector};

        Offset when Offset < Skip ->
            Trimmed = Offset - ?HEAD_WIDTH,
            to_line(
                Type,
                Sector,
                X,
                Top - (Trimmed div ?LINE_WIDTH),
                Trimmed rem ?LINE_WIDTH
            );

        Offset when Offset =:= Skip ->
            {X, skip, Offset, Type, Sector};

        Offset when Offset < End ->
            Trimmed = Offset - ?HEAD_WIDTH - 1,
            to_line(
                Type,
                Sector,
                X,
                Top - (Trimmed div ?LINE_WIDTH),
                Trimmed rem ?LINE_WIDTH
            );

        Offset when Offset < End + ?HEAD_WIDTH ->
            {X, tail, 10 + End - Offset, Type, Sector};

        Offset ->
            {X, column, Offset, Type, Sector}
    end.

%%--------------------------------------------------------------------

to_line(side, 0, X, Y, Index) when Index < 7 ->
    {X, Y, 0, zero, Index};
to_line(side, 0, X, Y, Index) when Index < 13 ->
    {X, Y, 1, zero, Index - 7};
to_line(side, 0, X, Y, Index) when Index < 20 ->
    {X, Y, 2, zero, Index - 13};
to_line(side, 0, X, Y, Index) when Index < 27 ->
    {X, Y, 3, zero, Index - 20};
to_line(side, 0, X, Y, Index) when Index < 33 ->
    {X, Y, 4, zero, Index - 27};
to_line(side, 0, X, Y, Index) when Index < 40 ->
    {X, Y, 5, zero, Index - 33};
to_line(side, 0, X, Y, Index) ->
    {X, Y, 6, zero, Index - 40};
to_line(side, Sector, X, Y, Index) when Index < 20 ->
    {X, Y, Index div 4, Index rem 4, side, Sector};
to_line(side, Sector, X, Y, Index) when Index < 26 ->
    {X, Y, line, Index, side, Sector};
to_line(side, Sector, X, Y, Index0) ->
    Index = Index0 - 6,
    {X, Y, Index div 4, 3 - (Index rem 4), side, Sector};
to_line(cell, Sector, X, Y, Index) when Index < 20 ->
    {X, Y, Index div 4, Index rem 4, cell, Sector};
to_line(cell, Sector, X, Y, Index) when Index < 26 ->
    {X, Y, line, Index, cell, Sector};
to_line(cell, Sector, X, Y, Index0) ->
    Index = 65 - Index0,
    {X, Y, Index div 4, Index rem 4, cell, Sector}.

%%====================================================================
%% to_name
%%====================================================================

-spec to_name(fuse(), density()) -> {ok, name()} | {error, location()}.

to_name(Fuse, epm240) ->
    to_epm240(Fuse);
to_name(Fuse, epm570) ->
    to_epm570(Fuse);
to_name(Fuse, epm1270) ->
    to_epm1270(Fuse);
to_name(Fuse, epm2210) ->
    to_epm2210(Fuse).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm240(Fuse) -> {ok, {user_code, Bit}}
).

?EPM240_USER_CODES()
to_epm240(Fuse) ->
    to_name_with(Fuse, epm240()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm570(Fuse) -> {ok, {user_code, Bit}}
).

?EPM570_USER_CODES()
to_epm570(Fuse) ->
    to_name_with(Fuse, epm570()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm1270(Fuse) -> {ok, {user_code, Bit}}
).

?EPM1270_USER_CODES()
to_epm1270(Fuse) ->
    to_name_with(Fuse, epm1270()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

-define(USER_CODE(Bit, Fuse),
    to_epm2210(Fuse) -> {ok, {user_code, Bit}}
).

?EPM2210_USER_CODES()
to_epm2210(Fuse) ->
    to_name_with(Fuse, epm2210()).

-undef(USER_CODE).

%%--------------------------------------------------------------------

to_name_with(Fuse, With) ->
    case to_density(Fuse, With) of
        {Side, Index, strip, R, C} ->
            to_strip(Side, Index, R, C, With);

        {X, head, Index, cell, Sector} ->
            to_cell_head(X, Index, Sector, With);

        {X, tail, Index, cell, Sector} ->
            to_cell_tail(X, Index, Sector, With);

        {X, Y, N, zero, I} ->
            to_zero(X, Y, N, I);

        {X, Y, line, Index, side, Sector} ->
            to_side_line(X, Y, Index, Sector);

        {X, Y, N, I, side, Sector} ->
            to_side(X, Y, N, I, Sector);

        {X, Y, line, Index, cell, Sector} ->
            to_cell_line(X, Y, Index, Sector, With);

        {X, Y, N, I, cell, Sector} ->
            to_cell(X, Y, N, I, Sector, With);

        Location ->
            {error, Location}
    end.

%%--------------------------------------------------------------------

to_strip(Side, Index, R, C, #with{density = epm240}) ->
    to_epm240_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm570}) ->
    to_epm570_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm1270}) ->
    to_epm1270_strip(Side, Index, R, C);
to_strip(Side, Index, R, C, #with{density = epm2210}) ->
    to_epm2210_strip(Side, Index, R, C).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm240_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM240_STRIPS()
to_epm240_strip(Side, Index, R, C) ->
    {error, {Side, Index, strip, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm570_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM570_STRIPS()
to_epm570_strip(Side, Index, R, C) ->
    {error, {Side, Index, strip, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm1270_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM1270_STRIPS()
to_epm1270_strip(Side, Index, R, C) ->
    {error, {Side, Index, strip, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(STRIP(X, Y, N, Side, Index),
    to_epm2210_strip(Side, Index, R, C) ->
        to_ioc_strip(X, Y, N, R, C)
).

?EPM2210_STRIPS()
to_epm2210_strip(Side, Index, R, C) ->
    {error, {Side, Index, strip, R, C}}.

-undef(STRIP).

%%--------------------------------------------------------------------

-define(IOC_STRIP(R, C, Name),
    to_ioc_strip(X, Y, N, R, C) ->
        to_ioc(X, Y, N, Name)
).

?IOC_STRIPS()
to_ioc_strip(X, Y, N, R, C) ->
    {error, {X, Y, N, strip, R, C}}.

-undef(IOC_STRIP).

%%--------------------------------------------------------------------

-define(IOB_HEAD(Sector, Index, Name),
    to_cell_head(X, Index, Sector, With) ->
        to_iob(X, With#with.top_y, Name)
).
-define(IOC_HEAD(Sector, Index, N, Name),
    to_cell_head(X, Index, Sector, With) ->
        to_ioc(X, With#with.top_y, N, Name)
).

?IOB_HEADS()
?IOC_HEADS()
to_cell_head(X, Index, Sector, _With) ->
    {error, {X, head, Index, cell, Sector}}.

-undef(IOB_HEAD).
-undef(IOC_HEAD).

%%--------------------------------------------------------------------

-define(IOB_TAIL(Sector, Index, Name),
    to_cell_tail(X, Index, Sector, With) when X < With#with.grow_x ->
        to_iob(X, 3, Name);
    to_cell_tail(X, Index, Sector, With) when X > With#with.grow_x ->
        to_iob(X, 0, Name)
).
-define(IOC_TAIL(Sector, Index, N, Name),
    to_cell_tail(X, Index, Sector, With) when X < With#with.grow_x ->
        to_ioc(X, 3, N, Name);
    to_cell_tail(X, Index, Sector, With) when X > With#with.grow_x ->
        to_ioc(X, 0, N, Name)
).

?IOB_TAILS()
?IOC_TAILS()
to_cell_tail(X, Index, Sector, _With) ->
    {error, {X, tail, Index, cell, Sector}}.

-undef(IOB_TAIL).
-undef(IOC_TAIL).

%%--------------------------------------------------------------------

-define(IOC_ZERO(I, Name),
    to_zero(X, Y, N, I) ->
        to_ioc(X, Y, N, Name)
).

?IOC_ZEROS()
to_zero(X, Y, N, I) ->
    {error, {X, Y, N, zero, I}}.

-undef(IOC_ZERO).

%%--------------------------------------------------------------------

-define(IOC_LEFT_LINE(Sector, Index, N, Name),
    to_side_line(X, Y, Index, Sector) when X =< 1 ->
        to_ioc(X, Y, N, Name)
).
-define(IOC_RIGHT_LINE(Sector, Index, N, Name),
    to_side_line(X, Y, Index, Sector) when X > 1 ->
        to_ioc(X, Y, N, Name)
).

?IOC_LEFT_LINES()
?IOC_RIGHT_LINES()
to_side_line(X, Y, Index, Sector) ->
    {error, {X, Y, line, Index, side, Sector}}.

-undef(IOC_LEFT_LINE).
-undef(IOC_RIGHT_LINE).

%%--------------------------------------------------------------------

-define(IOB_SIDE(Sector, N, Index, Name),
    to_side(X, Y, N, Index, Sector) ->
        to_iob(X, Y, Name)
).
-define(IOC_LEFT(Sector, U, V, N, Name),
    to_side(X, Y, U, V, Sector) when X =< 1 ->
        to_ioc(X, Y, N, Name)
).
-define(IOC_RIGHT(Sector, U, V, N, Name),
    to_side(X, Y, U, V, Sector) when X > 1 ->
        to_ioc(X, Y, N, Name)
).
-define(IOC_SIDE(Sector, Index, Name),
    to_side(X, Y, N, Index, Sector) when N >= 2 andalso N =< 8 ->
        to_ioc(X, Y, N - 2, Name);
    to_side(X, Y, N, Index, Sector) ->
        {error, {X, Y, N, Index, side, Sector}}
).

?IOB_SIDES()
?IOC_LEFTS()
?IOC_RIGHTS()
?IOC_SIDES()
to_side(X, Y, N, Index, Sector) ->
    {error, {X, Y, N, Index, side, Sector}}.

-undef(IOB_SIDE).
-undef(IOC_LEFT).
-undef(IOC_RIGHT).
-undef(IOC_SIDE).

%%--------------------------------------------------------------------

-define(LAB_LINE(Sector, Index, Name),
    to_cell_line(X, Y, Index, Sector, _) ->
        to_lab(X, Y, Name)
).

to_cell_line(X, Y, Index, Sector, With = #with{})
        when X =< With#with.grow_x andalso Y =< 3 ->
    {error, {X, Y, line, Index, cell, Sector}};
?LAB_LINES()
to_cell_line(X, Y, Index, Sector, _With) ->
    {error, {X, Y, line, Index, cell, Sector}}.

-undef(LAB_LINE).

%%--------------------------------------------------------------------

-define(LAB_CELL(Sector, N, I, Name),
    to_cell(X, Y, N, I, Sector, _) ->
        to_lab(X, Y, Name)
).
-define(LC_CELL(Sector, I, Name),
    to_cell(X, Y, N, I, Sector, _) ->
        to_lc(X, Y, N, Name)
).

to_cell(X, Y, N, I, Sector, With = #with{})
        when X =< With#with.grow_x andalso Y =< 3 ->
    {error, {X, Y, N, I, cell, Sector}};
?LAB_CELLS()
?LC_CELLS()
to_cell(X, Y, N, I, Sector, _With) ->
    {error, {X, Y, N, I, cell, Sector}}.

-undef(LAB_CELL).
-undef(LC_CELL).

%%--------------------------------------------------------------------

to_iob(X, Y, {Name, Key, Value}) ->
    {ok, {{iob, X, Y}, Name, Key, Value}};
to_iob(X, Y, {Name, Value}) ->
    {ok, {{iob, X, Y}, Name, Value}};
to_iob(X, Y, Name) ->
    {ok, {{iob, X, Y}, Name}}.

%%--------------------------------------------------------------------

to_ioc(X, Y, N, {Name, Value}) ->
    {ok, {{ioc, X, Y, N}, Name, Value}};
to_ioc(X, Y, N, Name) ->
    {ok, {{ioc, X, Y, N}, Name}}.

%%--------------------------------------------------------------------

to_lab(X, Y, {Name, Key, Value}) ->
    {ok, {{lab, X, Y}, Name, Key, Value}};
to_lab(X, Y, {Name, Value}) ->
    {ok, {{lab, X, Y}, Name, Value}};
to_lab(X, Y, Name) ->
    {ok, {{lab, X, Y}, Name}}.

%%--------------------------------------------------------------------

to_lc(X, Y, N, {Name, Value}) ->
    {ok, {{lc, X, Y, N}, Name, Value}};
to_lc(X, Y, N, Name) ->
    {ok, {{lc, X, Y, N}, Name}}.

%%====================================================================
%% density
%%====================================================================

epm240() ->
    #with{
        density = epm240,
        strip_width = 32,
        offset_x = 1,
        side_x = 7, % after offset subtracted
        % EPM240 does not have short and long columns,
        % Treat all columns as long
        grow_x = 0,
        short_y = 4,
        long_y = 4,
        top_y = 5,
        short_sector = 256,
        long_sector = 256,
        left_base = 32,
        short_base = 3360,
        grow_base = 3360,
        long_base = 3360,
        right_base = 46368,
        end_base = 49696,
        sector_skip = 103,
        left_strip = 1081,
        top_strip = 1177,
        right_strip = 1315,
        bottom_strip = 1429,
        end_strip = 1561
    }.

%%--------------------------------------------------------------------

epm570() ->
    #with{
        density = epm570,
        strip_width = 32,
        offset_x = 0,
        side_x = 13,
        grow_x = 9,
        short_y = 4,
        long_y = 7,
        top_y = 8,
        short_sector = 256,
        long_sector = 384,
        left_base = 32,
        short_base = 3360,
        grow_base = 60704,
        long_base = 68896,
        right_base = 101152,
        end_base = 106144,
        sector_skip = 149,
        left_strip = 2369,
        top_strip = 2561,
        right_strip = 2837,
        bottom_strip = 3089,
        end_strip = 3329
    }.

%%--------------------------------------------------------------------

epm1270() ->
    #with{
        density = epm1270,
        strip_width = 64,
        offset_x = 0,
        side_x = 17,
        grow_x = 11,
        short_y = 7,
        long_y = 10,
        top_y = 11,
        short_sector = 384,
        long_sector = 512,
        left_base = 64,
        short_base = 5056,
        grow_base = 112576,
        long_base = 124352,
        right_base = 196032,
        end_base = 202688,
        sector_skip = 241,
        left_strip = 1848,
        top_strip = 2154,
        right_strip = 2472,
        bottom_strip = 2857,
        end_strip = 3175
    }.

%%--------------------------------------------------------------------

epm2210() ->
    #with{
        density = epm2210,
        strip_width = 64,
        offset_x = 0,
        side_x = 21,
        grow_x = 13,
        short_y = 10,
        long_y = 13,
        top_y = 14,
        short_sector = 512,
        long_sector = 704,
        left_base = 64,
        short_base = 6720,
        grow_base = 178752,
        long_base = 194624,
        right_base = 332608,
        end_base = 341760,
        sector_skip = 287,
        left_strip = 3647,
        top_strip = 4055,
        right_strip = 4451,
        bottom_strip = 4955,
        end_strip = 5351
    }.

