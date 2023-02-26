-module(iob).

-export_type([iob/0]).
-export_type([col_interconnect/0]).
-export_type([row_interconnect/0]).

-type iob() :: {iob, non_neg_integer(), non_neg_integer()}.

% interconnect for IOBs along the top and bottom (of columns) with up to 4 IOCs.
-type col_interconnect() :: {interconnect, 0..9} | fast_out.

% interconnect for IOBs along the left and right (of rows) with up to 7 IOCs.
-type row_interconnect() :: {interconnect, 0..17}.

