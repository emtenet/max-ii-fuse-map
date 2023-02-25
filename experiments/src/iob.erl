-module(iob).

-export_type([iob/0]).
-export_type([interconnect4/0]).
-export_type([interconnect7/0]).

-type iob() :: {iob, non_neg_integer(), non_neg_integer()}.

% interconnect for IOBs along the top and bottom with up to 4 IOCs.
-type interconnect4() :: {interconnect, 0..9} | bypass.

% interconnect for IOBs along the left and right with up to 7 IOCs.
-type interconnect7() :: {interconnect, 0..17}.

