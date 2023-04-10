-module(epm570_f100).

-export([iocs/0]).
-export([pins/0]).
-export([top_iocs/1]).
-export([top_pins/1]).
-export([left_iocs/1]).
-export([left_pins/1]).
-export([right_iocs/1]).
-export([right_pins/1]).
-export([bottom_iocs/1]).
-export([bottom_pins/1]).

-type ioc() :: ioc:ioc().
-type pin() :: pin:pin().
-type x() :: max_ii:x().
-type y() :: max_ii:y().

-spec iocs() -> [{pin(), ioc()}].

iocs() ->
    [{a1,{ioc,3,8,0}},
     {a2,{ioc,5,8,2}},
     {a3,{ioc,5,8,1}},
     {a4,{ioc,6,8,2}},
     {a5,{ioc,7,8,0}},
     {a6,{ioc,8,8,2}},
     {a7,{ioc,8,8,1}},
     {a8,{ioc,10,8,3}},
     {a9,{ioc,12,8,2}},
     {a10,{ioc,12,8,1}},
     {b1,{ioc,1,8,2}},
     {b2,{ioc,3,8,3}},
     {b3,{ioc,4,8,1}},
     {b4,{ioc,6,8,3}},
     {b5,{ioc,7,8,1}},
     {b6,{ioc,8,8,3}},
     {b7,{ioc,8,8,0}},
     {b8,{ioc,12,8,3}},
     {b9,{ioc,13,7,1}},
     {b10,{ioc,13,6,1}},
     {c1,{ioc,0,7,0}},
     {c2,{ioc,1,8,0}},
     {c3,{ioc,4,8,2}},
     {c4,{ioc,5,8,0}},
     {c7,{ioc,9,8,0}},
     {c8,{ioc,13,7,3}},
     {c9,{ioc,13,7,5}},
     {c10,{ioc,13,5,0}},
     {d1,{ioc,0,7,4}},
     {d2,{ioc,0,7,3}},
     {d3,{ioc,0,7,2}},
     {d8,{ioc,13,6,2}},
     {d9,{ioc,13,6,5}},
     {d10,{ioc,13,4,0}},
     {e1,{ioc,0,5,1}},
     {e2,{ioc,0,5,0}},
     {e3,{ioc,0,7,5}},
     {e8,{ioc,13,4,1}},
     {e9,{ioc,13,4,2}},
     {e10,{ioc,13,4,3}},
     {f1,{ioc,0,5,4}},
     {f2,{ioc,0,5,2}},
     {f3,{ioc,0,5,3}},
     {f8,{ioc,13,4,4}},
     {f9,{ioc,13,3,1}},
     {f10,{ioc,13,2,0}},
     {g1,{ioc,0,5,5}},
     {g2,{ioc,1,3,3}},
     {g3,{ioc,1,3,1}},
     {g8,{ioc,13,2,4}},
     {g9,{ioc,13,2,2}},
     {g10,{ioc,13,2,1}},
     {h1,{ioc,0,4,5}},
     {h4,{ioc,4,3,1}},
     {h7,{ioc,10,0,2}},
     {h8,{ioc,10,0,0}},
     {h9,{ioc,13,1,4}},
     {h10,{ioc,13,1,1}},
     {j3,{ioc,3,3,1}},
     {j4,{ioc,6,3,3}},
     {j5,{ioc,6,3,1}},
     {j6,{ioc,7,3,2}},
     {j7,{ioc,8,3,3}},
     {j8,{ioc,10,0,1}},
     {j9,{ioc,12,0,2}},
     {j10,{ioc,13,1,3}},
     {k1,{ioc,3,3,2}},
     {k2,{ioc,4,3,3}},
     {k3,{ioc,4,3,2}},
     {k4,{ioc,6,3,2}},
     {k5,{ioc,6,3,0}},
     {k6,{ioc,7,3,3}},
     {k7,{ioc,7,3,1}},
     {k8,{ioc,7,3,0}},
     {k9,{ioc,8,3,2}},
     {k10,{ioc,11,0,2}}
    ].

-spec pins() -> [pin()].

pins() ->
    [a1,
     a2,
     a3,
     a4,
     a5,
     a6,
     a7,
     a8,
     a9,
     a10,
     b1,
     b2,
     b3,
     b4,
     b5,
     b6,
     b7,
     b8,
     b9,
     b10,
     c1,
     c2,
     c3,
     c4,
     c7,
     c8,
     c9,
     c10,
     d1,
     d2,
     d3,
     d8,
     d9,
     d10,
     e1,
     e2,
     e3,
     e8,
     e9,
     e10,
     f1,
     f2,
     f3,
     f8,
     f9,
     f10,
     g1,
     g2,
     g3,
     g8,
     g9,
     g10,
     h1,
     h4,
     h7,
     h8,
     h9,
     h10,
     j3,
     j4,
     j5,
     j6,
     j7,
     j8,
     j9,
     j10,
     k1,
     k2,
     k3,
     k4,
     k5,
     k6,
     k7,
     k8,
     k9,
     k10
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(1) ->
    [{c2,{ioc,1,8,0}},
     {b1,{ioc,1,8,2}}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{a1,{ioc,3,8,0}},
     {b2,{ioc,3,8,3}}];
top_iocs(4) ->
    [{b3,{ioc,4,8,1}},
     {c3,{ioc,4,8,2}}];
top_iocs(5) ->
    [{c4,{ioc,5,8,0}},
     {a3,{ioc,5,8,1}},
     {a2,{ioc,5,8,2}}];
top_iocs(6) ->
    [{a4,{ioc,6,8,2}},
     {b4,{ioc,6,8,3}}];
top_iocs(7) ->
    [{a5,{ioc,7,8,0}},
     {b5,{ioc,7,8,1}}];
top_iocs(8) ->
    [{b7,{ioc,8,8,0}},
     {a7,{ioc,8,8,1}},
     {a6,{ioc,8,8,2}},
     {b6,{ioc,8,8,3}}];
top_iocs(9) ->
    [{c7,{ioc,9,8,0}}];
top_iocs(10) ->
    [{a8,{ioc,10,8,3}}];
top_iocs(11) ->
    [];
top_iocs(12) ->
    [{a10,{ioc,12,8,1}},
     {a9,{ioc,12,8,2}},
     {b8,{ioc,12,8,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [b1, c2];
top_pins(2) ->
    [];
top_pins(3) ->
    [a1, b2];
top_pins(4) ->
    [b3, c3];
top_pins(5) ->
    [a2, a3, c4];
top_pins(6) ->
    [a4, b4];
top_pins(7) ->
    [a5, b5];
top_pins(8) ->
    [a6, a7, b6, b7];
top_pins(9) ->
    [c7];
top_pins(10) ->
    [a8];
top_pins(11) ->
    [];
top_pins(12) ->
    [a9, a10, b8].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{h1,{ioc,0,4,5}}];
left_iocs(5) ->
    [{e2,{ioc,0,5,0}},
     {e1,{ioc,0,5,1}},
     {f2,{ioc,0,5,2}},
     {f3,{ioc,0,5,3}},
     {f1,{ioc,0,5,4}},
     {g1,{ioc,0,5,5}}];
left_iocs(6) ->
    [];
left_iocs(7) ->
    [{c1,{ioc,0,7,0}},
     {d3,{ioc,0,7,2}},
     {d2,{ioc,0,7,3}},
     {d1,{ioc,0,7,4}},
     {e3,{ioc,0,7,5}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [h1];
left_pins(5) ->
    [e1, e2, f1, f2, f3, g1];
left_pins(6) ->
    [];
left_pins(7) ->
    [c1, d1, d2, d3, e3].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{h10,{ioc,13,1,1}},
     {j10,{ioc,13,1,3}},
     {h9,{ioc,13,1,4}}];
right_iocs(2) ->
    [{f10,{ioc,13,2,0}},
     {g10,{ioc,13,2,1}},
     {g9,{ioc,13,2,2}},
     {g8,{ioc,13,2,4}}];
right_iocs(3) ->
    [{f9,{ioc,13,3,1}}];
right_iocs(4) ->
    [{d10,{ioc,13,4,0}},
     {e8,{ioc,13,4,1}},
     {e9,{ioc,13,4,2}},
     {e10,{ioc,13,4,3}},
     {f8,{ioc,13,4,4}}];
right_iocs(5) ->
    [{c10,{ioc,13,5,0}}];
right_iocs(6) ->
    [{b10,{ioc,13,6,1}},
     {d8,{ioc,13,6,2}},
     {d9,{ioc,13,6,5}}];
right_iocs(7) ->
    [{b9,{ioc,13,7,1}},
     {c8,{ioc,13,7,3}},
     {c9,{ioc,13,7,5}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [h9, h10, j10];
right_pins(2) ->
    [f10, g8, g9, g10];
right_pins(3) ->
    [f9];
right_pins(4) ->
    [d10, e8, e9, e10, f8];
right_pins(5) ->
    [c10];
right_pins(6) ->
    [b10, d8, d9];
right_pins(7) ->
    [b9, c8, c9].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{g3,{ioc,1,3,1}},
     {g2,{ioc,1,3,3}}];
bottom_iocs(2) ->
    [];
bottom_iocs(3) ->
    [{j3,{ioc,3,3,1}},
     {k1,{ioc,3,3,2}}];
bottom_iocs(4) ->
    [{h4,{ioc,4,3,1}},
     {k3,{ioc,4,3,2}},
     {k2,{ioc,4,3,3}}];
bottom_iocs(5) ->
    [];
bottom_iocs(6) ->
    [{k5,{ioc,6,3,0}},
     {j5,{ioc,6,3,1}},
     {k4,{ioc,6,3,2}},
     {j4,{ioc,6,3,3}}];
bottom_iocs(7) ->
    [{k8,{ioc,7,3,0}},
     {k7,{ioc,7,3,1}},
     {j6,{ioc,7,3,2}},
     {k6,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{k9,{ioc,8,3,2}},
     {j7,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{h8,{ioc,10,0,0}},
     {j8,{ioc,10,0,1}},
     {h7,{ioc,10,0,2}}];
bottom_iocs(11) ->
    [{k10,{ioc,11,0,2}}];
bottom_iocs(12) ->
    [{j9,{ioc,12,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [g2, g3];
bottom_pins(2) ->
    [];
bottom_pins(3) ->
    [j3, k1];
bottom_pins(4) ->
    [h4, k2, k3];
bottom_pins(5) ->
    [];
bottom_pins(6) ->
    [j4, j5, k4, k5];
bottom_pins(7) ->
    [j6, k6, k7, k8];
bottom_pins(8) ->
    [j7, k9];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [h7, h8, j8];
bottom_pins(11) ->
    [k10];
bottom_pins(12) ->
    [j9].

