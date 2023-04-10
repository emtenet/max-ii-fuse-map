-module(epm240_f100).

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
    [{a1,{ioc,2,5,2}},
     {a2,{ioc,3,5,3}},
     {a3,{ioc,3,5,2}},
     {a4,{ioc,4,5,2}},
     {a5,{ioc,5,5,2}},
     {a6,{ioc,5,5,0}},
     {a7,{ioc,6,5,3}},
     {a8,{ioc,6,5,0}},
     {a9,{ioc,7,5,2}},
     {a10,{ioc,7,5,1}},
     {b1,{ioc,1,4,1}},
     {b2,{ioc,2,5,3}},
     {b3,{ioc,2,5,0}},
     {b4,{ioc,3,5,0}},
     {b5,{ioc,4,5,0}},
     {b6,{ioc,5,5,1}},
     {b7,{ioc,6,5,2}},
     {b8,{ioc,7,5,3}},
     {b9,{ioc,7,5,0}},
     {b10,{ioc,8,4,2}},
     {c1,{ioc,1,4,2}},
     {c2,{ioc,1,4,0}},
     {c3,{ioc,2,5,1}},
     {c4,{ioc,3,5,1}},
     {c5,{ioc,4,5,1}},
     {c6,{ioc,5,5,3}},
     {c7,{ioc,6,5,1}},
     {c8,{ioc,8,4,0}},
     {c9,{ioc,8,4,1}},
     {c10,{ioc,8,3,0}},
     {d1,{ioc,1,3,1}},
     {d2,{ioc,1,3,0}},
     {d3,{ioc,1,4,3}},
     {d8,{ioc,8,4,3}},
     {d9,{ioc,8,4,4}},
     {d10,{ioc,8,3,1}},
     {e1,{ioc,1,2,0}},
     {e2,{ioc,1,3,3}},
     {e3,{ioc,1,3,2}},
     {e8,{ioc,8,3,2}},
     {e9,{ioc,8,3,3}},
     {e10,{ioc,8,3,4}},
     {f1,{ioc,1,2,3}},
     {f2,{ioc,1,2,1}},
     {f3,{ioc,1,2,2}},
     {f8,{ioc,8,2,0}},
     {f9,{ioc,8,2,1}},
     {f10,{ioc,8,2,2}},
     {g1,{ioc,1,1,0}},
     {g2,{ioc,1,1,2}},
     {g3,{ioc,1,1,3}},
     {g8,{ioc,8,1,1}},
     {g9,{ioc,8,1,0}},
     {g10,{ioc,8,2,3}},
     {h1,{ioc,1,1,1}},
     {h4,{ioc,3,0,3}},
     {h5,{ioc,4,0,1}},
     {h6,{ioc,5,0,3}},
     {h7,{ioc,6,0,1}},
     {h8,{ioc,7,0,2}},
     {h9,{ioc,8,1,4}},
     {h10,{ioc,8,1,2}},
     {j3,{ioc,2,0,2}},
     {j4,{ioc,3,0,2}},
     {j5,{ioc,3,0,0}},
     {j6,{ioc,5,0,2}},
     {j7,{ioc,6,0,3}},
     {j8,{ioc,6,0,0}},
     {j9,{ioc,7,0,0}},
     {j10,{ioc,8,1,3}},
     {k1,{ioc,2,0,3}},
     {k2,{ioc,2,0,1}},
     {k3,{ioc,2,0,0}},
     {k4,{ioc,3,0,1}},
     {k5,{ioc,4,0,2}},
     {k6,{ioc,4,0,0}},
     {k7,{ioc,5,0,1}},
     {k8,{ioc,5,0,0}},
     {k9,{ioc,6,0,2}},
     {k10,{ioc,7,0,1}}
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
     c5,
     c6,
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
     h5,
     h6,
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

top_iocs(2) ->
    [{b3,{ioc,2,5,0}},
     {c3,{ioc,2,5,1}},
     {a1,{ioc,2,5,2}},
     {b2,{ioc,2,5,3}}];
top_iocs(3) ->
    [{b4,{ioc,3,5,0}},
     {c4,{ioc,3,5,1}},
     {a3,{ioc,3,5,2}},
     {a2,{ioc,3,5,3}}];
top_iocs(4) ->
    [{b5,{ioc,4,5,0}},
     {c5,{ioc,4,5,1}},
     {a4,{ioc,4,5,2}}];
top_iocs(5) ->
    [{a6,{ioc,5,5,0}},
     {b6,{ioc,5,5,1}},
     {a5,{ioc,5,5,2}},
     {c6,{ioc,5,5,3}}];
top_iocs(6) ->
    [{a8,{ioc,6,5,0}},
     {c7,{ioc,6,5,1}},
     {b7,{ioc,6,5,2}},
     {a7,{ioc,6,5,3}}];
top_iocs(7) ->
    [{b9,{ioc,7,5,0}},
     {a10,{ioc,7,5,1}},
     {a9,{ioc,7,5,2}},
     {b8,{ioc,7,5,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a1, b2, b3, c3];
top_pins(3) ->
    [a2, a3, b4, c4];
top_pins(4) ->
    [a4, b5, c5];
top_pins(5) ->
    [a5, a6, b6, c6];
top_pins(6) ->
    [a7, a8, b7, c7];
top_pins(7) ->
    [a9, a10, b8, b9].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{g1,{ioc,1,1,0}},
     {h1,{ioc,1,1,1}},
     {g2,{ioc,1,1,2}},
     {g3,{ioc,1,1,3}}];
left_iocs(2) ->
    [{e1,{ioc,1,2,0}},
     {f2,{ioc,1,2,1}},
     {f3,{ioc,1,2,2}},
     {f1,{ioc,1,2,3}}];
left_iocs(3) ->
    [{d2,{ioc,1,3,0}},
     {d1,{ioc,1,3,1}},
     {e3,{ioc,1,3,2}},
     {e2,{ioc,1,3,3}}];
left_iocs(4) ->
    [{c2,{ioc,1,4,0}},
     {b1,{ioc,1,4,1}},
     {c1,{ioc,1,4,2}},
     {d3,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [g1, g2, g3, h1];
left_pins(2) ->
    [e1, f1, f2, f3];
left_pins(3) ->
    [d1, d2, e2, e3];
left_pins(4) ->
    [b1, c1, c2, d3].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{g9,{ioc,8,1,0}},
     {g8,{ioc,8,1,1}},
     {h10,{ioc,8,1,2}},
     {j10,{ioc,8,1,3}},
     {h9,{ioc,8,1,4}}];
right_iocs(2) ->
    [{f8,{ioc,8,2,0}},
     {f9,{ioc,8,2,1}},
     {f10,{ioc,8,2,2}},
     {g10,{ioc,8,2,3}}];
right_iocs(3) ->
    [{c10,{ioc,8,3,0}},
     {d10,{ioc,8,3,1}},
     {e8,{ioc,8,3,2}},
     {e9,{ioc,8,3,3}},
     {e10,{ioc,8,3,4}}];
right_iocs(4) ->
    [{c8,{ioc,8,4,0}},
     {c9,{ioc,8,4,1}},
     {b10,{ioc,8,4,2}},
     {d8,{ioc,8,4,3}},
     {d9,{ioc,8,4,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [g8, g9, h9, h10, j10];
right_pins(2) ->
    [f8, f9, f10, g10];
right_pins(3) ->
    [c10, d10, e8, e9, e10];
right_pins(4) ->
    [b10, c8, c9, d8, d9].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{k3,{ioc,2,0,0}},
     {k2,{ioc,2,0,1}},
     {j3,{ioc,2,0,2}},
     {k1,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{j5,{ioc,3,0,0}},
     {k4,{ioc,3,0,1}},
     {j4,{ioc,3,0,2}},
     {h4,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [{k6,{ioc,4,0,0}},
     {h5,{ioc,4,0,1}},
     {k5,{ioc,4,0,2}}];
bottom_iocs(5) ->
    [{k8,{ioc,5,0,0}},
     {k7,{ioc,5,0,1}},
     {j6,{ioc,5,0,2}},
     {h6,{ioc,5,0,3}}];
bottom_iocs(6) ->
    [{j8,{ioc,6,0,0}},
     {h7,{ioc,6,0,1}},
     {k9,{ioc,6,0,2}},
     {j7,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{j9,{ioc,7,0,0}},
     {k10,{ioc,7,0,1}},
     {h8,{ioc,7,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [j3, k1, k2, k3];
bottom_pins(3) ->
    [h4, j4, j5, k4];
bottom_pins(4) ->
    [h5, k5, k6];
bottom_pins(5) ->
    [h6, j6, k7, k8];
bottom_pins(6) ->
    [h7, j7, j8, k9];
bottom_pins(7) ->
    [h8, j9, k10].

