-module(epm570_m100).

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
    [{a1,{ioc,3,8,3}},
     {a2,{ioc,4,8,2}},
     {a3,{ioc,5,8,2}},
     {a4,{ioc,5,8,0}},
     {a5,{ioc,6,8,2}},
     {a6,{ioc,7,8,1}},
     {a7,{ioc,8,8,2}},
     {a8,{ioc,8,8,0}},
     {a9,{ioc,10,8,3}},
     {a10,{ioc,12,8,2}},
     {a11,{ioc,12,8,1}},
     {b1,{ioc,1,8,0}},
     {b2,{ioc,3,8,0}},
     {b3,{ioc,4,8,1}},
     {b4,{ioc,5,8,1}},
     {b5,{ioc,6,8,3}},
     {b6,{ioc,7,8,0}},
     {b7,{ioc,8,8,1}},
     {b8,{ioc,9,8,0}},
     {b9,{ioc,12,8,3}},
     {b10,{ioc,13,7,1}},
     {b11,{ioc,13,7,3}},
     {c1,{ioc,0,7,0}},
     {c2,{ioc,1,8,2}},
     {c6,{ioc,8,8,3}},
     {c10,{ioc,13,7,5}},
     {c11,{ioc,13,6,1}},
     {d1,{ioc,0,7,4}},
     {d2,{ioc,0,7,3}},
     {d3,{ioc,0,7,2}},
     {d9,{ioc,13,5,0}},
     {d10,{ioc,13,6,2}},
     {d11,{ioc,13,6,5}},
     {e1,{ioc,0,5,1}},
     {e2,{ioc,0,7,5}},
     {e10,{ioc,13,4,0}},
     {e11,{ioc,13,4,1}},
     {f1,{ioc,0,5,2}},
     {f2,{ioc,0,5,0}},
     {f3,{ioc,0,5,5}},
     {f9,{ioc,13,3,1}},
     {f10,{ioc,13,4,4}},
     {f11,{ioc,13,4,2}},
     {g1,{ioc,0,5,3}},
     {g2,{ioc,0,5,4}},
     {g10,{ioc,13,2,0}},
     {g11,{ioc,13,4,3}},
     {h1,{ioc,0,4,5}},
     {h2,{ioc,1,3,1}},
     {h3,{ioc,1,3,3}},
     {h9,{ioc,13,2,4}},
     {h10,{ioc,13,2,2}},
     {h11,{ioc,13,2,1}},
     {j6,{ioc,7,3,2}},
     {j10,{ioc,13,1,3}},
     {j11,{ioc,13,1,1}},
     {k3,{ioc,4,3,3}},
     {k4,{ioc,4,3,1}},
     {k5,{ioc,6,3,2}},
     {k6,{ioc,7,3,3}},
     {k7,{ioc,7,3,0}},
     {k8,{ioc,8,3,2}},
     {k9,{ioc,10,0,1}},
     {k10,{ioc,11,0,2}},
     {k11,{ioc,13,1,4}},
     {l1,{ioc,3,3,2}},
     {l2,{ioc,3,3,1}},
     {l3,{ioc,4,3,2}},
     {l4,{ioc,6,3,3}},
     {l5,{ioc,6,3,1}},
     {l6,{ioc,6,3,0}},
     {l7,{ioc,7,3,1}},
     {l8,{ioc,8,3,3}},
     {l9,{ioc,10,0,2}},
     {l10,{ioc,10,0,0}},
     {l11,{ioc,12,0,2}}
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
     a11,
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
     b11,
     c1,
     c2,
     c6,
     c10,
     c11,
     d1,
     d2,
     d3,
     d9,
     d10,
     d11,
     e1,
     e2,
     e10,
     e11,
     f1,
     f2,
     f3,
     f9,
     f10,
     f11,
     g1,
     g2,
     g10,
     g11,
     h1,
     h2,
     h3,
     h9,
     h10,
     h11,
     j6,
     j10,
     j11,
     k3,
     k4,
     k5,
     k6,
     k7,
     k8,
     k9,
     k10,
     k11,
     l1,
     l2,
     l3,
     l4,
     l5,
     l6,
     l7,
     l8,
     l9,
     l10,
     l11
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(1) ->
    [{b1,{ioc,1,8,0}},
     {c2,{ioc,1,8,2}}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{b2,{ioc,3,8,0}},
     {a1,{ioc,3,8,3}}];
top_iocs(4) ->
    [{b3,{ioc,4,8,1}},
     {a2,{ioc,4,8,2}}];
top_iocs(5) ->
    [{a4,{ioc,5,8,0}},
     {b4,{ioc,5,8,1}},
     {a3,{ioc,5,8,2}}];
top_iocs(6) ->
    [{a5,{ioc,6,8,2}},
     {b5,{ioc,6,8,3}}];
top_iocs(7) ->
    [{b6,{ioc,7,8,0}},
     {a6,{ioc,7,8,1}}];
top_iocs(8) ->
    [{a8,{ioc,8,8,0}},
     {b7,{ioc,8,8,1}},
     {a7,{ioc,8,8,2}},
     {c6,{ioc,8,8,3}}];
top_iocs(9) ->
    [{b8,{ioc,9,8,0}}];
top_iocs(10) ->
    [{a9,{ioc,10,8,3}}];
top_iocs(11) ->
    [];
top_iocs(12) ->
    [{a11,{ioc,12,8,1}},
     {a10,{ioc,12,8,2}},
     {b9,{ioc,12,8,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [b1, c2];
top_pins(2) ->
    [];
top_pins(3) ->
    [a1, b2];
top_pins(4) ->
    [a2, b3];
top_pins(5) ->
    [a3, a4, b4];
top_pins(6) ->
    [a5, b5];
top_pins(7) ->
    [a6, b6];
top_pins(8) ->
    [a7, a8, b7, c6];
top_pins(9) ->
    [b8];
top_pins(10) ->
    [a9];
top_pins(11) ->
    [];
top_pins(12) ->
    [a10, a11, b9].

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
    [{f2,{ioc,0,5,0}},
     {e1,{ioc,0,5,1}},
     {f1,{ioc,0,5,2}},
     {g1,{ioc,0,5,3}},
     {g2,{ioc,0,5,4}},
     {f3,{ioc,0,5,5}}];
left_iocs(6) ->
    [];
left_iocs(7) ->
    [{c1,{ioc,0,7,0}},
     {d3,{ioc,0,7,2}},
     {d2,{ioc,0,7,3}},
     {d1,{ioc,0,7,4}},
     {e2,{ioc,0,7,5}}].

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
    [e1, f1, f2, f3, g1, g2];
left_pins(6) ->
    [];
left_pins(7) ->
    [c1, d1, d2, d3, e2].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{j11,{ioc,13,1,1}},
     {j10,{ioc,13,1,3}},
     {k11,{ioc,13,1,4}}];
right_iocs(2) ->
    [{g10,{ioc,13,2,0}},
     {h11,{ioc,13,2,1}},
     {h10,{ioc,13,2,2}},
     {h9,{ioc,13,2,4}}];
right_iocs(3) ->
    [{f9,{ioc,13,3,1}}];
right_iocs(4) ->
    [{e10,{ioc,13,4,0}},
     {e11,{ioc,13,4,1}},
     {f11,{ioc,13,4,2}},
     {g11,{ioc,13,4,3}},
     {f10,{ioc,13,4,4}}];
right_iocs(5) ->
    [{d9,{ioc,13,5,0}}];
right_iocs(6) ->
    [{c11,{ioc,13,6,1}},
     {d10,{ioc,13,6,2}},
     {d11,{ioc,13,6,5}}];
right_iocs(7) ->
    [{b10,{ioc,13,7,1}},
     {b11,{ioc,13,7,3}},
     {c10,{ioc,13,7,5}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [j10, j11, k11];
right_pins(2) ->
    [g10, h9, h10, h11];
right_pins(3) ->
    [f9];
right_pins(4) ->
    [e10, e11, f10, f11, g11];
right_pins(5) ->
    [d9];
right_pins(6) ->
    [c11, d10, d11];
right_pins(7) ->
    [b10, b11, c10].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{h2,{ioc,1,3,1}},
     {h3,{ioc,1,3,3}}];
bottom_iocs(2) ->
    [];
bottom_iocs(3) ->
    [{l2,{ioc,3,3,1}},
     {l1,{ioc,3,3,2}}];
bottom_iocs(4) ->
    [{k4,{ioc,4,3,1}},
     {l3,{ioc,4,3,2}},
     {k3,{ioc,4,3,3}}];
bottom_iocs(5) ->
    [];
bottom_iocs(6) ->
    [{l6,{ioc,6,3,0}},
     {l5,{ioc,6,3,1}},
     {k5,{ioc,6,3,2}},
     {l4,{ioc,6,3,3}}];
bottom_iocs(7) ->
    [{k7,{ioc,7,3,0}},
     {l7,{ioc,7,3,1}},
     {j6,{ioc,7,3,2}},
     {k6,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{k8,{ioc,8,3,2}},
     {l8,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{l10,{ioc,10,0,0}},
     {k9,{ioc,10,0,1}},
     {l9,{ioc,10,0,2}}];
bottom_iocs(11) ->
    [{k10,{ioc,11,0,2}}];
bottom_iocs(12) ->
    [{l11,{ioc,12,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [h2, h3];
bottom_pins(2) ->
    [];
bottom_pins(3) ->
    [l1, l2];
bottom_pins(4) ->
    [k3, k4, l3];
bottom_pins(5) ->
    [];
bottom_pins(6) ->
    [k5, l4, l5, l6];
bottom_pins(7) ->
    [j6, k6, k7, l7];
bottom_pins(8) ->
    [k8, l8];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [k9, l9, l10];
bottom_pins(11) ->
    [k10];
bottom_pins(12) ->
    [l11].

