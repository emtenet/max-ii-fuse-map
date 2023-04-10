-module(epm240_m100).

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
    [{a1,{ioc,2,5,3}},
     {a2,{ioc,2,5,1}},
     {a3,{ioc,3,5,3}},
     {a4,{ioc,3,5,1}},
     {a5,{ioc,4,5,2}},
     {a6,{ioc,4,5,0}},
     {a7,{ioc,5,5,0}},
     {a8,{ioc,6,5,2}},
     {a9,{ioc,6,5,0}},
     {a10,{ioc,7,5,2}},
     {a11,{ioc,7,5,1}},
     {b1,{ioc,1,4,0}},
     {b2,{ioc,2,5,2}},
     {b3,{ioc,2,5,0}},
     {b4,{ioc,3,5,2}},
     {b5,{ioc,3,5,0}},
     {b6,{ioc,5,5,2}},
     {b7,{ioc,6,5,3}},
     {b8,{ioc,6,5,1}},
     {b9,{ioc,7,5,3}},
     {b10,{ioc,7,5,0}},
     {b11,{ioc,8,4,0}},
     {c1,{ioc,1,4,2}},
     {c2,{ioc,1,4,1}},
     {c5,{ioc,4,5,1}},
     {c6,{ioc,5,5,1}},
     {c7,{ioc,5,5,3}},
     {c10,{ioc,8,4,1}},
     {c11,{ioc,8,4,2}},
     {d1,{ioc,1,3,1}},
     {d2,{ioc,1,3,0}},
     {d3,{ioc,1,4,3}},
     {d9,{ioc,8,3,0}},
     {d10,{ioc,8,4,3}},
     {d11,{ioc,8,4,4}},
     {e1,{ioc,1,2,0}},
     {e2,{ioc,1,3,2}},
     {e10,{ioc,8,3,1}},
     {e11,{ioc,8,3,2}},
     {f1,{ioc,1,2,1}},
     {f2,{ioc,1,3,3}},
     {f3,{ioc,1,1,0}},
     {f9,{ioc,8,2,1}},
     {f10,{ioc,8,2,0}},
     {f11,{ioc,8,3,3}},
     {g1,{ioc,1,2,2}},
     {g2,{ioc,1,2,3}},
     {g10,{ioc,8,2,2}},
     {g11,{ioc,8,3,4}},
     {h1,{ioc,1,1,1}},
     {h2,{ioc,1,1,3}},
     {h3,{ioc,1,1,2}},
     {h9,{ioc,8,1,1}},
     {h10,{ioc,8,1,0}},
     {h11,{ioc,8,2,3}},
     {j5,{ioc,4,0,1}},
     {j6,{ioc,5,0,2}},
     {j7,{ioc,5,0,3}},
     {j10,{ioc,8,1,3}},
     {j11,{ioc,8,1,2}},
     {k3,{ioc,2,0,1}},
     {k4,{ioc,3,0,3}},
     {k5,{ioc,3,0,1}},
     {k6,{ioc,4,0,0}},
     {k7,{ioc,5,0,0}},
     {k8,{ioc,6,0,2}},
     {k9,{ioc,6,0,0}},
     {k10,{ioc,7,0,1}},
     {k11,{ioc,8,1,4}},
     {l1,{ioc,2,0,3}},
     {l2,{ioc,2,0,2}},
     {l3,{ioc,2,0,0}},
     {l4,{ioc,3,0,2}},
     {l5,{ioc,3,0,0}},
     {l6,{ioc,4,0,2}},
     {l7,{ioc,5,0,1}},
     {l8,{ioc,6,0,3}},
     {l9,{ioc,6,0,1}},
     {l10,{ioc,7,0,2}},
     {l11,{ioc,7,0,0}}
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
     c5,
     c6,
     c7,
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
     j5,
     j6,
     j7,
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

top_iocs(2) ->
    [{b3,{ioc,2,5,0}},
     {a2,{ioc,2,5,1}},
     {b2,{ioc,2,5,2}},
     {a1,{ioc,2,5,3}}];
top_iocs(3) ->
    [{b5,{ioc,3,5,0}},
     {a4,{ioc,3,5,1}},
     {b4,{ioc,3,5,2}},
     {a3,{ioc,3,5,3}}];
top_iocs(4) ->
    [{a6,{ioc,4,5,0}},
     {c5,{ioc,4,5,1}},
     {a5,{ioc,4,5,2}}];
top_iocs(5) ->
    [{a7,{ioc,5,5,0}},
     {c6,{ioc,5,5,1}},
     {b6,{ioc,5,5,2}},
     {c7,{ioc,5,5,3}}];
top_iocs(6) ->
    [{a9,{ioc,6,5,0}},
     {b8,{ioc,6,5,1}},
     {a8,{ioc,6,5,2}},
     {b7,{ioc,6,5,3}}];
top_iocs(7) ->
    [{b10,{ioc,7,5,0}},
     {a11,{ioc,7,5,1}},
     {a10,{ioc,7,5,2}},
     {b9,{ioc,7,5,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a1, a2, b2, b3];
top_pins(3) ->
    [a3, a4, b4, b5];
top_pins(4) ->
    [a5, a6, c5];
top_pins(5) ->
    [a7, b6, c6, c7];
top_pins(6) ->
    [a8, a9, b7, b8];
top_pins(7) ->
    [a10, a11, b9, b10].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{f3,{ioc,1,1,0}},
     {h1,{ioc,1,1,1}},
     {h3,{ioc,1,1,2}},
     {h2,{ioc,1,1,3}}];
left_iocs(2) ->
    [{e1,{ioc,1,2,0}},
     {f1,{ioc,1,2,1}},
     {g1,{ioc,1,2,2}},
     {g2,{ioc,1,2,3}}];
left_iocs(3) ->
    [{d2,{ioc,1,3,0}},
     {d1,{ioc,1,3,1}},
     {e2,{ioc,1,3,2}},
     {f2,{ioc,1,3,3}}];
left_iocs(4) ->
    [{b1,{ioc,1,4,0}},
     {c2,{ioc,1,4,1}},
     {c1,{ioc,1,4,2}},
     {d3,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f3, h1, h2, h3];
left_pins(2) ->
    [e1, f1, g1, g2];
left_pins(3) ->
    [d1, d2, e2, f2];
left_pins(4) ->
    [b1, c1, c2, d3].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{h10,{ioc,8,1,0}},
     {h9,{ioc,8,1,1}},
     {j11,{ioc,8,1,2}},
     {j10,{ioc,8,1,3}},
     {k11,{ioc,8,1,4}}];
right_iocs(2) ->
    [{f10,{ioc,8,2,0}},
     {f9,{ioc,8,2,1}},
     {g10,{ioc,8,2,2}},
     {h11,{ioc,8,2,3}}];
right_iocs(3) ->
    [{d9,{ioc,8,3,0}},
     {e10,{ioc,8,3,1}},
     {e11,{ioc,8,3,2}},
     {f11,{ioc,8,3,3}},
     {g11,{ioc,8,3,4}}];
right_iocs(4) ->
    [{b11,{ioc,8,4,0}},
     {c10,{ioc,8,4,1}},
     {c11,{ioc,8,4,2}},
     {d10,{ioc,8,4,3}},
     {d11,{ioc,8,4,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [h9, h10, j10, j11, k11];
right_pins(2) ->
    [f9, f10, g10, h11];
right_pins(3) ->
    [d9, e10, e11, f11, g11];
right_pins(4) ->
    [b11, c10, c11, d10, d11].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{l3,{ioc,2,0,0}},
     {k3,{ioc,2,0,1}},
     {l2,{ioc,2,0,2}},
     {l1,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{l5,{ioc,3,0,0}},
     {k5,{ioc,3,0,1}},
     {l4,{ioc,3,0,2}},
     {k4,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [{k6,{ioc,4,0,0}},
     {j5,{ioc,4,0,1}},
     {l6,{ioc,4,0,2}}];
bottom_iocs(5) ->
    [{k7,{ioc,5,0,0}},
     {l7,{ioc,5,0,1}},
     {j6,{ioc,5,0,2}},
     {j7,{ioc,5,0,3}}];
bottom_iocs(6) ->
    [{k9,{ioc,6,0,0}},
     {l9,{ioc,6,0,1}},
     {k8,{ioc,6,0,2}},
     {l8,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{l11,{ioc,7,0,0}},
     {k10,{ioc,7,0,1}},
     {l10,{ioc,7,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [k3, l1, l2, l3];
bottom_pins(3) ->
    [k4, k5, l4, l5];
bottom_pins(4) ->
    [j5, k6, l6];
bottom_pins(5) ->
    [j6, j7, k7, l7];
bottom_pins(6) ->
    [k8, k9, l8, l9];
bottom_pins(7) ->
    [k10, l10, l11].

