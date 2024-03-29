-module(epm570_f256).

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
    [{a2,{ioc,3,8,3}},
     {a4,{ioc,3,8,1}},
     {a5,{ioc,4,8,3}},
     {a6,{ioc,5,8,2}},
     {a7,{ioc,6,8,2}},
     {a8,{ioc,7,8,2}},
     {a9,{ioc,7,8,1}},
     {a10,{ioc,8,8,2}},
     {a11,{ioc,9,8,1}},
     {a12,{ioc,10,8,1}},
     {a13,{ioc,11,8,1}},
     {a15,{ioc,12,8,3}},
     {b1,{ioc,2,8,1}},
     {b3,{ioc,2,8,0}},
     {b4,{ioc,4,8,2}},
     {b5,{ioc,6,8,3}},
     {b6,{ioc,4,8,1}},
     {b7,{ioc,5,8,0}},
     {b8,{ioc,6,8,0}},
     {b9,{ioc,7,8,0}},
     {b10,{ioc,8,8,0}},
     {b11,{ioc,10,8,3}},
     {b12,{ioc,11,8,3}},
     {b13,{ioc,11,8,0}},
     {b14,{ioc,12,8,0}},
     {b16,{ioc,12,8,1}},
     {c2,{ioc,2,8,3}},
     {c3,{ioc,2,8,2}},
     {c4,{ioc,3,8,0}},
     {c5,{ioc,5,8,1}},
     {c6,{ioc,6,8,1}},
     {c7,{ioc,7,8,3}},
     {c8,{ioc,8,8,3}},
     {c9,{ioc,8,8,1}},
     {c10,{ioc,9,8,2}},
     {c11,{ioc,10,8,2}},
     {c12,{ioc,11,8,2}},
     {c13,{ioc,12,8,2}},
     {c14,{ioc,13,7,1}},
     {c15,{ioc,13,7,3}},
     {d1,{ioc,1,8,1}},
     {d2,{ioc,1,8,2}},
     {d3,{ioc,1,8,0}},
     {d4,{ioc,3,8,2}},
     {d5,{ioc,4,8,0}},
     {d11,{ioc,9,8,0}},
     {d12,{ioc,10,8,0}},
     {d13,{ioc,13,7,5}},
     {d14,{ioc,13,6,1}},
     {d15,{ioc,13,7,0}},
     {d16,{ioc,13,7,2}},
     {e1,{ioc,1,8,3}},
     {e2,{ioc,0,7,4}},
     {e3,{ioc,0,7,2}},
     {e4,{ioc,0,7,0}},
     {e13,{ioc,13,6,3}},
     {e14,{ioc,13,6,5}},
     {e15,{ioc,13,7,4}},
     {e16,{ioc,13,6,0}},
     {f1,{ioc,0,7,3}},
     {f2,{ioc,0,7,1}},
     {f3,{ioc,0,7,6}},
     {f13,{ioc,13,5,1}},
     {f14,{ioc,13,5,3}},
     {f15,{ioc,13,6,2}},
     {f16,{ioc,13,6,4}},
     {g1,{ioc,0,6,0}},
     {g2,{ioc,0,7,5}},
     {g3,{ioc,0,6,1}},
     {g14,{ioc,13,5,5}},
     {g15,{ioc,13,5,0}},
     {g16,{ioc,13,5,2}},
     {h1,{ioc,0,6,4}},
     {h2,{ioc,0,6,2}},
     {h3,{ioc,0,6,3}},
     {h5,{ioc,0,5,0}},
     {h12,{ioc,13,4,3}},
     {h14,{ioc,13,4,1}},
     {h15,{ioc,13,5,4}},
     {h16,{ioc,13,4,0}},
     {j1,{ioc,0,6,6}},
     {j2,{ioc,0,5,3}},
     {j3,{ioc,0,6,5}},
     {j5,{ioc,0,5,1}},
     {j12,{ioc,13,4,4}},
     {j14,{ioc,13,4,5}},
     {j15,{ioc,13,3,0}},
     {j16,{ioc,13,4,2}},
     {k1,{ioc,0,5,5}},
     {k2,{ioc,0,4,0}},
     {k3,{ioc,0,5,2}},
     {k14,{ioc,13,3,1}},
     {k15,{ioc,13,3,4}},
     {k16,{ioc,13,3,2}},
     {l1,{ioc,0,4,2}},
     {l2,{ioc,0,4,4}},
     {l3,{ioc,0,5,4}},
     {l4,{ioc,0,5,6}},
     {l13,{ioc,13,3,5}},
     {l14,{ioc,13,3,3}},
     {l15,{ioc,13,2,2}},
     {l16,{ioc,13,2,0}},
     {m1,{ioc,0,4,6}},
     {m2,{ioc,0,4,1}},
     {m3,{ioc,0,4,3}},
     {m4,{ioc,0,4,5}},
     {m8,{ioc,8,3,3}},
     {m9,{ioc,8,3,2}},
     {m13,{ioc,13,2,3}},
     {m14,{ioc,13,2,1}},
     {m15,{ioc,13,1,0}},
     {m16,{ioc,13,2,4}},
     {n1,{ioc,1,3,2}},
     {n2,{ioc,1,3,3}},
     {n3,{ioc,1,3,1}},
     {n5,{ioc,3,3,1}},
     {n12,{ioc,11,0,3}},
     {n13,{ioc,13,1,1}},
     {n14,{ioc,13,2,5}},
     {n15,{ioc,13,1,4}},
     {n16,{ioc,13,1,2}},
     {p2,{ioc,1,3,0}},
     {p4,{ioc,2,3,1}},
     {p5,{ioc,4,3,3}},
     {p6,{ioc,5,3,3}},
     {p7,{ioc,5,3,1}},
     {p8,{ioc,6,3,3}},
     {p9,{ioc,6,3,1}},
     {p10,{ioc,7,3,1}},
     {p11,{ioc,8,3,1}},
     {p12,{ioc,10,0,1}},
     {p13,{ioc,12,0,3}},
     {p14,{ioc,13,1,5}},
     {p15,{ioc,13,1,3}},
     {r1,{ioc,2,3,2}},
     {r3,{ioc,2,3,3}},
     {r4,{ioc,3,3,3}},
     {r5,{ioc,4,3,1}},
     {r6,{ioc,4,3,2}},
     {r7,{ioc,5,3,2}},
     {r8,{ioc,6,3,2}},
     {r9,{ioc,7,3,2}},
     {r10,{ioc,8,3,0}},
     {r11,{ioc,10,0,0}},
     {r12,{ioc,10,0,3}},
     {r13,{ioc,11,0,1}},
     {r14,{ioc,12,0,1}},
     {r16,{ioc,12,0,0}},
     {t2,{ioc,2,3,0}},
     {t4,{ioc,3,3,2}},
     {t5,{ioc,3,3,0}},
     {t6,{ioc,4,3,0}},
     {t7,{ioc,5,3,0}},
     {t8,{ioc,6,3,0}},
     {t9,{ioc,7,3,3}},
     {t10,{ioc,7,3,0}},
     {t11,{ioc,10,0,2}},
     {t12,{ioc,11,0,2}},
     {t13,{ioc,11,0,0}},
     {t15,{ioc,12,0,2}}
    ].

-spec pins() -> [pin()].

pins() ->
    [a2,
     a4,
     a5,
     a6,
     a7,
     a8,
     a9,
     a10,
     a11,
     a12,
     a13,
     a15,
     b1,
     b3,
     b4,
     b5,
     b6,
     b7,
     b8,
     b9,
     b10,
     b11,
     b12,
     b13,
     b14,
     b16,
     c2,
     c3,
     c4,
     c5,
     c6,
     c7,
     c8,
     c9,
     c10,
     c11,
     c12,
     c13,
     c14,
     c15,
     d1,
     d2,
     d3,
     d4,
     d5,
     d11,
     d12,
     d13,
     d14,
     d15,
     d16,
     e1,
     e2,
     e3,
     e4,
     e13,
     e14,
     e15,
     e16,
     f1,
     f2,
     f3,
     f13,
     f14,
     f15,
     f16,
     g1,
     g2,
     g3,
     g14,
     g15,
     g16,
     h1,
     h2,
     h3,
     h5,
     h12,
     h14,
     h15,
     h16,
     j1,
     j2,
     j3,
     j5,
     j12,
     j14,
     j15,
     j16,
     k1,
     k2,
     k3,
     k14,
     k15,
     k16,
     l1,
     l2,
     l3,
     l4,
     l13,
     l14,
     l15,
     l16,
     m1,
     m2,
     m3,
     m4,
     m8,
     m9,
     m13,
     m14,
     m15,
     m16,
     n1,
     n2,
     n3,
     n5,
     n12,
     n13,
     n14,
     n15,
     n16,
     p2,
     p4,
     p5,
     p6,
     p7,
     p8,
     p9,
     p10,
     p11,
     p12,
     p13,
     p14,
     p15,
     r1,
     r3,
     r4,
     r5,
     r6,
     r7,
     r8,
     r9,
     r10,
     r11,
     r12,
     r13,
     r14,
     r16,
     t2,
     t4,
     t5,
     t6,
     t7,
     t8,
     t9,
     t10,
     t11,
     t12,
     t13,
     t15
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(1) ->
    [{d3,{ioc,1,8,0}},
     {d1,{ioc,1,8,1}},
     {d2,{ioc,1,8,2}},
     {e1,{ioc,1,8,3}}];
top_iocs(2) ->
    [{b3,{ioc,2,8,0}},
     {b1,{ioc,2,8,1}},
     {c3,{ioc,2,8,2}},
     {c2,{ioc,2,8,3}}];
top_iocs(3) ->
    [{c4,{ioc,3,8,0}},
     {a4,{ioc,3,8,1}},
     {d4,{ioc,3,8,2}},
     {a2,{ioc,3,8,3}}];
top_iocs(4) ->
    [{d5,{ioc,4,8,0}},
     {b6,{ioc,4,8,1}},
     {b4,{ioc,4,8,2}},
     {a5,{ioc,4,8,3}}];
top_iocs(5) ->
    [{b7,{ioc,5,8,0}},
     {c5,{ioc,5,8,1}},
     {a6,{ioc,5,8,2}}];
top_iocs(6) ->
    [{b8,{ioc,6,8,0}},
     {c6,{ioc,6,8,1}},
     {a7,{ioc,6,8,2}},
     {b5,{ioc,6,8,3}}];
top_iocs(7) ->
    [{b9,{ioc,7,8,0}},
     {a9,{ioc,7,8,1}},
     {a8,{ioc,7,8,2}},
     {c7,{ioc,7,8,3}}];
top_iocs(8) ->
    [{b10,{ioc,8,8,0}},
     {c9,{ioc,8,8,1}},
     {a10,{ioc,8,8,2}},
     {c8,{ioc,8,8,3}}];
top_iocs(9) ->
    [{d11,{ioc,9,8,0}},
     {a11,{ioc,9,8,1}},
     {c10,{ioc,9,8,2}}];
top_iocs(10) ->
    [{d12,{ioc,10,8,0}},
     {a12,{ioc,10,8,1}},
     {c11,{ioc,10,8,2}},
     {b11,{ioc,10,8,3}}];
top_iocs(11) ->
    [{b13,{ioc,11,8,0}},
     {a13,{ioc,11,8,1}},
     {c12,{ioc,11,8,2}},
     {b12,{ioc,11,8,3}}];
top_iocs(12) ->
    [{b14,{ioc,12,8,0}},
     {b16,{ioc,12,8,1}},
     {c13,{ioc,12,8,2}},
     {a15,{ioc,12,8,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [d1, d2, d3, e1];
top_pins(2) ->
    [b1, b3, c2, c3];
top_pins(3) ->
    [a2, a4, c4, d4];
top_pins(4) ->
    [a5, b4, b6, d5];
top_pins(5) ->
    [a6, b7, c5];
top_pins(6) ->
    [a7, b5, b8, c6];
top_pins(7) ->
    [a8, a9, b9, c7];
top_pins(8) ->
    [a10, b10, c8, c9];
top_pins(9) ->
    [a11, c10, d11];
top_pins(10) ->
    [a12, b11, c11, d12];
top_pins(11) ->
    [a13, b12, b13, c12];
top_pins(12) ->
    [a15, b14, b16, c13].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{k2,{ioc,0,4,0}},
     {m2,{ioc,0,4,1}},
     {l1,{ioc,0,4,2}},
     {m3,{ioc,0,4,3}},
     {l2,{ioc,0,4,4}},
     {m4,{ioc,0,4,5}},
     {m1,{ioc,0,4,6}}];
left_iocs(5) ->
    [{h5,{ioc,0,5,0}},
     {j5,{ioc,0,5,1}},
     {k3,{ioc,0,5,2}},
     {j2,{ioc,0,5,3}},
     {l3,{ioc,0,5,4}},
     {k1,{ioc,0,5,5}},
     {l4,{ioc,0,5,6}}];
left_iocs(6) ->
    [{g1,{ioc,0,6,0}},
     {g3,{ioc,0,6,1}},
     {h2,{ioc,0,6,2}},
     {h3,{ioc,0,6,3}},
     {h1,{ioc,0,6,4}},
     {j3,{ioc,0,6,5}},
     {j1,{ioc,0,6,6}}];
left_iocs(7) ->
    [{e4,{ioc,0,7,0}},
     {f2,{ioc,0,7,1}},
     {e3,{ioc,0,7,2}},
     {f1,{ioc,0,7,3}},
     {e2,{ioc,0,7,4}},
     {g2,{ioc,0,7,5}},
     {f3,{ioc,0,7,6}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [k2, l1, l2, m1, m2, m3, m4];
left_pins(5) ->
    [h5, j2, j5, k1, k3, l3, l4];
left_pins(6) ->
    [g1, g3, h1, h2, h3, j1, j3];
left_pins(7) ->
    [e2, e3, e4, f1, f2, f3, g2].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{m15,{ioc,13,1,0}},
     {n13,{ioc,13,1,1}},
     {n16,{ioc,13,1,2}},
     {p15,{ioc,13,1,3}},
     {n15,{ioc,13,1,4}},
     {p14,{ioc,13,1,5}}];
right_iocs(2) ->
    [{l16,{ioc,13,2,0}},
     {m14,{ioc,13,2,1}},
     {l15,{ioc,13,2,2}},
     {m13,{ioc,13,2,3}},
     {m16,{ioc,13,2,4}},
     {n14,{ioc,13,2,5}}];
right_iocs(3) ->
    [{j15,{ioc,13,3,0}},
     {k14,{ioc,13,3,1}},
     {k16,{ioc,13,3,2}},
     {l14,{ioc,13,3,3}},
     {k15,{ioc,13,3,4}},
     {l13,{ioc,13,3,5}}];
right_iocs(4) ->
    [{h16,{ioc,13,4,0}},
     {h14,{ioc,13,4,1}},
     {j16,{ioc,13,4,2}},
     {h12,{ioc,13,4,3}},
     {j12,{ioc,13,4,4}},
     {j14,{ioc,13,4,5}}];
right_iocs(5) ->
    [{g15,{ioc,13,5,0}},
     {f13,{ioc,13,5,1}},
     {g16,{ioc,13,5,2}},
     {f14,{ioc,13,5,3}},
     {h15,{ioc,13,5,4}},
     {g14,{ioc,13,5,5}}];
right_iocs(6) ->
    [{e16,{ioc,13,6,0}},
     {d14,{ioc,13,6,1}},
     {f15,{ioc,13,6,2}},
     {e13,{ioc,13,6,3}},
     {f16,{ioc,13,6,4}},
     {e14,{ioc,13,6,5}}];
right_iocs(7) ->
    [{d15,{ioc,13,7,0}},
     {c14,{ioc,13,7,1}},
     {d16,{ioc,13,7,2}},
     {c15,{ioc,13,7,3}},
     {e15,{ioc,13,7,4}},
     {d13,{ioc,13,7,5}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [m15, n13, n15, n16, p14, p15];
right_pins(2) ->
    [l15, l16, m13, m14, m16, n14];
right_pins(3) ->
    [j15, k14, k15, k16, l13, l14];
right_pins(4) ->
    [h12, h14, h16, j12, j14, j16];
right_pins(5) ->
    [f13, f14, g14, g15, g16, h15];
right_pins(6) ->
    [d14, e13, e14, e16, f15, f16];
right_pins(7) ->
    [c14, c15, d13, d15, d16, e15].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{p2,{ioc,1,3,0}},
     {n3,{ioc,1,3,1}},
     {n1,{ioc,1,3,2}},
     {n2,{ioc,1,3,3}}];
bottom_iocs(2) ->
    [{t2,{ioc,2,3,0}},
     {p4,{ioc,2,3,1}},
     {r1,{ioc,2,3,2}},
     {r3,{ioc,2,3,3}}];
bottom_iocs(3) ->
    [{t5,{ioc,3,3,0}},
     {n5,{ioc,3,3,1}},
     {t4,{ioc,3,3,2}},
     {r4,{ioc,3,3,3}}];
bottom_iocs(4) ->
    [{t6,{ioc,4,3,0}},
     {r5,{ioc,4,3,1}},
     {r6,{ioc,4,3,2}},
     {p5,{ioc,4,3,3}}];
bottom_iocs(5) ->
    [{t7,{ioc,5,3,0}},
     {p7,{ioc,5,3,1}},
     {r7,{ioc,5,3,2}},
     {p6,{ioc,5,3,3}}];
bottom_iocs(6) ->
    [{t8,{ioc,6,3,0}},
     {p9,{ioc,6,3,1}},
     {r8,{ioc,6,3,2}},
     {p8,{ioc,6,3,3}}];
bottom_iocs(7) ->
    [{t10,{ioc,7,3,0}},
     {p10,{ioc,7,3,1}},
     {r9,{ioc,7,3,2}},
     {t9,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{r10,{ioc,8,3,0}},
     {p11,{ioc,8,3,1}},
     {m9,{ioc,8,3,2}},
     {m8,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{r11,{ioc,10,0,0}},
     {p12,{ioc,10,0,1}},
     {t11,{ioc,10,0,2}},
     {r12,{ioc,10,0,3}}];
bottom_iocs(11) ->
    [{t13,{ioc,11,0,0}},
     {r13,{ioc,11,0,1}},
     {t12,{ioc,11,0,2}},
     {n12,{ioc,11,0,3}}];
bottom_iocs(12) ->
    [{r16,{ioc,12,0,0}},
     {r14,{ioc,12,0,1}},
     {t15,{ioc,12,0,2}},
     {p13,{ioc,12,0,3}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [n1, n2, n3, p2];
bottom_pins(2) ->
    [p4, r1, r3, t2];
bottom_pins(3) ->
    [n5, r4, t4, t5];
bottom_pins(4) ->
    [p5, r5, r6, t6];
bottom_pins(5) ->
    [p6, p7, r7, t7];
bottom_pins(6) ->
    [p8, p9, r8, t8];
bottom_pins(7) ->
    [p10, r9, t9, t10];
bottom_pins(8) ->
    [m8, m9, p11, r10];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [p12, r11, r12, t11];
bottom_pins(11) ->
    [n12, r13, t12, t13];
bottom_pins(12) ->
    [p13, r14, r16, t15].

