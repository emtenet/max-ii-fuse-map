-module(epm1270_f256).

-export([iocs/0]).
-export([pins/0]).

-type ioc() :: ioc:ioc().
-type pin() :: pin:pin().

-spec iocs() -> [{pin(), ioc()}].

iocs() ->
    [{a2,{ioc,2,11,3}},
     {a4,{ioc,4,11,1}},
     {a5,{ioc,5,11,0}},
     {a6,{ioc,6,11,0}},
     {a7,{ioc,8,11,2}},
     {a8,{ioc,9,11,3}},
     {a9,{ioc,9,11,2}},
     {a10,{ioc,10,11,1}},
     {a11,{ioc,11,11,1}},
     {a12,{ioc,12,11,0}},
     {a13,{ioc,14,11,2}},
     {a15,{ioc,15,11,0}},
     {b1,{ioc,1,11,1}},
     {b3,{ioc,2,11,1}},
     {b4,{ioc,3,11,0}},
     {b5,{ioc,5,11,2}},
     {b6,{ioc,6,11,2}},
     {b7,{ioc,7,11,1}},
     {b8,{ioc,8,11,0}},
     {b9,{ioc,9,11,0}},
     {b10,{ioc,11,11,3}},
     {b11,{ioc,12,11,2}},
     {b12,{ioc,13,11,1}},
     {b13,{ioc,14,11,0}},
     {b14,{ioc,15,11,2}},
     {b16,{ioc,16,11,1}},
     {c2,{ioc,0,10,1}},
     {c3,{ioc,0,10,3}},
     {c4,{ioc,3,11,2}},
     {c5,{ioc,2,11,2}},
     {c6,{ioc,2,11,0}},
     {c7,{ioc,4,11,0}},
     {c8,{ioc,7,11,2}},
     {c9,{ioc,10,11,0}},
     {c10,{ioc,13,11,2}},
     {c11,{ioc,15,11,3}},
     {c12,{ioc,16,11,2}},
     {c13,{ioc,16,11,0}},
     {c14,{ioc,17,10,2}},
     {c15,{ioc,17,10,4}},
     {d1,{ioc,0,9,0}},
     {d2,{ioc,0,10,5}},
     {d3,{ioc,0,10,0}},
     {d4,{ioc,1,11,2}},
     {d5,{ioc,1,11,0}},
     {d6,{ioc,3,11,1}},
     {d7,{ioc,5,11,1}},
     {d8,{ioc,7,11,0}},
     {d9,{ioc,10,11,2}},
     {d10,{ioc,12,11,1}},
     {d11,{ioc,14,11,1}},
     {d12,{ioc,15,11,1}},
     {d13,{ioc,17,10,0}},
     {d14,{ioc,17,9,1}},
     {d15,{ioc,17,9,3}},
     {d16,{ioc,17,8,0}},
     {e1,{ioc,0,9,4}},
     {e2,{ioc,0,9,2}},
     {e3,{ioc,0,10,2}},
     {e4,{ioc,0,10,4}},
     {e5,{ioc,0,10,6}},
     {e6,{ioc,4,11,2}},
     {e7,{ioc,6,11,3}},
     {e8,{ioc,8,11,1}},
     {e9,{ioc,9,11,1}},
     {e10,{ioc,11,11,0}},
     {e11,{ioc,13,11,0}},
     {e12,{ioc,17,9,0}},
     {e13,{ioc,17,10,3}},
     {e14,{ioc,17,10,1}},
     {e15,{ioc,17,8,2}},
     {e16,{ioc,17,8,4}},
     {f1,{ioc,0,8,1}},
     {f2,{ioc,0,9,6}},
     {f3,{ioc,0,9,1}},
     {f4,{ioc,0,9,3}},
     {f5,{ioc,0,9,5}},
     {f6,{ioc,0,8,0}},
     {f7,{ioc,6,11,1}},
     {f10,{ioc,11,11,2}},
     {f11,{ioc,17,8,3}},
     {f12,{ioc,17,8,1}},
     {f13,{ioc,17,9,4}},
     {f14,{ioc,17,9,2}},
     {f15,{ioc,17,7,0}},
     {f16,{ioc,17,7,2}},
     {g1,{ioc,0,8,5}},
     {g2,{ioc,0,8,3}},
     {g3,{ioc,0,8,2}},
     {g4,{ioc,0,8,4}},
     {g5,{ioc,0,8,6}},
     {g6,{ioc,0,7,1}},
     {g11,{ioc,17,7,3}},
     {g12,{ioc,17,6,0}},
     {g13,{ioc,17,7,1}},
     {g14,{ioc,17,8,5}},
     {g15,{ioc,17,7,4}},
     {g16,{ioc,17,6,1}},
     {h1,{ioc,0,7,2}},
     {h2,{ioc,0,7,0}},
     {h3,{ioc,0,7,3}},
     {h4,{ioc,0,6,0}},
     {h5,{ioc,0,7,5}},
     {h12,{ioc,17,5,0}},
     {h13,{ioc,17,6,4}},
     {h14,{ioc,17,6,2}},
     {h15,{ioc,17,6,3}},
     {h16,{ioc,17,6,5}},
     {j1,{ioc,0,7,4}},
     {j2,{ioc,0,6,1}},
     {j3,{ioc,0,6,4}},
     {j4,{ioc,0,6,2}},
     {j5,{ioc,0,7,6}},
     {j12,{ioc,17,5,1}},
     {j13,{ioc,17,5,2}},
     {j14,{ioc,17,5,4}},
     {j15,{ioc,17,5,5}},
     {j16,{ioc,17,5,3}},
     {k1,{ioc,0,6,3}},
     {k2,{ioc,0,6,5}},
     {k3,{ioc,0,5,5}},
     {k4,{ioc,0,5,3}},
     {k5,{ioc,0,5,1}},
     {k6,{ioc,0,6,6}},
     {k11,{ioc,17,4,0}},
     {k12,{ioc,17,4,2}},
     {k13,{ioc,17,4,4}},
     {k14,{ioc,17,3,1}},
     {k15,{ioc,17,4,3}},
     {k16,{ioc,17,4,1}},
     {l1,{ioc,0,5,0}},
     {l2,{ioc,0,5,2}},
     {l3,{ioc,0,4,4}},
     {l4,{ioc,0,4,2}},
     {l5,{ioc,0,4,0}},
     {l7,{ioc,6,3,1}},
     {l10,{ioc,10,3,2}},
     {l11,{ioc,17,3,3}},
     {l12,{ioc,17,3,5}},
     {l13,{ioc,17,2,1}},
     {l14,{ioc,17,2,3}},
     {l15,{ioc,17,3,2}},
     {l16,{ioc,17,3,0}},
     {m1,{ioc,0,5,4}},
     {m2,{ioc,0,5,6}},
     {m3,{ioc,0,4,1}},
     {m4,{ioc,0,4,6}},
     {m6,{ioc,4,3,2}},
     {m7,{ioc,5,3,0}},
     {m8,{ioc,9,3,0}},
     {m9,{ioc,10,3,3}},
     {m10,{ioc,10,3,0}},
     {m11,{ioc,13,0,1}},
     {m12,{ioc,15,0,2}},
     {m13,{ioc,17,1,0}},
     {m14,{ioc,17,1,2}},
     {m15,{ioc,17,2,0}},
     {m16,{ioc,17,3,4}},
     {n1,{ioc,0,4,3}},
     {n2,{ioc,0,4,5}},
     {n3,{ioc,1,3,3}},
     {n5,{ioc,2,3,1}},
     {n6,{ioc,3,3,0}},
     {n7,{ioc,5,3,2}},
     {n8,{ioc,7,3,1}},
     {n9,{ioc,8,3,3}},
     {n10,{ioc,12,0,1}},
     {n11,{ioc,14,0,3}},
     {n12,{ioc,15,0,0}},
     {n13,{ioc,17,1,4}},
     {n14,{ioc,17,1,1}},
     {n15,{ioc,17,2,4}},
     {n16,{ioc,17,2,2}},
     {p2,{ioc,1,3,2}},
     {p4,{ioc,1,3,1}},
     {p5,{ioc,2,3,3}},
     {p6,{ioc,3,3,2}},
     {p7,{ioc,4,3,0}},
     {p8,{ioc,7,3,3}},
     {p9,{ioc,9,3,2}},
     {p10,{ioc,13,0,3}},
     {p11,{ioc,14,0,1}},
     {p12,{ioc,16,0,2}},
     {p13,{ioc,16,0,0}},
     {p14,{ioc,17,1,5}},
     {p15,{ioc,17,1,3}},
     {r1,{ioc,1,3,0}},
     {r3,{ioc,2,3,0}},
     {r4,{ioc,3,3,1}},
     {r5,{ioc,4,3,1}},
     {r6,{ioc,5,3,1}},
     {r7,{ioc,6,3,0}},
     {r8,{ioc,7,3,0}},
     {r9,{ioc,8,3,0}},
     {r10,{ioc,10,3,1}},
     {r11,{ioc,12,0,0}},
     {r12,{ioc,13,0,0}},
     {r13,{ioc,14,0,0}},
     {r14,{ioc,15,0,1}},
     {r16,{ioc,16,0,1}},
     {t2,{ioc,2,3,2}},
     {t4,{ioc,4,3,3}},
     {t5,{ioc,5,3,3}},
     {t6,{ioc,6,3,2}},
     {t7,{ioc,7,3,2}},
     {t8,{ioc,8,3,2}},
     {t9,{ioc,8,3,1}},
     {t10,{ioc,9,3,1}},
     {t11,{ioc,12,0,2}},
     {t12,{ioc,13,0,2}},
     {t13,{ioc,14,0,2}},
     {t15,{ioc,16,0,3}}
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
     d6,
     d7,
     d8,
     d9,
     d10,
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
     e5,
     e6,
     e7,
     e8,
     e9,
     e10,
     e11,
     e12,
     e13,
     e14,
     e15,
     e16,
     f1,
     f2,
     f3,
     f4,
     f5,
     f6,
     f7,
     f10,
     f11,
     f12,
     f13,
     f14,
     f15,
     f16,
     g1,
     g2,
     g3,
     g4,
     g5,
     g6,
     g11,
     g12,
     g13,
     g14,
     g15,
     g16,
     h1,
     h2,
     h3,
     h4,
     h5,
     h12,
     h13,
     h14,
     h15,
     h16,
     j1,
     j2,
     j3,
     j4,
     j5,
     j12,
     j13,
     j14,
     j15,
     j16,
     k1,
     k2,
     k3,
     k4,
     k5,
     k6,
     k11,
     k12,
     k13,
     k14,
     k15,
     k16,
     l1,
     l2,
     l3,
     l4,
     l5,
     l7,
     l10,
     l11,
     l12,
     l13,
     l14,
     l15,
     l16,
     m1,
     m2,
     m3,
     m4,
     m6,
     m7,
     m8,
     m9,
     m10,
     m11,
     m12,
     m13,
     m14,
     m15,
     m16,
     n1,
     n2,
     n3,
     n5,
     n6,
     n7,
     n8,
     n9,
     n10,
     n11,
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

