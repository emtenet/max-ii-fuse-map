-module(epm570_m100).

-export([pins/0]).

-type lc() :: lc:lc().
-type pin() :: pin:pin().

-spec pins() -> [{pin(), lc()}].

pins() ->
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
