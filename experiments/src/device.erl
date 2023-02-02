-module(device).

-export([list/0]).
-export([name/1]).
-export([density/1]).
-export([package/1]).
-export([pins/1]).
-export([gclk_pins/1]).

-export_type([device/0]).

-type density() :: density:density().

-type device() ::
    epm240_m100 |
    epm240_f100 |
    epm240_t100 |
    epm570_m100 |
    epm570_f100 |
    epm570_t100 |
    epm570_t144 |
    epm570_m256 |
    epm570_f256 |
    epm1270_t144 |
    epm1270_m256 |
    epm1270_f256 |
    epm2210_f256 |
    epm2210_f324.

-type package() :: package:package().
-type pin() :: pin:pin().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [device()].

list() ->
    [epm240_m100,
     epm240_f100,
     epm240_t100,
     epm570_m100,
     epm570_f100,
     epm570_t100,
     epm570_t144,
     epm570_m256,
     epm570_f256,
     epm1270_t144,
     epm1270_m256,
     epm1270_f256,
     epm2210_f256,
     epm2210_f324
    ].

%%====================================================================
%% name
%%====================================================================

-spec name(device()) -> binary().

name(epm240_m100) -> <<"EPM240M100C5">>;
name(epm240_f100) -> <<"EPM240F100C5">>;
name(epm240_t100) -> <<"EPM240T100C5">>;
name(epm570_m100) -> <<"EPM570M100C5">>;
name(epm570_f100) -> <<"EPM570F100C5">>;
name(epm570_t100) -> <<"EPM570T100C5">>;
name(epm570_t144) -> <<"EPM570T144C5">>;
name(epm570_m256) -> <<"EPM570M256C5">>;
name(epm570_f256) -> <<"EPM570F256C5">>;
name(epm1270_t144) -> <<"EPM1270T144C5">>;
name(epm1270_m256) -> <<"EPM1270M256C5">>;
name(epm1270_f256) -> <<"EPM1270F256C5">>;
name(epm2210_f256) -> <<"EPM2210F256C5">>;
name(epm2210_f324) -> <<"EPM2210F324C5">>.

%%====================================================================
%% density
%%====================================================================

-spec density(device()) -> density().

density(epm240_m100) -> epm240;
density(epm240_f100) -> epm240;
density(epm240_t100) -> epm240;
density(epm570_m100) -> epm570;
density(epm570_f100) -> epm570;
density(epm570_t100) -> epm570;
density(epm570_t144) -> epm570;
density(epm570_m256) -> epm570;
density(epm570_f256) -> epm570;
density(epm1270_t144) -> epm1270;
density(epm1270_m256) -> epm1270;
density(epm1270_f256) -> epm1270;
density(epm2210_f256) -> epm2210;
density(epm2210_f324) -> epm2210.

%%====================================================================
%% package
%%====================================================================

-spec package(device()) -> package().

package(epm240_m100) -> m100;
package(epm240_f100) -> f100;
package(epm240_t100) -> t100;
package(epm570_m100) -> m100;
package(epm570_f100) -> f100;
package(epm570_t100) -> t100;
package(epm570_t144) -> t144;
package(epm570_m256) -> m256;
package(epm570_f256) -> f256;
package(epm1270_t144) -> t144;
package(epm1270_m256) -> m256;
package(epm1270_f256) -> f256;
package(epm2210_f256) -> f256;
package(epm2210_f324) -> f324.

%%====================================================================
%% pins
%%====================================================================

-spec pins(device()) -> [pin()].

pins(epm240_m100) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
     c1, c2, c5, c6, c7, c10, c11,
     d1, d2, d3, d9, d10, d11,
     e1, e2, e10, e11,
     f1, f2, f3, f9, f10, f11,
     g1, g2, g10, g11,
     h1, h2, h3, h9, h10, h11,
     j5, j6, j7, j10, j11,
     k3, k4, k5, k6, k7, k8, k9, k10, k11,
     l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11
    ];
pins(epm240_f100) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b10,
     c1, c2, c3, c4, c5, c6, c7, c8, c9, c10,
     d1, d2, d3, d8, d9, d10,
     e1, e2, e3, e8, e9, e10,
     f1, f2, f3, f8, f9, f10,
     g1, g2, g3, g8, g9, g10,
     h1, h4, h5, h6, h7, h8, h9, h10,
     j3, j4, j5, j6, j7, j8, j9, j10,
     k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    ];
pins(epm240_t100) ->
    [pin1, pin2, pin3, pin4, pin5, pin6, pin7, pin8,
     pin12, pin14, pin15, pin16, pin17, pin18, pin19,
     pin20, pin21, pin26, pin27, pin28, pin29,
     pin30, pin33, pin34, pin35, pin36, pin37, pin38, pin39,
     pin40, pin41, pin42, pin43, pin44, pin47, pin48, pin49,
     pin50, pin51, pin52, pin53, pin54, pin55, pin56, pin57, pin58,
     pin61, pin62, pin64, pin66, pin67, pin68, pin69,
     pin70, pin71, pin72, pin73, pin74, pin75, pin76, pin77, pin78,
     pin81, pin82, pin83, pin84, pin85, pin86, pin87, pin88, pin89,
     pin90, pin91, pin92, pin95, pin96, pin97, pin98, pin99,
     pin100
    ];
pins(epm570_m100) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
     c1, c2, c6, c10, c11,
     d1, d2, d3, d9, d10, d11,
     e1, e2, e10, e11,
     f1, f2, f3, f9, f10, f11,
     g1, g2, g10, g11,
     h1, h2, h3, h9, h10, h11,
     j6, j10, j11,
     k3, k4, k5, k6, k7, k8, k9, k10, k11,
     l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11
    ];
pins(epm570_f100) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b10,
     c1, c2, c3, c4, c7, c8, c9, c10,
     d1, d2, d3, d8, d9, d10,
     e1, e2, e3, e8, e9, e10,
     f1, f2, f3, f8, f9, f10,
     g1, g2, g3, g8, g9, g10,
     h1, h4, h7, h8, h9, h10,
     j3, j4, j5, j6, j7, j8, j9, j10,
     k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    ];
pins(epm570_t100) ->
    [pin1, pin2, pin3, pin4, pin5, pin6, pin7, pin8,
     pin12, pin14, pin15, pin16, pin17, pin18, pin19,
     pin20, pin21, pin26, pin27, pin28, pin29,
     pin30, pin33, pin34, pin35, pin36, pin38,
     pin40, pin41, pin42, pin43, pin44, pin47, pin48, pin49,
     pin50, pin51, pin52, pin53, pin54, pin55, pin56, pin57, pin58,
     pin61, pin62, pin64, pin66, pin67, pin68, pin69,
     pin70, pin71, pin72, pin73, pin74, pin75, pin76, pin77, pin78,
     pin81, pin82, pin83, pin84, pin85, pin86, pin87, pin89,
     pin91, pin92, pin95, pin96, pin97, pin98, pin99,
     pin100
    ];
pins(epm570_t144) ->
    [pin1, pin2, pin3, pin4, pin5, pin6, pin7, pin8,
     pin11, pin12, pin13, pin14, pin15, pin16, pin18,
     pin20, pin21, pin22, pin23, pin24, pin27, pin28, pin29,
     pin30, pin31, pin32, pin37, pin38, pin39,
     pin40, pin41, pin42, pin43, pin44, pin45, pin48, pin49,
     pin50, pin51, pin52, pin53, pin55, pin57, pin58, pin59,
     pin60, pin61, pin62, pin63, pin66, pin67, pin68, pin69,
     pin70, pin71, pin72, pin73, pin74, pin75, pin76, pin77, pin78, pin79,
     pin80, pin81, pin84, pin85, pin86, pin87, pin88, pin89,
     pin91, pin93, pin94, pin95, pin96, pin97, pin98,
     pin101, pin102, pin103, pin104, pin105, pin106, pin107, pin108, pin109,
     pin110, pin111, pin112, pin113, pin114, pin117, pin118, pin119,
     pin120, pin121, pin122, pin123, pin124, pin125, pin127, pin129,
     pin130, pin131, pin132, pin133, pin134, pin137, pin138, pin139,
     pin140, pin141, pin142, pin143, pin144
    ];
pins(epm570_m256) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b12, b13, b14, b15, b16, b17, b18, b19, b20,
     c1, c2, c3, c5, c15, c16, c17, c18, c19, c20,
     d1, d2, d3, d18, d19, d20,
     e1, e2, e3, e17, e18, e19, e20,
     f1, f2, f3, f19, f20,
     g1, g2, g19, g20,
     h1, h2, h19, h20,
     j1, j2, j19, j20,
     k1, k20,
     l1, l20,
     m1, m2, m19, m20,
     n1, n2, n19, n20,
     p1, p2, p19, p20,
     r1, r2, r3, r18, r19, r20,
     t1, t2, t4, t18, t19, t20,
     u1, u3, u4, u17, u18, u19, u20,
     v1, v3, v4, v5, v6, v16, v17, v18, v19, v20,
     w1, w3, w4, w5, w6, w7, w8, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20,
     y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20
    ];
pins(epm570_f256) ->
    [a2, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a15,
     b1, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b16,
     c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15,
     d1, d2, d3, d4, d5, d11, d12, d13, d14, d15, d16,
     e1, e2, e3, e4, e13, e14, e15, e16,
     f1, f2, f3, f13, f14, f15, f16,
     g1, g2, g3, g14, g15, g16,
     h1, h2, h3, h5, h12, h14, h15, h16,
     j1, j2, j3, j5, j12, j14, j15, j16,
     k1, k2, k3, k14, k15, k16,
     l1, l2, l3, l4, l13, l14, l15, l16,
     m1, m2, m3, m4, m8, m9, m13, m14, m15, m16,
     n1, n2, n3, n5, n12, n13, n14, n15, n16,
     p2, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15,
     r1, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r16,
     t2, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t15
    ];
pins(epm1270_t144) ->
    [pin1, pin2, pin3, pin4, pin5, pin6, pin7, pin8,
     pin11, pin12, pin13, pin14, pin15, pin16, pin18,
     pin20, pin21, pin22, pin23, pin24, pin27, pin28, pin29,
     pin30, pin31, pin32, pin37, pin38, pin39,
     pin40, pin41, pin42, pin43, pin44, pin45, pin48, pin49,
     pin50, pin51, pin52, pin53, pin55, pin57, pin58, pin59,
     pin60, pin61, pin62, pin63, pin66, pin67, pin68, pin69,
     pin70, pin71, pin72, pin73, pin74, pin75, pin76, pin77, pin78, pin79,
     pin80, pin81, pin84, pin85, pin86, pin87, pin88, pin89,
     pin91, pin93, pin94, pin95, pin96, pin97, pin98,
     pin101, pin102, pin103, pin104, pin105, pin106, pin107, pin108, pin109,
     pin110, pin111, pin112, pin113, pin114, pin117, pin118, pin119,
     pin120, pin121, pin122, pin123, pin124, pin125, pin127, pin129,
     pin130, pin131, pin132, pin133, pin134, pin137, pin138, pin139,
     pin140, pin141, pin142, pin143, pin144
    ];
pins(epm1270_m256) ->
    [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
     b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20,
     c1, c2, c3, c4, c5, c6, c7, c14, c15, c16, c17, c18, c19, c20,
     d1, d2, d3, d4, d5, d6, d7, d8, d13, d14, d15, d16, d17, d18, d19, d20,
     e1, e2, e3, e4, e17, e18, e19, e20,
     f1, f2, f3, f4, f17, f18, f19, f20,
     g1, g2, g3, g4, g17, g18, g19, g20,
     h1, h2, h4, h17, h19, h20,
     j1, j2, j19, j20,
     k1, k2, k19, k20,
     l1, l2, l19, l20,
     m1, m2, m19, m20,
     n1, n2, n4, n17, n19, n20,
     p1, p2, p3, p4, p17, p18, p19, p20,
     r1, r2, r3, r4, r17, r18, r19, r20,
     t1, t2, t4, t17, t18, t19, t20,
     u1, u3, u4, u5, u6, u7, u8, u13, u14, u15, u16, u17, u18, u19, u20,
     v1, v3, v4, v5, v6, v7, v14, v15, v16, v17, v18, v19, v20,
     w1, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20,
     y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20
    ];
pins(epm1270_f256) ->
    [a2, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a15,
     b1, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b16,
     c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15,
     d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16,
     e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16,
     f1, f2, f3, f4, f5, f6, f7, f10, f11, f12, f13, f14, f15, f16,
     g1, g2, g3, g4, g5, g6, g11, g12, g13, g14, g15, g16,
     h1, h2, h3, h4, h5, h12, h13, h14, h15, h16,
     j1, j2, j3, j4, j5, j12, j13, j14, j15, j16,
     k1, k2, k3, k4, k5, k6, k11, k12, k13, k14, k15, k16,
     l1, l2, l3, l4, l5, l7, l10, l11, l12, l13, l14, l15, l16,
     m1, m2, m3, m4, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16,
     n1, n2, n3, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16,
     p2, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15,
     r1, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r16,
     t2, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t15
    ];
pins(epm2210_f256) ->
    [a2, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a15,
     b1, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b16,
     c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15,
     d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16,
     e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16,
     f1, f2, f3, f4, f5, f6, f11, f12, f13, f14, f15, f16,
     g1, g2, g3, g4, g5, g12, g13, g14, g15, g16,
     h1, h2, h3, h4, h5, h12, h13, h14, h15, h16,
     j1, j2, j3, j4, j5, j12, j13, j14, j15, j16,
     k1, k2, k3, k4, k5, k12, k13, k14, k15, k16,
     l1, l2, l3, l4, l5, l11, l12, l13, l14, l15, l16,
     m1, m2, m3, m4, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16,
     n1, n2, n3, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16,
     p2, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15,
     r1, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r16,
     t2, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t15
    ];
pins(epm2210_f324) ->
    [a2, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a17,
     b1, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b18,
     c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17,
     d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18,
     e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18,
     f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18,
     g1, g2, g3, g4, g5, g6, g7, g12, g13, g14, g15, g16, g17, g18,
     h1, h2, h3, h4, h5, h6, h13, h14, h15, h16, h17, h18,
     j1, j2, j3, j4, j5, j6, j13, j14, j15, j16, j17, j18,
     k1, k2, k3, k4, k5, k6, k13, k14, k15, k16, k17, k18,
     l1, l2, l3, l4, l5, l6, l13, l14, l15, l16, l17, l18,
     m1, m2, m3, m4, m5, m6, m12, m13, m14, m15, m16, m17, m18,
     n1, n2, n3, n4, n5, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18,
     p1, p2, p3, p4, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18,
     r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18,
     t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17,
     u1, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u18,
     v2, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v17
    ].

%%====================================================================
%% gclk_pins
%%====================================================================

-spec gclk_pins(device()) -> [pin()].

gclk_pins(Device) ->
    package:gclk_pins(package(Device)).

