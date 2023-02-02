-module(pin).

-export([name/1]).

-export_type([pin/0]).

-type pin() ::
    a1 |
    a2 |
    a3 |
    a4 |
    a5 |
    a6 |
    a7 |
    a8 |
    a9 |
    a10 |
    a11 |
    a12 |
    a13 |
    a14 |
    a15 |
    a16 |
    a17 |
    a18 |
    a19 |
    a20 |
    b1 |
    b2 |
    b3 |
    b4 |
    b5 |
    b6 |
    b7 |
    b8 |
    b9 |
    b10 |
    b11 |
    b12 |
    b13 |
    b14 |
    b15 |
    b16 |
    b17 |
    b18 |
    b19 |
    b20 |
    c1 |
    c2 |
    c3 |
    c4 |
    c5 |
    c6 |
    c7 |
    c8 |
    c9 |
    c10 |
    c11 |
    c12 |
    c13 |
    c14 |
    c15 |
    c16 |
    c17 |
    c18 |
    c19 |
    c20 |
    d1 |
    d2 |
    d3 |
    d4 |
    d5 |
    d6 |
    d7 |
    d8 |
    d9 |
    d10 |
    d11 |
    d12 |
    d13 |
    d14 |
    d15 |
    d16 |
    d17 |
    d18 |
    d19 |
    d20 |
    e1 |
    e2 |
    e3 |
    e4 |
    e5 |
    e6 |
    e7 |
    e8 |
    e9 |
    e10 |
    e11 |
    e12 |
    e13 |
    e14 |
    e15 |
    e16 |
    e17 |
    e18 |
    e19 |
    e20 |
    f1 |
    f2 |
    f3 |
    f4 |
    f5 |
    f6 |
    f7 |
    f8 |
    f9 |
    f10 |
    f11 |
    f12 |
    f13 |
    f14 |
    f15 |
    f16 |
    f17 |
    f18 |
    f19 |
    f20 |
    g1 |
    g2 |
    g3 |
    g4 |
    g5 |
    g6 |
    g7 |
    g8 |
    g9 |
    g10 |
    g11 |
    g12 |
    g13 |
    g14 |
    g15 |
    g16 |
    g17 |
    g18 |
    g19 |
    g20 |
    h1 |
    h2 |
    h3 |
    h4 |
    h5 |
    h6 |
    h7 |
    h8 |
    h9 |
    h10 |
    h11 |
    h12 |
    h13 |
    h14 |
    h15 |
    h16 |
    h17 |
    h18 |
    h19 |
    h20 |
    j1 |
    j2 |
    j3 |
    j4 |
    j5 |
    j6 |
    j7 |
    j8 |
    j9 |
    j10 |
    j11 |
    j12 |
    j13 |
    j14 |
    j15 |
    j16 |
    j17 |
    j18 |
    j19 |
    j20 |
    k1 |
    k2 |
    k3 |
    k4 |
    k5 |
    k6 |
    k7 |
    k8 |
    k9 |
    k10 |
    k11 |
    k12 |
    k13 |
    k14 |
    k15 |
    k16 |
    k17 |
    k18 |
    k19 |
    k20 |
    l1 |
    l2 |
    l3 |
    l4 |
    l5 |
    l6 |
    l7 |
    l8 |
    l9 |
    l10 |
    l11 |
    l12 |
    l13 |
    l14 |
    l15 |
    l16 |
    l17 |
    l18 |
    l19 |
    l20 |
    m1 |
    m2 |
    m3 |
    m4 |
    m5 |
    m6 |
    m7 |
    m8 |
    m9 |
    m10 |
    m11 |
    m12 |
    m13 |
    m14 |
    m15 |
    m16 |
    m17 |
    m18 |
    m19 |
    m20 |
    n1 |
    n2 |
    n3 |
    n4 |
    n5 |
    n6 |
    n7 |
    n8 |
    n9 |
    n10 |
    n11 |
    n12 |
    n13 |
    n14 |
    n15 |
    n16 |
    n17 |
    n18 |
    n19 |
    n20 |
    p1 |
    p2 |
    p3 |
    p4 |
    p5 |
    p6 |
    p7 |
    p8 |
    p9 |
    p10 |
    p11 |
    p12 |
    p13 |
    p14 |
    p15 |
    p16 |
    p17 |
    p18 |
    p19 |
    p20 |
    r1 |
    r2 |
    r3 |
    r4 |
    r5 |
    r6 |
    r7 |
    r8 |
    r9 |
    r10 |
    r11 |
    r12 |
    r13 |
    r14 |
    r15 |
    r16 |
    r17 |
    r18 |
    r19 |
    r20 |
    t1 |
    t2 |
    t3 |
    t4 |
    t5 |
    t6 |
    t7 |
    t8 |
    t9 |
    t10 |
    t11 |
    t12 |
    t13 |
    t14 |
    t15 |
    t16 |
    t17 |
    t18 |
    t19 |
    t20 |
    u1 |
    u3 |
    u4 |
    u5 |
    u6 |
    u7 |
    u8 |
    u9 |
    u10 |
    u11 |
    u12 |
    u13 |
    u14 |
    u15 |
    u16 |
    u17 |
    u18 |
    u19 |
    u20 |
    v1 |
    v2 |
    v3 |
    v4 |
    v5 |
    v6 |
    v7 |
    v8 |
    v9 |
    v10 |
    v11 |
    v12 |
    v13 |
    v14 |
    v15 |
    v16 |
    v17 |
    v18 |
    v19 |
    v20 |
    w1 |
    w3 |
    w4 |
    w5 |
    w6 |
    w7 |
    w8 |
    w9 |
    w10 |
    w11 |
    w12 |
    w13 |
    w14 |
    w15 |
    w16 |
    w17 |
    w18 |
    w19 |
    w20 |
    y1 |
    y2 |
    y3 |
    y4 |
    y5 |
    y6 |
    y7 |
    y8 |
    y9 |
    y10 |
    y11 |
    y12 |
    y13 |
    y14 |
    y15 |
    y16 |
    y17 |
    y18 |
    y19 |
    y20 |
    pin1 |
    pin2 |
    pin3 |
    pin4 |
    pin5 |
    pin6 |
    pin7 |
    pin8 |
    pin11 |
    pin12 |
    pin13 |
    pin14 |
    pin15 |
    pin16 |
    pin17 |
    pin18 |
    pin19 |
    pin20 |
    pin21 |
    pin22 |
    pin23 |
    pin24 |
    pin26 |
    pin27 |
    pin28 |
    pin29 |
    pin30 |
    pin31 |
    pin32 |
    pin33 |
    pin34 |
    pin35 |
    pin36 |
    pin37 |
    pin38 |
    pin39 |
    pin40 |
    pin41 |
    pin42 |
    pin43 |
    pin44 |
    pin45 |
    pin47 |
    pin48 |
    pin49 |
    pin50 |
    pin51 |
    pin52 |
    pin53 |
    pin54 |
    pin55 |
    pin56 |
    pin57 |
    pin58 |
    pin59 |
    pin60 |
    pin61 |
    pin62 |
    pin63 |
    pin64 |
    pin66 |
    pin67 |
    pin68 |
    pin69 |
    pin70 |
    pin71 |
    pin72 |
    pin73 |
    pin74 |
    pin75 |
    pin76 |
    pin77 |
    pin78 |
    pin79 |
    pin80 |
    pin81 |
    pin82 |
    pin83 |
    pin84 |
    pin85 |
    pin86 |
    pin87 |
    pin88 |
    pin89 |
    pin90 |
    pin91 |
    pin92 |
    pin93 |
    pin94 |
    pin95 |
    pin96 |
    pin97 |
    pin98 |
    pin99 |
    pin100 |
    pin101 |
    pin102 |
    pin103 |
    pin104 |
    pin105 |
    pin106 |
    pin107 |
    pin108 |
    pin109 |
    pin110 |
    pin111 |
    pin112 |
    pin113 |
    pin114 |
    pin117 |
    pin118 |
    pin119 |
    pin120 |
    pin121 |
    pin122 |
    pin123 |
    pin124 |
    pin125 |
    pin127 |
    pin129 |
    pin130 |
    pin131 |
    pin132 |
    pin133 |
    pin134 |
    pin137 |
    pin138 |
    pin139 |
    pin140 |
    pin141 |
    pin142 |
    pin143 |
    pin144.

%%====================================================================
%% name
%%====================================================================

-spec name(pin()) -> binary().

name(a1) -> <<"PIN_A1">>;
name(a2) -> <<"PIN_A2">>;
name(a3) -> <<"PIN_A3">>;
name(a4) -> <<"PIN_A4">>;
name(a5) -> <<"PIN_A5">>;
name(a6) -> <<"PIN_A6">>;
name(a7) -> <<"PIN_A7">>;
name(a8) -> <<"PIN_A8">>;
name(a9) -> <<"PIN_A9">>;
name(a10) -> <<"PIN_A10">>;
name(a11) -> <<"PIN_A11">>;
name(a12) -> <<"PIN_A12">>;
name(a13) -> <<"PIN_A13">>;
name(a14) -> <<"PIN_A14">>;
name(a15) -> <<"PIN_A15">>;
name(a16) -> <<"PIN_A16">>;
name(a17) -> <<"PIN_A17">>;
name(a18) -> <<"PIN_A18">>;
name(a19) -> <<"PIN_A19">>;
name(a20) -> <<"PIN_A20">>;
name(b1) -> <<"PIN_B1">>;
name(b2) -> <<"PIN_B2">>;
name(b3) -> <<"PIN_B3">>;
name(b4) -> <<"PIN_B4">>;
name(b5) -> <<"PIN_B5">>;
name(b6) -> <<"PIN_B6">>;
name(b7) -> <<"PIN_B7">>;
name(b8) -> <<"PIN_B8">>;
name(b9) -> <<"PIN_B9">>;
name(b10) -> <<"PIN_B10">>;
name(b11) -> <<"PIN_B11">>;
name(b12) -> <<"PIN_B12">>;
name(b13) -> <<"PIN_B13">>;
name(b14) -> <<"PIN_B14">>;
name(b15) -> <<"PIN_B15">>;
name(b16) -> <<"PIN_B16">>;
name(b17) -> <<"PIN_B17">>;
name(b18) -> <<"PIN_B18">>;
name(b19) -> <<"PIN_B19">>;
name(b20) -> <<"PIN_B20">>;
name(c1) -> <<"PIN_C1">>;
name(c2) -> <<"PIN_C2">>;
name(c3) -> <<"PIN_C3">>;
name(c4) -> <<"PIN_C4">>;
name(c5) -> <<"PIN_C5">>;
name(c6) -> <<"PIN_C6">>;
name(c7) -> <<"PIN_C7">>;
name(c8) -> <<"PIN_C8">>;
name(c9) -> <<"PIN_C9">>;
name(c10) -> <<"PIN_C10">>;
name(c11) -> <<"PIN_C11">>;
name(c12) -> <<"PIN_C12">>;
name(c13) -> <<"PIN_C13">>;
name(c14) -> <<"PIN_C14">>;
name(c15) -> <<"PIN_C15">>;
name(c16) -> <<"PIN_C16">>;
name(c17) -> <<"PIN_C17">>;
name(c18) -> <<"PIN_C18">>;
name(c19) -> <<"PIN_C19">>;
name(c20) -> <<"PIN_C20">>;
name(d1) -> <<"PIN_D1">>;
name(d2) -> <<"PIN_D2">>;
name(d3) -> <<"PIN_D3">>;
name(d4) -> <<"PIN_D4">>;
name(d5) -> <<"PIN_D5">>;
name(d6) -> <<"PIN_D6">>;
name(d7) -> <<"PIN_D7">>;
name(d8) -> <<"PIN_D8">>;
name(d9) -> <<"PIN_D9">>;
name(d10) -> <<"PIN_D10">>;
name(d11) -> <<"PIN_D11">>;
name(d12) -> <<"PIN_D12">>;
name(d13) -> <<"PIN_D13">>;
name(d14) -> <<"PIN_D14">>;
name(d15) -> <<"PIN_D15">>;
name(d16) -> <<"PIN_D16">>;
name(d17) -> <<"PIN_D17">>;
name(d18) -> <<"PIN_D18">>;
name(d19) -> <<"PIN_D19">>;
name(d20) -> <<"PIN_D20">>;
name(e1) -> <<"PIN_E1">>;
name(e2) -> <<"PIN_E2">>;
name(e3) -> <<"PIN_E3">>;
name(e4) -> <<"PIN_E4">>;
name(e5) -> <<"PIN_E5">>;
name(e6) -> <<"PIN_E6">>;
name(e7) -> <<"PIN_E7">>;
name(e8) -> <<"PIN_E8">>;
name(e9) -> <<"PIN_E9">>;
name(e10) -> <<"PIN_E10">>;
name(e11) -> <<"PIN_E11">>;
name(e12) -> <<"PIN_E12">>;
name(e13) -> <<"PIN_E13">>;
name(e14) -> <<"PIN_E14">>;
name(e15) -> <<"PIN_E15">>;
name(e16) -> <<"PIN_E16">>;
name(e17) -> <<"PIN_E17">>;
name(e18) -> <<"PIN_E18">>;
name(e19) -> <<"PIN_E19">>;
name(e20) -> <<"PIN_E20">>;
name(f1) -> <<"PIN_F1">>;
name(f2) -> <<"PIN_F2">>;
name(f3) -> <<"PIN_F3">>;
name(f4) -> <<"PIN_F4">>;
name(f5) -> <<"PIN_F5">>;
name(f6) -> <<"PIN_F6">>;
name(f7) -> <<"PIN_F7">>;
name(f8) -> <<"PIN_F8">>;
name(f9) -> <<"PIN_F9">>;
name(f10) -> <<"PIN_F10">>;
name(f11) -> <<"PIN_F11">>;
name(f12) -> <<"PIN_F12">>;
name(f13) -> <<"PIN_F13">>;
name(f14) -> <<"PIN_F14">>;
name(f15) -> <<"PIN_F15">>;
name(f16) -> <<"PIN_F16">>;
name(f17) -> <<"PIN_F17">>;
name(f18) -> <<"PIN_F18">>;
name(f19) -> <<"PIN_F19">>;
name(f20) -> <<"PIN_F20">>;
name(g1) -> <<"PIN_G1">>;
name(g2) -> <<"PIN_G2">>;
name(g3) -> <<"PIN_G3">>;
name(g4) -> <<"PIN_G4">>;
name(g5) -> <<"PIN_G5">>;
name(g6) -> <<"PIN_G6">>;
name(g7) -> <<"PIN_G7">>;
name(g8) -> <<"PIN_G8">>;
name(g9) -> <<"PIN_G9">>;
name(g10) -> <<"PIN_G10">>;
name(g11) -> <<"PIN_G11">>;
name(g12) -> <<"PIN_G12">>;
name(g13) -> <<"PIN_G13">>;
name(g14) -> <<"PIN_G14">>;
name(g15) -> <<"PIN_G15">>;
name(g16) -> <<"PIN_G16">>;
name(g17) -> <<"PIN_G17">>;
name(g18) -> <<"PIN_G18">>;
name(g19) -> <<"PIN_G19">>;
name(g20) -> <<"PIN_G20">>;
name(h1) -> <<"PIN_H1">>;
name(h2) -> <<"PIN_H2">>;
name(h3) -> <<"PIN_H3">>;
name(h4) -> <<"PIN_H4">>;
name(h5) -> <<"PIN_H5">>;
name(h6) -> <<"PIN_H6">>;
name(h7) -> <<"PIN_H7">>;
name(h8) -> <<"PIN_H8">>;
name(h9) -> <<"PIN_H9">>;
name(h10) -> <<"PIN_H10">>;
name(h11) -> <<"PIN_H11">>;
name(h12) -> <<"PIN_H12">>;
name(h13) -> <<"PIN_H13">>;
name(h14) -> <<"PIN_H14">>;
name(h15) -> <<"PIN_H15">>;
name(h16) -> <<"PIN_H16">>;
name(h17) -> <<"PIN_H17">>;
name(h18) -> <<"PIN_H18">>;
name(h19) -> <<"PIN_H19">>;
name(h20) -> <<"PIN_H20">>;
name(j1) -> <<"PIN_J1">>;
name(j2) -> <<"PIN_J2">>;
name(j3) -> <<"PIN_J3">>;
name(j4) -> <<"PIN_J4">>;
name(j5) -> <<"PIN_J5">>;
name(j6) -> <<"PIN_J6">>;
name(j7) -> <<"PIN_J7">>;
name(j8) -> <<"PIN_J8">>;
name(j9) -> <<"PIN_J9">>;
name(j10) -> <<"PIN_J10">>;
name(j11) -> <<"PIN_J11">>;
name(j12) -> <<"PIN_J12">>;
name(j13) -> <<"PIN_J13">>;
name(j14) -> <<"PIN_J14">>;
name(j15) -> <<"PIN_J15">>;
name(j16) -> <<"PIN_J16">>;
name(j17) -> <<"PIN_J17">>;
name(j18) -> <<"PIN_J18">>;
name(j19) -> <<"PIN_J19">>;
name(j20) -> <<"PIN_J20">>;
name(k1) -> <<"PIN_K1">>;
name(k2) -> <<"PIN_K2">>;
name(k3) -> <<"PIN_K3">>;
name(k4) -> <<"PIN_K4">>;
name(k5) -> <<"PIN_K5">>;
name(k6) -> <<"PIN_K6">>;
name(k7) -> <<"PIN_K7">>;
name(k8) -> <<"PIN_K8">>;
name(k9) -> <<"PIN_K9">>;
name(k10) -> <<"PIN_K10">>;
name(k11) -> <<"PIN_K11">>;
name(k12) -> <<"PIN_K12">>;
name(k13) -> <<"PIN_K13">>;
name(k14) -> <<"PIN_K14">>;
name(k15) -> <<"PIN_K15">>;
name(k16) -> <<"PIN_K16">>;
name(k17) -> <<"PIN_K17">>;
name(k18) -> <<"PIN_K18">>;
name(k19) -> <<"PIN_K19">>;
name(k20) -> <<"PIN_K20">>;
name(l1) -> <<"PIN_L1">>;
name(l2) -> <<"PIN_L2">>;
name(l3) -> <<"PIN_L3">>;
name(l4) -> <<"PIN_L4">>;
name(l5) -> <<"PIN_L5">>;
name(l6) -> <<"PIN_L6">>;
name(l7) -> <<"PIN_L7">>;
name(l8) -> <<"PIN_L8">>;
name(l9) -> <<"PIN_L9">>;
name(l10) -> <<"PIN_L10">>;
name(l11) -> <<"PIN_L11">>;
name(l12) -> <<"PIN_L12">>;
name(l13) -> <<"PIN_L13">>;
name(l14) -> <<"PIN_L14">>;
name(l15) -> <<"PIN_L15">>;
name(l16) -> <<"PIN_L16">>;
name(l17) -> <<"PIN_L17">>;
name(l18) -> <<"PIN_L18">>;
name(l19) -> <<"PIN_L19">>;
name(l20) -> <<"PIN_L20">>;
name(m1) -> <<"PIN_M1">>;
name(m2) -> <<"PIN_M2">>;
name(m3) -> <<"PIN_M3">>;
name(m4) -> <<"PIN_M4">>;
name(m5) -> <<"PIN_M5">>;
name(m6) -> <<"PIN_M6">>;
name(m7) -> <<"PIN_M7">>;
name(m8) -> <<"PIN_M8">>;
name(m9) -> <<"PIN_M9">>;
name(m10) -> <<"PIN_M10">>;
name(m11) -> <<"PIN_M11">>;
name(m12) -> <<"PIN_M12">>;
name(m13) -> <<"PIN_M13">>;
name(m14) -> <<"PIN_M14">>;
name(m15) -> <<"PIN_M15">>;
name(m16) -> <<"PIN_M16">>;
name(m17) -> <<"PIN_M17">>;
name(m18) -> <<"PIN_M18">>;
name(m19) -> <<"PIN_M19">>;
name(m20) -> <<"PIN_M20">>;
name(n1) -> <<"PIN_N1">>;
name(n2) -> <<"PIN_N2">>;
name(n3) -> <<"PIN_N3">>;
name(n4) -> <<"PIN_N4">>;
name(n5) -> <<"PIN_N5">>;
name(n6) -> <<"PIN_N6">>;
name(n7) -> <<"PIN_N7">>;
name(n8) -> <<"PIN_N8">>;
name(n9) -> <<"PIN_N9">>;
name(n10) -> <<"PIN_N10">>;
name(n11) -> <<"PIN_N11">>;
name(n12) -> <<"PIN_N12">>;
name(n13) -> <<"PIN_N13">>;
name(n14) -> <<"PIN_N14">>;
name(n15) -> <<"PIN_N15">>;
name(n16) -> <<"PIN_N16">>;
name(n17) -> <<"PIN_N17">>;
name(n18) -> <<"PIN_N18">>;
name(n19) -> <<"PIN_N19">>;
name(n20) -> <<"PIN_N20">>;
name(p1) -> <<"PIN_P1">>;
name(p2) -> <<"PIN_P2">>;
name(p3) -> <<"PIN_P3">>;
name(p4) -> <<"PIN_P4">>;
name(p5) -> <<"PIN_P5">>;
name(p6) -> <<"PIN_P6">>;
name(p7) -> <<"PIN_P7">>;
name(p8) -> <<"PIN_P8">>;
name(p9) -> <<"PIN_P9">>;
name(p10) -> <<"PIN_P10">>;
name(p11) -> <<"PIN_P11">>;
name(p12) -> <<"PIN_P12">>;
name(p13) -> <<"PIN_P13">>;
name(p14) -> <<"PIN_P14">>;
name(p15) -> <<"PIN_P15">>;
name(p16) -> <<"PIN_P16">>;
name(p17) -> <<"PIN_P17">>;
name(p18) -> <<"PIN_P18">>;
name(p19) -> <<"PIN_P19">>;
name(p20) -> <<"PIN_P20">>;
name(r1) -> <<"PIN_R1">>;
name(r2) -> <<"PIN_R2">>;
name(r3) -> <<"PIN_R3">>;
name(r4) -> <<"PIN_R4">>;
name(r5) -> <<"PIN_R5">>;
name(r6) -> <<"PIN_R6">>;
name(r7) -> <<"PIN_R7">>;
name(r8) -> <<"PIN_R8">>;
name(r9) -> <<"PIN_R9">>;
name(r10) -> <<"PIN_R10">>;
name(r11) -> <<"PIN_R11">>;
name(r12) -> <<"PIN_R12">>;
name(r13) -> <<"PIN_R13">>;
name(r14) -> <<"PIN_R14">>;
name(r15) -> <<"PIN_R15">>;
name(r16) -> <<"PIN_R16">>;
name(r17) -> <<"PIN_R17">>;
name(r18) -> <<"PIN_R18">>;
name(r19) -> <<"PIN_R19">>;
name(r20) -> <<"PIN_R20">>;
name(t1) -> <<"PIN_T1">>;
name(t2) -> <<"PIN_T2">>;
name(t3) -> <<"PIN_T3">>;
name(t4) -> <<"PIN_T4">>;
name(t5) -> <<"PIN_T5">>;
name(t6) -> <<"PIN_T6">>;
name(t7) -> <<"PIN_T7">>;
name(t8) -> <<"PIN_T8">>;
name(t9) -> <<"PIN_T9">>;
name(t10) -> <<"PIN_T10">>;
name(t11) -> <<"PIN_T11">>;
name(t12) -> <<"PIN_T12">>;
name(t13) -> <<"PIN_T13">>;
name(t14) -> <<"PIN_T14">>;
name(t15) -> <<"PIN_T15">>;
name(t16) -> <<"PIN_T16">>;
name(t17) -> <<"PIN_T17">>;
name(t18) -> <<"PIN_T18">>;
name(t19) -> <<"PIN_T19">>;
name(t20) -> <<"PIN_T20">>;
name(u1) -> <<"PIN_U1">>;
name(u3) -> <<"PIN_U3">>;
name(u4) -> <<"PIN_U4">>;
name(u5) -> <<"PIN_U5">>;
name(u6) -> <<"PIN_U6">>;
name(u7) -> <<"PIN_U7">>;
name(u8) -> <<"PIN_U8">>;
name(u9) -> <<"PIN_U9">>;
name(u10) -> <<"PIN_U10">>;
name(u11) -> <<"PIN_U11">>;
name(u12) -> <<"PIN_U12">>;
name(u13) -> <<"PIN_U13">>;
name(u14) -> <<"PIN_U14">>;
name(u15) -> <<"PIN_U15">>;
name(u16) -> <<"PIN_U16">>;
name(u17) -> <<"PIN_U17">>;
name(u18) -> <<"PIN_U18">>;
name(u19) -> <<"PIN_U19">>;
name(u20) -> <<"PIN_U20">>;
name(v1) -> <<"PIN_V1">>;
name(v2) -> <<"PIN_V2">>;
name(v3) -> <<"PIN_V3">>;
name(v4) -> <<"PIN_V4">>;
name(v5) -> <<"PIN_V5">>;
name(v6) -> <<"PIN_V6">>;
name(v7) -> <<"PIN_V7">>;
name(v8) -> <<"PIN_V8">>;
name(v9) -> <<"PIN_V9">>;
name(v10) -> <<"PIN_V10">>;
name(v11) -> <<"PIN_V11">>;
name(v12) -> <<"PIN_V12">>;
name(v13) -> <<"PIN_V13">>;
name(v14) -> <<"PIN_V14">>;
name(v15) -> <<"PIN_V15">>;
name(v16) -> <<"PIN_V16">>;
name(v17) -> <<"PIN_V17">>;
name(v18) -> <<"PIN_V18">>;
name(v19) -> <<"PIN_V19">>;
name(v20) -> <<"PIN_V20">>;
name(w1) -> <<"PIN_W1">>;
name(w3) -> <<"PIN_W3">>;
name(w4) -> <<"PIN_W4">>;
name(w5) -> <<"PIN_W5">>;
name(w6) -> <<"PIN_W6">>;
name(w7) -> <<"PIN_W7">>;
name(w8) -> <<"PIN_W8">>;
name(w9) -> <<"PIN_W9">>;
name(w10) -> <<"PIN_W10">>;
name(w11) -> <<"PIN_W11">>;
name(w12) -> <<"PIN_W12">>;
name(w13) -> <<"PIN_W13">>;
name(w14) -> <<"PIN_W14">>;
name(w15) -> <<"PIN_W15">>;
name(w16) -> <<"PIN_W16">>;
name(w17) -> <<"PIN_W17">>;
name(w18) -> <<"PIN_W18">>;
name(w19) -> <<"PIN_W19">>;
name(w20) -> <<"PIN_W20">>;
name(y1) -> <<"PIN_Y1">>;
name(y2) -> <<"PIN_Y2">>;
name(y3) -> <<"PIN_Y3">>;
name(y4) -> <<"PIN_Y4">>;
name(y5) -> <<"PIN_Y5">>;
name(y6) -> <<"PIN_Y6">>;
name(y7) -> <<"PIN_Y7">>;
name(y8) -> <<"PIN_Y8">>;
name(y9) -> <<"PIN_Y9">>;
name(y10) -> <<"PIN_Y10">>;
name(y11) -> <<"PIN_Y11">>;
name(y12) -> <<"PIN_Y12">>;
name(y13) -> <<"PIN_Y13">>;
name(y14) -> <<"PIN_Y14">>;
name(y15) -> <<"PIN_Y15">>;
name(y16) -> <<"PIN_Y16">>;
name(y17) -> <<"PIN_Y17">>;
name(y18) -> <<"PIN_Y18">>;
name(y19) -> <<"PIN_Y19">>;
name(y20) -> <<"PIN_Y20">>;
name(pin1) -> <<"PIN_1">>;
name(pin2) -> <<"PIN_2">>;
name(pin3) -> <<"PIN_3">>;
name(pin4) -> <<"PIN_4">>;
name(pin5) -> <<"PIN_5">>;
name(pin6) -> <<"PIN_6">>;
name(pin7) -> <<"PIN_7">>;
name(pin8) -> <<"PIN_8">>;
name(pin11) -> <<"PIN_11">>;
name(pin12) -> <<"PIN_12">>;
name(pin13) -> <<"PIN_13">>;
name(pin14) -> <<"PIN_14">>;
name(pin15) -> <<"PIN_15">>;
name(pin16) -> <<"PIN_16">>;
name(pin17) -> <<"PIN_17">>;
name(pin18) -> <<"PIN_18">>;
name(pin19) -> <<"PIN_19">>;
name(pin20) -> <<"PIN_20">>;
name(pin21) -> <<"PIN_21">>;
name(pin22) -> <<"PIN_22">>;
name(pin23) -> <<"PIN_23">>;
name(pin24) -> <<"PIN_24">>;
name(pin26) -> <<"PIN_26">>;
name(pin27) -> <<"PIN_27">>;
name(pin28) -> <<"PIN_28">>;
name(pin29) -> <<"PIN_29">>;
name(pin30) -> <<"PIN_30">>;
name(pin31) -> <<"PIN_31">>;
name(pin32) -> <<"PIN_32">>;
name(pin33) -> <<"PIN_33">>;
name(pin34) -> <<"PIN_34">>;
name(pin35) -> <<"PIN_35">>;
name(pin36) -> <<"PIN_36">>;
name(pin37) -> <<"PIN_37">>;
name(pin38) -> <<"PIN_38">>;
name(pin39) -> <<"PIN_39">>;
name(pin40) -> <<"PIN_40">>;
name(pin41) -> <<"PIN_41">>;
name(pin42) -> <<"PIN_42">>;
name(pin43) -> <<"PIN_43">>;
name(pin44) -> <<"PIN_44">>;
name(pin45) -> <<"PIN_45">>;
name(pin47) -> <<"PIN_47">>;
name(pin48) -> <<"PIN_48">>;
name(pin49) -> <<"PIN_49">>;
name(pin50) -> <<"PIN_50">>;
name(pin51) -> <<"PIN_51">>;
name(pin52) -> <<"PIN_52">>;
name(pin53) -> <<"PIN_53">>;
name(pin54) -> <<"PIN_54">>;
name(pin55) -> <<"PIN_55">>;
name(pin56) -> <<"PIN_56">>;
name(pin57) -> <<"PIN_57">>;
name(pin58) -> <<"PIN_58">>;
name(pin59) -> <<"PIN_59">>;
name(pin60) -> <<"PIN_60">>;
name(pin61) -> <<"PIN_61">>;
name(pin62) -> <<"PIN_62">>;
name(pin63) -> <<"PIN_63">>;
name(pin64) -> <<"PIN_64">>;
name(pin66) -> <<"PIN_66">>;
name(pin67) -> <<"PIN_67">>;
name(pin68) -> <<"PIN_68">>;
name(pin69) -> <<"PIN_69">>;
name(pin70) -> <<"PIN_70">>;
name(pin71) -> <<"PIN_71">>;
name(pin72) -> <<"PIN_72">>;
name(pin73) -> <<"PIN_73">>;
name(pin74) -> <<"PIN_74">>;
name(pin75) -> <<"PIN_75">>;
name(pin76) -> <<"PIN_76">>;
name(pin77) -> <<"PIN_77">>;
name(pin78) -> <<"PIN_78">>;
name(pin79) -> <<"PIN_79">>;
name(pin80) -> <<"PIN_80">>;
name(pin81) -> <<"PIN_81">>;
name(pin82) -> <<"PIN_82">>;
name(pin83) -> <<"PIN_83">>;
name(pin84) -> <<"PIN_84">>;
name(pin85) -> <<"PIN_85">>;
name(pin86) -> <<"PIN_86">>;
name(pin87) -> <<"PIN_87">>;
name(pin88) -> <<"PIN_88">>;
name(pin89) -> <<"PIN_89">>;
name(pin90) -> <<"PIN_90">>;
name(pin91) -> <<"PIN_91">>;
name(pin92) -> <<"PIN_92">>;
name(pin93) -> <<"PIN_93">>;
name(pin94) -> <<"PIN_94">>;
name(pin95) -> <<"PIN_95">>;
name(pin96) -> <<"PIN_96">>;
name(pin97) -> <<"PIN_97">>;
name(pin98) -> <<"PIN_98">>;
name(pin99) -> <<"PIN_99">>;
name(pin100) -> <<"PIN_100">>;
name(pin101) -> <<"PIN_101">>;
name(pin102) -> <<"PIN_102">>;
name(pin103) -> <<"PIN_103">>;
name(pin104) -> <<"PIN_104">>;
name(pin105) -> <<"PIN_105">>;
name(pin106) -> <<"PIN_106">>;
name(pin107) -> <<"PIN_107">>;
name(pin108) -> <<"PIN_108">>;
name(pin109) -> <<"PIN_109">>;
name(pin110) -> <<"PIN_110">>;
name(pin111) -> <<"PIN_111">>;
name(pin112) -> <<"PIN_112">>;
name(pin113) -> <<"PIN_113">>;
name(pin114) -> <<"PIN_114">>;
name(pin117) -> <<"PIN_117">>;
name(pin118) -> <<"PIN_118">>;
name(pin119) -> <<"PIN_119">>;
name(pin120) -> <<"PIN_120">>;
name(pin121) -> <<"PIN_121">>;
name(pin122) -> <<"PIN_122">>;
name(pin123) -> <<"PIN_123">>;
name(pin124) -> <<"PIN_124">>;
name(pin125) -> <<"PIN_125">>;
name(pin127) -> <<"PIN_127">>;
name(pin129) -> <<"PIN_129">>;
name(pin130) -> <<"PIN_130">>;
name(pin131) -> <<"PIN_131">>;
name(pin132) -> <<"PIN_132">>;
name(pin133) -> <<"PIN_133">>;
name(pin134) -> <<"PIN_134">>;
name(pin137) -> <<"PIN_137">>;
name(pin138) -> <<"PIN_138">>;
name(pin139) -> <<"PIN_139">>;
name(pin140) -> <<"PIN_140">>;
name(pin141) -> <<"PIN_141">>;
name(pin142) -> <<"PIN_142">>;
name(pin143) -> <<"PIN_143">>;
name(pin144) -> <<"PIN_144">>.

