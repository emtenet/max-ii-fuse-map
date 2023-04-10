-module(epm1270_t144).

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
    [{pin1,{ioc,0,10,2}},
     {pin2,{ioc,0,10,3}},
     {pin3,{ioc,0,10,5}},
     {pin4,{ioc,0,9,0}},
     {pin5,{ioc,0,9,2}},
     {pin6,{ioc,0,9,3}},
     {pin7,{ioc,0,8,0}},
     {pin8,{ioc,0,8,1}},
     {pin11,{ioc,0,8,6}},
     {pin12,{ioc,0,7,0}},
     {pin13,{ioc,0,7,1}},
     {pin14,{ioc,0,7,2}},
     {pin15,{ioc,0,7,3}},
     {pin16,{ioc,0,7,4}},
     {pin18,{ioc,0,7,5}},
     {pin20,{ioc,0,7,6}},
     {pin21,{ioc,0,6,0}},
     {pin22,{ioc,0,6,1}},
     {pin23,{ioc,0,6,2}},
     {pin24,{ioc,0,6,3}},
     {pin27,{ioc,0,6,5}},
     {pin28,{ioc,0,5,1}},
     {pin29,{ioc,0,5,2}},
     {pin30,{ioc,0,5,6}},
     {pin31,{ioc,0,4,2}},
     {pin32,{ioc,0,4,5}},
     {pin37,{ioc,1,3,0}},
     {pin38,{ioc,2,3,3}},
     {pin39,{ioc,2,3,0}},
     {pin40,{ioc,3,3,1}},
     {pin41,{ioc,3,3,0}},
     {pin42,{ioc,4,3,0}},
     {pin43,{ioc,5,3,3}},
     {pin44,{ioc,5,3,2}},
     {pin45,{ioc,6,3,0}},
     {pin48,{ioc,7,3,3}},
     {pin49,{ioc,7,3,2}},
     {pin50,{ioc,7,3,1}},
     {pin51,{ioc,7,3,0}},
     {pin52,{ioc,8,3,3}},
     {pin53,{ioc,8,3,2}},
     {pin55,{ioc,8,3,1}},
     {pin57,{ioc,8,3,0}},
     {pin58,{ioc,9,3,2}},
     {pin59,{ioc,9,3,1}},
     {pin60,{ioc,9,3,0}},
     {pin61,{ioc,10,3,3}},
     {pin62,{ioc,10,3,2}},
     {pin63,{ioc,10,3,1}},
     {pin66,{ioc,12,0,1}},
     {pin67,{ioc,12,0,0}},
     {pin68,{ioc,13,0,1}},
     {pin69,{ioc,14,0,2}},
     {pin70,{ioc,15,0,2}},
     {pin71,{ioc,16,0,3}},
     {pin72,{ioc,16,0,0}},
     {pin73,{ioc,17,1,2}},
     {pin74,{ioc,17,1,1}},
     {pin75,{ioc,17,1,0}},
     {pin76,{ioc,17,2,3}},
     {pin77,{ioc,17,2,0}},
     {pin78,{ioc,17,3,2}},
     {pin79,{ioc,17,3,1}},
     {pin80,{ioc,17,3,0}},
     {pin81,{ioc,17,4,2}},
     {pin84,{ioc,17,4,0}},
     {pin85,{ioc,17,5,5}},
     {pin86,{ioc,17,5,4}},
     {pin87,{ioc,17,5,3}},
     {pin88,{ioc,17,5,2}},
     {pin89,{ioc,17,5,1}},
     {pin91,{ioc,17,5,0}},
     {pin93,{ioc,17,6,5}},
     {pin94,{ioc,17,6,4}},
     {pin95,{ioc,17,6,3}},
     {pin96,{ioc,17,6,2}},
     {pin97,{ioc,17,6,1}},
     {pin98,{ioc,17,6,0}},
     {pin101,{ioc,17,7,4}},
     {pin102,{ioc,17,7,0}},
     {pin103,{ioc,17,8,5}},
     {pin104,{ioc,17,8,1}},
     {pin105,{ioc,17,8,0}},
     {pin106,{ioc,17,9,3}},
     {pin107,{ioc,17,9,1}},
     {pin108,{ioc,17,10,4}},
     {pin109,{ioc,16,11,1}},
     {pin110,{ioc,16,11,2}},
     {pin111,{ioc,15,11,0}},
     {pin112,{ioc,14,11,1}},
     {pin113,{ioc,13,11,1}},
     {pin114,{ioc,12,11,1}},
     {pin117,{ioc,11,11,1}},
     {pin118,{ioc,11,11,2}},
     {pin119,{ioc,11,11,3}},
     {pin120,{ioc,10,11,0}},
     {pin121,{ioc,10,11,1}},
     {pin122,{ioc,10,11,2}},
     {pin123,{ioc,9,11,0}},
     {pin124,{ioc,9,11,1}},
     {pin125,{ioc,9,11,2}},
     {pin127,{ioc,9,11,3}},
     {pin129,{ioc,8,11,0}},
     {pin130,{ioc,8,11,1}},
     {pin131,{ioc,8,11,2}},
     {pin132,{ioc,7,11,0}},
     {pin133,{ioc,7,11,1}},
     {pin134,{ioc,7,11,2}},
     {pin137,{ioc,6,11,3}},
     {pin138,{ioc,5,11,0}},
     {pin139,{ioc,5,11,1}},
     {pin140,{ioc,4,11,1}},
     {pin141,{ioc,4,11,2}},
     {pin142,{ioc,3,11,2}},
     {pin143,{ioc,2,11,3}},
     {pin144,{ioc,1,11,2}}
    ].

-spec pins() -> [pin()].

pins() ->
    [pin1,
     pin2,
     pin3,
     pin4,
     pin5,
     pin6,
     pin7,
     pin8,
     pin11,
     pin12,
     pin13,
     pin14,
     pin15,
     pin16,
     pin18,
     pin20,
     pin21,
     pin22,
     pin23,
     pin24,
     pin27,
     pin28,
     pin29,
     pin30,
     pin31,
     pin32,
     pin37,
     pin38,
     pin39,
     pin40,
     pin41,
     pin42,
     pin43,
     pin44,
     pin45,
     pin48,
     pin49,
     pin50,
     pin51,
     pin52,
     pin53,
     pin55,
     pin57,
     pin58,
     pin59,
     pin60,
     pin61,
     pin62,
     pin63,
     pin66,
     pin67,
     pin68,
     pin69,
     pin70,
     pin71,
     pin72,
     pin73,
     pin74,
     pin75,
     pin76,
     pin77,
     pin78,
     pin79,
     pin80,
     pin81,
     pin84,
     pin85,
     pin86,
     pin87,
     pin88,
     pin89,
     pin91,
     pin93,
     pin94,
     pin95,
     pin96,
     pin97,
     pin98,
     pin101,
     pin102,
     pin103,
     pin104,
     pin105,
     pin106,
     pin107,
     pin108,
     pin109,
     pin110,
     pin111,
     pin112,
     pin113,
     pin114,
     pin117,
     pin118,
     pin119,
     pin120,
     pin121,
     pin122,
     pin123,
     pin124,
     pin125,
     pin127,
     pin129,
     pin130,
     pin131,
     pin132,
     pin133,
     pin134,
     pin137,
     pin138,
     pin139,
     pin140,
     pin141,
     pin142,
     pin143,
     pin144
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(1) ->
    [{pin144,{ioc,1,11,2}}];
top_iocs(2) ->
    [{pin143,{ioc,2,11,3}}];
top_iocs(3) ->
    [{pin142,{ioc,3,11,2}}];
top_iocs(4) ->
    [{pin140,{ioc,4,11,1}},
     {pin141,{ioc,4,11,2}}];
top_iocs(5) ->
    [{pin138,{ioc,5,11,0}},
     {pin139,{ioc,5,11,1}}];
top_iocs(6) ->
    [{pin137,{ioc,6,11,3}}];
top_iocs(7) ->
    [{pin132,{ioc,7,11,0}},
     {pin133,{ioc,7,11,1}},
     {pin134,{ioc,7,11,2}}];
top_iocs(8) ->
    [{pin129,{ioc,8,11,0}},
     {pin130,{ioc,8,11,1}},
     {pin131,{ioc,8,11,2}}];
top_iocs(9) ->
    [{pin123,{ioc,9,11,0}},
     {pin124,{ioc,9,11,1}},
     {pin125,{ioc,9,11,2}},
     {pin127,{ioc,9,11,3}}];
top_iocs(10) ->
    [{pin120,{ioc,10,11,0}},
     {pin121,{ioc,10,11,1}},
     {pin122,{ioc,10,11,2}}];
top_iocs(11) ->
    [{pin117,{ioc,11,11,1}},
     {pin118,{ioc,11,11,2}},
     {pin119,{ioc,11,11,3}}];
top_iocs(12) ->
    [{pin114,{ioc,12,11,1}}];
top_iocs(13) ->
    [{pin113,{ioc,13,11,1}}];
top_iocs(14) ->
    [{pin112,{ioc,14,11,1}}];
top_iocs(15) ->
    [{pin111,{ioc,15,11,0}}];
top_iocs(16) ->
    [{pin109,{ioc,16,11,1}},
     {pin110,{ioc,16,11,2}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin144];
top_pins(2) ->
    [pin143];
top_pins(3) ->
    [pin142];
top_pins(4) ->
    [pin140, pin141];
top_pins(5) ->
    [pin138, pin139];
top_pins(6) ->
    [pin137];
top_pins(7) ->
    [pin132, pin133, pin134];
top_pins(8) ->
    [pin129, pin130, pin131];
top_pins(9) ->
    [pin123, pin124, pin125, pin127];
top_pins(10) ->
    [pin120, pin121, pin122];
top_pins(11) ->
    [pin117, pin118, pin119];
top_pins(12) ->
    [pin114];
top_pins(13) ->
    [pin113];
top_pins(14) ->
    [pin112];
top_pins(15) ->
    [pin111];
top_pins(16) ->
    [pin109, pin110].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{pin31,{ioc,0,4,2}},
     {pin32,{ioc,0,4,5}}];
left_iocs(5) ->
    [{pin28,{ioc,0,5,1}},
     {pin29,{ioc,0,5,2}},
     {pin30,{ioc,0,5,6}}];
left_iocs(6) ->
    [{pin21,{ioc,0,6,0}},
     {pin22,{ioc,0,6,1}},
     {pin23,{ioc,0,6,2}},
     {pin24,{ioc,0,6,3}},
     {pin27,{ioc,0,6,5}}];
left_iocs(7) ->
    [{pin12,{ioc,0,7,0}},
     {pin13,{ioc,0,7,1}},
     {pin14,{ioc,0,7,2}},
     {pin15,{ioc,0,7,3}},
     {pin16,{ioc,0,7,4}},
     {pin18,{ioc,0,7,5}},
     {pin20,{ioc,0,7,6}}];
left_iocs(8) ->
    [{pin7,{ioc,0,8,0}},
     {pin8,{ioc,0,8,1}},
     {pin11,{ioc,0,8,6}}];
left_iocs(9) ->
    [{pin4,{ioc,0,9,0}},
     {pin5,{ioc,0,9,2}},
     {pin6,{ioc,0,9,3}}];
left_iocs(10) ->
    [{pin1,{ioc,0,10,2}},
     {pin2,{ioc,0,10,3}},
     {pin3,{ioc,0,10,5}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [pin31, pin32];
left_pins(5) ->
    [pin28, pin29, pin30];
left_pins(6) ->
    [pin21, pin22, pin23, pin24, pin27];
left_pins(7) ->
    [pin12, pin13, pin14, pin15, pin16, pin18, pin20];
left_pins(8) ->
    [pin7, pin8, pin11];
left_pins(9) ->
    [pin4, pin5, pin6];
left_pins(10) ->
    [pin1, pin2, pin3].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{pin75,{ioc,17,1,0}},
     {pin74,{ioc,17,1,1}},
     {pin73,{ioc,17,1,2}}];
right_iocs(2) ->
    [{pin77,{ioc,17,2,0}},
     {pin76,{ioc,17,2,3}}];
right_iocs(3) ->
    [{pin80,{ioc,17,3,0}},
     {pin79,{ioc,17,3,1}},
     {pin78,{ioc,17,3,2}}];
right_iocs(4) ->
    [{pin84,{ioc,17,4,0}},
     {pin81,{ioc,17,4,2}}];
right_iocs(5) ->
    [{pin91,{ioc,17,5,0}},
     {pin89,{ioc,17,5,1}},
     {pin88,{ioc,17,5,2}},
     {pin87,{ioc,17,5,3}},
     {pin86,{ioc,17,5,4}},
     {pin85,{ioc,17,5,5}}];
right_iocs(6) ->
    [{pin98,{ioc,17,6,0}},
     {pin97,{ioc,17,6,1}},
     {pin96,{ioc,17,6,2}},
     {pin95,{ioc,17,6,3}},
     {pin94,{ioc,17,6,4}},
     {pin93,{ioc,17,6,5}}];
right_iocs(7) ->
    [{pin102,{ioc,17,7,0}},
     {pin101,{ioc,17,7,4}}];
right_iocs(8) ->
    [{pin105,{ioc,17,8,0}},
     {pin104,{ioc,17,8,1}},
     {pin103,{ioc,17,8,5}}];
right_iocs(9) ->
    [{pin107,{ioc,17,9,1}},
     {pin106,{ioc,17,9,3}}];
right_iocs(10) ->
    [{pin108,{ioc,17,10,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin73, pin74, pin75];
right_pins(2) ->
    [pin76, pin77];
right_pins(3) ->
    [pin78, pin79, pin80];
right_pins(4) ->
    [pin81, pin84];
right_pins(5) ->
    [pin85, pin86, pin87, pin88, pin89, pin91];
right_pins(6) ->
    [pin93, pin94, pin95, pin96, pin97, pin98];
right_pins(7) ->
    [pin101, pin102];
right_pins(8) ->
    [pin103, pin104, pin105];
right_pins(9) ->
    [pin106, pin107];
right_pins(10) ->
    [pin108].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{pin37,{ioc,1,3,0}}];
bottom_iocs(2) ->
    [{pin39,{ioc,2,3,0}},
     {pin38,{ioc,2,3,3}}];
bottom_iocs(3) ->
    [{pin41,{ioc,3,3,0}},
     {pin40,{ioc,3,3,1}}];
bottom_iocs(4) ->
    [{pin42,{ioc,4,3,0}}];
bottom_iocs(5) ->
    [{pin44,{ioc,5,3,2}},
     {pin43,{ioc,5,3,3}}];
bottom_iocs(6) ->
    [{pin45,{ioc,6,3,0}}];
bottom_iocs(7) ->
    [{pin51,{ioc,7,3,0}},
     {pin50,{ioc,7,3,1}},
     {pin49,{ioc,7,3,2}},
     {pin48,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{pin57,{ioc,8,3,0}},
     {pin55,{ioc,8,3,1}},
     {pin53,{ioc,8,3,2}},
     {pin52,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [{pin60,{ioc,9,3,0}},
     {pin59,{ioc,9,3,1}},
     {pin58,{ioc,9,3,2}}];
bottom_iocs(10) ->
    [{pin63,{ioc,10,3,1}},
     {pin62,{ioc,10,3,2}},
     {pin61,{ioc,10,3,3}}];
bottom_iocs(11) ->
    [];
bottom_iocs(12) ->
    [{pin67,{ioc,12,0,0}},
     {pin66,{ioc,12,0,1}}];
bottom_iocs(13) ->
    [{pin68,{ioc,13,0,1}}];
bottom_iocs(14) ->
    [{pin69,{ioc,14,0,2}}];
bottom_iocs(15) ->
    [{pin70,{ioc,15,0,2}}];
bottom_iocs(16) ->
    [{pin72,{ioc,16,0,0}},
     {pin71,{ioc,16,0,3}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin37];
bottom_pins(2) ->
    [pin38, pin39];
bottom_pins(3) ->
    [pin40, pin41];
bottom_pins(4) ->
    [pin42];
bottom_pins(5) ->
    [pin43, pin44];
bottom_pins(6) ->
    [pin45];
bottom_pins(7) ->
    [pin48, pin49, pin50, pin51];
bottom_pins(8) ->
    [pin52, pin53, pin55, pin57];
bottom_pins(9) ->
    [pin58, pin59, pin60];
bottom_pins(10) ->
    [pin61, pin62, pin63];
bottom_pins(11) ->
    [];
bottom_pins(12) ->
    [pin66, pin67];
bottom_pins(13) ->
    [pin68];
bottom_pins(14) ->
    [pin69];
bottom_pins(15) ->
    [pin70];
bottom_pins(16) ->
    [pin71, pin72].

