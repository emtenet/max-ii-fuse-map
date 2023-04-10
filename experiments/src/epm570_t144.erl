-module(epm570_t144).

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
    [{pin1,{ioc,2,8,3}},
     {pin2,{ioc,1,8,0}},
     {pin3,{ioc,1,8,2}},
     {pin4,{ioc,0,7,0}},
     {pin5,{ioc,0,7,2}},
     {pin6,{ioc,0,7,3}},
     {pin7,{ioc,0,7,4}},
     {pin8,{ioc,0,7,5}},
     {pin11,{ioc,0,6,1}},
     {pin12,{ioc,0,6,2}},
     {pin13,{ioc,0,6,3}},
     {pin14,{ioc,0,6,4}},
     {pin15,{ioc,0,6,5}},
     {pin16,{ioc,0,6,6}},
     {pin18,{ioc,0,5,0}},
     {pin20,{ioc,0,5,1}},
     {pin21,{ioc,0,5,2}},
     {pin22,{ioc,0,5,3}},
     {pin23,{ioc,0,5,4}},
     {pin24,{ioc,0,5,5}},
     {pin27,{ioc,0,4,0}},
     {pin28,{ioc,0,4,2}},
     {pin29,{ioc,0,4,3}},
     {pin30,{ioc,0,4,5}},
     {pin31,{ioc,1,3,3}},
     {pin32,{ioc,1,3,1}},
     {pin37,{ioc,2,3,3}},
     {pin38,{ioc,2,3,2}},
     {pin39,{ioc,2,3,0}},
     {pin40,{ioc,3,3,2}},
     {pin41,{ioc,3,3,1}},
     {pin42,{ioc,4,3,3}},
     {pin43,{ioc,4,3,2}},
     {pin44,{ioc,4,3,1}},
     {pin45,{ioc,5,3,2}},
     {pin48,{ioc,5,3,1}},
     {pin49,{ioc,5,3,0}},
     {pin50,{ioc,6,3,3}},
     {pin51,{ioc,6,3,2}},
     {pin52,{ioc,6,3,1}},
     {pin53,{ioc,6,3,0}},
     {pin55,{ioc,7,3,3}},
     {pin57,{ioc,7,3,2}},
     {pin58,{ioc,7,3,1}},
     {pin59,{ioc,7,3,0}},
     {pin60,{ioc,8,3,3}},
     {pin61,{ioc,8,3,2}},
     {pin62,{ioc,8,3,1}},
     {pin63,{ioc,8,3,0}},
     {pin66,{ioc,10,0,2}},
     {pin67,{ioc,10,0,1}},
     {pin68,{ioc,10,0,0}},
     {pin69,{ioc,11,0,2}},
     {pin70,{ioc,11,0,1}},
     {pin71,{ioc,12,0,2}},
     {pin72,{ioc,12,0,0}},
     {pin73,{ioc,13,1,5}},
     {pin74,{ioc,13,1,4}},
     {pin75,{ioc,13,1,3}},
     {pin76,{ioc,13,1,1}},
     {pin77,{ioc,13,2,4}},
     {pin78,{ioc,13,2,2}},
     {pin79,{ioc,13,2,1}},
     {pin80,{ioc,13,2,0}},
     {pin81,{ioc,13,3,5}},
     {pin84,{ioc,13,3,3}},
     {pin85,{ioc,13,3,2}},
     {pin86,{ioc,13,3,1}},
     {pin87,{ioc,13,3,0}},
     {pin88,{ioc,13,4,5}},
     {pin89,{ioc,13,4,4}},
     {pin91,{ioc,13,4,3}},
     {pin93,{ioc,13,4,2}},
     {pin94,{ioc,13,4,1}},
     {pin95,{ioc,13,4,0}},
     {pin96,{ioc,13,5,5}},
     {pin97,{ioc,13,5,4}},
     {pin98,{ioc,13,5,3}},
     {pin101,{ioc,13,5,2}},
     {pin102,{ioc,13,5,0}},
     {pin103,{ioc,13,6,5}},
     {pin104,{ioc,13,6,2}},
     {pin105,{ioc,13,6,1}},
     {pin106,{ioc,13,7,5}},
     {pin107,{ioc,13,7,3}},
     {pin108,{ioc,13,7,1}},
     {pin109,{ioc,12,8,1}},
     {pin110,{ioc,12,8,2}},
     {pin111,{ioc,12,8,3}},
     {pin112,{ioc,11,8,1}},
     {pin113,{ioc,11,8,3}},
     {pin114,{ioc,10,8,1}},
     {pin117,{ioc,10,8,3}},
     {pin118,{ioc,9,8,0}},
     {pin119,{ioc,9,8,1}},
     {pin120,{ioc,9,8,2}},
     {pin121,{ioc,8,8,0}},
     {pin122,{ioc,8,8,1}},
     {pin123,{ioc,8,8,2}},
     {pin124,{ioc,8,8,3}},
     {pin125,{ioc,7,8,0}},
     {pin127,{ioc,7,8,1}},
     {pin129,{ioc,7,8,2}},
     {pin130,{ioc,7,8,3}},
     {pin131,{ioc,6,8,0}},
     {pin132,{ioc,6,8,1}},
     {pin133,{ioc,6,8,2}},
     {pin134,{ioc,6,8,3}},
     {pin137,{ioc,5,8,0}},
     {pin138,{ioc,5,8,1}},
     {pin139,{ioc,5,8,2}},
     {pin140,{ioc,4,8,1}},
     {pin141,{ioc,4,8,2}},
     {pin142,{ioc,3,8,0}},
     {pin143,{ioc,3,8,3}},
     {pin144,{ioc,2,8,1}}
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
    [{pin2,{ioc,1,8,0}},
     {pin3,{ioc,1,8,2}}];
top_iocs(2) ->
    [{pin144,{ioc,2,8,1}},
     {pin1,{ioc,2,8,3}}];
top_iocs(3) ->
    [{pin142,{ioc,3,8,0}},
     {pin143,{ioc,3,8,3}}];
top_iocs(4) ->
    [{pin140,{ioc,4,8,1}},
     {pin141,{ioc,4,8,2}}];
top_iocs(5) ->
    [{pin137,{ioc,5,8,0}},
     {pin138,{ioc,5,8,1}},
     {pin139,{ioc,5,8,2}}];
top_iocs(6) ->
    [{pin131,{ioc,6,8,0}},
     {pin132,{ioc,6,8,1}},
     {pin133,{ioc,6,8,2}},
     {pin134,{ioc,6,8,3}}];
top_iocs(7) ->
    [{pin125,{ioc,7,8,0}},
     {pin127,{ioc,7,8,1}},
     {pin129,{ioc,7,8,2}},
     {pin130,{ioc,7,8,3}}];
top_iocs(8) ->
    [{pin121,{ioc,8,8,0}},
     {pin122,{ioc,8,8,1}},
     {pin123,{ioc,8,8,2}},
     {pin124,{ioc,8,8,3}}];
top_iocs(9) ->
    [{pin118,{ioc,9,8,0}},
     {pin119,{ioc,9,8,1}},
     {pin120,{ioc,9,8,2}}];
top_iocs(10) ->
    [{pin114,{ioc,10,8,1}},
     {pin117,{ioc,10,8,3}}];
top_iocs(11) ->
    [{pin112,{ioc,11,8,1}},
     {pin113,{ioc,11,8,3}}];
top_iocs(12) ->
    [{pin109,{ioc,12,8,1}},
     {pin110,{ioc,12,8,2}},
     {pin111,{ioc,12,8,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin2, pin3];
top_pins(2) ->
    [pin1, pin144];
top_pins(3) ->
    [pin142, pin143];
top_pins(4) ->
    [pin140, pin141];
top_pins(5) ->
    [pin137, pin138, pin139];
top_pins(6) ->
    [pin131, pin132, pin133, pin134];
top_pins(7) ->
    [pin125, pin127, pin129, pin130];
top_pins(8) ->
    [pin121, pin122, pin123, pin124];
top_pins(9) ->
    [pin118, pin119, pin120];
top_pins(10) ->
    [pin114, pin117];
top_pins(11) ->
    [pin112, pin113];
top_pins(12) ->
    [pin109, pin110, pin111].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{pin27,{ioc,0,4,0}},
     {pin28,{ioc,0,4,2}},
     {pin29,{ioc,0,4,3}},
     {pin30,{ioc,0,4,5}}];
left_iocs(5) ->
    [{pin18,{ioc,0,5,0}},
     {pin20,{ioc,0,5,1}},
     {pin21,{ioc,0,5,2}},
     {pin22,{ioc,0,5,3}},
     {pin23,{ioc,0,5,4}},
     {pin24,{ioc,0,5,5}}];
left_iocs(6) ->
    [{pin11,{ioc,0,6,1}},
     {pin12,{ioc,0,6,2}},
     {pin13,{ioc,0,6,3}},
     {pin14,{ioc,0,6,4}},
     {pin15,{ioc,0,6,5}},
     {pin16,{ioc,0,6,6}}];
left_iocs(7) ->
    [{pin4,{ioc,0,7,0}},
     {pin5,{ioc,0,7,2}},
     {pin6,{ioc,0,7,3}},
     {pin7,{ioc,0,7,4}},
     {pin8,{ioc,0,7,5}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [pin27, pin28, pin29, pin30];
left_pins(5) ->
    [pin18, pin20, pin21, pin22, pin23, pin24];
left_pins(6) ->
    [pin11, pin12, pin13, pin14, pin15, pin16];
left_pins(7) ->
    [pin4, pin5, pin6, pin7, pin8].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{pin76,{ioc,13,1,1}},
     {pin75,{ioc,13,1,3}},
     {pin74,{ioc,13,1,4}},
     {pin73,{ioc,13,1,5}}];
right_iocs(2) ->
    [{pin80,{ioc,13,2,0}},
     {pin79,{ioc,13,2,1}},
     {pin78,{ioc,13,2,2}},
     {pin77,{ioc,13,2,4}}];
right_iocs(3) ->
    [{pin87,{ioc,13,3,0}},
     {pin86,{ioc,13,3,1}},
     {pin85,{ioc,13,3,2}},
     {pin84,{ioc,13,3,3}},
     {pin81,{ioc,13,3,5}}];
right_iocs(4) ->
    [{pin95,{ioc,13,4,0}},
     {pin94,{ioc,13,4,1}},
     {pin93,{ioc,13,4,2}},
     {pin91,{ioc,13,4,3}},
     {pin89,{ioc,13,4,4}},
     {pin88,{ioc,13,4,5}}];
right_iocs(5) ->
    [{pin102,{ioc,13,5,0}},
     {pin101,{ioc,13,5,2}},
     {pin98,{ioc,13,5,3}},
     {pin97,{ioc,13,5,4}},
     {pin96,{ioc,13,5,5}}];
right_iocs(6) ->
    [{pin105,{ioc,13,6,1}},
     {pin104,{ioc,13,6,2}},
     {pin103,{ioc,13,6,5}}];
right_iocs(7) ->
    [{pin108,{ioc,13,7,1}},
     {pin107,{ioc,13,7,3}},
     {pin106,{ioc,13,7,5}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin73, pin74, pin75, pin76];
right_pins(2) ->
    [pin77, pin78, pin79, pin80];
right_pins(3) ->
    [pin81, pin84, pin85, pin86, pin87];
right_pins(4) ->
    [pin88, pin89, pin91, pin93, pin94, pin95];
right_pins(5) ->
    [pin96, pin97, pin98, pin101, pin102];
right_pins(6) ->
    [pin103, pin104, pin105];
right_pins(7) ->
    [pin106, pin107, pin108].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{pin32,{ioc,1,3,1}},
     {pin31,{ioc,1,3,3}}];
bottom_iocs(2) ->
    [{pin39,{ioc,2,3,0}},
     {pin38,{ioc,2,3,2}},
     {pin37,{ioc,2,3,3}}];
bottom_iocs(3) ->
    [{pin41,{ioc,3,3,1}},
     {pin40,{ioc,3,3,2}}];
bottom_iocs(4) ->
    [{pin44,{ioc,4,3,1}},
     {pin43,{ioc,4,3,2}},
     {pin42,{ioc,4,3,3}}];
bottom_iocs(5) ->
    [{pin49,{ioc,5,3,0}},
     {pin48,{ioc,5,3,1}},
     {pin45,{ioc,5,3,2}}];
bottom_iocs(6) ->
    [{pin53,{ioc,6,3,0}},
     {pin52,{ioc,6,3,1}},
     {pin51,{ioc,6,3,2}},
     {pin50,{ioc,6,3,3}}];
bottom_iocs(7) ->
    [{pin59,{ioc,7,3,0}},
     {pin58,{ioc,7,3,1}},
     {pin57,{ioc,7,3,2}},
     {pin55,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{pin63,{ioc,8,3,0}},
     {pin62,{ioc,8,3,1}},
     {pin61,{ioc,8,3,2}},
     {pin60,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{pin68,{ioc,10,0,0}},
     {pin67,{ioc,10,0,1}},
     {pin66,{ioc,10,0,2}}];
bottom_iocs(11) ->
    [{pin70,{ioc,11,0,1}},
     {pin69,{ioc,11,0,2}}];
bottom_iocs(12) ->
    [{pin72,{ioc,12,0,0}},
     {pin71,{ioc,12,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin31, pin32];
bottom_pins(2) ->
    [pin37, pin38, pin39];
bottom_pins(3) ->
    [pin40, pin41];
bottom_pins(4) ->
    [pin42, pin43, pin44];
bottom_pins(5) ->
    [pin45, pin48, pin49];
bottom_pins(6) ->
    [pin50, pin51, pin52, pin53];
bottom_pins(7) ->
    [pin55, pin57, pin58, pin59];
bottom_pins(8) ->
    [pin60, pin61, pin62, pin63];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [pin66, pin67, pin68];
bottom_pins(11) ->
    [pin69, pin70];
bottom_pins(12) ->
    [pin71, pin72].

