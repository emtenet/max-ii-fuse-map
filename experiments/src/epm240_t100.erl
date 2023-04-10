-module(epm240_t100).

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
    [{pin1,{ioc,2,5,3}},
     {pin2,{ioc,1,4,0}},
     {pin3,{ioc,1,4,1}},
     {pin4,{ioc,1,4,2}},
     {pin5,{ioc,1,4,3}},
     {pin6,{ioc,1,3,0}},
     {pin7,{ioc,1,3,1}},
     {pin8,{ioc,1,3,2}},
     {pin12,{ioc,1,3,3}},
     {pin14,{ioc,1,2,0}},
     {pin15,{ioc,1,2,1}},
     {pin16,{ioc,1,2,2}},
     {pin17,{ioc,1,2,3}},
     {pin18,{ioc,1,1,0}},
     {pin19,{ioc,1,1,1}},
     {pin20,{ioc,1,1,2}},
     {pin21,{ioc,1,1,3}},
     {pin26,{ioc,2,0,3}},
     {pin27,{ioc,2,0,2}},
     {pin28,{ioc,2,0,1}},
     {pin29,{ioc,2,0,0}},
     {pin30,{ioc,3,0,3}},
     {pin33,{ioc,3,0,2}},
     {pin34,{ioc,3,0,1}},
     {pin35,{ioc,3,0,0}},
     {pin36,{ioc,4,0,2}},
     {pin37,{ioc,4,0,1}},
     {pin38,{ioc,4,0,0}},
     {pin39,{ioc,5,0,3}},
     {pin40,{ioc,5,0,2}},
     {pin41,{ioc,5,0,1}},
     {pin42,{ioc,5,0,0}},
     {pin43,{ioc,6,0,3}},
     {pin44,{ioc,6,0,2}},
     {pin47,{ioc,6,0,1}},
     {pin48,{ioc,6,0,0}},
     {pin49,{ioc,7,0,2}},
     {pin50,{ioc,7,0,1}},
     {pin51,{ioc,7,0,0}},
     {pin52,{ioc,8,1,4}},
     {pin53,{ioc,8,1,3}},
     {pin54,{ioc,8,1,2}},
     {pin55,{ioc,8,1,1}},
     {pin56,{ioc,8,1,0}},
     {pin57,{ioc,8,2,3}},
     {pin58,{ioc,8,2,2}},
     {pin61,{ioc,8,2,1}},
     {pin62,{ioc,8,2,0}},
     {pin64,{ioc,8,3,4}},
     {pin66,{ioc,8,3,3}},
     {pin67,{ioc,8,3,2}},
     {pin68,{ioc,8,3,1}},
     {pin69,{ioc,8,3,0}},
     {pin70,{ioc,8,4,4}},
     {pin71,{ioc,8,4,3}},
     {pin72,{ioc,8,4,2}},
     {pin73,{ioc,8,4,1}},
     {pin74,{ioc,8,4,0}},
     {pin75,{ioc,7,5,0}},
     {pin76,{ioc,7,5,1}},
     {pin77,{ioc,7,5,2}},
     {pin78,{ioc,7,5,3}},
     {pin81,{ioc,6,5,0}},
     {pin82,{ioc,6,5,1}},
     {pin83,{ioc,6,5,2}},
     {pin84,{ioc,6,5,3}},
     {pin85,{ioc,5,5,0}},
     {pin86,{ioc,5,5,1}},
     {pin87,{ioc,5,5,2}},
     {pin88,{ioc,5,5,3}},
     {pin89,{ioc,4,5,0}},
     {pin90,{ioc,4,5,1}},
     {pin91,{ioc,4,5,2}},
     {pin92,{ioc,3,5,0}},
     {pin95,{ioc,3,5,1}},
     {pin96,{ioc,3,5,2}},
     {pin97,{ioc,3,5,3}},
     {pin98,{ioc,2,5,0}},
     {pin99,{ioc,2,5,1}},
     {pin100,{ioc,2,5,2}}
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
     pin12,
     pin14,
     pin15,
     pin16,
     pin17,
     pin18,
     pin19,
     pin20,
     pin21,
     pin26,
     pin27,
     pin28,
     pin29,
     pin30,
     pin33,
     pin34,
     pin35,
     pin36,
     pin37,
     pin38,
     pin39,
     pin40,
     pin41,
     pin42,
     pin43,
     pin44,
     pin47,
     pin48,
     pin49,
     pin50,
     pin51,
     pin52,
     pin53,
     pin54,
     pin55,
     pin56,
     pin57,
     pin58,
     pin61,
     pin62,
     pin64,
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
     pin81,
     pin82,
     pin83,
     pin84,
     pin85,
     pin86,
     pin87,
     pin88,
     pin89,
     pin90,
     pin91,
     pin92,
     pin95,
     pin96,
     pin97,
     pin98,
     pin99,
     pin100
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(2) ->
    [{pin98,{ioc,2,5,0}},
     {pin99,{ioc,2,5,1}},
     {pin100,{ioc,2,5,2}},
     {pin1,{ioc,2,5,3}}];
top_iocs(3) ->
    [{pin92,{ioc,3,5,0}},
     {pin95,{ioc,3,5,1}},
     {pin96,{ioc,3,5,2}},
     {pin97,{ioc,3,5,3}}];
top_iocs(4) ->
    [{pin89,{ioc,4,5,0}},
     {pin90,{ioc,4,5,1}},
     {pin91,{ioc,4,5,2}}];
top_iocs(5) ->
    [{pin85,{ioc,5,5,0}},
     {pin86,{ioc,5,5,1}},
     {pin87,{ioc,5,5,2}},
     {pin88,{ioc,5,5,3}}];
top_iocs(6) ->
    [{pin81,{ioc,6,5,0}},
     {pin82,{ioc,6,5,1}},
     {pin83,{ioc,6,5,2}},
     {pin84,{ioc,6,5,3}}];
top_iocs(7) ->
    [{pin75,{ioc,7,5,0}},
     {pin76,{ioc,7,5,1}},
     {pin77,{ioc,7,5,2}},
     {pin78,{ioc,7,5,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [pin1, pin98, pin99, pin100];
top_pins(3) ->
    [pin92, pin95, pin96, pin97];
top_pins(4) ->
    [pin89, pin90, pin91];
top_pins(5) ->
    [pin85, pin86, pin87, pin88];
top_pins(6) ->
    [pin81, pin82, pin83, pin84];
top_pins(7) ->
    [pin75, pin76, pin77, pin78].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{pin18,{ioc,1,1,0}},
     {pin19,{ioc,1,1,1}},
     {pin20,{ioc,1,1,2}},
     {pin21,{ioc,1,1,3}}];
left_iocs(2) ->
    [{pin14,{ioc,1,2,0}},
     {pin15,{ioc,1,2,1}},
     {pin16,{ioc,1,2,2}},
     {pin17,{ioc,1,2,3}}];
left_iocs(3) ->
    [{pin6,{ioc,1,3,0}},
     {pin7,{ioc,1,3,1}},
     {pin8,{ioc,1,3,2}},
     {pin12,{ioc,1,3,3}}];
left_iocs(4) ->
    [{pin2,{ioc,1,4,0}},
     {pin3,{ioc,1,4,1}},
     {pin4,{ioc,1,4,2}},
     {pin5,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [pin18, pin19, pin20, pin21];
left_pins(2) ->
    [pin14, pin15, pin16, pin17];
left_pins(3) ->
    [pin6, pin7, pin8, pin12];
left_pins(4) ->
    [pin2, pin3, pin4, pin5].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{pin56,{ioc,8,1,0}},
     {pin55,{ioc,8,1,1}},
     {pin54,{ioc,8,1,2}},
     {pin53,{ioc,8,1,3}},
     {pin52,{ioc,8,1,4}}];
right_iocs(2) ->
    [{pin62,{ioc,8,2,0}},
     {pin61,{ioc,8,2,1}},
     {pin58,{ioc,8,2,2}},
     {pin57,{ioc,8,2,3}}];
right_iocs(3) ->
    [{pin69,{ioc,8,3,0}},
     {pin68,{ioc,8,3,1}},
     {pin67,{ioc,8,3,2}},
     {pin66,{ioc,8,3,3}},
     {pin64,{ioc,8,3,4}}];
right_iocs(4) ->
    [{pin74,{ioc,8,4,0}},
     {pin73,{ioc,8,4,1}},
     {pin72,{ioc,8,4,2}},
     {pin71,{ioc,8,4,3}},
     {pin70,{ioc,8,4,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin52, pin53, pin54, pin55, pin56];
right_pins(2) ->
    [pin57, pin58, pin61, pin62];
right_pins(3) ->
    [pin64, pin66, pin67, pin68, pin69];
right_pins(4) ->
    [pin70, pin71, pin72, pin73, pin74].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{pin29,{ioc,2,0,0}},
     {pin28,{ioc,2,0,1}},
     {pin27,{ioc,2,0,2}},
     {pin26,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{pin35,{ioc,3,0,0}},
     {pin34,{ioc,3,0,1}},
     {pin33,{ioc,3,0,2}},
     {pin30,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [{pin38,{ioc,4,0,0}},
     {pin37,{ioc,4,0,1}},
     {pin36,{ioc,4,0,2}}];
bottom_iocs(5) ->
    [{pin42,{ioc,5,0,0}},
     {pin41,{ioc,5,0,1}},
     {pin40,{ioc,5,0,2}},
     {pin39,{ioc,5,0,3}}];
bottom_iocs(6) ->
    [{pin48,{ioc,6,0,0}},
     {pin47,{ioc,6,0,1}},
     {pin44,{ioc,6,0,2}},
     {pin43,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{pin51,{ioc,7,0,0}},
     {pin50,{ioc,7,0,1}},
     {pin49,{ioc,7,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [pin26, pin27, pin28, pin29];
bottom_pins(3) ->
    [pin30, pin33, pin34, pin35];
bottom_pins(4) ->
    [pin36, pin37, pin38];
bottom_pins(5) ->
    [pin39, pin40, pin41, pin42];
bottom_pins(6) ->
    [pin43, pin44, pin47, pin48];
bottom_pins(7) ->
    [pin49, pin50, pin51].

