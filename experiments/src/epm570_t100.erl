-module(epm570_t100).

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
    [{pin1,{ioc,3,8,3}},
     {pin2,{ioc,1,8,0}},
     {pin3,{ioc,1,8,2}},
     {pin4,{ioc,0,7,0}},
     {pin5,{ioc,0,7,2}},
     {pin6,{ioc,0,7,3}},
     {pin7,{ioc,0,7,4}},
     {pin8,{ioc,0,7,5}},
     {pin12,{ioc,0,5,0}},
     {pin14,{ioc,0,5,1}},
     {pin15,{ioc,0,5,2}},
     {pin16,{ioc,0,5,3}},
     {pin17,{ioc,0,5,4}},
     {pin18,{ioc,0,5,5}},
     {pin19,{ioc,0,4,5}},
     {pin20,{ioc,1,3,3}},
     {pin21,{ioc,1,3,1}},
     {pin26,{ioc,3,3,2}},
     {pin27,{ioc,3,3,1}},
     {pin28,{ioc,4,3,3}},
     {pin29,{ioc,4,3,2}},
     {pin30,{ioc,4,3,1}},
     {pin33,{ioc,6,3,3}},
     {pin34,{ioc,6,3,2}},
     {pin35,{ioc,6,3,1}},
     {pin36,{ioc,6,3,0}},
     {pin38,{ioc,7,3,3}},
     {pin40,{ioc,7,3,2}},
     {pin41,{ioc,7,3,1}},
     {pin42,{ioc,7,3,0}},
     {pin43,{ioc,8,3,3}},
     {pin44,{ioc,8,3,2}},
     {pin47,{ioc,10,0,2}},
     {pin48,{ioc,10,0,1}},
     {pin49,{ioc,10,0,0}},
     {pin50,{ioc,11,0,2}},
     {pin51,{ioc,12,0,2}},
     {pin52,{ioc,13,1,4}},
     {pin53,{ioc,13,1,3}},
     {pin54,{ioc,13,1,1}},
     {pin55,{ioc,13,2,4}},
     {pin56,{ioc,13,2,2}},
     {pin57,{ioc,13,2,1}},
     {pin58,{ioc,13,2,0}},
     {pin61,{ioc,13,3,1}},
     {pin62,{ioc,13,4,4}},
     {pin64,{ioc,13,4,3}},
     {pin66,{ioc,13,4,2}},
     {pin67,{ioc,13,4,1}},
     {pin68,{ioc,13,4,0}},
     {pin69,{ioc,13,5,0}},
     {pin70,{ioc,13,6,5}},
     {pin71,{ioc,13,6,2}},
     {pin72,{ioc,13,6,1}},
     {pin73,{ioc,13,7,5}},
     {pin74,{ioc,13,7,3}},
     {pin75,{ioc,13,7,1}},
     {pin76,{ioc,12,8,1}},
     {pin77,{ioc,12,8,2}},
     {pin78,{ioc,12,8,3}},
     {pin81,{ioc,10,8,3}},
     {pin82,{ioc,9,8,0}},
     {pin83,{ioc,8,8,0}},
     {pin84,{ioc,8,8,1}},
     {pin85,{ioc,8,8,2}},
     {pin86,{ioc,8,8,3}},
     {pin87,{ioc,7,8,0}},
     {pin89,{ioc,7,8,1}},
     {pin91,{ioc,6,8,2}},
     {pin92,{ioc,6,8,3}},
     {pin95,{ioc,5,8,0}},
     {pin96,{ioc,5,8,1}},
     {pin97,{ioc,5,8,2}},
     {pin98,{ioc,4,8,1}},
     {pin99,{ioc,4,8,2}},
     {pin100,{ioc,3,8,0}}
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
     pin38,
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
     pin89,
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

top_iocs(1) ->
    [{pin2,{ioc,1,8,0}},
     {pin3,{ioc,1,8,2}}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{pin100,{ioc,3,8,0}},
     {pin1,{ioc,3,8,3}}];
top_iocs(4) ->
    [{pin98,{ioc,4,8,1}},
     {pin99,{ioc,4,8,2}}];
top_iocs(5) ->
    [{pin95,{ioc,5,8,0}},
     {pin96,{ioc,5,8,1}},
     {pin97,{ioc,5,8,2}}];
top_iocs(6) ->
    [{pin91,{ioc,6,8,2}},
     {pin92,{ioc,6,8,3}}];
top_iocs(7) ->
    [{pin87,{ioc,7,8,0}},
     {pin89,{ioc,7,8,1}}];
top_iocs(8) ->
    [{pin83,{ioc,8,8,0}},
     {pin84,{ioc,8,8,1}},
     {pin85,{ioc,8,8,2}},
     {pin86,{ioc,8,8,3}}];
top_iocs(9) ->
    [{pin82,{ioc,9,8,0}}];
top_iocs(10) ->
    [{pin81,{ioc,10,8,3}}];
top_iocs(11) ->
    [];
top_iocs(12) ->
    [{pin76,{ioc,12,8,1}},
     {pin77,{ioc,12,8,2}},
     {pin78,{ioc,12,8,3}}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin2, pin3];
top_pins(2) ->
    [];
top_pins(3) ->
    [pin1, pin100];
top_pins(4) ->
    [pin98, pin99];
top_pins(5) ->
    [pin95, pin96, pin97];
top_pins(6) ->
    [pin91, pin92];
top_pins(7) ->
    [pin87, pin89];
top_pins(8) ->
    [pin83, pin84, pin85, pin86];
top_pins(9) ->
    [pin82];
top_pins(10) ->
    [pin81];
top_pins(11) ->
    [];
top_pins(12) ->
    [pin76, pin77, pin78].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{pin19,{ioc,0,4,5}}];
left_iocs(5) ->
    [{pin12,{ioc,0,5,0}},
     {pin14,{ioc,0,5,1}},
     {pin15,{ioc,0,5,2}},
     {pin16,{ioc,0,5,3}},
     {pin17,{ioc,0,5,4}},
     {pin18,{ioc,0,5,5}}];
left_iocs(6) ->
    [];
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
    [pin19];
left_pins(5) ->
    [pin12, pin14, pin15, pin16, pin17, pin18];
left_pins(6) ->
    [];
left_pins(7) ->
    [pin4, pin5, pin6, pin7, pin8].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{pin54,{ioc,13,1,1}},
     {pin53,{ioc,13,1,3}},
     {pin52,{ioc,13,1,4}}];
right_iocs(2) ->
    [{pin58,{ioc,13,2,0}},
     {pin57,{ioc,13,2,1}},
     {pin56,{ioc,13,2,2}},
     {pin55,{ioc,13,2,4}}];
right_iocs(3) ->
    [{pin61,{ioc,13,3,1}}];
right_iocs(4) ->
    [{pin68,{ioc,13,4,0}},
     {pin67,{ioc,13,4,1}},
     {pin66,{ioc,13,4,2}},
     {pin64,{ioc,13,4,3}},
     {pin62,{ioc,13,4,4}}];
right_iocs(5) ->
    [{pin69,{ioc,13,5,0}}];
right_iocs(6) ->
    [{pin72,{ioc,13,6,1}},
     {pin71,{ioc,13,6,2}},
     {pin70,{ioc,13,6,5}}];
right_iocs(7) ->
    [{pin75,{ioc,13,7,1}},
     {pin74,{ioc,13,7,3}},
     {pin73,{ioc,13,7,5}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin52, pin53, pin54];
right_pins(2) ->
    [pin55, pin56, pin57, pin58];
right_pins(3) ->
    [pin61];
right_pins(4) ->
    [pin62, pin64, pin66, pin67, pin68];
right_pins(5) ->
    [pin69];
right_pins(6) ->
    [pin70, pin71, pin72];
right_pins(7) ->
    [pin73, pin74, pin75].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(1) ->
    [{pin21,{ioc,1,3,1}},
     {pin20,{ioc,1,3,3}}];
bottom_iocs(2) ->
    [];
bottom_iocs(3) ->
    [{pin27,{ioc,3,3,1}},
     {pin26,{ioc,3,3,2}}];
bottom_iocs(4) ->
    [{pin30,{ioc,4,3,1}},
     {pin29,{ioc,4,3,2}},
     {pin28,{ioc,4,3,3}}];
bottom_iocs(5) ->
    [];
bottom_iocs(6) ->
    [{pin36,{ioc,6,3,0}},
     {pin35,{ioc,6,3,1}},
     {pin34,{ioc,6,3,2}},
     {pin33,{ioc,6,3,3}}];
bottom_iocs(7) ->
    [{pin42,{ioc,7,3,0}},
     {pin41,{ioc,7,3,1}},
     {pin40,{ioc,7,3,2}},
     {pin38,{ioc,7,3,3}}];
bottom_iocs(8) ->
    [{pin44,{ioc,8,3,2}},
     {pin43,{ioc,8,3,3}}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{pin49,{ioc,10,0,0}},
     {pin48,{ioc,10,0,1}},
     {pin47,{ioc,10,0,2}}];
bottom_iocs(11) ->
    [{pin50,{ioc,11,0,2}}];
bottom_iocs(12) ->
    [{pin51,{ioc,12,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin20, pin21];
bottom_pins(2) ->
    [];
bottom_pins(3) ->
    [pin26, pin27];
bottom_pins(4) ->
    [pin28, pin29, pin30];
bottom_pins(5) ->
    [];
bottom_pins(6) ->
    [pin33, pin34, pin35, pin36];
bottom_pins(7) ->
    [pin38, pin40, pin41, pin42];
bottom_pins(8) ->
    [pin43, pin44];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [pin47, pin48, pin49];
bottom_pins(11) ->
    [pin50];
bottom_pins(12) ->
    [pin51].

