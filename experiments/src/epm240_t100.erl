-module(epm240_t100).

-export([pins/0]).

-type lc() :: lc:lc().
-type pin() :: pin:pin().

-spec pins() -> [{pin(), lc()}].

pins() ->
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
