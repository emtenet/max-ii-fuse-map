-module(fuse_map).

-export([run/0]).

-export([to_fuse/2]).
-export([to_name/2]).

-type density() :: density:density().
-type fuse() :: fuse:fuse().
-type name() :: name:name().

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Count = density:fuse_count(Density),
    Db = fuse_database:read(Density),
    run(0, Count, Density, Db).

%%--------------------------------------------------------------------

run(Stop, Stop, _, _) ->
    ok;
run(Fuse, Stop, Density, Db) ->
    case fuse_database:find(Fuse, Db) of
        {ok, DbName} ->
            case to_name(Fuse, Density) of
                {ok, MapName} when MapName =:= DbName ->
                    case to_fuse(DbName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({fuse, Fuse, db, DbName, to_fuse, MapFuse});

                        false ->
                            throw({fuse, Fuse, db, DbName, fuse_only})
                    end;

                {ok, MapName} ->
                    throw({fuse, Fuse, db, DbName, to_name, MapName});

                false ->
                    throw({fuse, Fuse, db, DbName, not_implemented});

                {unknown, Unknown} ->
                    throw({fuse, Fuse, db, DbName, unknown, Unknown})
            end;

        false ->
            case to_name(Fuse, Density) of
                {ok, MapName} ->
                    case to_fuse(MapName, Density) of
                        {ok, MapFuse} when MapFuse =:= Fuse ->
                            ok;

                        {ok, MapFuse} ->
                            throw({fuse, Fuse, to, MapName, to_fuse, MapFuse});

                        false ->
                            throw({fuse, Fuse, to, MapName, fuse_only})
                    end;

                false ->
                    ok;

                {unknown, _} ->
                    ok
            end
    end,
    run(Fuse + 1, Stop, Density, Db).

%%====================================================================
%% to_fuse
%%====================================================================

-spec to_fuse(name(), density()) -> {ok, fuse()} | false.

to_fuse(Fuse, epm240) ->
    epm240_fuse_map:to_fuse(Fuse);
to_fuse(Fuse, epm570) ->
    to_fuse_epm570(Fuse);
to_fuse(Fuse, epm1270) ->
    to_fuse_epm1270(Fuse);
to_fuse(Fuse, epm2210) ->
    to_fuse_epm2210(Fuse).

%%--------------------------------------------------------------------

to_fuse_epm570({user_code, 0}) -> {ok, 65786};
to_fuse_epm570({user_code, 1}) -> {ok, 65530};
to_fuse_epm570({user_code, 2}) -> {ok, 65274};
to_fuse_epm570({user_code, 3}) -> {ok, 65018};
to_fuse_epm570({user_code, 4}) -> {ok, 64762};
to_fuse_epm570({user_code, 5}) -> {ok, 64506};
to_fuse_epm570({user_code, 6}) -> {ok, 65785};
to_fuse_epm570({user_code, 7}) -> {ok, 65529};
to_fuse_epm570({user_code, 8}) -> {ok, 65273};
to_fuse_epm570({user_code, 9}) -> {ok, 65017};
to_fuse_epm570({user_code, 10}) -> {ok, 64761};
to_fuse_epm570({user_code, 11}) -> {ok, 64505};
to_fuse_epm570({user_code, 12}) -> {ok, 64249};
to_fuse_epm570({user_code, 13}) -> {ok, 63993};
to_fuse_epm570({user_code, 14}) -> {ok, 63737};
to_fuse_epm570({user_code, 15}) -> {ok, 63481};
to_fuse_epm570({user_code, 16}) -> {ok, 63225};
to_fuse_epm570({user_code, 17}) -> {ok, 62969};
to_fuse_epm570({user_code, 18}) -> {ok, 62713};
to_fuse_epm570({user_code, 19}) -> {ok, 62457};
to_fuse_epm570({user_code, 20}) -> {ok, 62201};
to_fuse_epm570({user_code, 21}) -> {ok, 61945};
to_fuse_epm570({user_code, 22}) -> {ok, 61689};
to_fuse_epm570({user_code, 23}) -> {ok, 61433};
to_fuse_epm570({user_code, 24}) -> {ok, 61177};
to_fuse_epm570({user_code, 25}) -> {ok, 60921};
to_fuse_epm570({user_code, 26}) -> {ok, 60665};
to_fuse_epm570({user_code, 27}) -> {ok, 60409};
to_fuse_epm570({user_code, 28}) -> {ok, 64250};
to_fuse_epm570({user_code, 29}) -> {ok, 63994};
to_fuse_epm570({user_code, 30}) -> {ok, 63738};
to_fuse_epm570({user_code, 31}) -> {ok, 63482};
to_fuse_epm570(_) ->
    false.

%%--------------------------------------------------------------------

to_fuse_epm1270({user_code, 0}) -> {ok, 120225};
to_fuse_epm1270({user_code, 1}) -> {ok, 119841};
to_fuse_epm1270({user_code, 2}) -> {ok, 119457};
to_fuse_epm1270({user_code, 3}) -> {ok, 119073};
to_fuse_epm1270({user_code, 4}) -> {ok, 118689};
to_fuse_epm1270({user_code, 5}) -> {ok, 118305};
to_fuse_epm1270({user_code, 6}) -> {ok, 120224};
to_fuse_epm1270({user_code, 7}) -> {ok, 119840};
to_fuse_epm1270({user_code, 8}) -> {ok, 119456};
to_fuse_epm1270({user_code, 9}) -> {ok, 119072};
to_fuse_epm1270({user_code, 10}) -> {ok, 118688};
to_fuse_epm1270({user_code, 11}) -> {ok, 118304};
to_fuse_epm1270({user_code, 12}) -> {ok, 117920};
to_fuse_epm1270({user_code, 13}) -> {ok, 117536};
to_fuse_epm1270({user_code, 14}) -> {ok, 117152};
to_fuse_epm1270({user_code, 15}) -> {ok, 116768};
to_fuse_epm1270({user_code, 16}) -> {ok, 116384};
to_fuse_epm1270({user_code, 17}) -> {ok, 116000};
to_fuse_epm1270({user_code, 18}) -> {ok, 115616};
to_fuse_epm1270({user_code, 19}) -> {ok, 115232};
to_fuse_epm1270({user_code, 20}) -> {ok, 114848};
to_fuse_epm1270({user_code, 21}) -> {ok, 114464};
to_fuse_epm1270({user_code, 22}) -> {ok, 114080};
to_fuse_epm1270({user_code, 23}) -> {ok, 113696};
to_fuse_epm1270({user_code, 24}) -> {ok, 113312};
to_fuse_epm1270({user_code, 25}) -> {ok, 112928};
to_fuse_epm1270({user_code, 26}) -> {ok, 112544};
to_fuse_epm1270({user_code, 27}) -> {ok, 112160};
to_fuse_epm1270({user_code, 28}) -> {ok, 117921};
to_fuse_epm1270({user_code, 29}) -> {ok, 117537};
to_fuse_epm1270({user_code, 30}) -> {ok, 117153};
to_fuse_epm1270({user_code, 31}) -> {ok, 116769};
to_fuse_epm1270(_) ->
    false.

%%--------------------------------------------------------------------

to_fuse_epm2210({user_code, 0}) -> {ok, 188977};
to_fuse_epm2210({user_code, 1}) -> {ok, 188465};
to_fuse_epm2210({user_code, 2}) -> {ok, 187953};
to_fuse_epm2210({user_code, 3}) -> {ok, 187441};
to_fuse_epm2210({user_code, 4}) -> {ok, 186929};
to_fuse_epm2210({user_code, 5}) -> {ok, 186417};
to_fuse_epm2210({user_code, 6}) -> {ok, 188976};
to_fuse_epm2210({user_code, 7}) -> {ok, 188464};
to_fuse_epm2210({user_code, 8}) -> {ok, 187952};
to_fuse_epm2210({user_code, 9}) -> {ok, 187440};
to_fuse_epm2210({user_code, 10}) -> {ok, 186928};
to_fuse_epm2210({user_code, 11}) -> {ok, 186416};
to_fuse_epm2210({user_code, 12}) -> {ok, 185904};
to_fuse_epm2210({user_code, 13}) -> {ok, 185392};
to_fuse_epm2210({user_code, 14}) -> {ok, 184880};
to_fuse_epm2210({user_code, 15}) -> {ok, 184368};
to_fuse_epm2210({user_code, 16}) -> {ok, 183856};
to_fuse_epm2210({user_code, 17}) -> {ok, 183344};
to_fuse_epm2210({user_code, 18}) -> {ok, 182832};
to_fuse_epm2210({user_code, 19}) -> {ok, 182320};
to_fuse_epm2210({user_code, 20}) -> {ok, 181808};
to_fuse_epm2210({user_code, 21}) -> {ok, 181296};
to_fuse_epm2210({user_code, 22}) -> {ok, 180784};
to_fuse_epm2210({user_code, 23}) -> {ok, 180272};
to_fuse_epm2210({user_code, 24}) -> {ok, 179760};
to_fuse_epm2210({user_code, 25}) -> {ok, 179248};
to_fuse_epm2210({user_code, 26}) -> {ok, 178736};
to_fuse_epm2210({user_code, 27}) -> {ok, 178224};
to_fuse_epm2210({user_code, 28}) -> {ok, 185905};
to_fuse_epm2210({user_code, 29}) -> {ok, 185393};
to_fuse_epm2210({user_code, 30}) -> {ok, 184881};
to_fuse_epm2210({user_code, 31}) -> {ok, 184369};
to_fuse_epm2210(_) ->
    false.

%%====================================================================
%% to_name
%%====================================================================

-spec to_name(fuse(), density()) -> {ok, name()} | false.

to_name(Fuse, epm240) ->
    epm240_fuse_map:to_name(Fuse);
to_name(Fuse, epm570) ->
    to_name_epm570(Fuse);
to_name(Fuse, epm1270) ->
    to_name_epm1270(Fuse);
to_name(Fuse, epm2210) ->
    to_name_epm2210(Fuse).

%%--------------------------------------------------------------------

to_name_epm570(60409) -> {ok, {user_code, 27}};
to_name_epm570(60665) -> {ok, {user_code, 26}};
to_name_epm570(60921) -> {ok, {user_code, 25}};
to_name_epm570(61177) -> {ok, {user_code, 24}};
to_name_epm570(61433) -> {ok, {user_code, 23}};
to_name_epm570(61689) -> {ok, {user_code, 22}};
to_name_epm570(61945) -> {ok, {user_code, 21}};
to_name_epm570(62201) -> {ok, {user_code, 20}};
to_name_epm570(62457) -> {ok, {user_code, 19}};
to_name_epm570(62713) -> {ok, {user_code, 18}};
to_name_epm570(62969) -> {ok, {user_code, 17}};
to_name_epm570(63225) -> {ok, {user_code, 16}};
to_name_epm570(63481) -> {ok, {user_code, 15}};
to_name_epm570(63482) -> {ok, {user_code, 31}};
to_name_epm570(63737) -> {ok, {user_code, 14}};
to_name_epm570(63738) -> {ok, {user_code, 30}};
to_name_epm570(63993) -> {ok, {user_code, 13}};
to_name_epm570(63994) -> {ok, {user_code, 29}};
to_name_epm570(64249) -> {ok, {user_code, 12}};
to_name_epm570(64250) -> {ok, {user_code, 28}};
to_name_epm570(64505) -> {ok, {user_code, 11}};
to_name_epm570(64506) -> {ok, {user_code, 5}};
to_name_epm570(64761) -> {ok, {user_code, 10}};
to_name_epm570(64762) -> {ok, {user_code, 4}};
to_name_epm570(65017) -> {ok, {user_code, 9}};
to_name_epm570(65018) -> {ok, {user_code, 3}};
to_name_epm570(65273) -> {ok, {user_code, 8}};
to_name_epm570(65274) -> {ok, {user_code, 2}};
to_name_epm570(65529) -> {ok, {user_code, 7}};
to_name_epm570(65530) -> {ok, {user_code, 1}};
to_name_epm570(65785) -> {ok, {user_code, 6}};
to_name_epm570(65786) -> {ok, {user_code, 0}};
to_name_epm570(_) ->
    false.

%%--------------------------------------------------------------------

to_name_epm1270(112160) -> {ok, {user_code, 27}};
to_name_epm1270(112544) -> {ok, {user_code, 26}};
to_name_epm1270(112928) -> {ok, {user_code, 25}};
to_name_epm1270(113312) -> {ok, {user_code, 24}};
to_name_epm1270(113696) -> {ok, {user_code, 23}};
to_name_epm1270(114080) -> {ok, {user_code, 22}};
to_name_epm1270(114464) -> {ok, {user_code, 21}};
to_name_epm1270(114848) -> {ok, {user_code, 20}};
to_name_epm1270(115232) -> {ok, {user_code, 19}};
to_name_epm1270(115616) -> {ok, {user_code, 18}};
to_name_epm1270(116000) -> {ok, {user_code, 17}};
to_name_epm1270(116384) -> {ok, {user_code, 16}};
to_name_epm1270(116768) -> {ok, {user_code, 15}};
to_name_epm1270(116769) -> {ok, {user_code, 31}};
to_name_epm1270(117152) -> {ok, {user_code, 14}};
to_name_epm1270(117153) -> {ok, {user_code, 30}};
to_name_epm1270(117536) -> {ok, {user_code, 13}};
to_name_epm1270(117537) -> {ok, {user_code, 29}};
to_name_epm1270(117920) -> {ok, {user_code, 12}};
to_name_epm1270(117921) -> {ok, {user_code, 28}};
to_name_epm1270(118304) -> {ok, {user_code, 11}};
to_name_epm1270(118305) -> {ok, {user_code, 5}};
to_name_epm1270(118688) -> {ok, {user_code, 10}};
to_name_epm1270(118689) -> {ok, {user_code, 4}};
to_name_epm1270(119072) -> {ok, {user_code, 9}};
to_name_epm1270(119073) -> {ok, {user_code, 3}};
to_name_epm1270(119456) -> {ok, {user_code, 8}};
to_name_epm1270(119457) -> {ok, {user_code, 2}};
to_name_epm1270(119840) -> {ok, {user_code, 7}};
to_name_epm1270(119841) -> {ok, {user_code, 1}};
to_name_epm1270(120224) -> {ok, {user_code, 6}};
to_name_epm1270(120225) -> {ok, {user_code, 0}};
to_name_epm1270(_) ->
    false.

%%--------------------------------------------------------------------

to_name_epm2210(178224) -> {ok, {user_code, 27}};
to_name_epm2210(178736) -> {ok, {user_code, 26}};
to_name_epm2210(179248) -> {ok, {user_code, 25}};
to_name_epm2210(179760) -> {ok, {user_code, 24}};
to_name_epm2210(180272) -> {ok, {user_code, 23}};
to_name_epm2210(180784) -> {ok, {user_code, 22}};
to_name_epm2210(181296) -> {ok, {user_code, 21}};
to_name_epm2210(181808) -> {ok, {user_code, 20}};
to_name_epm2210(182320) -> {ok, {user_code, 19}};
to_name_epm2210(182832) -> {ok, {user_code, 18}};
to_name_epm2210(183344) -> {ok, {user_code, 17}};
to_name_epm2210(183856) -> {ok, {user_code, 16}};
to_name_epm2210(184368) -> {ok, {user_code, 15}};
to_name_epm2210(184369) -> {ok, {user_code, 31}};
to_name_epm2210(184880) -> {ok, {user_code, 14}};
to_name_epm2210(184881) -> {ok, {user_code, 30}};
to_name_epm2210(185392) -> {ok, {user_code, 13}};
to_name_epm2210(185393) -> {ok, {user_code, 29}};
to_name_epm2210(185904) -> {ok, {user_code, 12}};
to_name_epm2210(185905) -> {ok, {user_code, 28}};
to_name_epm2210(186416) -> {ok, {user_code, 11}};
to_name_epm2210(186417) -> {ok, {user_code, 5}};
to_name_epm2210(186928) -> {ok, {user_code, 10}};
to_name_epm2210(186929) -> {ok, {user_code, 4}};
to_name_epm2210(187440) -> {ok, {user_code, 9}};
to_name_epm2210(187441) -> {ok, {user_code, 3}};
to_name_epm2210(187952) -> {ok, {user_code, 8}};
to_name_epm2210(187953) -> {ok, {user_code, 2}};
to_name_epm2210(188464) -> {ok, {user_code, 7}};
to_name_epm2210(188465) -> {ok, {user_code, 1}};
to_name_epm2210(188976) -> {ok, {user_code, 6}};
to_name_epm2210(188977) -> {ok, {user_code, 0}};
to_name_epm2210(_) ->
    false.

