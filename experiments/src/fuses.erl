-module(fuses).

-export([diff/2]).
-export([intersect/2]).
-export([union/2]).
-export([subtract/2]).
-export([subtract_stripe/2]).

-type fuse() :: fuse:fuse().
-type density() :: density:density().

%%====================================================================
%% diff
%%====================================================================

-spec diff([fuse()], [fuse()]) -> {[fuse()], [fuse()]}.

diff(From, Thru) ->
    diff(From, Thru, [], []).

%%--------------------------------------------------------------------

diff([], [], Add, Del) ->
    {lists:reverse(Add), lists:reverse(Del)};
diff([], After, Add, Del) ->
    {lists:reverse(Add, After), lists:reverse(Del)};
diff(Before, [], Add, Del) ->
    {lists:reverse(Add), lists:reverse(Del, Before)};
diff([B | Before], [A | After], Add, Del) when B =:= A ->
    diff(Before, After, Add, Del);
diff([B | Before], After = [A | _], Add, Del) when B < A ->
    diff(Before, After, Add, [B | Del]);
diff(Before, [A | After], Add, Del) ->
    diff(Before, After, [A | Add], Del).

%%====================================================================
%% intersect
%%====================================================================

-spec intersect([fuse()], [fuse()]) -> [fuse()].

intersect(As, Bs) ->
    intersect(As, Bs, []).

%%--------------------------------------------------------------------

intersect([], _, Is) ->
    lists:reverse(Is);
intersect(_, [], Is) ->
    lists:reverse(Is);
intersect([I | As], [I | Bs], Is) ->
    intersect(As, Bs, [I | Is]);
intersect([A | As], Bs = [B | _], Is) when A < B ->
    intersect(As, Bs, Is);
intersect(As, [_ | Bs], Is) ->
    intersect(As, Bs, Is).

%%====================================================================
%% union
%%====================================================================

-spec union([fuse()], [fuse()]) -> [fuse()].

union(Left, Right) ->
    lists:umerge(Left, Right).

%%====================================================================
%% subtract
%%====================================================================

-spec subtract([fuse()], [fuse()]) -> [fuse()].

subtract([], _) ->
    [];
subtract(Fuses, []) ->
    Fuses;
subtract(Fs, [S | Ss]) ->
    subtract(Fs, S, Ss, []).

%%--------------------------------------------------------------------

subtract([], _, _, Ks) ->
    lists:reverse(Ks);
subtract(Fs = [F | _], S, [], Ks) when S < F ->
    lists:reverse(Ks, Fs);
subtract(Fs = [F | _], S, [Sh | Ss], Ks) when S < F ->
    subtract(Fs, Sh, Ss, Ks);
subtract([F | Fs], S, [], Ks) when S =:= F ->
    lists:reverse(Ks, Fs);
subtract([F | Fs], S, [Sh | Ss], Ks) when S =:= F ->
    subtract(Fs, Sh, Ss, Ks);
subtract([F | Fs], S, Ss, Ks) ->
    subtract(Fs, S, Ss, [F | Ks]).

%%====================================================================
%% subtract_stripe
%%====================================================================

-spec subtract_stripe([fuse()], density()) -> [fuse()].

subtract_stripe(Fuses, epm240) ->
    lists:filtermap(fun subtract_stripe_small/1, Fuses);
subtract_stripe(Fuses, epm570) ->
    lists:filtermap(fun subtract_stripe_small/1, Fuses);
subtract_stripe(Fuses, epm1270) ->
    lists:filtermap(fun subtract_stripe_large/1, Fuses);
subtract_stripe(Fuses, epm2210) ->
    lists:filtermap(fun subtract_stripe_large/1, Fuses).

%%--------------------------------------------------------------------

subtract_stripe_small(Fuse) when Fuse rem 64 =:= 0 ->
    false;
subtract_stripe_small(Fuse) when Fuse rem 64 =:= 33 ->
    false;
subtract_stripe_small(_) ->
    true.

%%--------------------------------------------------------------------

subtract_stripe_large(Fuse) when Fuse rem 128 =:= 0 ->
    false;
subtract_stripe_large(Fuse) when Fuse rem 128 =:= 65 ->
    false;
subtract_stripe_large(_) ->
    true.

