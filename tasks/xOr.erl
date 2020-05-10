-module(xOr).

-export([xOr1/2, xOr2/2, xOr3/2, test/0]).

xOr1(A, B) ->
    A =/= B.

xOr2(A, B) ->
    not (A == B).

xOr3(A, B) ->
    (A == true) and (B == false) or
    (A == false) and (B == true).

% Tests

testFun(XorFun) ->
    (XorFun(true, true) == false) and
    (XorFun(true, false) == true) and
    (XorFun(false, true) == true) and
    (XorFun(false, false) == false).

test() ->
    true = testFun(fun xOr1/2),
    true = testFun(fun xOr2/2),
    true = testFun(fun xOr3/2),
    ok.
