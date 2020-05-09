-module(first).

-export([double/1,
         mult/2,
         area/3,
         sqr/1,
         trbl/1,
         test_double/0,
         test_mult/0,
         test_area/0,
         test_sqr/0,
         test_trbl/0,
         test_all/0]).

mult(X, Y) ->
    X * Y.

double(X) ->
    mult(2, X).

area(A, B, C) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

sqr(A) ->
    A * A.

trbl(A) ->
    sqr(A) * A.

% ======= Tests =======

test_mult() ->
    mult(2, 3) == 6.

test_double() ->
    double(4) == 8.

test_area() ->
    area(5, 4, 3) == 6.

test_sqr() ->
    sqr(3) == 9.

test_trbl() ->
    trbl(2) == 8.

test_all() ->
    test_mult() and test_double() and test_area() and test_sqr() and test_trbl().
