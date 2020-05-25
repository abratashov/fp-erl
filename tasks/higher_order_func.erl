-module(higher_order_func).
-export([
  double_all/1,
  evens/1,
  product/1,
  test/0
]).

double_all([]) -> [];
double_all([X|Xs]) -> [ 2*X | double_all(Xs) ].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 -> [X | evens(Xs) ];
evens([_|Xs]) -> evens(Xs).

product([]) -> 1;
product([X|Xs]) -> X * product(Xs).

% Tests

test_double_all() ->
  [] = double_all([]),
  [2,4,6,8,10] = double_all([1,2,3,4,5]),
  ok.

test_evens() ->
  [] = evens([]),
  [2,4] = evens([1,2,3,4,5]),
  ok.

test_product() ->
  1 = product([]),
  120 = product([1,2,3,4,5]),
  ok.

test() ->
  ok = test_double_all(),
  ok = test_evens(),
  ok = test_product(),
  ok.
