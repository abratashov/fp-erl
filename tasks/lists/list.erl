-module(list).
-export([
  product/1,
  product/2,
  max/1,
  max/2,
  test_product/0,
  test/0
]).

product(L) -> product(L, 1).
product([], M) -> M;
product([H|T], M) -> product(T, M*H).


max([H|T]) -> max(T, H).
max([], Max) -> Max;
max([H|T], Max) when H > Max -> max(T, H);
max([H|T], Max) when H < Max -> max(T, Max).

% Tests

test_product() ->
  1 = product([1]),
  2 = product([1,2]),
  6 = product([1,2,3]),
  24 = product([1,2,3,4]),
  120 = product([1,2,3,4,5]),
  ok.

test_max() ->
  1 = max([1]),
  2 = max([1,2]),
  3 = max([1,2,3]),
  12 = max([1,12,3,4]),
  40 = max([10,2,3,40,5]),
  ok.

test() ->
  ok = test_product(),
  ok = test_max(),
  ok.
