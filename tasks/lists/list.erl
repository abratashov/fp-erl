-module(list).
-export([
  product/1,
  product/2,
  max/1,
  max/2,
  double/1,
  evens/1,
  take/2,
  test_product/0,
  test_max/0,
  test_double/0,
  test_evens/0,
  test_take/0,
  test/0
]).

product(L) -> product(L, 1).
product([], M) -> M;
product([H|T], M) -> product(T, M*H).


max([H|T]) -> max(T, H).
max([], Max) -> Max;
max([H|T], Max) when H > Max -> max(T, H);
max([H|T], Max) when H < Max -> max(T, Max).

double([]) -> [];
double([H|T]) -> [H*2 | double(T)].

evens([]) -> [];
evens([H|T]) ->
  case H rem 2 of
    0 -> [H | evens(T)];
    _ -> evens(T)
  end.

take(0, _) -> [];
take(N, [H|T]) when N > 0 ->
  [H | take(N - 1, T)].

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

test_double() ->
  [] = double([]),
  [2] = double([1]),
  [2,4] = double([1,2]),
  [2,4,6] = double([1,2,3]),
  [2,24,6,8] = double([1,12,3,4]),
  [20,4,6,80,10] = double([10,2,3,40,5]),
  ok.

test_evens() ->
  [] = evens([]),
  [] = evens([1]),
  [2] = evens([1,2]),
  [2] = evens([1,2,3]),
  [12,4] = evens([1,12,3,4]),
  [10,2,40] = evens([10,2,3,40,5]),
  ok.

test_take() ->
  [] = take(0, [1,2,3,4,5]),
  [1] = take(1, [1,2,3,4,5]),
  [1,2] = take(2, [1,2,3,4,5]),
  [1,2,3,4,5] = take(5, [1,2,3,4,5]),
  ok.

test() ->
  ok = test_product(),
  ok = test_max(),
  ok = test_double(),
  ok = test_evens(),
  ok = test_take(),
  ok.
