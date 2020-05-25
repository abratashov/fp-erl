-module(higher_order_func).
-export([
  double_all/1,
  evens/1,
  product/1,
  zip/2,
  zip_with/3,
  test/0
]).

double_all(L) -> lists:map(fun(Num) -> 2*Num end, L).

evens(L) -> lists:filter(fun(Num) -> Num rem 2 == 0 end, L).

product(L) -> lists:foldr(fun(Num, Result) -> Result*Num end, 1, L).

zip(L1, L2) -> zip(L1, L2, []).
zip([], _, Result) -> lists:reverse(Result);
zip(_, [], Result) -> lists:reverse(Result);
zip([H1|T1], [H2|T2], Result) -> zip(T1, T2, [{H1,H2}|Result]).

zip_with(Func, L1, L2) -> lists:map(fun({X,Y}) -> Func(X,Y) end, zip(L1, L2)).

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

test_zip() ->
  [] = zip([1], []),
  [{1,2}, {3,4}] = zip([1,3,5,7], [2,4]),
  ok.

test_zip_with() ->
  [] = zip_with(fun(X,Y) -> X+Y end, [1], []),
  [3,7] = zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
  ok.

test() ->
  ok = test_double_all(),
  ok = test_evens(),
  ok = test_product(),
  ok = test_zip(),
  ok = test_zip_with(),
  ok.
