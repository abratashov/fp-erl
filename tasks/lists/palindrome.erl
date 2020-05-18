-module(palindrome).
-export([
  reverse/1,
  reverse/2,
  equal/2,
  is_palindrome/1,
  test/0
]).

reverse([]) -> [];
reverse(L) -> reverse(L, []).
reverse([], Result) -> Result;
reverse([H|T], Result) -> reverse(T, [H|Result]).

equal([], []) -> true;
equal([], [_|_]) -> false;
equal([_|_], []) -> false;
equal([H1|_], [H2|_]) when H1 =/= H2 -> false;
equal([_|T1], [_|T2]) -> equal(T1, T2).

is_palindrome([]) -> true;
is_palindrome(L) -> equal(L, reverse(L)).

% Tests

test_reverse() ->
  [] = reverse([]),
  [1] = reverse([1]),
  [2,1] = reverse([1,2]),
  [5,4,3,2,1] = reverse([1,2,3,4,5]),
  ok.

test_equal() ->
  true = equal([], []),
  true = equal([1],[1]),
  true = equal([1,2], [1,2]),
  true = equal([1,2,3,4,5], [1,2,3,4,5]),
  false = equal([], [1]),
  false = equal([1], [0]),
  false = equal([1,2], [1,3]),
  false = equal([1,2], [1,3,4]),
  false = equal([1,2,3], [1,3]),
  false = equal([1,3], [1,3,4]),
  ok.

test_is_palindrome() ->
  true = is_palindrome([]),
  true = is_palindrome([1]),
  true = is_palindrome([1,1,1,1]),
  true = is_palindrome([1,2,2,1]),
  true = is_palindrome([1,1,1,1,1]),
  true = is_palindrome([1,2,3,4,5,4,3,2,1]),
  false = is_palindrome([1,2,3,4,5,4,6,2,1]),
  false = is_palindrome([1,2,2]),
  ok.

test() ->
  ok = test_reverse(),
  ok = test_equal(),
  ok = test_is_palindrome(),
  ok.
