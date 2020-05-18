-module(palindrome).
-export([
  reverse/1,
  reverse/2,
  is_palindrome/1,
  test/0
]).

reverse([]) -> [];
reverse(L) -> reverse(L, []).
reverse([], Result) -> Result;
reverse([H|T], Result) -> reverse(T, [H|Result]).

is_palindrome([]) -> true;
is_palindrome(L) -> L == reverse(L).

% Tests

test_reverse() ->
  [] = reverse([]),
  [1] = reverse([1]),
  [2,1] = reverse([1,2]),
  [5,4,3,2,1] = reverse([1,2,3,4,5]),
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
  ok = test_is_palindrome(),
  ok.
