-module(sorting).

-export([
  join/2,
  concat/1,
  merge_sorted/2,
  sort_merge/1,
  test/0
]).

% Joins two lists into the one
join(L1, L2) ->
  % io:format("~p~n", [L1]),
  join_r(lists:reverse(L1), L2).
join_r([], L) -> L;
join_r([H|T], L) -> join_r(T, [H|L]).

% Joins several lists into the one
concat(L) ->
  concat(lists:reverse(L), []).
concat([], L) -> L;
concat([HL|TL], L) -> concat(TL, join(HL, L)).

% Merges two sorted lists
merge_sorted(L1, L2) -> lists:reverse(merge_sorted(L1, L2, [])).
merge_sorted([], [], Result) -> Result;
merge_sorted([], L, Result) -> join(lists:reverse(L), Result);
merge_sorted(L, [], Result) -> join(lists:reverse(L), Result);
merge_sorted([H1|T1], [H2|T2], Result) ->
  % io:format("L1~p~n", [[H1|T1]]),
  % io:format("L2~p~n", [[H2|T2]]),
  % io:format("R~p~n", [Result]),
  case H1 < H2 of
    true -> merge_sorted(T1, [H2|T2], [H1 | Result]);
    false -> merge_sorted([H1|T1], T2, [H2 | Result])
  end.

% Sorts list by merge sorting
sort_merge([]) -> [];
sort_merge(L) ->
  case lists:split(length(L) div 2, L) of
    {[], []} -> [];
    {[A], []} -> [A];
    {[], [A]} -> [A];
    {L1, L2} -> merge_sorted(sort_merge(L1), sort_merge(L2))
  end.

% Tests

test_join() ->
  [] = join([], []),
  [1,2] = join([1], [2]),
  [1,2,3] = join([1,2], [3]),
  [1,2,3,4] = join([1,2], [3,4]),
  [1,2,3,4,5] = join([1], [2,3,4,5]),
  ok.

test_concat() ->
  [] = concat([[]]),
  [1,2] = concat([[1], [2]]),
  [1,2,3] = concat([[1], [2], [3]]),
  [1,2,3,4] = concat([[1,2], [3,4]]),
  [1,2,3,4,5,6,7] = concat([[1,2], [3,4], [5,6], [7]]),
  ok.

test_merge_sorted() ->
  [] = merge_sorted([], []),
  [1,2] = merge_sorted([1], [2]),
  [1,2,3] = merge_sorted([2], [1, 3]),
  [1,2,3,4] = merge_sorted([1, 4], [2, 3]),
  [1,2,4,5,6] = merge_sorted([1, 2, 4], [5, 6]),
  [1,1,1,2,4,4,5,6,7] = merge_sorted([1, 1, 4, 7], [1, 2, 4, 5, 6]),
  ok.

test_sort_merge() ->
  [] = sort_merge([]),
  [1,2] = sort_merge([1, 2]),
  [1,2,3] = sort_merge([2, 1, 3]),
  [1,2,3,4] = sort_merge([1, 4, 3, 2]),
  [1,1,1,2,4,4,5,6,7] = sort_merge([1, 7, 1, 4, 5, 6, 2, 4, 1]),
  [0,0,1,1,1,1,2,2,3,3,3,4,4,4,5,6,7,9,14,32,44,53,68,78,432] = sort_merge(
    [1,7,3,9,432,1,4,53,3,2,0,5,32,78,1,3,4,68,0,14,44,6,2,4,1]
  ),
  ok.

test() ->
  ok = test_join(),
  ok = test_concat(),
  ok = test_merge_sorted(),
  ok = test_sort_merge(),
  ok.
