% Summing the bits

-module(bits).
-export([sum_of_bits/1, sum_of_bits/3, test/0]).

sum_of_bits(N) ->
  % io:fwrite("~.2B~n", [N]),
  sum_of_bits(N, 0, 0).

sum_of_bits(1, Remain, Sum) ->
  Sum + Remain + 1;

sum_of_bits(N, Remain, Sum) when N > 0 ->
  sum_of_bits(N div 2, N rem 2, Sum + Remain).

test() ->
  2 = sum_of_bits(5), %101
  3 = sum_of_bits(7), %111
  1 = sum_of_bits(8), %1000
  3 = sum_of_bits(50), %110010
  5 = sum_of_bits(9000), %10001100101000
  ok.
