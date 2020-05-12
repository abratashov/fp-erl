% https://en.wikipedia.org/wiki/Perfect_number
% https://oeis.org/A000396

% 1 + 2 + 3 = 6
% 1 + 2 + 4 + 7 + 14 = 28
% 1 + 2 + 4 + 8 + 16 + 31 + 62 + 124 + 248 = 496

-module(perf_num).

-export([ perf_num/1,
          perf_num/3,
          test/0]).

perf_num(N) ->
  % io:format("~p~n", [N]),
  perf_num(N, N, 0).

perf_num(N, C, Sum) when C == 0 ->
  % io:format("Sum:~p~n", [Sum]),
  N == (Sum - N);

perf_num(N, C, Sum) when C >= 0 andalso (N rem C) == 0 ->
  % io:format("1:~p~n", [C]),
  perf_num(N, C-1, Sum + N div C);

perf_num(N, C, Sum) when C >= 0 andalso (N rem C) =/= 0 ->
  % io:format("2:~p~n", [C]),
  perf_num(N, C-1, Sum).

test() ->
  true = perf_num(6),
  true = perf_num(28),
  true = perf_num(496),
  true = perf_num(8128),
  true = perf_num(33550336),
  false = perf_num(1),
  false = perf_num(2),
  false = perf_num(3),
  false = perf_num(4),
  false = perf_num(5),
  false = perf_num(7),
  false = perf_num(8),
  false = perf_num(9),
  false = perf_num(10),
  false = perf_num(20),
  false = perf_num(30),
  false = perf_num(50),
  false = perf_num(100),
  false = perf_num(1000),
  false = perf_num(10000),
  ok.
