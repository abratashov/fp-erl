% http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibtable.html
-module(fib).

-export([ fib/1,
          fib_tail/1,
          fib_tail/3,
          test_fib/1,
          test/0
        ]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 2) + fib(N - 1).

fib_tail(0) -> 0;
fib_tail(1) -> 1;
fib_tail(N) when N > 1 -> fib_tail(0, 1, N - 1).

fib_tail(A, B, 0) -> B;
fib_tail(A, B, N) when N >= 1 -> fib_tail(B, A + B, N - 1).

% Tests

test_fib(FibFun) ->
  0 = FibFun(0),
  1 = FibFun(1),
  1 = FibFun(2),
  2 = FibFun(3),
  3 = FibFun(4),
  5 = FibFun(5),
  8 = FibFun(6),
  13 = FibFun(7),
  21 = FibFun(8),
  34 = FibFun(9),
  55 = FibFun(10),
  6765 = FibFun(20),
  832040 = FibFun(30),
  102334155 = FibFun(40),
  %12586269025 = fib(50),
  %354224848179261915075 = fib(100),
  %280571172992510140037611932413038677189525 = fib(200),
  %222232244629420445529739893461909967206666939096499764990979600 = fib(300),
  ok.

test() ->
  ok = test_fib(fun fib/1),
  ok = test_fib(fun fib_tail/1),
  ok.
