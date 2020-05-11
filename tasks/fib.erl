% http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibtable.html
-module(fib).

-export([fib/1, test/0]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 2) + fib(N - 1).

% Tests

test() ->
  0 = fib(0),
  1 = fib(1),
  1 = fib(2),
  2 = fib(3),
  3 = fib(4),
  5 = fib(5),
  8 = fib(6),
  13 = fib(7),
  21 = fib(8),
  34 = fib(9),
  55 = fib(10),
  6765 = fib(20),
  832040 = fib(30),
  102334155 = fib(40),
  %12586269025 = fib(50),
  %354224848179261915075 = fib(100),
  %280571172992510140037611932413038677189525 = fib(200),
  %222232244629420445529739893461909967206666939096499764990979600 = fib(300),
  ok.

