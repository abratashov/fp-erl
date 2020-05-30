-module(functions).

-export([compose/1, twice/1, iterate/3, test/0]).

% Add = fun (X,Y) -> X+Y end.

% Sum = fun (Xs) -> lists:foldr(Add,0,Xs) end.

% EmptyTest = fun ([]) -> true ; ([_|_]) -> false end.

% Foo = fun Product([]) -> 1 ; Product([X|Xs]) -> X*Product(Xs) end.

compose([]) -> [];
compose([F|T]) -> compose(T, F).
compose([], G) -> G;
compose([F|T], G) -> compose(T, fun(X) -> G(F(X)) end).

twice(N) ->
  Func = fun(X) -> 3*X end,
  Twice = compose([Func, Func]),
  Twice(N).

iterate(0, Func) -> Func;
iterate(N, Func) -> iterate(N, Func, []).
iterate(0, _, Composition) -> compose(Composition);
iterate(N, Func, Composition) when N > 0 -> iterate(N - 1, Func, [Func|Composition]).

test_twice() ->
  18 = twice(2),
  ok.

test_iterate() ->
  % Test 1
  Func1 = fun(X) -> 3*X end,
  IteratedFunc1 = iterate(2, Func1),
  18 = IteratedFunc1(2),

  % Test 2
  Func2 = fun(X) -> X*X end,
  IteratedFunc2 = iterate(3, Func2),
  256 = IteratedFunc2(2),
  ok.

test() ->
  ok = test_twice(),
  ok = test_iterate(),
  ok.
