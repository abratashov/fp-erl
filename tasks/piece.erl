% https://puzzling.stackexchange.com/questions/15694/cutting-a-cake-into-7-pieces-with-3-straight-cuts-no-3d
-module(piece).

-export([piece/1, test/0]).

piece(1) -> 2;
piece(N) when N > 1 -> piece(N-1) + N.

% Tests

test() ->
  2 = piece(1),
  4 = piece(2),
  7 = piece(3),
  11 = piece(4),
  16 = piece(5),
  22 = piece(6),
  29 = piece(7),
  ok.

