-module(shapes).
-export([ perimeter/1,
          calc_perimeter/1,
          test_perimetr/0,
          test/0
        ]).

perimeter(shape) ->
  calc_perimeter(shape).

calc_perimeter({circle, R}) ->
  2*math:pi()*R;

calc_perimeter({rectangle, H, W}) ->
  2*H + 2*W;

calc_perimeter({triangle, A, B, C}) ->
  A + B + C.

test_perimetr() ->
  18.84955592153876 = calc_perimeter({circle, 3}),
  16 = calc_perimeter({rectangle, 5, 3}),
  12 = calc_perimeter({triangle, 5, 4, 3}),
  ok.

test() ->
  ok = test_perimetr(),
  ok.
