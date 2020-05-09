% https://hastebin.com/anotegezam.pl
-module(second).
-export([ hypotenuse/2,
          triangle_area/2,
          triangle_perimeter/2,
          % Tests
          test_hypotenuse/0,
          test_triangle_area/0,
          test_triangle_perimeter/0,
          test_all/0
        ]).

-import(first, [sqr/1]).

hypotenuse(A, B) ->
  math:sqrt(first:sqr(A) + first:sqr(B)).

triangle_area(A, B) ->
  A * B / 2.

triangle_perimeter(A, B) ->
  A + B + hypotenuse(A, B).

% ======= Tests =======

test_hypotenuse() ->
  hypotenuse(3, 4) == 5.

test_triangle_area() ->
  triangle_area(3, 4) == 6.

test_triangle_perimeter() ->
  triangle_perimeter(3, 4) == 12.

test_all() ->
  test_hypotenuse() and
    test_triangle_area() and
    test_triangle_perimeter().
