-module(rock_paper_scissors).

-export([test/0]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(scissors) -> paper;
lose(paper) -> rock.

result(Gesture1, Gesture1) -> draw;
result(Gesture1, Gesture2) ->
  case beat(Gesture1) of
    Gesture2 -> lose;
    _ -> win
  end.

tournament(Gestures1, Gestures2) ->
  Result = lists:map(
    fun({Gesture1,Gesture2}) ->
      case result(Gesture1, Gesture2) of
        draw -> 0;
        win -> 1;
        lose -> -1
      end
    end,
    lists:zip(Gestures1, Gestures2)
  ),
  % io:format("~p~n", [Result]),
  lists:foldr(fun(R, Acc) -> Acc + R end, 0, Result).

test_result() ->
  lose = result(rock,paper),
  win = result(rock,scissors),
  draw = result(rock,rock),

  draw = result(paper,paper),
  lose = result(paper,scissors),
  win = result(paper,rock),

  win = result(scissors,paper),
  draw = result(scissors,scissors),
  lose = result(scissors,rock),

  ok.

test_tournament() ->
  -1 = tournament([rock,rock,paper,paper],[rock,paper,scissors,rock]),
  ok.

test() ->
  % ok = test_result(),
  ok = test_tournament(),
  ok.
