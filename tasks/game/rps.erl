-module(rps).
-export([
  play/1,
  echo/1,
  play_two/3,
  rock/1,
  no_repeat/1,
  const/1,
  enum/1,
  cycle/1,
  rand/1,
  val/1,
  tournament/2,
  least_frequent/1,
  random_strategy/1
]).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
  play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

play_two(_,_,PlaysL,PlaysR, 0) ->
  tournament(PlaysL,PlaysR);

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
  PlayL = StrategyL(PlaysR),
  PlayR = StrategyR(PlaysL),
  result(PlayL,PlayR),
  play_two(StrategyL,StrategyR,[PlayL|PlaysL],[PlayR|PlaysR],N-1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy, Moves) -> play(Strategy, Moves, []).
play(Strategy, PlayerMoves, MachineMoves) ->
  {ok,P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop ->
        io:format("Stopped~n"),
        io:format("Tournament result: ~p~n", [tournament(PlayerMoves, MachineMoves)]);
    _ ->
        MachinePlay = Strategy(PlayerMoves),
        Result = result(Play, MachinePlay),
        io:format("Result: ~p~n", [Result]),
        play(Strategy,[Play|PlayerMoves], [MachinePlay|MachineMoves])
  end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
  lists:sum(
    lists:map(fun outcome/1,
      lists:zipwith(fun result/2,PlaysL,PlaysR)
    )
  ).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock) -> 0;
val(paper) -> 1;
val(scissors) -> 2.

% give the play which the argument beats.

beats(rock) -> scissors;
beats(paper) -> rock;
beats(scissors) -> paper.

%
% strategies.
%
echo([]) -> paper;
echo([Last|_]) -> Last.

rock(_) -> rock.

no_repeat([]) -> paper;
no_repeat([X|_]) ->
  case {X, random:uniform(2)} of
    {rock, 1} -> scissors;
    {rock, 2} -> paper;

    {scissors, 1} -> paper;
    {scissors, 2} -> rock;

    {paper, 1} -> rock;
    {paper, 2} -> scissors
  end.

const(Play) -> dummy.

cycle(Xs) -> lists:nth((length(Xs) rem 3) + 1, [rock, paper, scissors]).

rand(_) -> enum(random:uniform(3)-1).

rps_frequency(List) ->
  [
    rps_frequency(List, rock),
    rps_frequency(List, paper),
    rps_frequency(List, scissors)
  ].

rps_frequency(List, Type) ->
  {Matched, _} = lists:partition(
    fun(X) -> X == Type end,
    List
  ),
  {length(Matched), Type}.

least_frequent(Xs) ->
  % io:format("Frequency: ~p~n",[rps_frequency(Xs)]),
  Frequency = lists:sort(rps_frequency(Xs)),
  {_, Play} = lists:nth(1, Frequency),
  Play.

most_frequent(Xs) ->
  % io:format("Frequency: ~p~n",[rps_frequency(Xs)]),
  Frequency = lists:reverse(lists:sort(rps_frequency(Xs))),
  {_, Play} = lists:nth(1, Frequency),
  Play.

random_strategy(Xs) ->
  StrategiesList = [
    fun echo/1,
    fun rock/1,
    fun no_repeat/1,
    fun cycle/1,
    fun rand/1,
    fun least_frequent/1,
    fun most_frequent/1
  ],
  Strategy = lists:nth(
    random:uniform(length(StrategiesList)),
    StrategiesList
  ),
  Strategy(Xs).

% Run

% rps:play(fun rps:random_strategy/1).
% OR
% rps:play(fun(Plays) -> rock end).
