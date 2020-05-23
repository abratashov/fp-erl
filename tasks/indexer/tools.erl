-module(tools).
-export([
  get_file_contents/1,
  parse_and_clean_up_file/1,
  test/0
]).

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% % Auxiliary function for get_file_contents.
% % Not exported.
get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File), Partial;
    Line ->
      {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
  end.

% % Show the contents of a list of strings.
% % Can be used to check the results of calling get_file_contents.
show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);

show_file_contents([]) ->
  ok.

parse_and_clean_up_file(File) ->
  Result = lists:map(
    fun(A) -> string:tokens(string:lowercase(A), " ") end,
    clean_up(get_file_contents(File))
  ),
  % io:format("~p~n",[Result]),
  Result.

clean_up(Lines) ->
  StopChars = "`~!@#$%^&*()[]_-+=\\|,<.>/?\"';:\t\r",
  lists:map(
    fun(Line) -> clean_up_line(Line, StopChars) end,
    Lines
  ).

clean_up_line(Line, []) -> Line;
clean_up_line(Line, [StopChar|T]) ->
  clean_up_line(
    lists:filter(fun(X) -> X /= StopChar end, Line),
    T
  ).

% Tests

hello_lines() ->
  [
    "Hello my new world,,",
    "the world of cats",
    "and world of dogs,",
    "and human world.",
    "`~!@#$%^&*()[]_-+=\\|,<.>/?\"';:"
  ].

hello_cleaned_lines() ->
  [
    ["hello", "my", "new", "world"],
    ["the", "world", "of", "cats"],
    ["and", "world", "of", "dogs"],
    ["and", "human", "world"],
    []
  ].

test_get_file_contents() ->
  Text = hello_lines(),
  Text = get_file_contents('/home/alex/Projects/abratashov/fp-erl/tasks/indexer/data/hello_world.txt'),
  ok.

test_parse_and_clean_up_file() ->
  Text = hello_cleaned_lines(),
  Text = parse_and_clean_up_file('/home/alex/Projects/abratashov/fp-erl/tasks/indexer/data/hello_world.txt'),
  ok.

test() ->
  ok = test_get_file_contents(),
  ok = test_parse_and_clean_up_file(),
  ok.
