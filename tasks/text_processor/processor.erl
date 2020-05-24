% Fits text into the page bye length of line

-module(processor).
-export([align/2, test/0]).

-import(tools, [
  get_file_contents/1
]).

example_text() ->
  ["ab", "cd", "efgh"].

text_to_words(Text) ->
  Words = string:tokens(string:join(Text, " "), " "),
  % io:format("~p~n",[Words]),
  Words.

text_align([], _) -> "";
text_align([Word|T], LineSize) -> text_align(T, LineSize, [Word], []).

text_align([], _, [Word], Page) -> lists:reverse([Word|Page]);
text_align([], _, Line, Page) -> lists:reverse([Line|Page]);
text_align([Word|T], LineSize, Line, Page) ->
  case length(string:join([Word|Line], " ")) =< LineSize of
    true -> text_align(T, LineSize, [Word|Line], Page);
    false -> text_align(T, LineSize, [Word], [string:join(lists:reverse(Line), " ")|Page])
  end.

align(File, LineSize) ->
  Res = text_align(text_to_words(tools:get_file_contents(File)), LineSize),
  % io:format("~p~n",[Res]),
  Res.


% Tests

test_align_example_text() ->
  ["ab cd", "efgh"].

test_align_text_1() ->
  [
    "The heat bloomed in",
    "December as the",
    "carnival season",
    "kicked into gear.",
    "Nearly helpless with",
    "sun and glare, I",
    "avoided Rio's",
    "brilliant sidewalks",
    "and glittering",
    "beaches, panting in",
    "dark corners and",
    "waiting out the",
    "inverted southern",
    "summer."
  ].

test_align() ->
  test_align_example_text() == text_align(example_text(), 8),
  test_align_text_1() == align('/home/alex/Projects/abratashov/fp-erl/tasks/text_processor/data/text1.txt', 20),
  ok.

test() ->
  ok = test_align(),
  ok.
