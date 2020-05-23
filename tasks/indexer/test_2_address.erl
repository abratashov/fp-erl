-module(test_2_address).
-export([test/0]).
-import(indexer, [index/1, lookup/1]).
-import(tools, [parse_and_clean_up_file/1]).

test() ->
  Text = tools:parse_and_clean_up_file('/home/alex/Projects/abratashov/fp-erl/tasks/indexer/data/gettysburg-address.txt'),
  Index = indexer:index(Text),
  io:format("~p~n",[Index]),
  {"advanced",[20]} = indexer:lookup({"advanced", Index}),
  {"civil",[5]} = indexer:lookup({"civil", Index}),
  {"equal",[3]} = indexer:lookup({"equal", Index}),
  {"that",[3,6,7,8,9,10,11,22,23,24,25,26,27]} = indexer:lookup({"that", Index}),
  {"work",[19]} = indexer:lookup({"work", Index}),
  {"world",[17]} = indexer:lookup({"world", Index}),
  ok.
