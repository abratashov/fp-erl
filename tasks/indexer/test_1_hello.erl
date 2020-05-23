-module(test_1_hello).
-export([test/0]).
-import(indexer, [index/1, lookup/1]).
-import(tools, [parse_and_clean_up_file/1]).

test_hello() ->
  Index = indexer:index(
    tools:parse_and_clean_up_file('/home/alex/Projects/abratashov/fp-erl/tasks/indexer/data/hello_world.txt')
  ),
  io:format("~p~n",[Index]),
  {"hello",[1]} = lookup({"hello", Index}),
  {"world",[1,2,3,4]} = lookup({"world", Index}),
  ok.

test() ->
  ok = test_hello(),
  ok.
