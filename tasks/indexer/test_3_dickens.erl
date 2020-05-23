-module(test_3_dickens).
-export([test/0]).
-import(indexer, [index/1, lookup/1]).
-import(tools, [parse_and_clean_up_file/1]).

test() ->
  Text = tools:parse_and_clean_up_file('/home/alex/Projects/abratashov/fp-erl/tasks/indexer/data/dickens-christmas.txt'),
  Index = indexer:index(Text),
  io:format("~p~n",[Index]),
  {"ancient",[367,576,1165,1816,1891]} = indexer:lookup({"ancient", Index}),
  {"pavement",[116,1852,1949]} = indexer:lookup({"pavement", Index}),
  {"hats",[276]} = indexer:lookup({"hats", Index}),
  {"whistle",[2511]} = indexer:lookup({"whistle", Index}),
  {"young", [
    401,459,527,1293,1350,1386,1387,1509,1608,1619,1626,1824,2030,2044,2058,
    2081,2108,2119,2129,2149,2262,2466,2563,3159,3264,3352,3687]} = indexer:lookup({"young", Index}),
  ok.
