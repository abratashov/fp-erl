-module(billing).
-export([
  lookup/1,
  test/0
]).

db() ->
  [
    {4719, "Fish Fingers" , 121},
    {5643, "Nappies" , 1010},
    {3814, "Orange Jelly", 56},
    {1111, "Hula Hoops", 21},
    {1112, "Hula Hoops (Giant)", 133},
    {1234, "Dry Sherry, 1lt", 540}
  ].

build_bill([]) -> bill_template([], 0.0);
build_bill(Items) -> build_bill(Items, [], 0.0).
build_bill([], Result, Total) -> bill_template(lists:reverse(Result), Total);
build_bill([Item|T], Result, Total) -> build_bill(T, [build_item_line(Item)|Result], Total + item_price(Item)).

bill_template(Items, Total) ->
  ["Erlang Stores", "\n"] ++ Items ++ ["\n" , "Total          - " ++ io_lib:format("~.2f", [Total/100.0])].

build_item_line(Id) ->
  {_, Title, Price} = lookup(Id),
  Title ++ "          - " ++ io_lib:format("~.2f", [Price/100.0]).

item_price(Id) ->
  {_, _, Price} = lookup(Id),
  Price.

lookup(Id) -> lookup(Id, db()).
lookup(Id, [{Id, Title, Price}|_]) -> {Id, Title, Price};
lookup(Id, [_|Items]) -> lookup(Id, Items);
lookup(Id, []) -> {Id, "Unknown Item", 0.0}.

test_result() ->
  [
    "Erlang Stores",
    "\n",
    "Dry Sherry, 1lt          - 5.40",
    "Fish Fingers          - 1.21",
    "Orange Jelly          - 0.56",
    "Hula Hoops (Giant)          - 1.33",
    "Unknown Item          - 0.00",
    "Dry Sherry, 1lt          - 5.40",
    "\n",
    "Total          - 13.90"
  ].

test() ->
  BillExample = test_result(),
  BillExample = build_bill([1234,4719,3814,1112,1113,1234]),
  ok.
