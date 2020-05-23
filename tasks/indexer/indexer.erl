% Indexing list text content

-module(indexer).
-export([
  index/1, % Index = index([["hello", "world"], ["new", "world"]])
  lookup/1 % lookup({"world", Index}) % => {"world", [1,2]}
]).

lookup({Word, Index}) -> lookup(Word, Index).
lookup(Word, [{Word, _}=R|_]) -> R;
lookup(Word, [_|T]) -> lookup(Word, T).

build_line_index(Line, N) -> build_line_index(Line, [], N).
build_line_index([], Index, _) -> Index;
build_line_index([Word|T], Index, N) -> build_line_index(T, [{Word, N}|Index], N).

build_index(Lines) -> build_index(Lines, [], 1).
build_index([], Index, _) -> Index;
build_index([Line|T], Index, N) -> build_index(T, [build_line_index(Line, N)|Index], N + 1).

compress_index_records(Records) -> compress_index_records(Records, []).
compress_index_records([], Index) -> Index;
compress_index_records([{Word, N}|T], Index) -> compress_index_record(Word, T, [], [N], Index).

compress_index_record(Word, [], Skipped, WordIndex, Index) -> compress_index_records(Skipped, [{Word,WordIndex}|Index]);
compress_index_record(Word, [{Word, N}|T], Skipped, WordIndex, Index) -> compress_index_record(Word, T, Skipped, [N|lists:sort(WordIndex)], Index);
compress_index_record(Word, [{_, _}=S|T], Skipped, WordIndex, Index) -> compress_index_record(Word, T, [S|Skipped], WordIndex, Index).

index(Lines) ->
  lists:map(
    fun({Word, Index}) -> {Word, lists:sort(Index)} end,
    lists:sort(
      compress_index_records(
        lists:merge(
          build_index(Lines)
        )
      )
    )
  ).
