-module(solution).
-export([main/0, read_input/1]).


read_input(FileName) ->
  {ok, Data} = file:read_file(FileName),
  binary_to_list(Data).


main() ->
  Data = read_input("input.txt"),
  io:fwrite("Part One:~n"),
  solve_part1(Data),
  io:fwrite("Part Two:~n"),
  solve_part2(Data).


extract_int(Memory, Start, Len) ->
  list_to_integer(lists:sublist(Memory, Start, Len)).


extract_pairs(Memory, Pairs) ->
  case re:run(Memory, "mul\\((\\d+),(\\d+)\\)") of
    {match, Captured} ->
      [_, {Index1, Length1}, {Index2, Length2}] = Captured,
      X1 = extract_int(Memory, Index1+1, Length1),
      X2 = extract_int(Memory, Index2+1, Length2),
      NewPairs = Pairs ++ [{X1, X2}],
      ReducedMemory = lists:nthtail(Index2 + Length2, Memory),
      extract_pairs(ReducedMemory, NewPairs);
    nomatch ->
      Pairs
  end.


conditionally_extract_pairs(Memory, Pairs) ->
  case re:run(Memory, "don't\\(\\)") of
    {match, Captured} ->
      [{DoNotPass, _}] = Captured,
      case re:run(Memory, "mul\\((\\d+),(\\d+)\\)") of
        {match, NumsCaptured} ->
          [{_, _}, {Index1, Length1}, {Index2, Length2}] = NumsCaptured,
          if
            Index1 > DoNotPass ->
              ReducedMemory = lists:nthtail(DoNotPass + 7, Memory),
              % find the next occurrence of `do()` if it exists
              case re:run(ReducedMemory, "do\\(\\)") of
                {match, DoCaptured} ->
                  [{DoIndex, _}] = DoCaptured,
                  MoreReducedMemory = lists:nthtail(DoIndex + 4, ReducedMemory),
                  conditionally_extract_pairs(MoreReducedMemory, Pairs);
                nomatch ->
                  Pairs
              end;
            true ->
              X1 = extract_int(Memory, Index1+1, Length1),
              X2 = extract_int(Memory, Index2+1, Length2),
              NewPairs = Pairs ++ [{X1, X2}],
              ReducedMemory = lists:nthtail(Index2 + Length2, Memory),
              conditionally_extract_pairs(ReducedMemory, NewPairs)
          end;
          nomatch -> Pairs
        end;
    nomatch ->
      extract_pairs(Memory, Pairs)
  end.


solve_part1(Memory) ->
  Pairs = extract_pairs(Memory, []),
  Products = [X1 * X2 || {X1, X2} <- Pairs],
  ProductSum = lists:foldl(fun (X, Acc) -> X + Acc end, 0, Products),
  io:format("~w~n", [ProductSum]).


solve_part2(Memory) ->
  Pairs = conditionally_extract_pairs(Memory, []),
  Products = [X1 * X2 || {X1, X2} <- Pairs],
  ProductSum = lists:foldl(fun (X, Acc) -> X + Acc end, 0, Products),
  io:format("~w~n", [ProductSum]).
  