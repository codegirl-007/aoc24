-module(day3).
-export([part1/0]).

read_file_to_string(FilePath) ->
  case file:read_file(FilePath) of
    {ok, BinaryContent} ->
      binary_to_list(BinaryContent);
    {error, Reason} ->
      {error, Reason}
  end.

process_valid_mul(Input) ->
    %% Split the input by "do()" or "don't()"
    Sections = re:split(Input, "(do\\(\\)|don't\\(\\))", [{return, list}]),
    %% Start processing with valid context
    process_sections(Sections, true, []).

process_sections([], _, Acc) ->
    %% When no more sections, return the accumulated results
    lists:reverse(Acc);
process_sections(["do()" | Rest], _, Acc) ->
    %% Enable valid context when "do()" appears
    process_sections(Rest, true, Acc);
process_sections(["don't()" | Rest], _, Acc) ->
    %% Disable context when "don't()" appears
    process_sections(Rest, false, Acc);
process_sections([Section | Rest], true, Acc) ->
    %% Extract mul(...) pairs if context is valid
    MulMatches = extract_mul(Section),
    process_sections(Rest, true, MulMatches ++ Acc);
process_sections([_Section | Rest], false, Acc) ->
    %% Skip all sections if context is invalid
    process_sections(Rest, false, Acc).

extract_mul(Section) ->
    %% Match all "mul(...)" in the given section
    Pattern = "mul\\((\\d+),(\\d+)\\)",
    case re:run(Section, Pattern, [{capture, all_but_first, list}, global]) of
        {match, Matches} ->
            %% Convert matches to integer pairs
            lists:map(fun([Num1Str, Num2Str]) ->
                [list_to_integer(Num1Str), list_to_integer(Num2Str)]
            end, Matches);
        error -> []
    end.

part1() ->
  FileName = "input.txt",
  case read_file_to_string(FileName) of
    {error, _} -> 
      io:format("Failed to read file: ~p~n", [FileName]);
    Input -> 
      Lists = process_valid_mul(Input),
      Product = lists:map(fun([A, B]) -> A * B end, Lists),
      Sum = lists:sum(Product),
      io:format("Answer: ~p~n", [Sum])
  end.
  