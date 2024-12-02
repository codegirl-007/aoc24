-module(day1).
-export([read_file_to_two_lists/1, day1/0]).

% Reads a file and converts each line to a string in a list
read_file_to_two_lists(FileName) ->
    case file:open(FileName, [read]) of
        {ok, File} ->
            Lines = read_lines_to_two_lists(File, [], []),
            file:close(File),
            Lines;
        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason]),
            {error, Reason}
    end.

% Helper function to read lines recursively
read_lines_to_two_lists(File, List1Acc, List2Acc) ->
    case file:read_line(File) of
        {ok, Line} ->
            LineString = lists:flatten(Line),
            case string:split(LineString, "   ", all) of
                [First, Second] ->
                    % Convert strings to integers and append to respective lists
                    {FirstNum, _} = string:to_integer(First),
                    {SecondNum, _} = string:to_integer(Second),
                    read_lines_to_two_lists(File, [FirstNum | List1Acc], [SecondNum | List2Acc]);
                _ ->
                    io:format("Error parsing line: ~p~n", [LineString]),
                    read_lines_to_two_lists(File, List1Acc, List2Acc)
            end;
        eof ->
            {lists:reverse(List1Acc), lists:reverse(List2Acc)};
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason]),
            {error, Reason}
    end.

% Function to calculate differences between corresponding elements of two lists
differences(List1, List2) ->
    calculate_differences(List1, List2, []).

% Helper function to iterate through both lists and calculate differences
calculate_differences([H1 | T1], [H2 | T2], Acc) ->
    calculate_differences(T1, T2, [H1 - H2 | Acc]);
calculate_differences([], [], Acc) ->
    lists:reverse(Acc).

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

day1() -> 
  FileName = "input.txt",
    case read_file_to_two_lists(FileName) of
        {error, _} -> 
            io:format("Failed to read file: ~p~n", [FileName]);
        {List1, List2} -> 
            SortedList1 = lists:sort(List1),
            SortedList2 = lists:sort(List2),
            DiffList = differences(SortedList1, SortedList2),
            Total = sum(DiffList),
            io:format("Sum: ~p~n", [Total])
    end.