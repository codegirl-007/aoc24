-module(day2).
-export([read_file_to_list/1, part1/0]).


% Reads a file and returns its lines as a list of strings
read_file_to_list(FileName) ->
    case file:open(FileName, [read]) of
        {ok, File} -> 
            Lines = read_lines(File, []), 
            file:close(File), 
            Lines;
        {error,_} -> 
            error
    end.

read_lines(File, Acc) ->
    case file:read_line(File) of
        {ok, Line} -> read_lines(File, [lists:flatten(Line) | Acc]);
        eof -> lists:reverse(Acc);
        {error, _} -> error
    end.


% Check if a list is strictly ordered (either increasing or decreasing)
is_strictly_ordered(List) ->
    is_strictly_increasing(List) orelse is_strictly_decreasing(List).

% Check if removing at most one element can make the list strictly ordered
is_strictly_ordered_with_removal(List) ->
    case is_strictly_ordered(List) of
        true -> true;  % Already strictly ordered
        false -> can_be_made_strictly_ordered(List)
    end.

% Check if removing one value makes the list strictly ordered
can_be_made_strictly_ordered([]) -> false;
can_be_made_strictly_ordered([_]) -> true;  % A single value is trivially ordered
can_be_made_strictly_ordered(List) ->
    lists:any(fun(RemovedIndex) ->
        is_strictly_ordered(remove_at(RemovedIndex, List))
    end, lists:seq(1, length(List))).  % Try removing each index

% Remove the element at the given index
remove_at(Index, List) ->
    {Left, [_ | Right]} = lists:split(Index - 1, List),
    Left ++ Right.

% Check if a list is strictly increasing with differences between 1 and 3
is_strictly_increasing([]) -> true;
is_strictly_increasing([_]) -> true;
is_strictly_increasing([A, B | Rest]) ->
    A < B andalso B - A >= 1 andalso B - A =< 3 andalso is_strictly_increasing([B | Rest]).

% Check if a list is strictly decreasing with differences between 1 and 3
is_strictly_decreasing([]) -> true;
is_strictly_decreasing([_]) -> true;
is_strictly_decreasing([A, B | Rest]) ->
    A > B andalso A - B >= 1 andalso A - B =< 3 andalso is_strictly_decreasing([B | Rest]).

% Count the number of strictly ordered lists in a list of lists
count_strictly_ordered(Lists) ->
    count_strictly_ordered(Lists, 0).

% Helper function to iterate over lists and increment counter
count_strictly_ordered([], Counter) ->
    Counter;  % Return the final counter
count_strictly_ordered([List | Rest], Counter) ->
    case is_strictly_ordered_with_removal(List) of
        true -> count_strictly_ordered(Rest, Counter + 1);  % Increment counter
        false -> count_strictly_ordered(Rest, Counter)  % Continue without incrementing
    end.
% Checks if a list of numbers is strictly increasing or decreasing
process(Line) ->
    Substrings = string:split(string:trim(Line), " ", all),
    lists:map(fun(Str) ->
        {Value, _} = string:to_integer(Str),  % Extract only the integer part
        Value
    end, Substrings).

% Main function to process the file
part1() ->
    FileName = "input.txt",
    case read_file_to_list(FileName) of
        {error, _} -> 
            io:format("Failed to read file: ~p~n", [FileName]);
        Lines -> 
            Results = lists:map(fun process/1, Lines),
            Count = count_strictly_ordered(Results),  % Get the counter value
            io:format("Processed Lists: ~p~n", [Results]),
            io:format("Count of strictly ordered lists: ~p~n", [Count])
    end.