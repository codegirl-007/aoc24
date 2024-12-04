-module(day4).
-export([part1/0]).

part1() ->
    {ok, RawData} = file:read_file("input.txt"),
    Data = [binary_to_list(Line) || Line <- binary:split(RawData, <<"\n">>, [global, trim])],
    Map = load(Data),
    io:format("part 1: ~p~n", [find_xmas(Map)]),
    io:format("part 2: ~p~n", [find_mas_intersections(Map)]).


load(Data) ->
    maps:from_list([{{X, Y}, N}
                    || {Y, Line} <- lists:enumerate(Data), 
                       {X, N}    <- lists:enumerate(Line)]).

find_xmas(Map) ->
    Directions = [[{0, 0}, { 0, 1}, { 0, 2}, { 0, 3}], %row
          [{0, 0}, { 1, 0}, { 2, 0}, { 3, 0}], %column
          [{0, 0}, { 1, 1}, { 2, 2}, { 3, 3}], % top to bottom right
          [{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}]], % top to bottom left
    lists:sum([1 || XY <- maps:keys(Map), A <- Directions, is_xmas(XY, A, Map)]).

is_xmas(XY, Direction, Map) ->
    Values = get_coords(XY, Direction, Map),
    Values =:= "XMAS" orelse Values =:= "SAMX".

get_coords({X, Y}, Direction, Map) ->
    [maps:get({X + Xc, Y + Yc}, Map, undefined) || {Xc, Yc} <- Direction].

find_mas_intersections(Map) ->
    Direction_1 = [{-1, -1}, {0, 0}, {1,  1}],
    Direction_2 = [{-1,  1}, {0, 0}, {1, -1}],
    lists:sum([1 || XY <- maps:keys(Map), is_mas(XY, Direction_1, Map) andalso is_mas(XY, Direction_2, Map)]).

is_mas(XY, Direction, Map) ->
    Values = get_coords(XY, Direction, Map),
    Values =:= "MAS" orelse Values =:= "SAM".