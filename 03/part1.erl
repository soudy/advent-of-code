%%% Santa is delivering presents to an infinite two-dimensional grid of houses.
%%%
%%% He begins by delivering a present to the house at his starting location, and
%%% then an elf at the North Pole calls him via radio and tells him where to move
%%% next. Moves are always exactly one house to the north (^), south (v), east
%%% (>), or west (<). After each move, he delivers another present to the house at
%%% his new location.
%%%
%%% However, the elf back at the north pole has had a little too much eggnog, and
%%% so his directions are a little off, and Santa ends up visiting some houses
%%% more than once. How many houses receive at least one present?

-module(part1).
-export([main/0, deliver_presents/1]).

main() ->
    {ok, Data} = file:read_file("./input"),
    io:fwrite("Part 1: ~p~n", [sets:size(deliver_presents(Data))]).

deliver_presents(Pattern) ->
    deliver_presents(Pattern, 0, 0, sets:new()).
deliver_presents(<<H, T/binary>>, X, Y, Houses) ->
    NewHouses = sets:add_element({X, Y}, Houses),
    case H of
        $^ -> deliver_presents(T, X, Y + 1, NewHouses);
        $v -> deliver_presents(T, X, Y - 1, NewHouses);
        $< -> deliver_presents(T, X - 1, Y, NewHouses);
        $> -> deliver_presents(T, X + 1, Y, NewHouses);
        _  -> deliver_presents(T, X, Y, Houses)
    end;
deliver_presents(<<>>, _, _, Houses) ->
    Houses.
