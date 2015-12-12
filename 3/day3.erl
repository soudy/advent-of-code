%%% -- Part One --
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
%%%
%%% -- Part Two --
%%% The next year, to speed up the process, Santa creates a robot version of
%%% himself, Robo-Santa, to deliver presents with him.
%%%
%%% Santa and Robo-Santa start at the same location (delivering two presents to
%%% the same starting house), then take turns moving based on instructions from
%%% the elf, who is eggnoggedly reading from the same script as the previous
%%% year.
%%%
%%% This year, how many houses receive at least one present?

-module(day3).
-export([main/0, part1/1, part2/1]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    file:close(Fd),
    io:fwrite("Solution 1: ~p~n", [part1(Fd)]),
    io:fwrite("Solution 2: ~p~n", [part2(Fd)]).

part1(Fd) -> sets:size(deliver_presents(Fd)).
part2(Fd) -> deliver_robo_presents(Fd).

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

deliver_robo_presents(Pattern) ->
    {Santa, Robo} = binary_split_odds(Pattern),
    sets:size(sets:union(deliver_presents(Santa), deliver_presents(Robo))).

%% Splits a list into a tuple by each odd and each even value, so we get 2
%% paths: Santa's and Robot Santa's path
binary_split_odds(Input) ->
    binary_split_odds(Input, <<>>, <<>>).
binary_split_odds(<<H,T/binary>>, L, R) when (size(T) rem 2) =:= 0 ->
    binary_split_odds(T, <<L/binary, <<H>>/binary>>, R);
binary_split_odds(<<H,T/binary>>, L, R) ->
    binary_split_odds(T, L, <<R/binary, <<H>>/binary>>);
binary_split_odds(<<>>, L, R) ->
    {L, R}.
