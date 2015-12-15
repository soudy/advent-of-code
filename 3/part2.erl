%%% The next year, to speed up the process, Santa creates a robot version of
%%% himself, Robo-Santa, to deliver presents with him.
%%%
%%% Santa and Robo-Santa start at the same location (delivering two presents to
%%% the same starting house), then take turns moving based on instructions from
%%% the elf, who is eggnoggedly reading from the same script as the previous
%%% year.
%%%
%%% This year, how many houses receive at least one present?

-module(part2).
-export([main/0]).

-import(part1, [deliver_presents/1]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    io:fwrite("Part 2: ~p~n", [deliver_robo_presents(Fd)]),
    file:close(Fd).

deliver_robo_presents(Pattern) ->
    {Santa, Robo} = binary_split_odds(Pattern),
    sets:size(sets:union(part1:deliver_presents(Santa), part1:deliver_presents(Robo))).

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
