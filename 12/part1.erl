%%% Santa's Accounting-Elves need help balancing the books after a recent order.
%%% Unfortunately, their accounting software uses a peculiar storage format.
%%% That's where you come in.
%%%
%%% They have a JSON document which contains a variety of things: arrays
%%% ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is
%%% to simply find all of the numbers throughout the document and add them
%%% together.
%%%
%%% For example:
%%%
%%% - [1,2,3] and {"a":2,"b":4} both have a sum of 6.
%%% - [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
%%% - {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
%%% - [] and {} both have a sum of 0.
%%% You will not encounter any strings containing numbers.
%%%
%%% What is the sum of all numbers in the document?

-module(part1).
-export([main/0]).

main() ->
    {ok, Data} = file:read_file("./input"),
    io:fwrite("Part 1: ~p~n", [lists:sum(get_integers(Data))]).

get_integers(Data) ->
    {match, Ints} = re:run(Data, "(-?\\d+)", [global,{capture,[1],list}]),
    lists:map(fun(X) -> list_to_integer(lists:flatten(X)) end, Ints).
