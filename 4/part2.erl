%%% Now find one that starts with six zeroes.

-module(part2).
-export([main/0]).

-import(part1, [mine_advent_coins/2]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Contents = binary:replace(Fd, <<"\n">>, <<>>),
    file:close(Fd),
    io:fwrite("Part 2: ~p~n", [part1:mine_advent_coins(Contents, "000000")]).
