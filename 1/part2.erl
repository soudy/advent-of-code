%%% Now, given the same instructions, find the position of the first character that
%%% causes him to enter the basement (floor -1). The first character in the

-module(part2).
-export([main/0]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    io:fwrite("Part 2: ~p~n", [find_basement(Fd)]),
    file:close(Fd).

find_basement(L) ->
    find_basement(L, 0, 0).
find_basement(_, -1, Count) ->
	Count;
find_basement(<<H,T/binary>>, Floor, Count) ->
    case H of
        $( -> find_basement(T, Floor + 1, Count + 1);
        $) -> find_basement(T, Floor - 1, Count + 1);
        _  -> error
    end.
