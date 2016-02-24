%%% Uh oh - the Accounting-Elves have realized that they double-counted
%%% everything red.

%%% Ignore any object (and all of its children) which has any property with the
%%% value "red". Do this only for objects ({...}), not arrays ([...]).

-module(part2).
-export([main/0]).

main() ->
    {ok, Data} = file:read_file("./input"),
    io:fwrite("Part 2: ~p~n", [lists:sum(get_integers(Data))]).

get_integers(Data) ->
    % I stole this regex from /u/askalski
    Redless = re:replace(Data, "{(\\[(\\[(?2)*]|{(?2)*}|[^][}{])*]|{(?2)*}|[^][}{])*red(?1)*}", "", [global,{return,list}]),
    {match, Ints} = re:run(Redless, "(-?\\d+)", [global,{capture,[1],list}]),
    lists:map(fun(X) -> list_to_integer(lists:flatten(X)) end, Ints).
