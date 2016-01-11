%%% -- Part One --
%%% Look-and-say sequences are generated iteratively, using the previous value
%%% as input for the next step. For each step, take the previous value, and
%%% replace each run of digits (like 111) with the number of digits (3) followed
%%% by the digit itself (1).
%%%
%%% For example:
%%%
%%% - 1 becomes 11 (1 copy of digit 1).
%%% - 11 becomes 21 (2 copies of digit 1).
%%% - 21 becomes 1211 (one 2 followed by one 1).
%%% - 1211 becomes 111221 (one 1, one 2, and two 1s).
%%% - 111221 becomes 312211 (three 1s, two 2s, and one 1).
%%%
%%% Starting with the digits in your puzzle input, apply this process 40 times.
%%% What is the length of the result?
%%%
%%% -- Part Two --
%%% Now, starting again with the digits in your puzzle input, apply this process
%%% 50 times. What is the length of the new result?

-module(bothparts).
-export([main/0]).

main() ->
    Input = "1113122113",
    io:fwrite("Part 1: ~p~n", [look_and_say(Input, 40)]),
    io:fwrite("Part 2: ~p~n", [look_and_say(Input, 50)]).

look_and_say(L, A) ->
    look_and_say(L, [], A).
look_and_say(L, _Result, 0) ->
    length(L);
look_and_say([], Result, A) ->
    Res = lists:flatten(
        lists:map(fun(X) ->
                case X of
                    {Val, N} -> [$0 + N, Val];
                    Val      -> [$1, Val]
                end
            end,
            lists:reverse(Result))
     ),
    look_and_say(Res, [], A - 1);
look_and_say([H|T], [{H, C}|Result], A) ->
    look_and_say(T, [{H, C + 1}|Result], A);
look_and_say([H, H|T], Result, A) ->
    look_and_say(T, [{H, 2}|Result], A);
look_and_say([H|T], Result, A) ->
    look_and_say(T, [H|Result], A).
