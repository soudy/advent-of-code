%%% Santa needs help mining some AdventCoins (very similar to bitcoins) to use
%%% as gifts for all the economically forward-thinking little girls and boys.
%%%
%%% To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
%%% least five zeroes. The input to the MD5 hash is some secret key (your puzzle
%%% input, given below) followed by a number in decimal. To mine AdventCoins,
%%% you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3,
%%% ...) that produces such a hash.

-module(part1).
-export([main/0, mine_advent_coins/2]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Contents = binary:replace(Fd, <<"\n">>, <<>>),
    file:close(Fd),
    io:fwrite("Part 1: ~p~n", [mine_advent_coins(Contents, "00000")]).

mine_advent_coins(Input, Match) ->
    mine_advent_coins(Input, 1, Match).
mine_advent_coins(Input, C, Match) ->
    case string:str(md5_hex([Input|integer_to_list(C)]), Match) of
        1 -> C;
        _ -> mine_advent_coins(Input, C + 1, Match)
    end.

md5_hex(Input) ->
    lists:flatten(list_to_hex(binary_to_list(erlang:md5(Input)))).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
