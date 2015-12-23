%%% Now, let's go the other way. In addition to finding the number of characters
%%% of code, you should now encode each code representation as a new string and
%%% find the number of characters of the new encoded representation, including
%%% the surrounding double quotes.
%%%
%%% For example:
%%%
%%% - "" encodes to "\"\"", an increase from 2 characters to 6.
%%% - "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
%%% - "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
%%% - "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.
%%%
%%% Your task is to find the total number of characters to represent the newly
%%% encoded strings minus the number of characters of code in each original
%%% string literal.

-module(part2).
-export([main/0, enc_length/1]).

main() ->
    {ok, Data} = file:read_file("./input"),
    Strings = [binary_to_list(X) || X <- binary:split(Data, <<"\n">>,
                                                      [global, trim_all])],
    RealStrings = lists:foldl(fun(X, Sum) -> Sum + length(X) end, 0, Strings),
    EncStrings = lists:foldl(fun(X, Sum) -> Sum + enc_length(X) end, 0, Strings),
    io:fwrite("Part 2: ~p~n", [EncStrings - RealStrings]).

enc_length(S)                   -> enc_length(S, 2).
enc_length([], Acc)             -> Acc;
enc_length([$\\,$x,_,_|T], Acc) -> enc_length(T, Acc + 5);
enc_length([$\\|T], Acc)        -> enc_length(T, Acc + 2);
enc_length([$"|T], Acc)         -> enc_length(T, Acc + 2);
enc_length([_|T], Acc)          -> enc_length(T, Acc + 1).
