%%% Santa needs help figuring out which strings in his text file are naughty or nice.
%%%
%%% A nice string is one with all of the following properties:
%%%
%%% - It contains at least three vowels (aeiou only), like aei, xazegov, or
%%%   aeiouaeiouaeiou.
%%% - It contains at least one letter that appears twice in a row, like xx, abcdde
%%%   (dd), or aabbccdd (aa, bb, cc, or dd).
%%% - It does not contain the strings ab, cd, pq, or xy, even if they are part of
%%%   one of the other requirements.
%%%
%%% How many strings are nice?

-module(part1).
-export([main/0, is_nice/1, re_bool/2]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 1: ~p~n", [nice_string_len(Lines)]).

nice_string_len(Lines) ->
    length(lists:filtermap(fun part1:is_nice/1, Lines)).

is_nice(L) ->
    (num_vowels(L) >= 3) and appears_twice(L) and not has_evil_string(L).

num_vowels(L) ->
    case re:run(L, "[aeuio]", [global]) of
        {match, Matches} -> length(Matches);
        nomatch          -> 0
    end.

has_evil_string(L) -> re_bool(L, "(ab|cd|pq|xy)").
appears_twice(L) -> re_bool(L, "([a-z])\\1").

re_bool(L, Pattern) ->
    case re:run(L, Pattern, [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.
