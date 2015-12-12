%%% -- Part One --
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
%%%
%%% -- Part Two --
%%% Realizing the error of his ways, Santa has switched to a better model of
%%% determining whether a string is naughty or nice. None of the old rules apply,
%%% as they are all clearly ridiculous.
%%%
%%% Now, a nice string is one with all of the following properties:
%%%
%%% - It contains a pair of any two letters that appears at least twice in the
%%%   string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like
%%%   aaa (aa, but it overlaps).
%%% - It contains at least one letter which repeats with exactly one letter
%%%   between them, like xyx, abcdefeghi (efe), or even aaa.


-module(day5).
-export([main/0]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 1: ~p~n", [part1(Lines)]),
    io:fwrite("Part 2: ~p~n", [part2(Lines)]).

part1(Lines) ->
    length(lists:filtermap(fun(X) -> is_nice_one(X) end, Lines)).

part2(Lines) ->
    length(lists:filtermap(fun(X) -> is_nice_two(X) end, Lines)).

is_nice_one(L) ->
    (num_vowels(L) >= 3) and appears_twice(L) and not has_evil_string(L).

is_nice_two(L) ->
    letter_inbetween(L) and repeating_pair(L).

num_vowels(L) ->
    case re:run(L, "[aeuio]", [global]) of
        {match, Matches} -> length(Matches);
        nomatch          -> 0
    end.

has_evil_string(L) ->
    case re:run(L, "(ab|cd|pq|xy)", [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.

appears_twice(L) ->
    case re:run(L, "([a-z])\\1", [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.

letter_inbetween(L) ->
    case re:run(L, "([a-z])[a-z]\\1", [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.

repeating_pair(L) ->
    case re:run(L, "([a-z][a-z]).*\\1", [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.
