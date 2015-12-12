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
-export([main/0, is_nice_one/1, is_nice_two/1, part1/1, part2/1]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 1: ~p~n", [part1(Lines)]),
    io:fwrite("Part 2: ~p~n", [part2(Lines)]).

part1(Lines) ->
    length(lists:filtermap(fun day5:is_nice_one/1, Lines)).

part2(Lines) ->
    length(lists:filtermap(fun day5:is_nice_two/1, Lines)).

is_nice_one(L) ->
    (num_vowels(L) >= 3) and appears_twice(L) and not has_evil_string(L).

is_nice_two(L) ->
    letter_inbetween(L) and repeating_pair(L).

%% part one rules
num_vowels(L) ->
    case re:run(L, "[aeuio]", [global]) of
        {match, Matches} -> length(Matches);
        nomatch          -> 0
    end.
has_evil_string(L) -> re_bool(L, "(ab|cd|pq|xy)").
appears_twice(L) -> re_bool(L, "([a-z])\\1").

%% part two rules
letter_inbetween(L) -> re_bool(L, "([a-z])[a-z]\\1").
repeating_pair(L) -> re_bool(L, "([a-z][a-z]).*\\1").

re_bool(L, Pattern) ->
    case re:run(L, Pattern, [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.
