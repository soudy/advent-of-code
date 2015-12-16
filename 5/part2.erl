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

-module(part2).
-export([main/0, is_nice/1]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 2: ~p~n", [nice_string_len(Lines)]).

nice_string_len(Lines) ->
    length(lists:filtermap(fun part2:is_nice/1, Lines)).

is_nice(L) ->
    letter_inbetween(L) and repeating_pair(L).

letter_inbetween(L) -> part1:re_bool(L, "([a-z])[a-z]\\1").
repeating_pair(L) -> part1:re_bool(L, "([a-z][a-z]).*\\1").
