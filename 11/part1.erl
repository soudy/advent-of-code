%%% Unfortunately for Santa, a new Security-Elf recently started, and he has
%%% imposed some additional password requirements:
%%%
%%% - Passwords must include one increasing straight of at least three letters,
%%% like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
%%% doesn't count.
%%%
%%% - Passwords may not contain the letters i, o, or l, as these letters can be
%%% mistaken for other characters and are therefore confusing.
%%%
%%% - Passwords must contain at least two different, non-overlapping pairs of
%%% letters, like aa, bb, or zz.
%%%
%%% For example:
%%%
%%% - hijklmmn meets the first requirement (because it contains the straight hij)
%%% but fails the second requirement requirement (because it contains i and l).
%%%
%%% - abbceffg meets the third requirement (because it repeats bb and ff) but
%%% fails the first requirement.
%%%
%%% - abbcegjk fails the third requirement, because it only has one double
%%% letter (bb).
%%%
%%% The next password after abcdefgh is abcdffaa.
%%% The next password after ghijklmn is ghjaabcc, because you eventually skip
%%% all the passwords that start with ghi..., since i is not allowed.
%%%
%%% Given Santa's current password (your puzzle input), what should his next
%%% password be?

-module(part1).
-export([main/0]).

main() ->
    Input = "vzbxkghb",
    Input.

has_straight_letters(L) ->
    [[X, X+1, X+2] || X <- "abcdefghijklmnopqrstuvwx"].

re_bool(L, Pattern) ->
    case re:run(L, Pattern, [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.
