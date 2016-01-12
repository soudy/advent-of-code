%%% Santa's previous password expired, and he needs help choosing a new one.
%%%
%%% To help him remember his new password after the old one expires, Santa has
%%% devised a method of coming up with a password based on the previous one.
%%% Corporate policy dictates that passwords must be exactly eight lowercase
%%% letters (for security reasons), so he finds his new password by incrementing
%%% his old password string repeatedly until it is valid.
%%%
%%% Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
%%% on. Increase the rightmost letter one step; if it was z, it wraps around to
%%% a, and repeat with the next letter to the left until one doesn't wrap
%%% around.
%%%
%%% Unfortunately for Santa, a new Security-Elf recently started, and he has
%%% imposed some additional password requirements:
%%%
%%% - Passwords must include one increasing straight of at least three letters,
%%% like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
%%% doesn't count.
%%% - Passwords may not contain the letters i, o, or l, as these letters can be
%%% mistaken for other characters and are therefore confusing.
%%% - Passwords must contain at least two different, non-overlapping pairs of
%%% letters, like aa, bb, or zz.
%%%
%%% For example:
%%%
%%% - hijklmmn meets the first requirement (because it contains the straight hij)
%%% but fails the second requirement requirement (because it contains i and l).
%%% - abbceffg meets the third requirement (because it repeats bb and ff) but
%%% fails the first requirement.
%%% - abbcegjk fails the third requirement, because it only has one double
%%% letter (bb).
%%%
%%% The next password after abcdefgh is abcdffaa.
%%% The next password after ghijklmn is ghjaabcc, because you eventually skip
%%% all the passwords that start with ghi..., since i is not allowed.
%%%
%%% Given Santa's current password (your puzzle input), what should his next
%%% password be?

-module(bothparts).
-export([main/0]).

main() ->
    Input = "vzbxkghb",
    First = get_next(Input),
    io:fwrite("Part 1: ~s~n", [First]),
    io:fwrite("Part 2: ~s~n", [get_next(First)]).

is_valid_password(L) ->
    has_straight_letters(L) and has_no_confusing_letters(L) and has_pairs(L).

has_straight_letters(L) ->
    % Generate abc, bcd, cde ... xyz regex pattern
    Pattern = string:join([[X, X+1, X+2] || X <- lists:seq($a, $x)], "|"),
    re_bool(L, Pattern).

has_no_confusing_letters(L) ->
    not re_bool(L, "i|o|l").

has_pairs(L) ->
    re_bool(L, "([a-z])\\1.*([a-z])\\2").

get_next(L) ->
    case is_valid_password(next_pass(L)) of
        true -> next_pass(L);
        false -> get_next(next_pass(L))
    end.

next_pass(L) -> lists:reverse(increment_pass(lists:reverse(L))).

increment_pass([$z|T]) -> [$a|increment_pass(T)];
increment_pass([H|T])  -> [H + 1|T].

re_bool(L, Pattern) ->
    case re:run(L, Pattern, [global]) of
        {match, _} -> true;
        nomatch    -> false
    end.
