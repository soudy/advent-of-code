#! /usr/bin/env escript

% -- Part One --
% Santa is trying to deliver presents in a large apartment building, but he can't
% find the right floor - the directions he got are a little confusing. He starts
% on the ground floor (floor 0) and then follows the instructions one character
% at a time.
%
% An opening parenthesis, (, means he should go up one floor, and a closing
% parenthesis, ), means he should go down one floor.
%
% The apartment building is very tall, and the basement is very deep; he will
% never find the top or bottom floors.
%
% -- Part Two --
% Now, given the same instructions, find the position of the first character that
% causes him to enter the basement (floor -1). The first character in the
% instructions has position 1, the second character has position 2, and so on.

get_floor_level(L) ->
    get_floor_level(L, 0).
get_floor_level(<<H,T/binary>>, N) ->
    case H of
        $( -> get_floor_level(T, N + 1);
        $) -> get_floor_level(T, N - 1);
        _  -> N
    end.

start(File) ->
    case file:read_file(File) of
        {ok, Fd} ->
            io:fwrite("Solution 1: ~b~n", [get_floor_level(Fd)]),
            file:close(Fd);
        {error, Reason} ->
            io:fwrite("Something went wrong: ~s~n", [Reason]),
            usage()
    end.

main([File]) ->
    start(File);
main([]) ->
    start("./input/day1");
main(_) ->
    usage().

usage() ->
    io:fwrite("Usage: ~s [input-file]~n", [escript:script_name()]).
