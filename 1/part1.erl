%%% Santa is trying to deliver presents in a large apartment building, but he can't
%%% find the right floor - the directions he got are a little confusing. He starts
%%% on the ground floor (floor 0) and then follows the instructions one character
%%% at a time.
%%%
%%% An opening parenthesis, (, means he should go up one floor, and a closing
%%% parenthesis, ), means he should go down one floor.
%%%
%%% The apartment building is very tall, and the basement is very deep; he will
%%% never find the top or bottom floors.

-module(part1).
-export([main/0]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    io:fwrite("Part 1: ~p~n", [get_floor_level(Fd)]),
    file:close(Fd).

get_floor_level(L) ->
    get_floor_level(L, 0).
get_floor_level(<<H,T/binary>>, Floor) ->
    case H of
        $( -> get_floor_level(T, Floor + 1);
        $) -> get_floor_level(T, Floor - 1);
        _  -> get_floor_level(T, Floor)
    end;
get_floor_level(<<>>, Floor) ->
    Floor.
