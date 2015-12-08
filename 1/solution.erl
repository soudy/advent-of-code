#! /usr/bin/env escript

%%% -- Part One --
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
%%%
%%% -- Part Two --
%%% Now, given the same instructions, find the position of the first character that
%%% causes him to enter the basement (floor -1). The first character in the

%% Part One
get_floor_level(L) ->
    get_floor_level(L, 0).
get_floor_level(<<H,T/binary>>, Floor) ->
    case H of
        $( -> get_floor_level(T, Floor + 1);
        $) -> get_floor_level(T, Floor - 1);
        _  -> error
    end;
get_floor_level(<<>>, Floor) ->
    Floor.

%% Part Two
find_basement(L) ->
    find_basement(L, 0, 0).
find_basement(_, -1, Count) ->
	Count;
find_basement(<<H,T/binary>>, Floor, Count) ->
    case H of
        $( -> find_basement(T, Floor + 1, Count + 1);
        $) -> find_basement(T, Floor - 1, Count + 1);
        _  -> error
    end.

start(File) ->
    case file:read_file(File) of
        {ok, Fd} ->
            Contents = binary:replace(Fd, <<"\n">>, <<>>),
            io:fwrite("Solution 1: ~p~n", [get_floor_level(Contents)]),
            io:fwrite("Solution 2: ~p~n", [find_basement(Contents)]),
            file:close(Fd);
        {error, Reason} ->
            io:fwrite("Something went wrong: ~s~n", [Reason]),
            usage()
    end.

main([File]) ->
    start(File);
main([]) ->
    start(filename:join(filename:dirname(escript:script_name()), "input"));
main(_) ->
    usage().

usage() ->
    io:fwrite("Usage: ~s [input-file]~n", [escript:script_name()]).
