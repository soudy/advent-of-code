%%% Because your neighbors keep defeating you in the holiday house decorating
%%% contest year after year, you've decided to deploy one million lights in a
%%% 1000x1000 grid.
%%%
%%% Furthermore, because you've been especially nice this year, Santa has mailed
%%% you instructions on how to display the ideal lighting configuration.
%%%
%%% Lights in your grid are numbered from 0 to 999 in each direction; the lights
%%% at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
%%% include whether to turn on, turn off, or toggle various inclusive ranges
%%% given as coordinate pairs. Each coordinate pair represents opposite corners
%%% of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
%%% refers to 9 lights in a 3x3 square. The lights all start turned off.

-module(part1).
-export([main/0, parse_range/1]).

main() ->
    {ok, Data} = file:read_file("./input"),
    Instructions = binary:split(Data, <<"\n">>, [global, trim_all]),
    io:fwrite("Part 1: ~p~n", [count_lights(Instructions)]).

count_lights(Instructions) ->
    count_lights(Instructions, maps:new()).
count_lights([H|T], Lights) ->
    case string:tokens(binary_to_list(H), " ,") of
        ["turn"|Rest] ->
            count_lights(T, switch_lights(lists:nth(1, Rest),
                                          parse_range(lists:nthtail(1, Rest)),
                                          Lights));
        ["toggle"|Rest] ->
            count_lights(T, toggle_lights(parse_range(Rest), Lights))
    end;
count_lights([], Lights) ->
    maps:size(Lights).

parse_range(Instruction) ->
    % Cut Instruction in the 4 pieces we're interested in: the positions 1, 2, 4
    % and 5.
    [X1, X2, Y1, Y2] = lists:map(fun(X) ->
                                    list_to_integer(lists:nth(X, Instruction))
                                 end, [1, 2, 4, 5]),
    [{X, Y} || X <- lists:seq(X1, Y1), Y <- lists:seq(X2, Y2)].

toggle_lights([H|T], Lights) ->
    case maps:get(H, Lights, off) of
        on  -> toggle_lights(T, maps:remove(H, Lights));
        off -> toggle_lights(T, maps:put(H, on, Lights))
    end;
toggle_lights([], Lights) ->
    Lights.

switch_lights(Status, [H|T], Lights) ->
    case Status of
        "on"  -> switch_lights(Status, T, maps:put(H, on, Lights));
        "off" -> switch_lights(Status, T, maps:remove(H, Lights))
    end;
switch_lights(_, [], Lights) ->
    Lights.
