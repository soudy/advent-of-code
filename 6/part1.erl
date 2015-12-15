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
-export([main/0]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Instructions = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 1: ~p~n", [parse_instructions(Instructions)]).

parse_instructions(Instructions) ->
    parse_instructions(Instructions, maps:new()).
parse_instructions([H|T], Lights) ->
    case string:tokens(binary_to_list(H), " ,") of
        ["turn"|Rest] ->
            parse_instructions(T, switch_lights(lists:nth(1, Rest),
                                                parse_range(lists:nthtail(1, Rest)),
                                                Lights));
        ["toggle"|Rest] ->
            parse_instructions(T, toggle_lights(parse_range(Rest), Lights))
    end;
parse_instructions([], Lights) ->
    Lights.

parse_range(Instruction) ->
    X1 = list_to_integer(lists:nth(1, Instruction)),
    X2 = list_to_integer(lists:nth(2, Instruction)),
    Y1 = list_to_integer(lists:nth(4, Instruction)),
    Y2 = list_to_integer(lists:nth(5, Instruction)),
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
