%%% You just finish implementing your winning light pattern when you realize you
%%% mistranslated Santa's message from Ancient Nordic Elvish.
%%% The light grid you bought actually has individual brightness controls; each
%%% light can have a brightness of zero or more. The lights all start at zero.
%%%
%%% The phrase turn on actually means that you should increase the brightness of
%%% those lights by 1.
%%%
%%% The phrase turn off actually means that you should decrease the brightness
%%% of those lights by 1, to a minimum of zero.
%%%
%%% The phrase toggle actually means that you should increase the brightness of
%%% those lights by 2.
%%%
%%% What is the total brightness of all lights combined after following Santa's
%%% instructions?

-module(part2).
-export([main/0]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Instructions = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 2: ~p~n", [count_lights(Instructions)]).

count_lights(Instructions) ->
    count_lights(Instructions, maps:new()).
count_lights([H|T], Lights) ->
    case string:tokens(binary_to_list(H), " ,") of
        ["turn"|Rest] ->
            count_lights(T, switch_lights(lists:nth(1, Rest),
                                          part1:parse_range(lists:nthtail(1, Rest)),
                                          Lights));
        ["toggle"|Rest] ->
            count_lights(T, toggle_lights(part1:parse_range(Rest), Lights))
    end;
count_lights([], Lights) ->
    maps:fold(fun(_, V, Sum) -> Sum + V end, 0, Lights).

toggle_lights([H|T], Lights) ->
    toggle_lights(T, maps:put(H, maps:get(H, Lights, 0) + 2, Lights));
toggle_lights([], Lights) ->
    Lights.

switch_lights(Status, [H|T], Lights) ->
    CurrentLight = maps:get(H, Lights, 0),
    case Status of
        "on" ->
            switch_lights(Status, T, maps:put(H, CurrentLight + 1, Lights));
        "off" when (CurrentLight > 0) ->
            switch_lights(Status, T, maps:put(H, CurrentLight - 1, Lights));
        "off" ->
            switch_lights(Status, T, maps:put(H, CurrentLight, Lights))
    end;
switch_lights(_, [], Lights) ->
    Lights.
