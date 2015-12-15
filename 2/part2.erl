%%% The elves are also running low on ribbon. Ribbon is all the same width, so
%%% they only have to worry about the length they need to order, which they would
%%% again like to be exact.
%%%
%%% The ribbon required to wrap a present is the shortest distance around its
%%% sides, or the smallest perimeter of any one face. Each present also requires a
%%% bow made out of ribbon as well; the feet of ribbon required for the perfect
%%% bow is equal to the cubic feet of volume of the present. Don't ask how they
%%% tie the bow, though; they'll never tell.
%%%
%%% How many total feet of ribbon should they order?

-module(part2).
-export([main/0]).

-import(part1, [parse_dimensions/1]).

main() ->
    {ok, Fd} = file:read_file("./input"),
    Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
    file:close(Fd),
    io:fwrite("Part 2: ~p~n", [total_feet_ribbon(Lines)]).

total_feet_ribbon(Lines) ->
    lists:sum(lists:map(fun(X) -> ribbon_surface_area(X) end, Lines)).

ribbon_surface_area(Dimensions) ->
    case part1:parse_dimensions(Dimensions) of
        {L, W, H} ->
            % FIXME: this is kind of ugly, find a better way to do this
            Addition = lists:sum(lists:sublist(lists:sort([L, W, H]), 2)) * 2,
            L * W * H + Addition;
        _ ->
            error
    end.

