%%% The elves are running low on wrapping paper, and so they need to submit an
%%% order for more. They have a list of the dimensions (length l, width w, and
%%% height h) of each present, and only want to order exactly as much as they
%%% need.
%%%
%%% Fortunately, every present is a box (a perfect right rectangular prism), which
%%% makes calculating the required wrapping paper for each gift a little easier:
%%% find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves
%%% also need a little extra paper for each present: the area of the smallest
%%% side.
%%%
%%% All numbers in the elves' list are in feet. How many total square feet of
%%% wrapping paper should they order?

-module(part1).
-export([main/0, parse_dimensions/1]).

main() ->
    {ok, Data} = file:read_file("./input"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    io:fwrite("Part 1: ~p~n", [total_surface_area(Lines)]).

total_surface_area(Lines) ->
    lists:sum(lists:map(fun(X) -> rect_surface_area(X) end, Lines)).

rect_surface_area(Dimensions) ->
    case parse_dimensions(Dimensions) of
        {L, W, H} ->
            Slack = lists:min([L * W, L * H, W * H]),
            (2 * L * W + 2 * W * H + 2 * H * L) + Slack;
        _ ->
            error
    end.

parse_dimensions(Dimensions) ->
    case re:run(Dimensions, "^\\d+x\\d+x\\d+$") of
        {match, _} ->
            list_to_tuple(
                lists:map(fun(X) ->
                            {Int, _} = string:to_integer(X), Int
                          end,
                          re:split(Dimensions, "x", [{return, list}]))
             );
        _ ->
            error
    end.
