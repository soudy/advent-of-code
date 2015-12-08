#! /usr/bin/env escript
%% -*- erlang -*-

%%% -- Part One --
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
%%%
%%% --- Part Two ---
%%%
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

%% Part One
rect_surface_area(Dimensions) ->
    case parse_dimensions(Dimensions) of
        {L, W, H} ->
            Slack = lists:min([L * W, L * H, W * H]),
            (2 * L * W + 2 * W * H + 2 * H * L) + Slack;
        _ ->
            error
    end.

%% Part Two
ribbon_surface_area(Dimensions) ->
    case parse_dimensions(Dimensions) of
        {L, W, H} ->
            Addition = lists:sum(lists:sublist(lists:sort([L, W, H]), 2)) * 2,
            L * W * H + Addition;
        _ ->
            error
    end.

start(File) ->
    case file:read_file(File) of
        {ok, Fd} ->
            Lines = binary:split(Fd, <<"\n">>, [global, trim_all]),
            PaperSqFeet = lists:sum(lists:map(fun(X) ->
                                                rect_surface_area(X)
                                              end, Lines)),
            RibbonSqFeet = lists:sum(lists:map(fun(X) ->
                                                ribbon_surface_area(X)
                                               end, Lines)),
            io:fwrite("Solution 1: ~p~n", [PaperSqFeet]),
            io:fwrite("Solution 2: ~p~n", [RibbonSqFeet]),
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
