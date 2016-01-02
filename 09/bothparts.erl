%%% --- Part One ---
%%% Every year, Santa manages to deliver all of his presents in a single night.
%%%
%%% This year, however, he has some new locations to visit; his elves have
%%% provided him the distances between every pair of locations. He can start and
%%% end at any two (different) locations he wants, but he must visit each
%%% location exactly once. What is the shortest distance he can travel to
%%% achieve this?
%%%
%%% --- Part Two ---
%%% The next year, just to show off, Santa decides to take the route with the
%%% longest distance instead.
%%%
%%% He can still start and end at any two (different) locations he wants, and he
%%% still must visit each location exactly once.
%%%
%%% For example, given the distances above, the longest route would be 982 via
%%% (for example) Dublin -> London -> Belfast.
%%%
%%% What is the distance of the longest route?

-module(bothparts).
-export([main/0]).

main() ->
    {ok, Data} = file:read_file("./input"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Routes = init_routes(Lines),
    Perms = generate_perms(Routes),
    Distances = lists:map(fun(X) -> distance(X, Routes) end, Perms),
    io:fwrite("Part 1: ~p~n", [lists:min(Distances)]),
    io:fwrite("Part 2: ~p~n", [lists:max(Distances)]).

init_routes(L) ->
    init_routes(L, maps:new()).
init_routes([H|T], Routes) ->
    [F, _, To, _, D] = re:split(H, "\s+", [{return, list}, trim]),
    init_routes(T, maps:put([F, To], list_to_integer(D), Routes));
init_routes([], Routes) ->
    Routes.

%% http://www.erlang.org/doc/programming_examples/list_comprehensions.html#id65030
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L -- [H])].

generate_perms(Routes) ->
    perms(sets:to_list(
            sets:from_list(
                lists:flatmap(fun(X) -> {[F, T], _} = X, [F, T] end, maps:to_list(Routes))
            ))).

distance(L, Routes) ->
    distance(L, Routes, 0).
distance([C1,C2|T], Routes, Acc) ->
    case maps:get([C1, C2], Routes, none) of
        none -> case maps:get([C2, C1], Routes, none) of
                    none -> error;
                    Val  -> distance([C2|T], Routes, Acc + Val)
                end;
        Val -> distance([C2|T], Routes, Acc + Val)
    end;
distance([_|[]], _Routes, Acc) ->
    Acc.
