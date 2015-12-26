%%% This year, Santa brought little Bobby Tables a set of wires and bitwise
%%% logic gates! Unfortunately, little Bobby is a little under the recommended
%%% age range, and he needs help assembling the circuit.
%%%
%%% Each wire has an identifier (some lowercase letters) and can carry a 16-bit
%%% signal (a number from 0 to 65535). A signal is provided to each wire by a
%%% gate, another wire, or some specific value. Each wire can only get a signal
%%% from one source, but can provide its signal to multiple destinations. A gate
%%% provides no signal until all of its inputs have a signal.
%%%
%%% The included instructions booklet describes how to connect the parts
%%% together: x AND y -> z means to connect wires x and y to an AND gate, and
%%% then connect its output to wire z.
%%%
%%% In little Bobby's kit's instructions booklet (provided as your puzzle
%%% input), what signal is ultimately provided to wire a?

-module(part2).
-export([main/0]).

main() ->
    {ok, Data} = file:read_file("./input"),
    ets:new(circuits, [public, named_table]),
    init_circuits(Data),
    A = lookup("a"),
    init_circuits(Data),
    ets:insert(circuits, {"b", A}),
    io:fwrite("Part 2: ~p~n", [lookup("a")]).

init_circuits(Operations) ->
    Circuits = [string:tokens(binary_to_list(Op), " ->")
                || Op <- binary:split(Operations, <<"\n">>, [global, trim_all])],
    lists:foreach(fun(X) ->
        ets:insert(circuits, {lists:last(X), lists:droplast(X)})
    end, Circuits).

lookup(Key) ->
    R = case string:to_integer(Key) of
        {error, _} ->
            case ets:lookup(circuits, Key) of
                [{_, Val}] when is_integer(Val) -> Val;
                [{_, Val}] when is_list(Val) ->
                    case Val of
                        [X]              -> lookup(X);
                        ["NOT", X]       -> bnot lookup(X);
                        [X, "OR", Y]     -> lookup(X) bor lookup(Y);
                        [X, "AND", Y]    -> lookup(X) band lookup(Y);
                        [X, "RSHIFT", Y] -> lookup(X) bsr lookup(Y);
                        [X, "LSHIFT", Y] -> lookup(X) bsl lookup(Y)
                    end
            end;
        {Val, _} -> Val
    end,
    ets:insert(circuits, {Key, R}),
    R.
