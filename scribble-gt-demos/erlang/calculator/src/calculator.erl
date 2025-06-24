
-module(calculator).
-export([main/1]).

main(Args) ->
    try
        [A, B, Op] = Args,
        NumA = list_to_integer(A),
        NumB = list_to_integer(B),
        application:start(calculator),
        carol:start_link(),
        sry:start_link(),
        alice:start_link(),
        case Op of
            "sum" ->
                io:format("Result: ~p~n", [NumA + NumB]);
            "diff" ->
                io:format("Result: ~p~n", [NumA - NumB]);
            _ ->
                io:format("Invalid operation~n")
        end
    catch
        _:_ ->
            io:format("Usage: erl -noshell -s calculator main -s init stop -- <num1> <num2> <sum|diff>~n")
    end.
