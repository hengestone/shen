-module(sample).
-compile({parse_transform, shen}).
-export([start/0, start2/2, fac/1, macro/3, main/0]).

-output("priv").
-js([start/0,start2/2,fac/1]).
-jsmacro([macro/3]).
-postfix("start();").

-prelude("
const jObject = Object;
const jMap    = Map;
").

macro(A, B, C) ->
    X = document,
    jq(X),
    jq("document"),
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).

start2(X, Y) ->
    _MyMap = jObject:create(jMap),
    F = fun(A) -> A:log("100") end,
    F(console),
    case X of
        1 -> console:log([X,Y]);
        Y -> console:log("ok") end.

h1(_) -> ok.

start() ->
    h1(#{id => "myproduct_id"}),
    start2(1, 3),
    J = 5,
    N = fac(J),
    {_A, _B} = start2(J, N),
    console:log("factorial ~p", [J, N]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

main() ->
    A = "1",
    B = "2",
    Script = macro(A,B,"3"),
    io:format("JS Macro: ~s",[Script]).

