-module(sample).
-compile({parse_transform, shen}).
-export([fac/1]).

-output("priv").
-js([fac/1]).
-jsmacro([macro/3]).
-postfix("start();").

-prelude("
const jObject = Object;
const jMap    = Map;
").

jq(D) -> ok.
macro(A, B, C) ->
    X = document,
    jq(X),
    jq("document"),
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).

% mult(X) ->
%     start2(X, 2).
% mult(X, Y) ->
%     start2(X, Y).
% mult(X, Y, Z) ->
%     start2(X, Y).

% start2(X, Y) ->
%     _MyMap = jObject:create(jMap),
%     F = fun(A) -> A:log("100") end,
%     F(console),
%     case X of
%         1 -> console:log([X,Y]);
%         Y -> console:log("ok") end.

% h1(_) -> ok.

% start() ->
%     h1(#{id => "myproduct_id"}),
%     start2(1, 3),
%     J = 5,
%     N = fac(J),
%     {_A, _B} = start2(J, N),
%     console:log("factorial ~p", [J, N]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

% main() ->
%     A = "1",
%     B = "2",
%     Script = macro(A,B,"3"),
%     io:format("JS Macro: ~s",[Script]).

