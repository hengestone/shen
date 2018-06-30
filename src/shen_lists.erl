-module(shen_lists).
-author('Maxim Sokhatsky').
-copyright('Synrc Research Center').
-import(shen,[exp/2]).
-export([map/3, foldl/4, foldr/4]).

map(Fun,List,Mode) ->
    io_lib:format("~s.map(~s);\n", [exp(List, Mode), exp(Fun, Mode)]).

foldl(Fun, Acc, List, Mode) ->
    io_lib:format("~s.reduce(~s, ~s);\n",
                  [exp(List, Mode), exp(Fun, Mode), exp(Acc, Mode)]
    ).

foldr(Fun, Acc, List, Mode) -> foldl(Fun, Acc, List, Mode).
