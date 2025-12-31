---
description: コード風プロンプト例8b Erlangのactor:エージェント定義ありのspawn
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(actor_definition).
-export([main/0, worker/2]).

worker(Id, Parent) ->
    io:format("foo~p~n", [Id]),
    Parent ! {done, Id}.

main() ->
    Self = self(),

    spawn(?MODULE, worker, [1, Self]),
    spawn(?MODULE, worker, [2, Self]),

    receive
        {done, 1} -> ok
    end,
    receive
        {done, 2} -> ok
    end,

    io:format("bar~n").
```
