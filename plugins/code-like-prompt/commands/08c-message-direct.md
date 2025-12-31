---
description: コード風プロンプト例8c Erlangのactor:メッセージパッシング（直接記述）
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(message_passing).
-export([main/0]).

main() ->
    Self = self(),

    Pid = spawn(fun() ->
        receive
            {request, From, Data} ->
                Result = "processed_" ++ Data,
                From ! {response, Result}
        end
    end),

    Pid ! {request, Self, "foo"},

    receive
        {response, Result} ->
            io:format("~s~n", [Result])
    end,

    io:format("bar~n").
```
