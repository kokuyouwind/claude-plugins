---
description: コード風プロンプト例8e Erlangのactor:パターンマッチングによる選択的受信
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(selective_receive).
-export([main/0]).

main() ->
    Self = self(),

    % Send messages in specific order
    Self ! {msg, "foo"},
    Self ! {data, 123},
    Self ! {msg, "bar"},

    % Receive only {msg, _} messages
    receive
        {msg, Text} ->
            io:format("~s~n", [Text])
    end,

    receive
        {msg, Text} ->
            io:format("~s~n", [Text])
    end,

    % Now receive the {data, _} message
    receive
        {data, Num} ->
            io:format("~p~n", [Num])
    end,

    io:format("baz~n").
```
