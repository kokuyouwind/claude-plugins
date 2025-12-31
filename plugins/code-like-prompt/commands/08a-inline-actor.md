---
description: コード風プロンプト例8a Erlangのactor:インラインアクター（エージェント定義なし）
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(inline_actor).
-export([main/0]).

main() ->
    Self = self(),

    spawn(fun() ->
        io:format("foo~n"),
        Self ! done_foo
    end),

    spawn(fun() ->
        io:format("bar~n"),
        Self ! done_bar
    end),

    receive
        done_foo -> ok
    end,
    receive
        done_bar -> ok
    end,

    io:format("baz~n").
```
