---
description: コード風プロンプト例8f Erlangのactor:複数アクター間のメッセージルーティング
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(multi_actor).
-export([main/0]).

main() ->
    Self = self(),

    % Coordinator actor
    Coordinator = spawn(fun() ->
        receive
            {result, Worker1Result} ->
                receive
                    {result, Worker2Result} ->
                        io:format("~s~n", [Worker1Result ++ Worker2Result]),
                        Self ! done
                end
        end
    end),

    % Worker actors
    spawn(fun() ->
        Coordinator ! {result, "foo"}
    end),

    spawn(fun() ->
        Coordinator ! {result, "bar"}
    end),

    receive
        done -> ok
    end,

    io:format("baz~n").
```
