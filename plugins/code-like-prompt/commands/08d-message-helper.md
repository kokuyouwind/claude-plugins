---
description: コード風プロンプト例8d Erlangのactor:メッセージパッシング（ヘルパー関数経由）
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(message_helper).
-export([main/0, send_and_wait/2]).

send_and_wait(Pid, Message) ->
    Pid ! {request, self(), Message},
    receive
        {response, Result} -> Result
    end.

worker() ->
    receive
        {request, From, Data} ->
            Result = "result_" ++ Data,
            From ! {response, Result}
    end.

main() ->
    Pid = spawn(fun worker/0),

    Result1 = send_and_wait(Pid, "foo"),
    io:format("~s~n", [Result1]),

    io:format("bar~n").
```
