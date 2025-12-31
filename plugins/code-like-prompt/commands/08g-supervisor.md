---
description: コード風プロンプト例8g Erlangのactor:監視パターン（supervisor）
argument-hint: ""
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

```erlang
-module(supervisor).
-export([main/0]).

supervised_worker(State) ->
    receive
        {get, From} ->
            From ! {state, State},
            supervised_worker(State);
        {set, NewState} ->
            supervised_worker(NewState);
        crash ->
            error(deliberate_crash)
    end.

supervisor(WorkerFun) ->
    Pid = spawn(fun() -> WorkerFun(0) end),
    receive
        {'EXIT', Pid, _Reason} ->
            io:format("foo~n"),
            supervisor(WorkerFun)
    end.

main() ->
    process_flag(trap_exit, true),
    SupPid = spawn_link(fun() -> supervisor(fun supervised_worker/1) end),

    % Demonstrate supervisor pattern without actual crash
    io:format("bar~n"),
    io:format("baz~n").
```
