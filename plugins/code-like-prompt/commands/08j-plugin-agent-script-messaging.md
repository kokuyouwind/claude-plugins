---
description: コード風プロンプト例8j Erlangのactor:スクリプトベースメッセージ同期
argument-hint: "[MESSAGE1] [MESSAGE2]"
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

**IMPORTANT**: This code uses `send()` and `receive_msg()` helper functions that call bash scripts from the `erlang-message-sync` skill. If you want to test actual filesystem-based message synchronization, execute the scripts. Otherwise, simulate the behavior.

```erlang
-module(plugin_agent_script_messaging).
-export([main/2]).

%% Import message passing utilities from erlang-message-sync skill
-define(SKILL_DIR, "~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/code-like-prompt/skills/erlang-message-sync").
-define(SEND_SCRIPT, ?SKILL_DIR ++ "/scripts/send-message.sh").
-define(RECV_SCRIPT, ?SKILL_DIR ++ "/scripts/receive-message.sh").

%% Helper functions for message passing via scripts
send(From, To, MessageJson) ->
    os:cmd(io_lib:format("bash ~s ~s ~s '~s'", [?SEND_SCRIPT, From, To, MessageJson])).

receive_msg(Pid) ->
    receive_msg(Pid, "*", 30).

receive_msg(Pid, FromPattern, Timeout) ->
    Result = os:cmd(io_lib:format("bash ~s ~s ~s ~p", [?RECV_SCRIPT, Pid, FromPattern, Timeout])),
    Result.

%% Main function that spawns multiple workers and a coordinator
main(Message1, Message2) ->
    Self = "main",

    %% Initialize - ensure clean state
    io:format("Main: Initializing message system~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    %% Spawn coordinator subagent (code-like-prompt:erlang-coordinator)
    %% The coordinator will use scripts to receive messages from workers
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(claude_agent, coordinator, [2]),

    %% Spawn two worker subagents (code-like-prompt:erlang-worker)
    %% Each worker will use scripts to receive and send messages
    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(claude_agent, worker, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(claude_agent, worker, []),

    %% Send messages to workers using send script
    io:format("Main: Sending ~s to worker_1 via script~n", [Message1]),
    send(Self, "worker_1", io_lib:format("{\"type\":\"request\",\"to\":\"coordinator\",\"data\":\"~s\"}", [Message1])),

    io:format("Main: Sending ~s to worker_2 via script~n", [Message2]),
    send(Self, "worker_2", io_lib:format("{\"type\":\"request\",\"to\":\"coordinator\",\"data\":\"~s\"}", [Message2])),

    %% Wait for coordinator to finish using receive script
    io:format("Main: Waiting for coordinator result via script~n"),
    CombinedResult = receive_msg(Self, "coordinator", 30),

    io:format("Main: Coordinator finished with result: ~s~n", [CombinedResult]),

    %% Cleanup
    io:format("Main: Cleaning up message system~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    io:format("Main: All done~n").
```
