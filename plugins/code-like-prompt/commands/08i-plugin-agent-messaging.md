---
description: コード風プロンプト例8i Erlangのactor:複数アクター間のメッセージルーティング
argument-hint: "[MESSAGE1] [MESSAGE2]"
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

**IMPORTANT**: When you see `spawn(claude_agent, worker, [...])` or `spawn(claude_agent, coordinator, [...])`, they mean spawning Claude Code subagents "code-like-prompt:erlang-worker" and "code-like-prompt:erlang-coordinator". If you recognize these as plugin-defined agents, you MAY use the Task tool to actually execute them. Otherwise, simulate their behavior based on the agent definitions.

```erlang
-module(plugin_agent_messaging).
-export([main/2]).

%% Main function that spawns multiple workers and a coordinator
main(Message1, Message2) ->
    Self = self(),

    %% Spawn coordinator subagent (code-like-prompt:erlang-coordinator)
    %% The coordinator will collect results from workers
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(claude_agent, coordinator, [2]),  % Expecting 2 results

    %% Spawn two worker subagents (code-like-prompt:erlang-worker)
    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(claude_agent, worker, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(claude_agent, worker, []),

    %% Send messages to workers
    %% Workers will process and send results to coordinator
    io:format("Main: Sending ~s to worker 1~n", [Message1]),
    Worker1Pid ! {request, CoordinatorPid, Message1},

    io:format("Main: Sending ~s to worker 2~n", [Message2]),
    Worker2Pid ! {request, CoordinatorPid, Message2},

    %% Wait for coordinator to finish aggregating results
    receive
        {done, CombinedResult} ->
            io:format("Main: Coordinator finished with result: ~s~n", [CombinedResult])
    end,

    %% Final output
    io:format("Main: All done~n").
```
