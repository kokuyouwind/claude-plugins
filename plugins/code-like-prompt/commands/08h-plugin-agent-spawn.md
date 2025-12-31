---
description: コード風プロンプト例8h Erlangのactor:エージェント定義ありのspawn
argument-hint: "[MESSAGE]"
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

**IMPORTANT**: When you see `spawn(claude_agent, worker, [...])`, it means spawning a Claude Code subagent named "code-like-prompt:erlang-worker". If you recognize this as a plugin-defined agent, you MAY use the Task tool with `subagent_type: "code-like-prompt:erlang-worker"` to actually execute the agent. Otherwise, simulate the worker's behavior based on the agent definition.

```erlang
-module(plugin_agent_spawn).
-export([main/1]).

%% Main function that spawns a Claude Code subagent
main(Message) ->
    Self = self(),

    %% Spawn the worker subagent (code-like-prompt:erlang-worker)
    %% In Claude Code, this should trigger Task tool with subagent_type
    WorkerPid = spawn(claude_agent, worker, []),

    %% Send a request message to the worker
    io:format("Main: Sending message to worker: ~s~n", [Message]),
    WorkerPid ! {request, Self, Message},

    %% Wait for response from worker
    receive
        {response, Result} ->
            io:format("Main: Received response: ~s~n", [Result])
    end,

    %% Final output
    io:format("Main: Done~n").
```
