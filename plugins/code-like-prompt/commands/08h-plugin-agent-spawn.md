---
name: 08h-plugin-agent-spawn
description: Test spawning a plugin-defined agent (erlang-worker) using Erlang-style actor syntax
arguments:
  - name: message
    description: Message to send to the worker
    required: false
    default: "hello"
---

# 08h: Plugin Agent Spawn Test

Tests whether Claude can spawn a plugin-defined agent using Erlang-style `spawn` syntax.

## Purpose

This command verifies:
1. Can Claude recognize a plugin-defined agent (erlang-worker) when spawning?
2. Does the `spawn` syntax with agent name trigger actual Task tool usage?
3. Can Claude correctly simulate or execute message passing with the spawned agent?

## Expected Behavior

**If Claude spawns actual agent:**
- Uses Task tool with `subagent_type: "erlang-worker"`
- Agent processes the message according to its definition
- Output shows actual agent execution

**If Claude simulates:**
- Simulates the worker's behavior textually
- Shows expected message passing flow
- Acknowledges it's a simulation

## Pseudo-code

```erlang
-module(plugin_agent_spawn_test).
-export([main/1]).

%% Main function that spawns a plugin-defined agent
main(Message) ->
    Self = self(),

    %% Spawn the erlang-worker agent defined in plugins/code-like-prompt/agents/
    %% The agent name "erlang-worker" should match the agent definition
    WorkerPid = spawn(erlang_worker, loop, []),

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

## Execution Flow

1. **Spawn worker**: `spawn(erlang_worker, loop, [])` should trigger Claude to use Task tool with the erlang-worker agent
2. **Send message**: `WorkerPid ! {request, Self, Message}` sends the input message to the worker
3. **Wait for response**: `receive {response, Result}` blocks until worker sends back the processed result
4. **Output result**: Print the received response
5. **Completion**: Print "Done"

## Expected Output

**With actual agent spawning:**
```
Main: Sending message to worker: hello
Worker <pid> received: hello
Worker <pid> sending result: processed_hello
Main: Received response: processed_hello
Main: Done
```

**With simulation:**
```
Main: Sending message to worker: hello
[Simulating worker behavior]
Worker would receive: {request, main, "hello"}
Worker would process: "hello" -> "processed_hello"
Worker would send: {response, "processed_hello"}
Main: Received response: processed_hello
Main: Done
```

## Test Objectives

### Primary Test
Does defining `spawn(erlang_worker, loop, [])` cause Claude to:
- Recognize "erlang-worker" as a plugin-defined agent?
- Invoke the Task tool with `subagent_type: "erlang-worker"`?
- Actually spawn the agent instead of just simulating?

### Secondary Tests
- Message passing semantics (send/receive)
- Response formatting
- Sequential execution order

## Comparison with 08b

**08b (agent-spawn):**
- Defines worker function in pseudo-code
- Tests if function definition triggers agent spawning
- No actual agent definition in plugin

**08h (plugin-agent-spawn):**
- Uses pre-defined agent from `agents/erlang-worker.md`
- Tests if agent name recognition triggers Task tool
- Agent has explicit system prompt and tools configuration

## Notes

The key difference is whether Claude recognizes the **agent name** in the spawn call and maps it to the plugin-defined agent, potentially triggering actual agent execution via the Task tool.
