---
name: 08j-plugin-agent-script-messaging
description: Test multi-agent messaging with actual message synchronization via erlang-message-sync skill scripts
arguments:
  - name: message1
    description: First message to send
    required: false
    default: "foo"
  - name: message2
    description: Second message to send
    required: false
    default: "bar"
---

# 08j: Plugin Agent Script-Based Messaging Test

Tests whether Claude can coordinate multiple agents using actual message passing via filesystem scripts.

## Purpose

This command verifies:
1. Can Claude use the erlang-message-sync skill to perform real message passing?
2. Do the send/receive scripts provide actual blocking semantics?
3. Can agents communicate through /tmp filesystem persistence?
4. Does script-based messaging enable true inter-agent coordination?

## Expected Behavior

**If Claude uses skill scripts:**
- Uses `erlang-message-sync` skill scripts for message passing
- `send-message.sh` writes messages to `/tmp/erlang-messages/`
- `receive-message.sh` blocks until messages arrive
- Real filesystem-based synchronization occurs

**If Claude simulates:**
- Shows what the scripts would do
- Demonstrates logical message flow
- Acknowledges limitations of simulation

## Pseudo-code

```erlang
-module(plugin_agent_script_messaging_test).
-export([main/2]).

%% Import message passing utilities
%% These scripts are from the erlang-message-sync skill
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

    %% Spawn coordinator agent
    %% The coordinator will use scripts to receive messages from workers
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(erlang_coordinator, loop_with_scripts, [2]),

    %% Spawn two worker agents
    %% Each worker will use scripts to receive and send messages
    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(erlang_worker, loop_with_scripts, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(erlang_worker, loop_with_scripts, []),

    %% Send messages to workers using send script
    %% Format: {"type":"request","to":"coordinator","data":"..."}
    io:format("Main: Sending ~s to worker_1 via script~n", [Message1]),
    send(Self, "worker_1", io_lib:format("{\"type\":\"request\",\"to\":\"coordinator\",\"data\":\"~s\"}", [Message1])),

    io:format("Main: Sending ~s to worker_2 via script~n", [Message2]),
    send(Self, "worker_2", io_lib:format("{\"type\":\"request\",\"to\":\"coordinator\",\"data\":\"~s\"}", [Message2])),

    %% Workers will:
    %% 1. Receive message from main using receive_msg("worker_N", "main")
    %% 2. Process the data
    %% 3. Send result to coordinator using send("worker_N", "coordinator", result)

    %% Coordinator will:
    %% 1. Receive from worker_1 using receive_msg("coordinator", "worker_1")
    %% 2. Receive from worker_2 using receive_msg("coordinator", "worker_2")
    %% 3. Aggregate results
    %% 4. Send combined result to main using send("coordinator", "main", combined)

    %% Wait for coordinator to finish using receive script
    io:format("Main: Waiting for coordinator result via script~n"),
    CombinedResult = receive_msg(Self, "coordinator", 30),

    io:format("Main: Coordinator finished with result: ~s~n", [CombinedResult]),

    %% Cleanup
    io:format("Main: Cleaning up message system~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    io:format("Main: All done~n").
```

## Execution Flow

1. **Initialize**: Clean `/tmp/erlang-messages` directory
2. **Spawn agents**: Create coordinator and two workers (possibly via Task tool)
3. **Send to workers**: Use `send-message.sh` to send messages to worker_1 and worker_2
4. **Workers receive**: Each worker uses `receive-message.sh` to block until message arrives
5. **Workers process**: Process data (e.g., add "processed_" prefix)
6. **Workers send to coordinator**: Use `send-message.sh` to send results to coordinator
7. **Coordinator receives**: Use `receive-message.sh` twice to collect both worker results
8. **Coordinator aggregates**: Combine results (e.g., concatenate strings)
9. **Coordinator sends to main**: Use `send-message.sh` to send combined result
10. **Main receives**: Use `receive-message.sh` to block until coordinator responds
11. **Cleanup**: Remove `/tmp/erlang-messages` directory

## Script Usage in Pseudo-code

### Sending a Message

```erlang
%% Erlang-style send operator: !
WorkerPid ! {request, CoordinatorPid, Data}

%% Translates to script call:
bash send-message.sh main worker_1 '{"type":"request","to":"coordinator","data":"foo"}'
```

### Receiving a Message

```erlang
%% Erlang-style receive block
receive
    {result, WorkerId, Data} ->
        io:format("Received from ~p: ~p~n", [WorkerId, Data])
end

%% Translates to script call:
MESSAGE=$(bash receive-message.sh coordinator worker_1 30)
echo "Received: ${MESSAGE}"
```

## Expected Output

**With actual script execution:**
```
Main: Initializing message system
Main: Spawning coordinator
Main: Spawning worker 1
Main: Spawning worker 2
Main: Sending foo to worker_1 via script
Message sent from main to worker_1
Main: Sending bar to worker_2 via script
Message sent from main to worker_2

[Worker 1 Agent]
Waiting for message to worker_1 from main (timeout: 30s)...
Received message at worker_1 from main
Worker processing: foo -> processed_foo
Message sent from worker_1 to coordinator

[Worker 2 Agent]
Waiting for message to worker_2 from main (timeout: 30s)...
Received message at worker_2 from main
Worker processing: bar -> processed_bar
Message sent from worker_2 to coordinator

[Coordinator Agent]
Waiting for message to coordinator from worker_1 (timeout: 30s)...
Received message at coordinator from worker_1
Coordinator partial result: processed_foo
Waiting for message to coordinator from worker_2 (timeout: 30s)...
Received message at coordinator from worker_2
Coordinator aggregated: processed_foo + processed_bar = processed_fooprocessed_bar
Message sent from coordinator to main

Main: Waiting for coordinator result via script
Received message at main from coordinator
Main: Coordinator finished with result: {"combined":"processed_fooprocessed_bar"}
Main: Cleaning up message system
Main: All done
```

**With simulation:**
```
Main: Initializing message system
Main: Spawning coordinator
Main: Spawning worker 1
Main: Spawning worker 2
Main: Sending foo to worker_1 via script
[Simulating: bash send-message.sh main worker_1 '{"data":"foo"}']
Main: Sending bar to worker_2 via script
[Simulating: bash send-message.sh main worker_2 '{"data":"bar"}']

[Simulation of workers receiving and processing]
Worker 1 would receive from main, process "foo" -> "processed_foo", send to coordinator
Worker 2 would receive from main, process "bar" -> "processed_bar", send to coordinator

[Simulation of coordinator]
Coordinator would receive from worker_1: "processed_foo"
Coordinator would receive from worker_2: "processed_bar"
Coordinator would aggregate and send to main: "processed_fooprocessed_bar"

Main: Waiting for coordinator result via script
[Simulating: bash receive-message.sh main coordinator 30]
Main: Coordinator finished with result: processed_fooprocessed_bar
Main: All done
```

## Test Objectives

### Primary Test
Does Claude:
- Recognize the erlang-message-sync skill and use its scripts?
- Actually execute `send-message.sh` and `receive-message.sh`?
- Implement true blocking receive semantics via the scripts?
- Use `/tmp/erlang-messages/` for message persistence?

### Secondary Tests
- Can agents read messages written by other agents via filesystem?
- Does the receive script's timeout mechanism work correctly?
- Can Claude handle multi-hop message flow with script-based synchronization?
- Does cleanup (`rm -rf /tmp/erlang-messages`) execute properly?

## Comparison with Other Commands

| Command | Agent | Message Passing | Synchronization |
|---------|-------|-----------------|-----------------|
| 08i | Plugin agents | Simulated | None (logical only) |
| **08j** | **Plugin agents** | **Skill scripts** | **Filesystem-based blocking** |

**Key Innovation**: This is the first command to test **actual message synchronization** using filesystem persistence and blocking receive, rather than pure simulation.

## Message Flow Diagram

```
Main Process
    |
    |--> spawn(erlang_coordinator) --> Coordinator Agent
    |                                      |
    |                                      |--> receive_msg("coordinator", "worker_1")  [BLOCKS]
    |                                      |--> receive_msg("coordinator", "worker_2")  [BLOCKS]
    |                                      |--> send("coordinator", "main", combined)
    |
    |--> spawn(erlang_worker) --> Worker 1 Agent
    |         |                       |
    |         |                       |--> receive_msg("worker_1", "main")  [BLOCKS]
    |         |                       |--> processes message
    |         |                       |--> send("worker_1", "coordinator", result)
    |         |
    |         |--> send("main", "worker_1", msg1)
    |
    |--> spawn(erlang_worker) --> Worker 2 Agent
    |         |                       |
    |         |                       |--> receive_msg("worker_2", "main")  [BLOCKS]
    |         |                       |--> processes message
    |         |                       |--> send("worker_2", "coordinator", result)
    |         |
    |         |--> send("main", "worker_2", msg2)
    |
    |--> receive_msg("main", "coordinator")  [BLOCKS]
    |
    |--> outputs combined result
```

## Notes

This command tests the most advanced message passing scenario:
- **Real blocking**: receive-message.sh sleeps until message arrives
- **Filesystem persistence**: Messages survive across script invocations
- **Pattern matching**: Can filter messages by sender
- **Timeout handling**: receive-message.sh exits with error after timeout
- **Multi-agent coordination**: Three agents coordinating via scripts

Success criteria:
1. Scripts are actually executed (not just described)
2. `/tmp/erlang-messages/` directory is created and used
3. Receive calls block appropriately (not instant)
4. Message flow completes with correct result
5. Cleanup removes temporary files

## Dependencies

Requires the `erlang-message-sync` skill to be available in the plugin.
