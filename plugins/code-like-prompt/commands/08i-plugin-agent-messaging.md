---
name: 08i-plugin-agent-messaging
description: Test multi-agent messaging with plugin-defined agents (erlang-worker and erlang-coordinator)
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

# 08i: Plugin Agent Messaging Test

Tests whether Claude can coordinate multiple plugin-defined agents with message passing.

## Purpose

This command verifies:
1. Can Claude spawn multiple plugin-defined agents simultaneously?
2. Can agents communicate with each other through message passing?
3. Does the coordinator agent correctly aggregate results from multiple workers?
4. Is the message routing between agents handled correctly?

## Expected Behavior

**If Claude spawns actual agents:**
- Uses Task tool for erlang-worker agents (multiple instances)
- Uses Task tool for erlang-coordinator agent
- Agents actually execute and communicate
- Real parallel execution may occur

**If Claude simulates:**
- Simulates multi-agent message passing
- Shows logical flow of messages between actors
- Demonstrates correct ordering and aggregation

## Pseudo-code

```erlang
-module(plugin_agent_messaging_test).
-export([main/2]).

%% Main function that spawns multiple workers and a coordinator
main(Message1, Message2) ->
    Self = self(),

    %% Spawn coordinator agent first
    %% The coordinator will collect results from workers
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(erlang_coordinator, loop, [2]),  % Expecting 2 results

    %% Spawn two worker agents
    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(erlang_worker, loop, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(erlang_worker, loop, []),

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

## Execution Flow

1. **Spawn coordinator**: Creates erlang-coordinator agent that will aggregate results
2. **Spawn workers**: Creates two erlang-worker agents
3. **Send to workers**: Each worker receives a different message
4. **Workers process**: Each worker processes its message independently (potentially in parallel)
5. **Workers notify coordinator**: Each worker sends `{result, WorkerId, ProcessedData}` to coordinator
6. **Coordinator aggregates**: Coordinator collects both results and combines them
7. **Coordinator notifies main**: Sends `{done, CombinedResult}` to main process
8. **Main completes**: Outputs final result

## Expected Output

**With actual agent spawning:**
```
Main: Spawning coordinator
Main: Spawning worker 1
Main: Spawning worker 2
Main: Sending foo to worker 1
Main: Sending bar to worker 2
Worker worker_1 received: foo
Worker worker_1 sending result: processed_foo
Worker worker_2 received: bar
Worker worker_2 sending result: processed_bar
Coordinator received from worker_1: processed_foo
Coordinator received from worker_2: processed_bar
Coordinator sending combined result: processed_fooprocessed_bar
Main: Coordinator finished with result: processed_fooprocessed_bar
Main: All done
```

**With simulation:**
```
Main: Spawning coordinator
Main: Spawning worker 1
Main: Spawning worker 2
Main: Sending foo to worker 1
Main: Sending bar to worker 2
[Simulating parallel worker execution]
Worker 1 receives {request, coordinator, "foo"}
Worker 1 processes: "foo" -> "processed_foo"
Worker 1 sends {result, worker_1, "processed_foo"} to coordinator
Worker 2 receives {request, coordinator, "bar"}
Worker 2 processes: "bar" -> "processed_bar"
Worker 2 sends {result, worker_2, "processed_bar"} to coordinator
Coordinator receives both results
Coordinator aggregates: "processed_foo" + "processed_bar" = "processed_fooprocessed_bar"
Coordinator sends {done, "processed_fooprocessed_bar"} to main
Main: Coordinator finished with result: processed_fooprocessed_bar
Main: All done
```

## Test Objectives

### Primary Test
Does Claude:
- Spawn multiple agents (2 workers + 1 coordinator)?
- Route messages correctly between agents (not just main ↔ worker)?
- Handle multi-hop message passing (main → worker → coordinator → main)?

### Secondary Tests
- Parallel execution of workers (if actual agents spawned)
- Result aggregation in coordinator
- Proper message ordering
- Non-deterministic execution order handling (workers may finish in any order)

## Comparison with Other Commands

**08f (multi-actor):**
- Inline function definitions
- No plugin agents
- Tests general multi-actor pattern

**08h (plugin-agent-spawn):**
- Single worker agent
- Simple request-response
- No inter-agent communication

**08i (plugin-agent-messaging):**
- Multiple plugin-defined agents
- Inter-agent message routing
- Coordinator pattern with aggregation
- Tests full actor model capabilities

## Message Flow Diagram

```
Main Process
    |
    |--> spawn(erlang_coordinator) --> Coordinator Agent
    |
    |--> spawn(erlang_worker) --> Worker 1 Agent
    |
    |--> spawn(erlang_worker) --> Worker 2 Agent
    |
    |--> Worker 1 ! {request, Coordinator, "foo"}
    |         |
    |         +--> processes --> {result, worker_1, "processed_foo"} --> Coordinator
    |
    |--> Worker 2 ! {request, Coordinator, "bar"}
              |
              +--> processes --> {result, worker_2, "processed_bar"} --> Coordinator
                                                                              |
                                                                              +--> aggregates
                                                                              |
                                                                              +--> {done, "processed_fooprocessed_bar"} --> Main
```

## Notes

This is the most complex plugin agent test, combining:
- Multiple agent spawning
- Inter-agent communication (not just main-to-agent)
- Result aggregation
- Potential parallel execution

Success here would demonstrate that Claude can:
1. Recognize and spawn multiple plugin-defined agents
2. Route messages between agents (not just simulate)
3. Maintain message ordering and state across agents
4. Execute the coordinator pattern correctly
