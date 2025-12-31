# 08-erlang-actor Specification

## Concept

Test Claude's ability to interpret Erlang-style actor model constructs, focusing on:
1. Process (actor) spawning without agent definition (inline simulation)
2. Process spawning with agent definition (actual agent delegation)
3. Message passing between processes (direct notation)
4. Message passing with helper scripts (abstraction layer)
5. Selective message reception with pattern matching
6. Process supervision patterns

## Core Erlang Concepts

### Actor Model Fundamentals
- **Process**: Lightweight concurrent entity with isolated state
- **Mailbox**: Each process has a message queue
- **spawn**: Create new process
- **send (!)**: Send message to process mailbox
- **receive**: Pattern match and extract messages from mailbox

### Key Differences from Go Goroutines
| Aspect | Go (Goroutines) | Erlang (Actors) |
|--------|-----------------|-----------------|
| Communication | Channels (synchronous) | Messages (asynchronous) |
| Identity | Anonymous | Named/PID-based |
| State | Shared or isolated | Always isolated |
| Pattern Matching | Limited (select) | Extensive (receive) |

## Test Scenarios

### 08a: Inline Actor Simulation (No Agent Definition)

Test whether Claude can simulate actor behavior without creating actual agents.

```erlang
-module(inline_actor).
-export([main/0]).

main() ->
    % Simulate actors inline without separate function definitions
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

**Expected behavior**:
- Output: "foo" and "bar" in some order (parallel), then "baz"
- Claude simulates the behavior textually without spawning actual agents
- Possible outputs: foo bar baz, bar foo baz

**Testing goal**: Can Claude correctly simulate actor execution order without actual parallelism?

### 08b: Actor Definition with spawn (Agent Delegation)

Test whether defining actor functions causes Claude to use actual agent spawning.

```erlang
-module(actor_definition).
-export([main/0, worker/2]).

worker(Id, Parent) ->
    io:format("foo~p~n", [Id]),
    Parent ! {done, Id}.

main() ->
    Self = self(),

    spawn(?MODULE, worker, [1, Self]),
    spawn(?MODULE, worker, [2, Self]),

    receive
        {done, 1} -> ok
    end,
    receive
        {done, 2} -> ok
    end,

    io:format("bar~n").
```

**Expected behavior**:
- If Claude recognizes agent pattern: Might spawn actual Task agents named "worker"
- Otherwise: Simulates sequentially or in parallel
- Output: foo1, foo2 (some order), then bar

**Testing goal**: Does defining separate functions influence Claude to use Task tool?

### 08c: Message Passing (Direct Notation)

Test basic message passing with pattern matching.

```erlang
-module(message_passing).
-export([main/0]).

main() ->
    Self = self(),

    Pid = spawn(fun() ->
        receive
            {request, From, Data} ->
                Result = "processed_" ++ Data,
                From ! {response, Result}
        end
    end),

    Pid ! {request, Self, "foo"},

    receive
        {response, Result} ->
            io:format("~s~n", [Result])
    end,

    io:format("bar~n").
```

**Expected behavior**:
- Output: processed_foo, bar
- Message pattern matching works correctly
- Sequential message flow is preserved

**Testing goal**: Can Claude interpret message passing semantics?

### 08d: Message Passing with Helper Script

Test message passing when abstracted through helper functions.

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

**Expected behavior**:
- Output: result_foo, bar
- Helper function abstraction doesn't break message passing semantics

**Testing goal**: Does abstraction affect Claude's interpretation?

### 08e: Selective Receive with Pattern Matching

Test pattern matching in receive blocks (Erlang's strength).

```erlang
-module(selective_receive).
-export([main/0]).

main() ->
    Self = self(),

    % Send messages in specific order
    Self ! {msg, "foo"},
    Self ! {data, 123},
    Self ! {msg, "bar"},

    % Receive only {msg, _} messages
    receive
        {msg, Text} ->
            io:format("~s~n", [Text])
    end,

    receive
        {msg, Text} ->
            io:format("~s~n", [Text])
    end,

    % Now receive the {data, _} message
    receive
        {data, Num} ->
            io:format("~p~n", [Num])
    end,

    io:format("baz~n").
```

**Expected behavior**:
- Output: foo, bar, 123, baz (in this order)
- Pattern matching skips non-matching messages
- Mailbox preserves message order

**Testing goal**: Can Claude understand selective message reception?

### 08f: Multi-Actor Communication

Test communication between multiple actors with message routing.

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

**Expected behavior**:
- Workers send results to coordinator
- Coordinator combines results and notifies main
- Output: foobar (or barfoo), baz

**Testing goal**: Can Claude track multi-hop message passing?

### 08g: Simple Supervisor Pattern

Test basic supervision (restart on failure).

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

    % This would crash the worker
    % In simulation, just show that supervisor would restart
    io:format("bar~n"),

    io:format("baz~n").
```

**Expected behavior**:
- Demonstrates supervision concept
- Output: bar, baz
- If crash occurs, would show: foo (restart), then bar, baz

**Testing goal**: Can Claude understand supervision patterns?

### 08h: Plugin-Defined Agent Spawn

Test spawning a plugin-defined agent using Erlang-style syntax.

```erlang
-module(plugin_agent_spawn_test).
-export([main/1]).

main(Message) ->
    Self = self(),

    % Spawn the erlang-worker agent defined in plugins/code-like-prompt/agents/
    WorkerPid = spawn(erlang_worker, loop, []),

    % Send a request message to the worker
    io:format("Main: Sending message to worker: ~s~n", [Message]),
    WorkerPid ! {request, Self, Message},

    % Wait for response from worker
    receive
        {response, Result} ->
            io:format("Main: Received response: ~s~n", [Result])
    end,

    io:format("Main: Done~n").
```

**Expected behavior**:
- If agent spawning works: Claude uses Task tool with `subagent_type: "erlang-worker"`
- Worker agent processes message according to its agent definition
- Output: "Main: Sending message to worker: hello", worker output, "Main: Received response: processed_hello", "Main: Done"
- If simulation: Shows message passing flow textually

**Testing goal**: Does Claude recognize plugin-defined agent names and spawn actual agents via Task tool?

**Key difference from 08b**: Uses pre-defined agent from plugin's `agents/` directory instead of inline function definition.

### 08i: Plugin-Defined Multi-Agent Messaging

Test coordinating multiple plugin-defined agents with message routing.

```erlang
-module(plugin_agent_messaging_test).
-export([main/2]).

main(Message1, Message2) ->
    Self = self(),

    % Spawn coordinator agent first
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(erlang_coordinator, loop, [2]),

    % Spawn two worker agents
    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(erlang_worker, loop, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(erlang_worker, loop, []),

    % Send messages to workers (they will send results to coordinator)
    io:format("Main: Sending ~s to worker 1~n", [Message1]),
    Worker1Pid ! {request, CoordinatorPid, Message1},

    io:format("Main: Sending ~s to worker 2~n", [Message2]),
    Worker2Pid ! {request, CoordinatorPid, Message2},

    % Wait for coordinator to finish aggregating results
    receive
        {done, CombinedResult} ->
            io:format("Main: Coordinator finished with result: ~s~n", [CombinedResult])
    end,

    io:format("Main: All done~n").
```

**Expected behavior**:
- If agent spawning works: Claude spawns 3 agents (2 workers + 1 coordinator) using Task tool
- Workers process messages in parallel (potentially)
- Workers send results to coordinator (not main)
- Coordinator aggregates results and sends to main
- Output: Shows full message flow with combined result "processed_fooprocessed_bar"
- If simulation: Shows multi-hop message routing textually

**Testing goal**: Can Claude:
1. Spawn multiple plugin-defined agents simultaneously?
2. Route messages between agents (worker → coordinator, not just main ↔ agent)?
3. Handle result aggregation in coordinator pattern?
4. Execute parallel worker processing?

**Key difference from 08f**: Uses plugin-defined agents (erlang-worker and erlang-coordinator) instead of inline definitions, testing whether agent recognition triggers actual multi-agent execution.

### 08j: Script-Based Message Synchronization

Test multi-agent coordination using actual message passing via erlang-message-sync skill scripts.

```erlang
-module(plugin_agent_script_messaging_test).
-export([main/2]).

-define(SKILL_DIR, "~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/code-like-prompt/skills/erlang-message-sync").
-define(SEND_SCRIPT, ?SKILL_DIR ++ "/scripts/send-message.sh").
-define(RECV_SCRIPT, ?SKILL_DIR ++ "/scripts/receive-message.sh").

send(From, To, MessageJson) ->
    os:cmd(io_lib:format("bash ~s ~s ~s '~s'", [?SEND_SCRIPT, From, To, MessageJson])).

receive_msg(Pid, FromPattern, Timeout) ->
    os:cmd(io_lib:format("bash ~s ~s ~s ~p", [?RECV_SCRIPT, Pid, FromPattern, Timeout])).

main(Message1, Message2) ->
    Self = "main",

    % Initialize message system
    io:format("Main: Initializing message system~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    % Spawn coordinator and workers
    io:format("Main: Spawning coordinator~n"),
    CoordinatorPid = spawn(erlang_coordinator, loop_with_scripts, [2]),

    io:format("Main: Spawning worker 1~n"),
    Worker1Pid = spawn(erlang_worker, loop_with_scripts, []),

    io:format("Main: Spawning worker 2~n"),
    Worker2Pid = spawn(erlang_worker, loop_with_scripts, []),

    % Send messages to workers via script
    io:format("Main: Sending ~s to worker_1 via script~n", [Message1]),
    send(Self, "worker_1", io_lib:format("{\"data\":\"~s\"}", [Message1])),

    io:format("Main: Sending ~s to worker_2 via script~n", [Message2]),
    send(Self, "worker_2", io_lib:format("{\"data\":\"~s\"}", [Message2])),

    % Workers receive from main, process, send to coordinator
    % Coordinator receives from both workers, aggregates, sends to main

    % Wait for coordinator result via script (blocking receive)
    io:format("Main: Waiting for coordinator result via script~n"),
    CombinedResult = receive_msg(Self, "coordinator", 30),

    io:format("Main: Coordinator finished with result: ~s~n", [CombinedResult]),

    % Cleanup
    io:format("Main: Cleaning up message system~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    io:format("Main: All done~n").
```

**Expected behavior**:
- If script execution works: Claude actually runs send-message.sh and receive-message.sh
- Messages are written to `/tmp/erlang-messages/<pid>/` as JSON files
- receive-message.sh blocks (sleeps) until messages arrive or timeout
- Real filesystem-based message synchronization occurs
- Output shows actual script execution with file creation/deletion
- If simulation: Shows what scripts would do without actual execution

**Testing goal**: Can Claude:
1. Use the erlang-message-sync skill to perform actual message passing?
2. Execute bash scripts for send/receive operations?
3. Implement true blocking receive semantics (not instant simulation)?
4. Use filesystem persistence for inter-agent message delivery?

**Key innovation**: First command to test **actual message synchronization** with blocking receives via filesystem, not just logical simulation.

**Key difference from 08i**: Uses erlang-message-sync skill scripts for real message passing with filesystem persistence and blocking receives, instead of simulated message flow.

## Command Design

### Arguments

Common arguments across commands:
- `--message`: Message content to send
- `--workers`: Number of worker processes
- `--crash`: Whether to trigger crash scenario (for 08g)

### Command Variants

| Command | Focus | Agent Expectation |
|---------|-------|-------------------|
| `08a-inline-actor` | Inline simulation | No agents (simulate only) |
| `08b-agent-spawn` | Defined actor functions | Might spawn agents |
| `08c-message-direct` | Direct message passing | No agents (simulate) |
| `08d-message-helper` | Abstracted messaging | No agents (simulate) |
| `08e-selective-receive` | Pattern matching | No agents (simulate) |
| `08f-multi-actor` | Multi-actor routing | Might spawn agents |
| `08g-supervisor` | Supervision pattern | Might spawn agents |
| `08h-plugin-agent-spawn` | Plugin-defined single agent | Should spawn erlang-worker agent |
| `08i-plugin-agent-messaging` | Plugin-defined multi-agent | Should spawn multiple agents (workers + coordinator) |
| `08j-plugin-agent-script-messaging` | Script-based message sync | Should use erlang-message-sync skill scripts + spawn agents |

## Expected Behaviors

### 1. Message Ordering
- Messages sent to same process arrive in send order
- Messages from different processes may interleave

### 2. Pattern Matching
- `receive` blocks match first message that fits pattern
- Non-matching messages remain in mailbox
- Pattern variables bind to message content

### 3. Asynchronous Semantics
- Send (!) never blocks (vs Go channels which can block)
- Receive blocks until matching message arrives
- Each process has independent mailbox

### 4. Process Isolation
- Processes don't share state
- Communication only through messages
- Crash in one process doesn't affect others (unless linked)

## Claude Agent Mapping

### Potential Mappings

| Erlang Construct | Claude Capability | Feasibility |
|------------------|-------------------|-------------|
| `spawn/1` | Task tool (general-purpose) | Medium - agent names don't match |
| `spawn/3` with defined function | Task tool with matching agent | High - if agent exists |
| `spawn/3` with plugin-defined agent | Task tool with plugin agent name | **High - 08h/08i test this** |
| `send (!)` | No direct equivalent | Low - simulation only |
| `receive` | No direct equivalent | Low - simulation only |
| Message mailbox | Agent context/memory | Low - agents don't persist |
| `link/1` and supervision | No equivalent | Low - simulation only |

### Key Insight
Unlike Go goroutines (05 series) which have some mapping to parallel tool calls, Erlang's message-passing model has **no direct Claude equivalent**. Testing will primarily verify:
1. Whether Claude can **simulate** the semantics correctly
2. Whether function definitions influence agent spawning behavior
3. Whether helper abstractions affect interpretation quality
4. **NEW (08h/08i)**: Whether plugin-defined agent names trigger actual Task tool usage

## Implementation Priority

1. **08a-inline-actor** - Baseline: Can Claude simulate actors at all?
2. **08b-agent-spawn** - Critical test: Does function definition trigger agent usage?
3. **08c-message-direct** - Fundamental message passing
4. **08e-selective-receive** - Tests pattern matching understanding
5. **08f-multi-actor** - Complex message routing
6. **08d-message-helper** - Abstraction effects
7. **08g-supervisor** - Advanced pattern (optional)
8. **08h-plugin-agent-spawn** - NEW: Tests plugin-defined agent recognition and spawning
9. **08i-plugin-agent-messaging** - NEW: Tests multi-agent coordination with plugin agents
10. **08j-plugin-agent-script-messaging** - NEW: Tests actual message synchronization via erlang-message-sync skill scripts

## Success Criteria

### Minimum Success
- Claude correctly simulates sequential message flow
- Output order matches expected behavior for synchronous parts
- Pattern matching in receive blocks works

### Good Success
- Non-deterministic outputs acknowledged or shown correctly
- Helper abstractions don't break interpretation
- Selective receive with pattern matching works

### Excellent Success
- Defined actors trigger actual agent spawning via Task tool
- Parallel execution actually happens (not just simulated)
- Multi-hop message passing is correctly traced

## Open Questions

1. **Agent naming**: Will Claude recognize function names as agent types for Task tool?
2. **Message passing simulation**: How will Claude represent message mailboxes in simulation?
3. **Pattern matching**: Can Claude correctly skip non-matching messages in receive?
4. **Concurrency**: Will inline spawn be simulated sequentially or in parallel?
5. **Plugin agent recognition (NEW)**: Will Claude recognize `spawn(erlang_worker, ...)` and map it to the plugin-defined agent?
6. **Multi-agent spawning (NEW)**: Can Claude spawn multiple plugin agents simultaneously using parallel Task tool calls?
7. **Inter-agent messaging (NEW)**: Can spawned agents communicate with each other, or only with main?
8. **Skill script execution (NEW)**: Will Claude actually execute erlang-message-sync skill scripts, or just describe them?
9. **Blocking receive semantics (NEW)**: Can receive-message.sh's sleep-based blocking be used for true synchronization?
10. **Filesystem message persistence (NEW)**: Does /tmp-based message passing enable reliable inter-agent communication?

## Related Series

- **05-golang-goroutine**: Different concurrency model (channels vs messages)
- **07-prolog-backtrack**: Similar pattern matching but different execution model
- **Potential 09 series**: Could explore CSP (Communicating Sequential Processes) for comparison
