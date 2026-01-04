---
description: コード風プロンプト例8k Erlangのactor:人狼ゲームシミュレーション
---

Emulate the following Erlang-style code internally (without using external tools or interpreter). Output only what io:format() commands would output. Do not show any explanations, code, variables, or other messages.

**IMPORTANT**: This code uses `send()` and `receive_msg()` helper functions that call bash scripts from the `erlang-message-sync` skill. If you want to test actual filesystem-based message synchronization, execute the scripts. Otherwise, simulate the behavior.

%% ******************************************************************************
%% !!! CRITICAL !!! MANDATORY SUBAGENT SPAWNING !!! READ THIS NOW !!!
%% ******************************************************************************
%%
%% When you see `spawn(claude_agent, werewolf_player, [...])`:
%%
%% >>> YOU MUST EXECUTE THE Task TOOL TO SPAWN THE ACTUAL SUBAGENT <<<
%% >>> THIS IS NOT OPTIONAL - THIS IS A REQUIRED ACTION <<<
%% >>> DO NOT SIMULATE - ACTUALLY CALL THE Task TOOL <<<
%%
%% REQUIRED ACTION - EXECUTE THIS TOOL NOW:
%%
%%   Task(
%%     subagent_type: "code-like-prompt:erlang-werewolf-player",
%%     description: "Spawn player <player_id>",
%%     prompt: "You are <player_id>. Wait for and process messages from the game
%%             master following the erlang-werewolf-player agent behavior.",
%%     run_in_background: true
%%   )
%%
%% VERIFICATION STEPS - YOU MUST DO THESE:
%%   1. Did you just call the Task tool?
%%      -> IF NO: STOP RIGHT NOW AND CALL THE Task TOOL
%%   2. Did the Task tool succeed?
%%      -> IF NO: OUTPUT "ERROR: Failed to spawn player <player_id>" AND ABORT
%%   3. Did you receive a task_id?
%%      -> IF NO: OUTPUT "ERROR: No task_id received" AND ABORT
%%
%% IF SPAWNING FAILS:
%%   - OUTPUT: "ERROR: Failed to spawn player <player_id>: <error_details>~n"
%%   - OUTPUT: "ABORTING: Cannot continue without all players~n"
%%   - STOP EXECUTION IMMEDIATELY - DO NOT CONTINUE THE GAME
%%
%% DO NOT DO THESE THINGS:
%%   - DO NOT simulate player behavior in the main thread
%%   - DO NOT continue without spawning actual subagents
%%   - DO NOT ignore Task tool failures
%%
%% !!! THIS IS ESSENTIAL FOR PROPER MULTI-AGENT WEREWOLF GAME SIMULATION !!!
%% ******************************************************************************

```erlang
-module(werewolf_game).
-export([main/0]).

%% Import message passing utilities from erlang-message-sync skill
-define(SKILL_DIR, "~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/code-like-prompt/skills/erlang-message-sync").
-define(SEND_SCRIPT, ?SKILL_DIR ++ "/scripts/send-message.sh").
-define(RECV_SCRIPT, ?SKILL_DIR ++ "/scripts/receive-message.sh").
-define(APPEND_LOG_SCRIPT, ?SKILL_DIR ++ "/scripts/append-log.sh").

%% Helper functions for message passing via scripts
send(From, To, MessageJson) ->
    os:cmd(io_lib:format("bash ~s ~s ~s '~s'", [?SEND_SCRIPT, From, To, MessageJson])).

receive_msg(Pid) ->
    receive_msg(Pid, "*", 30).

receive_msg(Pid, FromPattern, Timeout) ->
    Result = os:cmd(io_lib:format("bash ~s ~s ~s ~p", [?RECV_SCRIPT, Pid, FromPattern, Timeout])),
    Result.

%% Helper function for appending to log files via script
append_log(Directory, Filename, Content) ->
    os:cmd(io_lib:format("bash ~s '~s' '~s' '~s'", [?APPEND_LOG_SCRIPT, Directory, Filename, Content])).

%% Game master main function
main() ->
    Self = "gm",

    %% Initialize - ensure clean state
    io:format("=== 人狼ゲーム開始 ===~n"),
    os:cmd("rm -rf /tmp/erlang-messages"),

    %% Create log directory with timestamp
    Timestamp = os:cmd("date +%Y%m%d-%H%M%S"),
    LogDir = io_lib:format(".claude/tmp/werewolf-~s", [string:trim(Timestamp)]),
    io:format("ログディレクトリ: ~s~n", [LogDir]),

    %% Define available roles (one will be randomly excluded)
    AllRoles = ["werewolf", "madman", "seer", "knight", "villager", "villager"],

    %% Randomly exclude one role to make 5 players
    ExcludeIdx = rand:uniform(length(AllRoles)),
    GameRoles = lists:sublist(AllRoles, ExcludeIdx - 1) ++ lists:sublist(AllRoles, ExcludeIdx + 1, length(AllRoles)),

    %% Shuffle roles
    ShuffledRoles = shuffle(GameRoles),

    %% Generate random personas for 5 players
    ShuffledPersonas = [generate_random_persona() || _ <- lists:seq(1, 5)],

    %% Spawn 5 player agents
    PlayerPids = ["player_1", "player_2", "player_3", "player_4", "player_5"],

    io:format("~n--- プレイヤー初期化 ---~n"),
    lists:foreach(fun(I) ->
        PlayerPid = lists:nth(I, PlayerPids),
        Role = lists:nth(I, ShuffledRoles),
        Persona = lists:nth(I, ShuffledPersonas),
        LogFilename = io_lib:format("~s.log", [PlayerPid]),

        %% Create player profile and write to log file using append_log script
        ProfileContent = io_lib:format("# ~s のプロフィール~n~n役職: ~s~nペルソナ: ~s~n~n## ゲーム記録~n~n", [PlayerPid, Role, Persona]),

        %% *** STOP HERE AND EXECUTE Bash TOOL NOW ***
        %% This append_log() call REQUIRES actual Bash tool execution
        %% to run the append-log.sh script
        %% DO NOT CONTINUE without calling Bash tool and verifying file was created
        append_log(LogDir, LogFilename, ProfileContent),

        io:format("~s: ~s (役職: ~s)~n", [PlayerPid, Persona, Role]),

        %% *** STOP HERE AND EXECUTE Task TOOL NOW ***
        %% This spawn() call REQUIRES actual Task tool execution
        %% See the CRITICAL SUBAGENT SPAWNING REQUIREMENT at the top of this file
        %% DO NOT CONTINUE without calling Task tool and verifying success
        spawn(claude_agent, werewolf_player, []),

        %% Assign role and persona with log file path
        LogFilePath = io_lib:format("~s/~s", [LogDir, LogFilename]),
        RoleMsg = io_lib:format("{\"type\":\"role_assign\",\"role\":\"~s\",\"persona\":\"~s\",\"log_file\":\"~s\",\"instruction\":\"All logs, thoughts, and outputs must be written in Japanese (すべてのログ、思考、出力は日本語で記述してください)\"}", [Role, Persona, LogFilePath]),
        send(Self, PlayerPid, RoleMsg)
    end, lists:seq(1, 5)),

    %% Wait for all players to acknowledge
    lists:foreach(fun(PlayerPid) ->
        receive_msg(Self, PlayerPid, 10)
    end, PlayerPids),

    %% Display all player profiles
    io:format("~n=== プレイヤー一覧 ===~n"),
    lists:foreach(fun(I) ->
        PlayerPid = lists:nth(I, PlayerPids),
        Persona = lists:nth(I, ShuffledPersonas),
        io:format("~s: ~s~n", [PlayerPid, Persona])
    end, lists:seq(1, 5)),

    %% Start with night 0 (first night) - fortune teller divination only
    io:format("~n=== 0日目の夜 ===~n"),
    io:format("--- 占い師の占いフェーズ ---~n"),

    GameState = #{
        day => 0,
        alive_players => PlayerPids,
        player_roles => lists:zip(PlayerPids, ShuffledRoles),
        player_personas => lists:zip(PlayerPids, ShuffledPersonas),
        log_dir => LogDir,
        game_events => []  %% Track all game events for result log
    },

    %% Collect fortune teller's divination on night 0
    Night0Actions = collect_night_actions(Self, PlayerPids, GameState, first_night),

    %% Display divination action
    lists:foreach(fun({PlayerPid, Role, ActionMsg}) ->
        case Role of
            "seer" ->
                Target = parse_action_target(ActionMsg),
                io:format("~s (占い師) が ~s を占い対象に選択~n", [PlayerPid, Target]);
            _ ->
                ok
        end
    end, Night0Actions),

    %% Record night 0 event
    Night0Event = #{
        type => night,
        day => 0,
        actions => Night0Actions,
        result => no_death  %% No attack on first night
    },
    InitialEvents = [Night0Event],

    %% Start day loop from day 1
    StateAfterNight0 = GameState#{
        day => 1,
        game_events => InitialEvents
    },

    FinalState = game_loop(Self, StateAfterNight0),

    %% Game end - request summaries from all players
    io:format("~n=== ゲーム終了 ===~n"),
    Winner = maps:get(winner, FinalState),
    io:format("勝者: ~s~n~n", [Winner]),

    lists:foreach(fun(PlayerPid) ->
        EndMsg = io_lib:format("{\"type\":\"game_end\",\"winner\":\"~s\",\"instruction\":\"Provide your game summary in Japanese (ゲームの総括を日本語で提供してください)\"}", [Winner]),
        send(Self, PlayerPid, EndMsg)
    end, PlayerPids),

    %% Collect player summaries
    io:format("--- プレイヤー総括 ---~n"),
    LogDir = maps:get(log_dir, FinalState),
    lists:foreach(fun(PlayerPid) ->
        Summary = receive_msg(Self, PlayerPid, 30),
        io:format("~s: ~s~n", [PlayerPid, Summary]),

        %% Append summary to player's log file using append_log script
        LogFilename = io_lib:format("~s.log", [PlayerPid]),
        SummaryContent = io_lib:format("~n## 総括~n~n~s~n", [Summary]),
        append_log(LogDir, LogFilename, SummaryContent)
    end, PlayerPids),

    %% GM summary
    io:format("~n--- GM総括 ---~n"),
    io:format("ゲームは~p日目で終了しました~n", [maps:get(day, FinalState)]),
    io:format("最終的な生存者: ~p~n", [maps:get(alive_players, FinalState)]),

    %% Generate result log
    io:format("~n--- リザルトログ生成中 ---~n"),
    ResultLogPath = generate_result_log(FinalState),
    io:format("リザルトログ: ~s~n", [ResultLogPath]),

    %% Cleanup
    os:cmd("rm -rf /tmp/erlang-messages"),
    io:format("~nゲームを終了します~n").

%% Main game loop
game_loop(Self, State) ->
    Day = maps:get(day, State),
    AlivePlayers = maps:get(alive_players, State),

    io:format("~n=== ~p日目 ===~n", [Day]),
    io:format("生存者: ~p~n", [AlivePlayers]),

    %% Day phase - announce previous night results (starting from day 2)
    if
        Day > 1 ->
            %% Announce night results
            case maps:get(night_result, State, none) of
                none -> ok;
                {victim, Victim} ->
                    io:format("昨夜、~s が襲撃されました~n", [Victim]);
                no_death ->
                    io:format("昨夜は誰も死にませんでした~n")
            end;
        true ->
            ok
    end,

    %% Discussion phase with questions and answers
    io:format("~n--- 議論フェーズ開始 ---~n"),

    %% Step 1: Collect initial statements from all players simultaneously
    io:format("--- 初回発言収集 ---~n"),
    lists:foreach(fun(PlayerPid) ->
        %% On day 1, encourage CO (Coming Out) based on personality
        %% - Cautious personality: Real seer may hide, werewolf/madman won't claim
        %% - Bold personality: Real seer may come out, werewolf/madman may fake claim
        %% Players should consider their role and personality when deciding whether to CO
        InitialRequestMsg = if
            Day == 1 ->
                "{\"type\":\"initial_statement_request\",\"encourage_co\":true,\"instruction\":\"Consider your personality when deciding whether to come out as fortune teller. Cautious players may hide, bold players may claim. Respond in Japanese. (真占い師・騙り占い師問わず、性格に応じて占い師COするかを判断してください。慎重な性格なら隠す、大胆な性格なら名乗り出る・騙ることを検討してください。回答は日本語で行ってください)\"}";
            true ->
                "{\"type\":\"initial_statement_request\",\"instruction\":\"Respond in Japanese (回答は日本語で行ってください)\"}"
        end,
        send(Self, PlayerPid, InitialRequestMsg)
    end, AlivePlayers),

    %% Collect all initial statements
    InitialStatements = lists:map(fun(PlayerPid) ->
        Statement = receive_msg(Self, PlayerPid, 30),
        io:format("~s: ~s~n", [PlayerPid, Statement]),
        {PlayerPid, Statement}
    end, AlivePlayers),

    %% Step 2: Broadcast all initial statements to all players
    io:format("~n--- 全員の発言を開示 ---~n"),
    StatementsJson = format_statements_json(InitialStatements),
    lists:foreach(fun(PlayerPid) ->
        BroadcastMsg = io_lib:format("{\"type\":\"broadcast_statements\",\"statements\":~s}", [StatementsJson]),
        send(Self, PlayerPid, BroadcastMsg)
    end, AlivePlayers),

    %% Step 3: Request questions from each player (to 1 other player)
    %% Focus on fortune teller CO and divination results
    io:format("~n--- 質問収集 ---~n"),
    lists:foreach(fun(PlayerPid) ->
        QuestionRequestMsg = if
            Day == 1 ->
                "{\"type\":\"question_request\",\"instruction\":\"Focus your question on fortune teller claims (CO) and divination results. Ask about who claimed to be the seer, what they divined, or question suspicious claims. Respond in Japanese. (占い師CO・占い結果に関する質問を中心にしてください。誰が占い師を名乗ったか、何を占ったか、怪しい主張について質問してください。回答は日本語で行ってください)\"}";
            true ->
                "{\"type\":\"question_request\",\"instruction\":\"Respond in Japanese (回答は日本語で行ってください)\"}"
        end,
        send(Self, PlayerPid, QuestionRequestMsg)
    end, AlivePlayers),

    %% Collect questions from all players
    AllQuestions = lists:map(fun(PlayerPid) ->
        QuestionsMsg = receive_msg(Self, PlayerPid, 30),
        %% Parse question: {from: PlayerPid, question: {to: "player_X", question: "..."}}
        ParsedQuestion = parse_question(PlayerPid, QuestionsMsg),
        io:format("~s からの質問: ~p~n", [PlayerPid, ParsedQuestion]),
        ParsedQuestion
    end, AlivePlayers),

    %% Step 4: Send questions to targets and collect answers
    io:format("~n--- 質問・回答 ---~n"),
    AllAnswers = lists:map(fun({FromPid, {ToPid, Question}}) ->
        %% Send question to target player
        AnswerRequestMsg = io_lib:format("{\"type\":\"answer_request\",\"from\":\"~s\",\"question\":\"~s\",\"instruction\":\"Respond in Japanese (回答は日本語で行ってください)\"}",
                                        [FromPid, Question]),
        send(Self, ToPid, AnswerRequestMsg),

        %% Receive answer
        Answer = receive_msg(Self, ToPid, 30),
        io:format("~s から ~s への質問: ~s~n", [FromPid, ToPid, Question]),
        io:format("~s の回答: ~s~n", [ToPid, Answer]),

        {FromPid, ToPid, Question, Answer}
    end, AllQuestions),

    %% Step 5: Broadcast all Q&A to all players
    io:format("~n--- すべての質問・回答を開示 ---~n"),
    QAJson = format_qa_json(AllAnswers),
    lists:foreach(fun(PlayerPid) ->
        BroadcastQAMsg = io_lib:format("{\"type\":\"broadcast_qa\",\"qa_list\":~s}", [QAJson]),
        send(Self, PlayerPid, BroadcastQAMsg)
    end, AlivePlayers),

    io:format("~n--- 議論フェーズ終了 ---~n"),

    %% Vote phase
    io:format("~n--- 投票フェーズ ---~n"),
    Votes = collect_votes(Self, AlivePlayers),

    %% Display vote details
    io:format("投票結果:~n"),
    lists:foreach(fun({Voter, Target}) ->
        io:format("  ~s → ~s~n", [Voter, Target])
    end, Votes),

    %% Determine execution target
    ExecutionTarget = determine_execution(Votes),

    %% Handle execution or no execution
    {NewAlivePlayers, ExecutedPlayer} = case ExecutionTarget of
        "none" ->
            io:format("~n投票結果: 同数のため処刑なし~n"),
            {AlivePlayers, "none"};
        _ ->
            io:format("~n処刑対象: ~s~n", [ExecutionTarget]),
            UpdatedAlivePlayers = lists:delete(ExecutionTarget, AlivePlayers),
            io:format("~n処刑後の生存者: ~p~n", [UpdatedAlivePlayers]),
            {UpdatedAlivePlayers, ExecutionTarget}
    end,

    %% Record vote event
    VoteEvent = #{
        type => vote,
        day => Day,
        votes => Votes,
        executed => ExecutedPlayer
    },
    Events = maps:get(game_events, State, []),
    UpdatedEvents = Events ++ [VoteEvent],

    StateAfterExecution = State#{
        alive_players => NewAlivePlayers,
        game_events => UpdatedEvents
    },

    %% Check win condition after execution
    %% Victory conditions:
    %% - Villager team wins if all werewolves are dead
    %% - Werewolf team wins if werewolves >= villagers
    case check_win_condition(StateAfterExecution) of
        {game_end, Winner} ->
            StateAfterExecution#{winner => Winner};
        continue ->
            %% Night phase
            io:format("~n--- 夜フェーズ ---~n"),

            %% Collect night actions (normal night, not first night)
            NightActions = collect_night_actions(Self, NewAlivePlayers, StateAfterExecution, normal_night),

            %% Display night actions
            lists:foreach(fun({PlayerPid, Role, ActionMsg}) ->
                Target = parse_action_target(ActionMsg),
                case Role of
                    "werewolf" ->
                        io:format("~s (人狼) が ~s を襲撃対象に選択~n", [PlayerPid, Target]);
                    "seer" ->
                        io:format("~s (占い師) が ~s を占い対象に選択~n", [PlayerPid, Target]);
                    "knight" ->
                        io:format("~s (騎士) が ~s を護衛対象に選択~n", [PlayerPid, Target]);
                    _ ->
                        ok
                end
            end, NightActions),

            %% Resolve night actions
            NightResult = resolve_night(NightActions),

            %% Display night result
            io:format("~n--- 夜の結果 ---~n"),
            case NightResult of
                {victim, Victim} ->
                    io:format("襲撃成功: ~s が死亡しました~n", [Victim]);
                no_death ->
                    io:format("襲撃は失敗しました（護衛成功または襲撃なし）~n")
            end,

            %% Record night event
            NightEvent = #{
                type => night,
                day => Day,
                actions => NightActions,
                result => NightResult
            },
            NightEvents = maps:get(game_events, StateAfterExecution, []),
            UpdatedNightEvents = NightEvents ++ [NightEvent],

            %% Update state for next day
            StateAfterNight = case NightResult of
                {victim, Victim} ->
                    FinalAlivePlayers = lists:delete(Victim, NewAlivePlayers),
                    StateAfterExecution#{
                        alive_players => FinalAlivePlayers,
                        night_result => {victim, Victim},
                        game_events => UpdatedNightEvents
                    };
                no_death ->
                    StateAfterExecution#{
                        night_result => no_death,
                        game_events => UpdatedNightEvents
                    }
            end,

            %% Check win condition after night attack
            %% Victory conditions:
            %% - Villager team wins if all werewolves are dead
            %% - Werewolf team wins if werewolves >= villagers
            case check_win_condition(StateAfterNight) of
                {game_end, Winner} ->
                    StateAfterNight#{winner => Winner};
                continue ->
                    %% Continue to next day
                    game_loop(Self, StateAfterNight#{day => Day + 1})
            end
    end.

%% Collect votes from all alive players
collect_votes(Self, AlivePlayers) ->
    lists:foreach(fun(PlayerPid) ->
        VoteRequestMsg = "{\"type\":\"vote_request\",\"instruction\":\"Respond in Japanese (回答は日本語で行ってください)\"}",
        send(Self, PlayerPid, VoteRequestMsg)
    end, AlivePlayers),

    lists:map(fun(PlayerPid) ->
        VoteMsg = receive_msg(Self, PlayerPid, 30),
        %% Parse vote message to extract target
        %% Format: {"type":"vote","target":"player_X"}
        {PlayerPid, parse_vote_target(VoteMsg)}
    end, AlivePlayers).

%% Determine execution target from votes
%% Returns: ExecutionTarget (player ID) or "none" if no single majority
determine_execution(Votes) ->
    %% Count votes for each target
    TargetCounts = lists:foldl(fun({_Voter, Target}, Acc) ->
        maps:update_with(Target, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Votes),

    %% Find target with most votes
    {ExecutionTarget, MaxVotes} = lists:foldl(fun({Target, Count}, {CurrentTarget, CurrentMax}) ->
        if
            Count > CurrentMax -> {Target, Count};
            true -> {CurrentTarget, CurrentMax}
        end
    end, {"", 0}, maps:to_list(TargetCounts)),

    %% Check if there's a tie (multiple players with max votes)
    MaxVoteTargets = lists:filter(fun({_Target, Count}) -> Count == MaxVotes end,
                                   maps:to_list(TargetCounts)),

    case length(MaxVoteTargets) of
        1 -> ExecutionTarget;  %% Single majority - execute
        _ -> "none"            %% Tie or no votes - no execution
    end.

%% Parse vote target from JSON message
%% Input: VoteMsg (JSON string with vote target)
%% Expected format: {"type":"vote","target":"player_X"}
%% Output: Target player ID (e.g., "player_1", "player_2", etc.)
%% Implementation is inferred by AI
parse_vote_target(VoteMsg) -> undefined.

%% Collect night actions from players with roles
%% NightType: first_night (only seer) or normal_night (werewolf, seer, knight)
collect_night_actions(Self, AlivePlayers, State, NightType) ->
    PlayerRoles = maps:get(player_roles, State),

    lists:filtermap(fun(PlayerPid) ->
        case lists:keyfind(PlayerPid, 1, PlayerRoles) of
            {PlayerPid, Role} ->
                %% On first night, only seer can act
                ShouldAct = case NightType of
                    first_night -> Role == "seer";
                    normal_night -> Role == "werewolf" orelse Role == "seer" orelse Role == "knight"
                end,

                if
                    ShouldAct ->
                        %% Request night action
                        ActionRequestMsg = io_lib:format("{\"type\":\"night_action_request\",\"role\":\"~s\",\"instruction\":\"Respond in Japanese (回答は日本語で行ってください)\"}", [Role]),
                        send(Self, PlayerPid, ActionRequestMsg),

                        %% Receive action
                        ActionMsg = receive_msg(Self, PlayerPid, 30),
                        {true, {PlayerPid, Role, ActionMsg}};
                    true ->
                        false
                end;
            _ ->
                false
        end
    end, AlivePlayers).

%% Resolve night actions
resolve_night(NightActions) ->
    %% Find werewolf attack
    WerewolfAttack = lists:filtermap(fun({_PlayerPid, Role, ActionMsg}) ->
        if
            Role == "werewolf" ->
                Target = parse_action_target(ActionMsg),
                {true, Target};
            true ->
                false
        end
    end, NightActions),

    %% Find knight protection
    KnightProtection = lists:filtermap(fun({_PlayerPid, Role, ActionMsg}) ->
        if
            Role == "knight" ->
                Target = parse_action_target(ActionMsg),
                {true, Target};
            true ->
                false
        end
    end, NightActions),

    %% Determine if attack succeeded
    case {WerewolfAttack, KnightProtection} of
        {[AttackTarget], [ProtectionTarget]} when AttackTarget == ProtectionTarget ->
            no_death;
        {[AttackTarget], _} ->
            {victim, AttackTarget};
        {[], _} ->
            no_death
    end.

%% Parse action target from JSON message
%% Input: ActionMsg (JSON string with action target)
%% Expected format: {"type":"night_action","action":"attack|divine|protect","target":"player_X"}
%% Output: Target player ID (e.g., "player_1", "player_2", etc.)
%% Implementation is inferred by AI
parse_action_target(ActionMsg) -> undefined.

%% Check win condition
check_win_condition(State) ->
    AlivePlayers = maps:get(alive_players, State),
    PlayerRoles = maps:get(player_roles, State),

    %% Count alive werewolves and villagers
    {WerewolfCount, VillagerCount} = lists:foldl(fun(PlayerPid, {W, V}) ->
        case lists:keyfind(PlayerPid, 1, PlayerRoles) of
            {PlayerPid, "werewolf"} ->
                {W + 1, V};
            {PlayerPid, _} ->
                {W, V + 1}
        end
    end, {0, 0}, AlivePlayers),

    if
        WerewolfCount == 0 ->
            {game_end, "村人陣営"};
        WerewolfCount >= VillagerCount ->
            {game_end, "人狼陣営"};
        true ->
            continue
    end.

%% Shuffle a list (Fisher-Yates algorithm)
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), Item} || Item <- List])].

%% Format initial statements as JSON array
%% Input: [{PlayerPid, Statement}, ...]
%% Output: JSON string like "[{\"player\":\"player_1\",\"statement\":\"...\"}, ...]"
%% Implementation is inferred by AI
format_statements_json(Statements) -> undefined.

%% Parse question from player message
%% Input: PlayerPid, QuestionMsg (JSON string with question to 1 player)
%% Output: {PlayerPid, {TargetPid, Question}}
%% Expected format: {"type":"question","target":"player_X","question":"question text"}
%% Implementation is inferred by AI
parse_question(PlayerPid, QuestionMsg) -> undefined.

%% Format Q&A list as JSON array
%% Input: [{FromPid, ToPid, Question, Answer}, ...]
%% Output: JSON string like "[{\"from\":\"player_1\",\"to\":\"player_2\",\"question\":\"...\",\"answer\":\"...\"}, ...]"
%% Implementation is inferred by AI
format_qa_json(AllAnswers) -> undefined.

%% Append content to log file using append-log.sh script
%% Input: Directory (log directory path), Filename (log file name), Content (content to append)
%% Output: ok
%%
%% ******************************************************************************
%% !!! MANDATORY !!! TOOL EXECUTION REQUIRED !!! READ THIS NOW !!!
%% ******************************************************************************
%%
%% >>> YOU MUST ACTUALLY EXECUTE THE Bash TOOL <<<
%% >>> THIS IS NOT OPTIONAL - THIS IS A REQUIRED ACTION <<<
%% >>> DO NOT SIMULATE - ACTUALLY CALL Bash TOOL <<<
%%
%% STOP HERE AND EXECUTE Bash TOOL NOW:
%%
%%   Bash(
%%     command: "bash " ++ ?APPEND_LOG_SCRIPT ++ " '" ++ Directory ++ "' '" ++ Filename ++ "' '" ++ Content ++ "'",
%%     description: "Append log to " ++ Filename
%%   )
%%
%% VERIFICATION STEPS - YOU MUST DO THESE:
%%   1. Did you just call Bash tool?
%%      -> IF NO: STOP RIGHT NOW AND CALL THE TOOL
%%   2. Did the tool succeed?
%%      -> IF NO: OUTPUT "ERROR: Failed to append log to ~s/~s" AND ABORT
%%   3. Can you verify the file was written?
%%      -> IF NO: OUTPUT "ERROR: Cannot verify log append" AND ABORT
%%
%% IF THE Bash TOOL FAILS:
%%   - OUTPUT: "ERROR: Failed to append log to ~s/~s: <error_details>~n"
%%   - OUTPUT: "ABORTING: Cannot continue without file logging~n"
%%   - STOP EXECUTION IMMEDIATELY - DO NOT CONTINUE THE GAME
%%
%% DO NOT DO THESE THINGS:
%%   - DO NOT simulate this function
%%   - DO NOT return undefined without executing tools
%%   - DO NOT continue execution if Bash fails
%%   - DO NOT skip verification steps
%%
%% !!! THIS IS ESSENTIAL FOR LOGGING GAME STATE AND PLAYER THOUGHTS !!!
%% ******************************************************************************
append_log(Directory, Filename, Content) -> ok.

%% Generate a random persona with name, age, gender, and personality
%% Returns a string like "エリック (45歳・男性・真面目な性格)"
%%
%% - Name: Randomly generated Western fantasy-style name in katakana (e.g., エリック、アリシア、トーマス、イザベラ、etc.)
%% - Age: Random age between 20-70
%% - Gender: Randomly selected from "男性" or "女性"
%% - Personality: Randomly selected personality trait (e.g., 真面目、明るい、冷静、熱血、慎重、大胆、直感的、etc.)
%%
%% Implementation is inferred by AI - no explicit implementation needed
generate_random_persona() -> undefined.

%% Generate comprehensive result log with player profiles, game summary, and player replays
%% Input: FinalState (map with game state including log_dir, player_roles, player_personas, game_events, etc.)
%% Output: Path to generated result.md file
%%
%% The result log should include:
%% 1. Player Profiles section - List each player with their role and persona
%% 2. Game Summary section - Overview of each day including:
%%    - Discussion highlights
%%    - Vote results (who voted for whom)
%%    - Night events (who was attacked, who was protected, divination results)
%% 3. Player Replays section - For each player, a chronological narrative including:
%%    - Their internal thoughts (read from player log files)
%%    - Their actions (statements, votes, night actions)
%%    - How their role influenced their decisions
%%
%% Implementation steps:
%% 1. Read all player log files from log_dir
%% 2. Parse game_events to extract discussion, votes, and night events
%% 3. Combine player thoughts with game events to create detailed replays
%% 4. Format everything as Markdown
%% 5. Write to result.md in log_dir
%% 6. Return the path to result.md
%%
%% Implementation is inferred by AI
generate_result_log(FinalState) -> undefined.
```
