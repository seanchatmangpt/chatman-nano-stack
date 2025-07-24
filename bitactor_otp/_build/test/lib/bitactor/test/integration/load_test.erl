%%%-------------------------------------------------------------------
%%% @doc BitActor Load Testing
%%% High-throughput load testing for production validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(load_test).

-include_lib("common_test/include/ct.hrl").
-include("../../include/bitactor.hrl").

%% API
-export([run_load_test/0, run_load_test/1]).
-export([concurrent_actor_test/1, message_throughput_test/1, 
         memory_stress_test/1, supervisor_stress_test/1]).

-define(DEFAULT_ACTORS, 1000).
-define(DEFAULT_MESSAGES_PER_ACTOR, 100).
-define(DEFAULT_DURATION_SECONDS, 30).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run_load_test() -> ok.
run_load_test() ->
    run_load_test(#{}).

-spec run_load_test(map()) -> ok.
run_load_test(Config) ->
    ct:pal("Starting BitActor load testing with config: ~p", [Config]),
    
    %% Ensure application is started
    application:ensure_all_started(bitactor),
    
    %% Run test suites
    Results = [
        {concurrent_actors, concurrent_actor_test(Config)},
        {message_throughput, message_throughput_test(Config)},
        {memory_stress, memory_stress_test(Config)},
        {supervisor_stress, supervisor_stress_test(Config)}
    ],
    
    %% Report results
    report_load_test_results(Results),
    ok.

%%%===================================================================
%%% Load Test Implementations
%%%===================================================================

-spec concurrent_actor_test(map()) -> map().
concurrent_actor_test(Config) ->
    NumActors = maps:get(num_actors, Config, ?DEFAULT_ACTORS),
    ct:pal("Running concurrent actor test with ~p actors", [NumActors]),
    
    StartTime = erlang:system_time(millisecond),
    InitialStats = bitactor_server:get_stats(),
    InitialHealth = bitactor_health:get_health(),
    
    %% Spawn actors concurrently
    Self = self(),
    SpawnPids = [spawn_link(fun() ->
        try
            ActorType = lists:nth((I rem 5) + 1, [market_data, order_book, risk_engine, execution, position]),
            {ok, ActorRef} = bitactor_server:spawn_actor(ActorType, #{id => I}),
            Self ! {spawned, I, ActorRef}
        catch
            Error:Reason ->
                Self ! {spawn_error, I, Error, Reason}
        end
    end) || I <- lists:seq(1, NumActors)],
    
    %% Collect spawn results
    SpawnResults = [receive
        {spawned, I, ActorRef} -> {ok, I, ActorRef};
        {spawn_error, I, Error, Reason} -> {error, I, Error, Reason}
    end || I <- lists:seq(1, NumActors)],
    
    SpawnTime = erlang:system_time(millisecond) - StartTime,
    
    %% Count successful spawns
    SuccessfulSpawns = [R || {ok, _, _} = R <- SpawnResults],
    ActorRefs = [Ref || {ok, _, Ref} <- SuccessfulSpawns],
    
    ct:pal("Spawned ~p/~p actors in ~p ms", [length(SuccessfulSpawns), NumActors, SpawnTime]),
    
    %% Send messages to all actors
    MessageStartTime = erlang:system_time(millisecond),
    MessagesPerActor = maps:get(messages_per_actor, Config, 10),
    
    MessagePids = [spawn_link(fun() ->
        lists:foreach(fun(MsgNum) ->
            Message = generate_test_message(MsgNum),
            bitactor_server:send_message(ActorRef, Message)
        end, lists:seq(1, MessagesPerActor)),
        Self ! {messages_sent, ActorRef}
    end) || ActorRef <- ActorRefs],
    
    %% Wait for all messages to be sent
    [receive {messages_sent, ActorRef} -> ok end || ActorRef <- ActorRefs],
    MessageTime = erlang:system_time(millisecond) - MessageStartTime,
    
    %% Allow processing time
    timer:sleep(1000),
    
    %% Cleanup actors
    CleanupStartTime = erlang:system_time(millisecond),
    lists:foreach(fun(ActorRef) ->
        bitactor_server:kill_actor(ActorRef)
    end, ActorRefs),
    CleanupTime = erlang:system_time(millisecond) - CleanupStartTime,
    
    TotalTime = erlang:system_time(millisecond) - StartTime,
    FinalStats = bitactor_server:get_stats(),
    FinalHealth = bitactor_health:get_health(),
    
    #{
        test_type => concurrent_actors,
        num_actors_requested => NumActors,
        num_actors_spawned => length(SuccessfulSpawns),
        messages_per_actor => MessagesPerActor,
        spawn_time_ms => SpawnTime,
        message_time_ms => MessageTime,
        cleanup_time_ms => CleanupTime,
        total_time_ms => TotalTime,
        actors_per_second => length(SuccessfulSpawns) * 1000 / SpawnTime,
        messages_per_second => (length(SuccessfulSpawns) * MessagesPerActor * 1000) / MessageTime,
        initial_stats => InitialStats,
        final_stats => FinalStats,
        initial_health => maps:get(status, InitialHealth),
        final_health => maps:get(status, FinalHealth),
        success_rate => length(SuccessfulSpawns) / NumActors
    }.

-spec message_throughput_test(map()) -> map().
message_throughput_test(Config) ->
    NumActors = maps:get(throughput_actors, Config, 100),
    Duration = maps:get(duration_seconds, Config, ?DEFAULT_DURATION_SECONDS),
    
    ct:pal("Running message throughput test with ~p actors for ~p seconds", [NumActors, Duration]),
    
    %% Spawn test actors
    ActorRefs = [begin
        {ok, Ref} = bitactor_server:spawn_actor(market_data, #{id => I}),
        Ref
    end || I <- lists:seq(1, NumActors)],
    
    %% Start message sending processes
    Self = self(),
    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + (Duration * 1000),
    
    SenderPids = [spawn_link(fun() ->
        message_sender_loop(ActorRef, EndTime, 0, Self)
    end) || ActorRef <- ActorRefs],
    
    %% Collect results
    MessageCounts = [receive
        {message_count, ActorRef, Count} -> Count
    end || ActorRef <- ActorRefs],
    
    ActualDuration = (erlang:system_time(millisecond) - StartTime) / 1000,
    TotalMessages = lists:sum(MessageCounts),
    
    %% Cleanup
    lists:foreach(fun(ActorRef) ->
        bitactor_server:kill_actor(ActorRef)
    end, ActorRefs),
    
    #{
        test_type => message_throughput,
        num_actors => NumActors,
        duration_seconds => ActualDuration,
        total_messages => TotalMessages,
        messages_per_second => TotalMessages / ActualDuration,
        messages_per_actor_per_second => TotalMessages / (NumActors * ActualDuration),
        avg_messages_per_actor => TotalMessages / NumActors
    }.

-spec memory_stress_test(map()) -> map().
memory_stress_test(Config) ->
    MaxActors = maps:get(max_actors, Config, 5000),
    BatchSize = maps:get(batch_size, Config, 100),
    
    ct:pal("Running memory stress test up to ~p actors in batches of ~p", [MaxActors, BatchSize]),
    
    InitialMemory = erlang:memory(),
    InitialHealth = bitactor_health:get_health(),
    
    %% Gradually increase actor count
    Results = memory_stress_loop(0, MaxActors, BatchSize, [], InitialMemory),
    
    FinalHealth = bitactor_health:get_health(),
    
    #{
        test_type => memory_stress,
        max_actors => MaxActors,
        batch_size => BatchSize,
        memory_progression => Results,
        initial_health => maps:get(status, InitialHealth),
        final_health => maps:get(status, FinalHealth),
        peak_memory_mb => lists:max([M || {_, M, _} <- Results])
    }.

-spec supervisor_stress_test(map()) -> map().
supervisor_stress_test(Config) ->
    NumRestarts = maps:get(num_restarts, Config, 10),
    ActorsPerRestart = maps:get(actors_per_restart, Config, 50),
    
    ct:pal("Running supervisor stress test with ~p restarts, ~p actors each", 
           [NumRestarts, ActorsPerRestart]),
    
    Results = lists:map(fun(RestartNum) ->
        %% Spawn actors
        ActorRefs = [begin
            {ok, Ref} = bitactor_server:spawn_actor(order_book, #{restart => RestartNum, id => I}),
            Ref
        end || I <- lists:seq(1, ActorsPerRestart)],
        
        %% Kill the server
        ServerPid = whereis(bitactor_server),
        exit(ServerPid, kill),
        
        %% Wait for restart
        RestartStartTime = erlang:system_time(millisecond),
        wait_for_server_restart(ServerPid),
        RestartTime = erlang:system_time(millisecond) - RestartStartTime,
        
        %% Verify system is responsive
        try
            ActorCount = bitactor_server:get_actor_count(),
            {RestartNum, RestartTime, {recovered, ActorCount}}
        catch
            Error:Reason ->
                {RestartNum, RestartTime, {error, Error, Reason}}
        end
    end, lists:seq(1, NumRestarts)),
    
    SuccessfulRestarts = [R || {_, _, {recovered, _}} <- Results],
    AvgRestartTime = lists:sum([T || {_, T, _} <- Results]) / length(Results),
    
    #{
        test_type => supervisor_stress,
        num_restarts => NumRestarts,
        successful_restarts => length(SuccessfulRestarts),
        success_rate => length(SuccessfulRestarts) / NumRestarts,
        avg_restart_time_ms => AvgRestartTime,
        restart_details => Results
    }.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

-spec message_sender_loop(reference(), integer(), integer(), pid()) -> ok.
message_sender_loop(ActorRef, EndTime, Count, Parent) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Message = generate_test_message(Count),
            bitactor_server:send_message(ActorRef, Message),
            message_sender_loop(ActorRef, EndTime, Count + 1, Parent);
        false ->
            Parent ! {message_count, ActorRef, Count}
    end.

-spec memory_stress_loop(integer(), integer(), integer(), list(), list()) -> list().
memory_stress_loop(CurrentActors, MaxActors, BatchSize, Acc, _InitialMemory) when CurrentActors >= MaxActors ->
    lists:reverse(Acc);
memory_stress_loop(CurrentActors, MaxActors, BatchSize, Acc, InitialMemory) ->
    %% Spawn batch of actors
    NewActors = min(BatchSize, MaxActors - CurrentActors),
    ActorRefs = [begin
        {ok, Ref} = bitactor_server:spawn_actor(market_data, #{batch => CurrentActors, id => I}),
        Ref
    end || I <- lists:seq(1, NewActors)],
    
    %% Measure memory
    CurrentMemory = erlang:memory(),
    MemoryMB = proplists:get_value(total, CurrentMemory) div (1024 * 1024),
    
    %% Force garbage collection and measure again
    erlang:garbage_collect(),
    timer:sleep(100),
    ActorCount = bitactor_server:get_actor_count(),
    
    Result = {CurrentActors + NewActors, MemoryMB, ActorCount},
    
    memory_stress_loop(CurrentActors + NewActors, MaxActors, BatchSize, [Result | Acc], InitialMemory).

-spec wait_for_server_restart(pid()) -> ok.
wait_for_server_restart(OldPid) ->
    wait_for_server_restart(OldPid, 50).

wait_for_server_restart(_OldPid, 0) ->
    error(server_restart_timeout);
wait_for_server_restart(OldPid, Retries) ->
    case whereis(bitactor_server) of
        undefined ->
            timer:sleep(100),
            wait_for_server_restart(OldPid, Retries - 1);
        NewPid when NewPid =/= OldPid ->
            ok;
        _SamePid ->
            timer:sleep(100),
            wait_for_server_restart(OldPid, Retries - 1)
    end.

-spec generate_test_message(integer()) -> tuple().
generate_test_message(Num) ->
    MessageTypes = [
        {tick, <<"LOAD_TEST">>, 100.0 + (Num rem 100), 1000 + Num},
        {order, buy, 99.0 + (Num rem 10), 100 + (Num rem 50)},
        {order, sell, 101.0 + (Num rem 10), 100 + (Num rem 50)},
        {position, <<"TEST">>, Num, 100.0}
    ],
    lists:nth((Num rem length(MessageTypes)) + 1, MessageTypes).

-spec report_load_test_results(list()) -> ok.
report_load_test_results(Results) ->
    ct:pal("=== BitActor Load Test Results ==="),
    lists:foreach(fun({TestType, Result}) ->
        ct:pal("~n--- ~p ---", [TestType]),
        report_test_result(Result)
    end, Results),
    ct:pal("=== End Load Test Results ===").

-spec report_test_result(map()) -> ok.
report_test_result(#{test_type := concurrent_actors} = Result) ->
    ct:pal("Actors spawned: ~p/~p (~.2f%)", [
        maps:get(num_actors_spawned, Result),
        maps:get(num_actors_requested, Result),
        maps:get(success_rate, Result) * 100
    ]),
    ct:pal("Spawn rate: ~.2f actors/sec", [maps:get(actors_per_second, Result)]),
    ct:pal("Message rate: ~.2f messages/sec", [maps:get(messages_per_second, Result)]),
    ct:pal("Health: ~p -> ~p", [
        maps:get(initial_health, Result),
        maps:get(final_health, Result)
    ]);

report_test_result(#{test_type := message_throughput} = Result) ->
    ct:pal("Total messages: ~p over ~.2f seconds", [
        maps:get(total_messages, Result),
        maps:get(duration_seconds, Result)
    ]),
    ct:pal("Throughput: ~.2f messages/sec", [maps:get(messages_per_second, Result)]),
    ct:pal("Per-actor rate: ~.2f messages/sec/actor", [
        maps:get(messages_per_actor_per_second, Result)
    ]);

report_test_result(#{test_type := memory_stress} = Result) ->
    ct:pal("Peak memory: ~p MB", [maps:get(peak_memory_mb, Result)]),
    ct:pal("Health: ~p -> ~p", [
        maps:get(initial_health, Result),
        maps:get(final_health, Result)
    ]);

report_test_result(#{test_type := supervisor_stress} = Result) ->
    ct:pal("Successful restarts: ~p/~p (~.2f%)", [
        maps:get(successful_restarts, Result),
        maps:get(num_restarts, Result),
        maps:get(success_rate, Result) * 100
    ]),
    ct:pal("Average restart time: ~.2f ms", [maps:get(avg_restart_time_ms, Result)]);

report_test_result(Result) ->
    ct:pal("Result: ~p", [Result]).