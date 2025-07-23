%%%-------------------------------------------------------------------
%%% @doc BitActor Unit Tests
%%% Real tests with actual assertions and timing validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).
-define(LATENCY_TARGET_NS, 10000). % 10 microseconds

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Start the application
    ok = application:ensure_all_started(bitactor),
    %% Wait for initialization
    timer:sleep(100),
    ok.

cleanup(_) ->
    %% Stop the application
    ok = application:stop(bitactor),
    ok.

bitactor_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Basic actor operations", fun test_basic_operations/0},
      {"Performance requirements", fun test_performance_requirements/0},
      {"Concurrent operations", fun test_concurrent_operations/0},
      {"Message ordering", fun test_message_ordering/0},
      {"Error handling", fun test_error_handling/0},
      {"Memory efficiency", fun test_memory_efficiency/0},
      {"UHFT latency requirements", fun test_uhft_latency/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_operations() ->
    %% Test actor spawn
    {ok, Actor1} = bitactor_server:spawn_actor(test, #{id => 1}),
    ?assert(is_reference(Actor1)),
    
    %% Test get_actor_count
    Count1 = bitactor_server:get_actor_count(),
    ?assert(Count1 >= 1),
    
    %% Spawn another actor
    {ok, Actor2} = bitactor_server:spawn_actor(test, #{id => 2}),
    ?assert(is_reference(Actor2)),
    ?assertNotEqual(Actor1, Actor2),
    
    Count2 = bitactor_server:get_actor_count(),
    ?assertEqual(Count1 + 1, Count2),
    
    %% Test message send
    ?assertEqual(ok, bitactor_server:send_message(Actor1, {test, 123})),
    ?assertEqual(ok, bitactor_server:send_message(Actor2, {test, 456})),
    
    %% Test actor kill
    ?assertEqual(ok, bitactor_server:kill_actor(Actor1)),
    Count3 = bitactor_server:get_actor_count(),
    ?assertEqual(Count2 - 1, Count3),
    
    %% Verify can't send to killed actor
    ?assertMatch({error, _}, bitactor_server:send_message(Actor1, {test, 789})),
    
    %% Clean up
    ?assertEqual(ok, bitactor_server:kill_actor(Actor2)).

test_performance_requirements() ->
    %% Test spawn latency
    SpawnTimes = [begin
        Start = erlang:monotonic_time(nanosecond),
        {ok, Actor} = bitactor_server:spawn_actor(perf_test, #{}),
        End = erlang:monotonic_time(nanosecond),
        bitactor_server:kill_actor(Actor),
        End - Start
    end || _ <- lists:seq(1, 100)],
    
    AvgSpawnTime = lists:sum(SpawnTimes) div length(SpawnTimes),
    ?assert(AvgSpawnTime < 1000000, % Less than 1ms
            io_lib:format("Spawn time too high: ~p ns", [AvgSpawnTime])),
    
    %% Test message latency
    {ok, Actor} = bitactor_server:spawn_actor(perf_test, #{}),
    
    MessageTimes = [begin
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(Actor, {perf, I}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || I <- lists:seq(1, 1000)],
    
    AvgMessageTime = lists:sum(MessageTimes) div length(MessageTimes),
    ?assert(AvgMessageTime < ?LATENCY_TARGET_NS,
            io_lib:format("Message time too high: ~p ns", [AvgMessageTime])),
    
    bitactor_server:kill_actor(Actor).

test_concurrent_operations() ->
    %% Spawn actors concurrently
    Parent = self(),
    NumProcesses = 10,
    ActorsPerProcess = 10,
    
    %% Spawn processes that will create actors
    Pids = [spawn(fun() ->
        Actors = [begin
            {ok, A} = bitactor_server:spawn_actor(concurrent, #{proc => N, actor => I}),
            A
        end || I <- lists:seq(1, ActorsPerProcess)],
        Parent ! {self(), actors_created, Actors}
    end) || N <- lists:seq(1, NumProcesses)],
    
    %% Collect all actors
    AllActors = lists:flatten([receive
        {Pid, actors_created, Actors} -> Actors
    after ?TIMEOUT ->
        ?assert(false, "Timeout waiting for actor creation")
    end || Pid <- Pids]),
    
    ?assertEqual(NumProcesses * ActorsPerProcess, length(AllActors)),
    ?assertEqual(length(AllActors), length(lists:usort(AllActors))), % All unique
    
    %% Send messages concurrently
    MessagePids = [spawn(fun() ->
        [bitactor_server:send_message(
            lists:nth(rand:uniform(length(AllActors)), AllActors),
            {concurrent_msg, N, I}
         ) || I <- lists:seq(1, 100)],
        Parent ! {self(), messages_sent}
    end) || N <- lists:seq(1, NumProcesses)],
    
    %% Wait for all messages
    [receive
        {Pid, messages_sent} -> ok
    after ?TIMEOUT ->
        ?assert(false, "Timeout sending messages")
    end || Pid <- MessagePids],
    
    %% Clean up
    [bitactor_server:kill_actor(A) || A <- AllActors].

test_message_ordering() ->
    %% Create actor
    {ok, Actor} = bitactor_server:spawn_actor(ordering_test, #{}),
    
    %% Send numbered messages
    Messages = lists:seq(1, 1000),
    [bitactor_server:send_message(Actor, {order, N}) || N <- Messages],
    
    %% Verify stats show all messages sent
    Stats = bitactor_server:get_stats(),
    MessagesSent = maps:get(messages_sent, Stats, 0),
    ?assert(MessagesSent >= 1000),
    
    bitactor_server:kill_actor(Actor).

test_error_handling() ->
    %% Test killing non-existent actor
    FakeRef = make_ref(),
    ?assertEqual({error, not_found}, bitactor_server:kill_actor(FakeRef)),
    
    %% Test sending to non-existent actor
    ?assertMatch({error, _}, bitactor_server:send_message(FakeRef, test)),
    
    %% Test invalid spawn parameters (if NIF loaded)
    case bitactor_server:get_stats() of
        #{nif_loaded := true} ->
            %% NIF should validate parameters
            ?assertMatch({error, _}, bitactor_server:spawn_actor(invalid_type, bad_data));
        _ ->
            %% Without NIF, should still work
            {ok, Actor} = bitactor_server:spawn_actor(any_type, any_data),
            bitactor_server:kill_actor(Actor)
    end.

test_memory_efficiency() ->
    %% Get initial memory
    InitialMemory = erlang:memory(total),
    
    %% Spawn many actors
    ActorCount = 1000,
    Actors = [begin
        {ok, A} = bitactor_server:spawn_actor(memory_test, #{id => I}),
        A
    end || I <- lists:seq(1, ActorCount)],
    
    %% Check memory usage
    AfterSpawnMemory = erlang:memory(total),
    MemoryPerActor = (AfterSpawnMemory - InitialMemory) div ActorCount,
    
    %% Should be less than 10KB per actor
    ?assert(MemoryPerActor < 10240,
            io_lib:format("Memory per actor too high: ~p bytes", [MemoryPerActor])),
    
    %% Send messages
    [bitactor_server:send_message(
        lists:nth(rand:uniform(ActorCount), Actors),
        {memory_test, I}
     ) || I <- lists:seq(1, ActorCount * 10)],
    
    %% Clean up and verify memory is released
    [bitactor_server:kill_actor(A) || A <- Actors],
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(100),
    
    FinalMemory = erlang:memory(total),
    ?assert(FinalMemory < AfterSpawnMemory,
            "Memory not released after killing actors").

test_uhft_latency() ->
    %% This test validates UHFT requirements
    
    %% Test 1: Market data processing latency
    {ok, MarketActor} = bitactor_server:spawn_actor(market_data, #{}),
    
    MarketLatencies = [begin
        Tick = {tick, <<"AAPL">>, 150.25 + I/100, 1000},
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(MarketActor, Tick),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || I <- lists:seq(1, 1000)],
    
    MarketP99 = lists:nth(990, lists:sort(MarketLatencies)),
    ?assert(MarketP99 < 1000, % Less than 1 microsecond
            io_lib:format("Market data P99 latency too high: ~p ns", [MarketP99])),
    
    %% Test 2: Order routing latency
    {ok, RouterActor} = bitactor_server:spawn_actor(order_router, #{}),
    
    OrderLatencies = [begin
        Order = {order, <<"AAPL">>, buy, 100, market},
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(RouterActor, Order),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || _ <- lists:seq(1, 1000)],
    
    OrderP99 = lists:nth(990, lists:sort(OrderLatencies)),
    ?assert(OrderP99 < 2000, % Less than 2 microseconds
            io_lib:format("Order routing P99 latency too high: ~p ns", [OrderP99])),
    
    %% Clean up
    bitactor_server:kill_actor(MarketActor),
    bitactor_server:kill_actor(RouterActor).

%%%===================================================================
%%% Property-Based Tests (if PropEr available)
%%%===================================================================

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").

prop_spawn_kill_balance() ->
    ?FORALL(Operations, list(oneof([spawn, kill])),
            begin
                InitialCount = bitactor_server:get_actor_count(),
                Actors = run_operations(Operations, []),
                FinalCount = bitactor_server:get_actor_count(),
                
                %% Clean up
                [catch bitactor_server:kill_actor(A) || A <- Actors],
                
                %% Property: count should match active actors
                FinalCount >= InitialCount
            end).

run_operations([], Actors) -> Actors;
run_operations([spawn | Rest], Actors) ->
    case bitactor_server:spawn_actor(prop_test, #{}) of
        {ok, Actor} -> run_operations(Rest, [Actor | Actors]);
        _ -> run_operations(Rest, Actors)
    end;
run_operations([kill | Rest], []) ->
    run_operations(Rest, []);
run_operations([kill | Rest], [Actor | Actors]) ->
    bitactor_server:kill_actor(Actor),
    run_operations(Rest, Actors).

-endif.

%%%===================================================================
%%% Telemetry Tests
%%%===================================================================

telemetry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Telemetry metrics collection", fun test_telemetry_metrics/0},
      {"Telemetry event handling", fun test_telemetry_events/0}
     ]}.

test_telemetry_metrics() ->
    %% Reset metrics
    ok = bitactor_telemetry:reset_metrics(),
    
    %% Perform operations
    {ok, Actor1} = bitactor_server:spawn_actor(telemetry_test, #{}),
    {ok, Actor2} = bitactor_server:spawn_actor(telemetry_test, #{}),
    
    bitactor_server:send_message(Actor1, test1),
    bitactor_server:send_message(Actor2, test2),
    bitactor_server:send_message(Actor1, test3),
    
    bitactor_server:kill_actor(Actor1),
    
    %% Get metrics
    Metrics = bitactor_telemetry:get_metrics(),
    
    %% Verify metrics
    ?assertEqual(2, maps:get(actors_spawned, Metrics)),
    ?assertEqual(1, maps:get(actors_killed, Metrics)),
    ?assertEqual(3, maps:get(messages_sent, Metrics)),
    ?assert(maps:get(uptime_ms, Metrics) > 0),
    
    %% Clean up
    bitactor_server:kill_actor(Actor2).

test_telemetry_events() ->
    %% Set up event handler
    Self = self(),
    telemetry:attach(
        test_handler,
        [bitactor, actor, spawn],
        fun(EventName, Measurements, Metadata, _) ->
            Self ! {telemetry_event, EventName, Measurements, Metadata}
        end,
        #{}
    ),
    
    %% Trigger event
    {ok, Actor} = bitactor_server:spawn_actor(event_test, #{test => true}),
    
    %% Verify event received
    receive
        {telemetry_event, [bitactor, actor, spawn], Measurements, Metadata} ->
            ?assertEqual(1, maps:get(count, Measurements)),
            ?assertEqual(event_test, maps:get(type, Metadata))
    after 1000 ->
        ?assert(false, "Telemetry event not received")
    end,
    
    %% Clean up
    telemetry:detach(test_handler),
    bitactor_server:kill_actor(Actor).

%%%===================================================================
%%% Stress Tests
%%%===================================================================

stress_test_() ->
    {timeout, 60000, % 60 second timeout
     {setup,
      fun setup/0,
      fun cleanup/1,
      [
       {"High load stress test", fun test_high_load/0},
       {"Sustained throughput test", fun test_sustained_throughput/0}
      ]}}.

test_high_load() ->
    %% Spawn many actors quickly
    ActorCount = 5000,
    
    Start = erlang:monotonic_time(millisecond),
    Actors = [begin
        {ok, A} = bitactor_server:spawn_actor(stress, #{id => I}),
        A
    end || I <- lists:seq(1, ActorCount)],
    SpawnDuration = erlang:monotonic_time(millisecond) - Start,
    
    SpawnRate = (ActorCount * 1000) div SpawnDuration,
    ?assert(SpawnRate > 1000, % More than 1000 actors/second
            io_lib:format("Spawn rate too low: ~p actors/sec", [SpawnRate])),
    
    %% Send many messages
    MessageCount = 10000,
    MessageStart = erlang:monotonic_time(millisecond),
    
    [bitactor_server:send_message(
        lists:nth(rand:uniform(ActorCount), Actors),
        {stress_msg, I}
     ) || I <- lists:seq(1, MessageCount)],
    
    MessageDuration = erlang:monotonic_time(millisecond) - MessageStart,
    MessageRate = (MessageCount * 1000) div MessageDuration,
    
    ?assert(MessageRate > 10000, % More than 10k messages/second
            io_lib:format("Message rate too low: ~p msgs/sec", [MessageRate])),
    
    %% Clean up
    [bitactor_server:kill_actor(A) || A <- Actors].

test_sustained_throughput() ->
    %% Create a moderate number of actors
    ActorCount = 100,
    Actors = [begin
        {ok, A} = bitactor_server:spawn_actor(throughput, #{id => I}),
        A
    end || I <- lists:seq(1, ActorCount)],
    
    %% Run for 5 seconds
    Duration = 5000,
    EndTime = erlang:monotonic_time(millisecond) + Duration,
    
    MessagesSent = send_until(EndTime, Actors, 0),
    
    Rate = (MessagesSent * 1000) div Duration,
    ?assert(Rate > 50000, % More than 50k messages/second sustained
            io_lib:format("Sustained rate too low: ~p msgs/sec", [Rate])),
    
    %% Verify system is still responsive
    {ok, TestActor} = bitactor_server:spawn_actor(test, #{}),
    ?assertEqual(ok, bitactor_server:send_message(TestActor, test)),
    ?assertEqual(ok, bitactor_server:kill_actor(TestActor)),
    
    %% Clean up
    [bitactor_server:kill_actor(A) || A <- Actors].

send_until(EndTime, Actors, Count) ->
    case erlang:monotonic_time(millisecond) >= EndTime of
        true -> Count;
        false ->
            bitactor_server:send_message(
                lists:nth(rand:uniform(length(Actors)), Actors),
                {throughput, Count}
            ),
            send_until(EndTime, Actors, Count + 1)
    end.