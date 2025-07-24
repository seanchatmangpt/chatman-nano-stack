%%%-------------------------------------------------------------------
%%% @doc BitActor Server Test Suite
%%% Comprehensive unit tests for bitactor_server module
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_server_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Common Test Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        test_start_stop_server,
        test_spawn_actor,
        test_kill_actor,
        test_send_message,
        test_send_message_direct,
        test_get_stats,
        test_get_actor_count,
        test_nif_loading,
        test_tick_processing,
        test_performance_metrics,
        test_memory_management,
        test_error_handling,
        test_concurrent_operations,
        test_ultra_fast_cache,
        test_telemetry_integration
    ].

init_per_suite(Config) ->
    %% Start BitActor application
    application:ensure_all_started(bitactor),
    Config.

end_per_suite(_Config) ->
    application:stop(bitactor),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure server is running
    case whereis(bitactor_server) of
        undefined ->
            {ok, _} = bitactor_server:start_link();
        _Pid ->
            ok
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up any test actors
    case whereis(bitactor_server) of
        undefined -> ok;
        _Pid ->
            %% Get current stats to see if cleanup is needed
            try
                Stats = bitactor_server:get_stats(),
                ActorCount = maps:get(actors_spawned, Stats, 0) - maps:get(actors_killed, Stats, 0),
                ct:pal("Active actors after test: ~p", [ActorCount])
            catch
                _:_ -> ok
            end
    end,
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_start_stop_server(_Config) ->
    %% Stop existing server
    bitactor_server:stop(),
    timer:sleep(100),
    
    %% Test start
    {ok, Pid} = bitactor_server:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(bitactor_server)),
    
    %% Test server is responding
    ?assertMatch(#{}, bitactor_server:get_stats()),
    
    %% Test stop
    ok = bitactor_server:stop(),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(bitactor_server)),
    
    %% Restart for other tests
    {ok, _} = bitactor_server:start_link(),
    ok.

test_spawn_actor(_Config) ->
    %% Test successful actor spawn
    {ok, ActorRef, LatencyNs} = bitactor_server:spawn_actor(test_actor, #{}),
    
    ?assert(is_reference(ActorRef)),
    ?assert(is_integer(LatencyNs)),
    ?assert(LatencyNs > 0),
    
    %% Verify actor count increased
    Count1 = bitactor_server:get_actor_count(),
    
    %% Spawn another actor
    {ok, ActorRef2, _} = bitactor_server:spawn_actor(another_test, #{data => <<"test">>}),
    ?assert(is_reference(ActorRef2)),
    ?assertNotEqual(ActorRef, ActorRef2),
    
    %% Verify count increased
    Count2 = bitactor_server:get_actor_count(),
    ?assert(Count2 > Count1),
    
    %% Clean up
    ok = bitactor_server:kill_actor(ActorRef),
    ok = bitactor_server:kill_actor(ActorRef2),
    
    ok.

test_kill_actor(_Config) ->
    %% Spawn an actor first
    {ok, ActorRef, _} = bitactor_server:spawn_actor(test_actor, #{}),
    InitialCount = bitactor_server:get_actor_count(),
    
    %% Kill the actor
    ok = bitactor_server:kill_actor(ActorRef),
    
    %% Verify count decreased
    FinalCount = bitactor_server:get_actor_count(),
    ?assert(FinalCount < InitialCount),
    
    %% Test killing non-existent actor
    FakeRef = make_ref(),
    ?assertMatch({error, not_found}, bitactor_server:kill_actor(FakeRef)),
    
    ok.

test_send_message(_Config) ->
    %% Spawn an actor
    {ok, ActorRef, _} = bitactor_server:spawn_actor(test_actor, #{}),
    
    %% Test sending message
    ok = bitactor_server:send_message(ActorRef, <<"test_message">>),
    ok = bitactor_server:send_message(ActorRef, {complex, message, 123}),
    ok = bitactor_server:send_message(ActorRef, test_atom),
    
    %% Test sending to non-existent actor
    FakeRef = make_ref(),
    ?assertMatch(ok, bitactor_server:send_message(FakeRef, <<"test">>)), % Should not fail immediately
    
    %% Clean up
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_send_message_direct(_Config) ->
    %% Test the ultra-fast direct message path
    {ok, ActorRef, _} = bitactor_server:spawn_actor(test_actor, #{}),
    
    %% Send multiple messages to populate cache
    lists:foreach(fun(N) ->
        ok = bitactor_server:send_message(ActorRef, {message, N})
    end, lists:seq(1, 100)),
    
    %% Measure performance
    StartTime = erlang:monotonic_time(nanosecond),
    ok = bitactor_server:send_message(ActorRef, <<"cached_message">>),
    EndTime = erlang:monotonic_time(nanosecond),
    
    Latency = EndTime - StartTime,
    ct:pal("Direct message latency: ~p ns", [Latency]),
    
    %% Clean up
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_get_stats(_Config) ->
    %% Get initial stats
    Stats = bitactor_server:get_stats(),
    
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(actors_spawned, Stats)),
    ?assert(maps:is_key(actors_killed, Stats)),
    ?assert(maps:is_key(messages_sent, Stats)),
    ?assert(maps:is_key(ticks_processed, Stats)),
    ?assert(maps:is_key(start_time, Stats)),
    
    %% Verify stats are numbers
    ?assert(is_integer(maps:get(actors_spawned, Stats))),
    ?assert(is_integer(maps:get(messages_sent, Stats))),
    
    ok.

test_get_actor_count(_Config) ->
    InitialCount = bitactor_server:get_actor_count(),
    ?assert(is_integer(InitialCount)),
    ?assert(InitialCount >= 0),
    
    %% Spawn some actors
    {ok, Actor1, _} = bitactor_server:spawn_actor(test1, #{}),
    {ok, Actor2, _} = bitactor_server:spawn_actor(test2, #{}),
    
    Count2 = bitactor_server:get_actor_count(),
    ?assert(Count2 >= InitialCount + 2),
    
    %% Kill one actor
    ok = bitactor_server:kill_actor(Actor1),
    Count3 = bitactor_server:get_actor_count(),
    ?assert(Count3 == Count2 - 1),
    
    %% Clean up
    ok = bitactor_server:kill_actor(Actor2),
    ok.

test_nif_loading(_Config) ->
    %% Check if NIF is loaded by examining server state
    Stats = bitactor_server:get_stats(),
    
    %% If NIF is loaded, we should be able to spawn actors successfully
    {ok, ActorRef, LatencyNs} = bitactor_server:spawn_actor(nif_test, #{}),
    
    ?assert(is_reference(ActorRef)),
    ?assert(is_integer(LatencyNs)),
    
    %% NIF-loaded actors should have very low latency
    ?assert(LatencyNs < 10000000), % Less than 10ms
    
    %% Clean up
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_tick_processing(_Config) ->
    %% Get initial tick count
    InitialStats = bitactor_server:get_stats(),
    InitialTicks = maps:get(ticks_processed, InitialStats, 0),
    
    %% Wait for some ticks to process
    timer:sleep(2500), % Wait for ~2 ticks (default interval is 1000ms)
    
    %% Check tick count increased
    FinalStats = bitactor_server:get_stats(),
    FinalTicks = maps:get(ticks_processed, FinalStats, 0),
    
    ?assert(FinalTicks >= InitialTicks + 1),
    ct:pal("Ticks processed: ~p -> ~p", [InitialTicks, FinalTicks]),
    
    ok.

test_performance_metrics(_Config) ->
    %% Test latency measurements
    NumTests = 1000,
    Latencies = [begin
        Start = erlang:monotonic_time(nanosecond),
        {ok, ActorRef, _} = bitactor_server:spawn_actor(perf_test, #{}),
        End = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:kill_actor(ActorRef),
        End - Start
    end || _ <- lists:seq(1, NumTests)],
    
    %% Calculate statistics
    Sorted = lists:sort(Latencies),
    Min = lists:min(Latencies),
    Max = lists:max(Latencies),
    Mean = lists:sum(Latencies) div length(Latencies),
    P95 = lists:nth(round(NumTests * 0.95), Sorted),
    P99 = lists:nth(round(NumTests * 0.99), Sorted),
    
    ct:pal("Spawn latency stats (ns): Min=~p, Mean=~p, P95=~p, P99=~p, Max=~p", 
           [Min, Mean, P95, P99, Max]),
    
    %% Performance assertions
    ?assert(P99 < 10000000), % P99 under 10ms
    ?assert(Mean < 1000000),  % Mean under 1ms
    
    ok.

test_memory_management(_Config) ->
    %% Test memory usage during actor lifecycle
    InitialMemory = erlang:memory(total),
    
    %% Spawn many actors
    ActorRefs = [begin
        {ok, ActorRef, _} = bitactor_server:spawn_actor({memory_test, N}, #{}),
        ActorRef
    end || N <- lists:seq(1, 1000)],
    
    PeakMemory = erlang:memory(total),
    MemoryIncrease = PeakMemory - InitialMemory,
    
    ct:pal("Memory increase for 1000 actors: ~p bytes (~p KB)", 
           [MemoryIncrease, MemoryIncrease div 1024]),
    
    %% Kill all actors
    lists:foreach(fun(ActorRef) ->
        ok = bitactor_server:kill_actor(ActorRef)
    end, ActorRefs),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    FinalMemory = erlang:memory(total),
    
    %% Memory should be released (allowing some overhead)
    MemoryDiff = FinalMemory - InitialMemory,
    ?assert(MemoryDiff < MemoryIncrease div 2), % At least 50% should be released
    
    ok.

test_error_handling(_Config) ->
    %% Test server behavior with invalid inputs
    
    %% Invalid actor type
    ?assertMatch({ok, _, _}, bitactor_server:spawn_actor(invalid_type, #{})),
    
    %% Invalid init data  
    ?assertMatch({ok, _, _}, bitactor_server:spawn_actor(test, invalid_data)),
    
    %% Test server continues working after errors
    {ok, ActorRef, _} = bitactor_server:spawn_actor(test_after_error, #{}),
    ?assert(is_reference(ActorRef)),
    
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_concurrent_operations(_Config) ->
    %% Test concurrent actor operations
    NumProcesses = 50,
    ActorsPerProcess = 20,
    
    %% Spawn processes that create and destroy actors concurrently
    Pids = [spawn_link(fun() ->
        ActorRefs = [begin
            {ok, ActorRef, _} = bitactor_server:spawn_actor({concurrent, self(), N}, #{}),
            ok = bitactor_server:send_message(ActorRef, {test, N}),
            ActorRef
        end || N <- lists:seq(1, ActorsPerProcess)],
        
        %% Kill actors
        lists:foreach(fun(ActorRef) ->
            ok = bitactor_server:kill_actor(ActorRef)
        end, ActorRefs)
    end) || _ <- lists:seq(1, NumProcesses)],
    
    %% Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive
            {'DOWN', MRef, process, Pid, normal} -> ok;
            {'DOWN', MRef, process, Pid, Reason} -> 
                ct:fail("Concurrent process failed: ~p", [Reason])
        after 10000 ->
            ct:fail("Concurrent test timeout")
        end
    end, Pids),
    
    ok.

test_ultra_fast_cache(_Config) ->
    %% Test the ultra-fast actor lookup cache
    {ok, ActorRef, _} = bitactor_server:spawn_actor(cache_test, #{}),
    
    %% Send initial message to populate cache
    ok = bitactor_server:send_message(ActorRef, warm_up),
    
    %% Measure cached vs non-cached performance
    CachedLatencies = [begin
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(ActorRef, cached_message),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || _ <- lists:seq(1, 100)],
    
    AvgCachedLatency = lists:sum(CachedLatencies) div length(CachedLatencies),
    ct:pal("Average cached message latency: ~p ns", [AvgCachedLatency]),
    
    %% Cached operations should be very fast
    ?assert(AvgCachedLatency < 1000000), % Under 1ms
    
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_telemetry_integration(_Config) ->
    %% Test telemetry events are emitted
    InitialStats = bitactor_server:get_stats(),
    
    %% Perform operations that should emit telemetry
    {ok, ActorRef, _} = bitactor_server:spawn_actor(telemetry_test, #{}),
    ok = bitactor_server:send_message(ActorRef, telemetry_message),
    ok = bitactor_server:kill_actor(ActorRef),
    
    %% Check stats were updated
    FinalStats = bitactor_server:get_stats(),
    
    InitialSpawned = maps:get(actors_spawned, InitialStats, 0),
    FinalSpawned = maps:get(actors_spawned, FinalStats, 0),
    ?assert(FinalSpawned > InitialSpawned),
    
    InitialMessages = maps:get(messages_sent, InitialStats, 0),
    FinalMessages = maps:get(messages_sent, FinalStats, 0),
    ?assert(FinalMessages > InitialMessages),
    
    ok.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

%% Wait for condition with timeout
wait_for_condition(Fun, Timeout) ->
    wait_for_condition(Fun, Timeout, 10).

wait_for_condition(_Fun, 0, _Interval) ->
    timeout;
wait_for_condition(Fun, Timeout, Interval) ->
    case Fun() of
        true -> ok;
        false ->
            timer:sleep(Interval),
            wait_for_condition(Fun, Timeout - Interval, Interval)
    end.