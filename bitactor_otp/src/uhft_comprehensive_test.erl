%%%-------------------------------------------------------------------
%%% @doc BitActor UHFT Comprehensive Test Suite
%%% Complete validation of 80/20 optimizations
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(uhft_comprehensive_test).
-export([run_all_tests/0, generate_report/1]).

-define(TARGET_P99_NS, 1000).      % UHFT target
-define(TARGET_THROUGHPUT, 1000000). % 1M msgs/sec

run_all_tests() ->
    io:format("=== BitActor UHFT Comprehensive Test Suite ===~n"),
    io:format("Target: P99 ≤ ~p ns, Throughput ≥ ~p msgs/sec~n~n", [?TARGET_P99_NS, ?TARGET_THROUGHPUT]),
    
    %% Test categories
    Tests = [
        {"Baseline Performance", fun run_baseline_test/0},
        {"NIF Loading Validation", fun validate_nif_loading/0},
        {"Latency Test (100K msgs)", fun run_latency_test_100k/0},
        {"Throughput Test (1M msgs)", fun run_throughput_test_1m/0},
        {"Concurrent Actor Test", fun run_concurrent_actor_test/0},
        {"Memory Efficiency Test", fun run_memory_test/0},
        {"Sustained Load Test", fun run_sustained_load_test/0},
        {"Stress Scenario Test", fun run_stress_scenario/0}
    ],
    
    %% Run all tests
    Results = lists:map(fun({Name, TestFun}) ->
        io:format("~nRunning: ~s...~n", [Name]),
        Result = TestFun(),
        {Name, Result}
    end, Tests),
    
    %% Generate comprehensive report
    generate_report(Results),
    
    %% Return summary
    #{
        tests_run => length(Results),
        tests_passed => length([ok || {_, #{status := passed}} <- Results]),
        uhft_compliant => is_uhft_compliant(Results),
        results => Results
    }.

%% Baseline test without optimizations
run_baseline_test() ->
    try
        {ok, Actor, _} = bitactor_server:spawn_actor(baseline, #{}),
        
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server:send_message(Actor, <<"test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 1000)],
        
        bitactor_server:kill_actor(Actor),
        
        analyze_latencies("Baseline", Latencies)
    catch
        _:_ -> #{status => failed, reason => baseline_error}
    end.

%% Validate NIF is properly loaded
validate_nif_loading() ->
    try
        NIFLoaded = erlang:function_exported(bitactor_nif, measure_latency, 0),
        
        case NIFLoaded of
            true ->
                {ok, Min, Avg, Max} = bitactor_nif:measure_latency(),
                #{
                    status => passed,
                    nif_loaded => true,
                    timing_overhead => #{
                        min_ns => Min,
                        avg_ns => Avg,
                        max_ns => Max
                    },
                    performance_impact => if
                        Avg < 20 -> minimal;
                        Avg < 50 -> low;
                        Avg < 100 -> moderate;
                        true -> high
                    end
                };
            false ->
                #{
                    status => failed,
                    nif_loaded => false,
                    reason => "NIF not loaded - running in fallback mode"
                }
        end
    catch
        _:_ -> #{status => failed, reason => nif_validation_error}
    end.

%% High-volume latency test
run_latency_test_100k() ->
    try
        {ok, Actor, SpawnLatency} = bitactor_server:spawn_actor(latency_test, #{}),
        
        %% Warmup
        [bitactor_server:send_message(Actor, <<"warmup">>) || _ <- lists:seq(1, 1000)],
        timer:sleep(10),
        
        %% Measure 100k messages
        io:format("  Measuring 100,000 message latencies...~n"),
        Latencies = measure_latencies(Actor, 100000),
        
        bitactor_server:kill_actor(Actor),
        
        Result = analyze_latencies("100K Latency", Latencies),
        Result#{spawn_latency_ns => SpawnLatency}
        
    catch
        _:_ -> #{status => failed, reason => latency_test_error}
    end.

%% High-throughput test
run_throughput_test_1m() ->
    try
        %% Create actor pool
        ActorCount = 100,
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(throughput, #{id => N}),
            A
        end || N <- lists:seq(1, ActorCount)],
        
        %% Send 1M messages
        MessageCount = 1000000,
        io:format("  Sending ~p messages to ~p actors...~n", [MessageCount, ActorCount]),
        
        StartTime = erlang:monotonic_time(nanosecond),
        
        %% Parallel sending
        send_messages_parallel(Actors, MessageCount),
        
        EndTime = erlang:monotonic_time(nanosecond),
        Duration = EndTime - StartTime,
        
        %% Cleanup
        [bitactor_server:kill_actor(A) || A <- Actors],
        
        ThroughputMsgsSec = (MessageCount * 1000000000) / Duration,
        
        #{
            status => passed,
            messages_sent => MessageCount,
            duration_ms => Duration / 1000000,
            throughput_msgs_sec => ThroughputMsgsSec,
            avg_latency_ns => Duration / MessageCount,
            uhft_capable => ThroughputMsgsSec >= ?TARGET_THROUGHPUT,
            performance_rating => rate_throughput(ThroughputMsgsSec)
        }
        
    catch
        _:_ -> #{status => failed, reason => throughput_test_error}
    end.

%% Concurrent actor stress test
run_concurrent_actor_test() ->
    try
        io:format("  Testing concurrent actor operations...~n"),
        
        %% Spawn many actors concurrently
        StartTime = erlang:monotonic_time(millisecond),
        
        Tasks = [spawn_monitor(fun() ->
            {ok, Actor, _} = bitactor_server:spawn_actor(concurrent, #{id => N}),
            %% Send messages
            [bitactor_server:send_message(Actor, <<"test">>) || _ <- lists:seq(1, 100)],
            %% Kill actor
            bitactor_server:kill_actor(Actor)
        end) || N <- lists:seq(1, 1000)],
        
        %% Wait for completion
        [receive {'DOWN', Ref, process, Pid, _} -> ok end || {Pid, Ref} <- Tasks],
        
        EndTime = erlang:monotonic_time(millisecond),
        
        #{
            status => passed,
            concurrent_actors => 1000,
            messages_per_actor => 100,
            total_operations => 100000,
            duration_ms => EndTime - StartTime,
            ops_per_sec => (100000 * 1000) / (EndTime - StartTime)
        }
        
    catch
        _:_ -> #{status => failed, reason => concurrent_test_error}
    end.

%% Memory efficiency test
run_memory_test() ->
    try
        InitialMemory = erlang:memory(total),
        
        %% Create many actors
        ActorCount = 10000,
        io:format("  Creating ~p actors...~n", [ActorCount]),
        
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(memory_test, #{id => N}),
            A
        end || N <- lists:seq(1, ActorCount)],
        
        PeakMemory = erlang:memory(total),
        
        %% Send messages
        [bitactor_server:send_message(lists:nth(rand:uniform(ActorCount), Actors), <<"test">>) 
         || _ <- lists:seq(1, 10000)],
        
        %% Force GC
        erlang:garbage_collect(),
        PostGCMemory = erlang:memory(total),
        
        %% Cleanup
        [bitactor_server:kill_actor(A) || A <- Actors],
        erlang:garbage_collect(),
        
        FinalMemory = erlang:memory(total),
        
        #{
            status => passed,
            actors_created => ActorCount,
            memory_per_actor_bytes => (PeakMemory - InitialMemory) / ActorCount,
            peak_memory_mb => PeakMemory / (1024 * 1024),
            gc_efficiency => (PeakMemory - PostGCMemory) / PeakMemory * 100,
            memory_leak => FinalMemory > InitialMemory * 1.1
        }
        
    catch
        _:_ -> #{status => failed, reason => memory_test_error}
    end.

%% Sustained load test
run_sustained_load_test() ->
    try
        io:format("  Running 10-second sustained load test...~n"),
        
        %% Create actors
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(sustained, #{id => N}),
            A
        end || N <- lists:seq(1, 50)],
        
        %% Run for 10 seconds
        StartTime = erlang:monotonic_time(millisecond),
        EndTime = StartTime + 10000, % 10 seconds
        
        MessagesSent = sustained_load_loop(Actors, EndTime, 0),
        
        ActualDuration = erlang:monotonic_time(millisecond) - StartTime,
        
        %% Cleanup
        [bitactor_server:kill_actor(A) || A <- Actors],
        
        #{
            status => passed,
            duration_ms => ActualDuration,
            messages_sent => MessagesSent,
            sustained_throughput => (MessagesSent * 1000) / ActualDuration,
            actors => length(Actors)
        }
        
    catch
        _:_ -> #{status => failed, reason => sustained_test_error}
    end.

%% Real-world stress scenario
run_stress_scenario() ->
    try
        io:format("  Running real-world UHFT scenario...~n"),
        
        %% Simulate market data feed
        {ok, MarketData, _} = bitactor_server:spawn_actor(market_data, #{}),
        {ok, OrderBook, _} = bitactor_server:spawn_actor(order_book, #{}),
        {ok, RiskEngine, _} = bitactor_server:spawn_actor(risk_engine, #{}),
        {ok, Execution, _} = bitactor_server:spawn_actor(execution, #{}),
        
        %% Simulate trading scenario
        TradingLatencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            
            %% Market data update
            bitactor_server:send_message(MarketData, {tick, <<"AAPL">>, 150.25, 1000}),
            
            %% Order book update
            bitactor_server:send_message(OrderBook, {update, <<"AAPL">>, bid, 150.24}),
            
            %% Risk check
            bitactor_server:send_message(RiskEngine, {check, <<"AAPL">>, buy, 100}),
            
            %% Execute order
            bitactor_server:send_message(Execution, {order, <<"AAPL">>, buy, 100, 150.25}),
            
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 10000)],
        
        %% Cleanup
        bitactor_server:kill_actor(MarketData),
        bitactor_server:kill_actor(OrderBook),
        bitactor_server:kill_actor(RiskEngine),
        bitactor_server:kill_actor(Execution),
        
        analyze_latencies("Trading Scenario", TradingLatencies)
        
    catch
        _:_ -> #{status => failed, reason => scenario_test_error}
    end.

%% Helper functions
measure_latencies(Actor, Count) ->
    measure_latencies(Actor, Count, []).

measure_latencies(_Actor, 0, Acc) -> 
    lists:reverse(Acc);
measure_latencies(Actor, Count, Acc) ->
    Start = erlang:monotonic_time(nanosecond),
    ok = bitactor_server:send_message(Actor, <<"test">>),
    End = erlang:monotonic_time(nanosecond),
    measure_latencies(Actor, Count - 1, [End - Start | Acc]).

send_messages_parallel(Actors, Count) ->
    ActorCount = length(Actors),
    Self = self(),
    
    %% Spawn parallel senders
    Workers = [spawn_link(fun() ->
        send_messages_worker(Actors, Count div 10, ActorCount),
        Self ! {done, self()}
    end) || _ <- lists:seq(1, 10)],
    
    %% Wait for completion
    [receive {done, W} -> ok end || W <- Workers].

send_messages_worker(_Actors, 0, _ActorCount) -> ok;
send_messages_worker(Actors, Count, ActorCount) ->
    Actor = lists:nth(rand:uniform(ActorCount), Actors),
    bitactor_server:send_message(Actor, <<"test">>),
    send_messages_worker(Actors, Count - 1, ActorCount).

sustained_load_loop(Actors, EndTime, Count) ->
    case erlang:monotonic_time(millisecond) >= EndTime of
        true -> Count;
        false ->
            Actor = lists:nth(rand:uniform(length(Actors)), Actors),
            bitactor_server:send_message(Actor, <<"sustained">>),
            sustained_load_loop(Actors, EndTime, Count + 1)
    end.

analyze_latencies(Name, Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50 = lists:nth(Len div 2, Sorted),
    P95 = lists:nth(round(Len * 0.95), Sorted),
    P99 = lists:nth(round(Len * 0.99), Sorted),
    P999 = lists:nth(round(Len * 0.999), Sorted),
    
    #{
        status => passed,
        name => Name,
        samples => Len,
        min_ns => lists:min(Sorted),
        p50_ns => P50,
        p95_ns => P95,
        p99_ns => P99,
        p999_ns => P999,
        max_ns => lists:max(Sorted),
        mean_ns => lists:sum(Sorted) div Len,
        uhft_compliant => P99 =< ?TARGET_P99_NS,
        performance_rating => rate_latency(P99)
    }.

rate_latency(P99) when P99 =< 500 -> elite;
rate_latency(P99) when P99 =< 1000 -> excellent;
rate_latency(P99) when P99 =< 5000 -> good;
rate_latency(P99) when P99 =< 10000 -> acceptable;
rate_latency(_) -> poor.

rate_throughput(T) when T >= 10000000 -> elite;      % 10M+ msgs/sec
rate_throughput(T) when T >= 1000000 -> excellent;   % 1M+ msgs/sec
rate_throughput(T) when T >= 100000 -> good;         % 100K+ msgs/sec
rate_throughput(T) when T >= 10000 -> acceptable;    % 10K+ msgs/sec
rate_throughput(_) -> poor.

is_uhft_compliant(Results) ->
    %% Check all latency tests
    LatencyTests = [Result || {_, Result = #{p99_ns := P99}} <- Results, is_number(P99)],
    AllCompliant = lists:all(fun(#{p99_ns := P99}) -> P99 =< ?TARGET_P99_NS end, LatencyTests),
    
    %% Check throughput
    ThroughputTests = [Result || {_, Result = #{throughput_msgs_sec := T}} <- Results, is_number(T)],
    ThroughputMet = lists:any(fun(#{throughput_msgs_sec := T}) -> T >= ?TARGET_THROUGHPUT end, ThroughputTests),
    
    AllCompliant and ThroughputMet.

generate_report(Results) ->
    io:format("~n=== UHFT Comprehensive Test Report ===~n"),
    
    %% Summary
    Passed = length([ok || {_, #{status := passed}} <- Results]),
    Total = length(Results),
    
    io:format("~nTest Summary: ~p/~p passed~n", [Passed, Total]),
    
    %% Detailed results
    io:format("~n--- Detailed Results ---~n"),
    lists:foreach(fun({Name, Result}) ->
        Status = maps:get(status, Result),
        io:format("~n~s: ~p~n", [Name, Status]),
        
        case Status of
            passed ->
                case maps:get(p99_ns, Result, undefined) of
                    P99 when is_number(P99) ->
                        io:format("  P99 Latency: ~p ns (~s)~n", [P99, maps:get(performance_rating, Result, unknown)]),
                        io:format("  UHFT Compliant: ~p~n", [maps:get(uhft_compliant, Result, false)]);
                    _ -> ok
                end,
                
                case maps:get(throughput_msgs_sec, Result, undefined) of
                    T when is_number(T) ->
                        io:format("  Throughput: ~.2f msgs/sec~n", [T]);
                    _ -> ok
                end;
            failed ->
                io:format("  Reason: ~p~n", [maps:get(reason, Result, unknown)])
        end
    end, Results),
    
    %% Mermaid visualization
    io:format("~n```mermaid~n"),
    io:format("graph TB~n"),
    io:format("    A[BitActor UHFT Tests] --> B[Results: ~p/~p Passed]~n", [Passed, Total]),
    
    lists:foreach(fun({Name, Result}) ->
        TestId = string:replace(Name, " ", "_", all),
        Status = maps:get(status, Result),
        
        case Status of
            passed ->
                io:format("    B --> ~s[✓ ~s]~n", [TestId, Name]),
                
                case maps:get(p99_ns, Result, undefined) of
                    P99 when is_number(P99) ->
                        UHFTStatus = if P99 =< ?TARGET_P99_NS -> "✓ UHFT"; true -> "✗ Over" end,
                        io:format("    ~s --> ~s_L[P99: ~p ns ~s]~n", [TestId, TestId, P99, UHFTStatus]);
                    _ -> ok
                end;
            failed ->
                io:format("    B --> ~s[✗ ~s]~n", [TestId, Name])
        end
    end, Results),
    
    io:format("```~n"),
    
    %% Final verdict
    UHFTCompliant = is_uhft_compliant(Results),
    io:format("~n=== FINAL VERDICT ===~n"),
    io:format("UHFT Compliant: ~p~n", [UHFTCompliant]),
    
    if UHFTCompliant ->
        io:format("✅ BitActor achieves UHFT performance targets!~n");
    true ->
        io:format("❌ BitActor does not meet UHFT requirements~n")
    end,
    
    ok.