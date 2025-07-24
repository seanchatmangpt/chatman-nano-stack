%%%-------------------------------------------------------------------
%%% @doc UHFT Stress Test with 80/20 Optimizations
%%% Target: Reduce P99 latency from 2916ns to ≤1000ns
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(uhft_stress_test).
-export([run_optimized_tests/0, run_latency_test_optimized/0]).

%%%===================================================================
%%% Optimized Test Suite
%%%===================================================================

-spec run_optimized_tests() -> #{atom() => term()}.
run_optimized_tests() ->
    io:format("=== UHFT 80/20 Optimized Stress Tests ===~n"),
    
    %% Ensure system is ready with optimizations
    ok = ensure_optimized_system_ready(),
    
    %% Run individual test components with optimizations
    Results = #{
        latency_test_optimized => run_latency_test_optimized(),
        throughput_test_optimized => run_throughput_test_optimized(), 
        memory_efficiency_test => run_memory_efficiency_test(),
        nif_performance_test => run_nif_performance_test()
    },
    
    %% Generate performance report
    generate_uhft_performance_report(Results),
    
    io:format("~n=== 80/20 Optimization Results ===~n"),
    Results.

%% OPTIMIZED: Ultra-low latency test using optimized server
-spec run_latency_test_optimized() -> #{atom() => term()}.
run_latency_test_optimized() ->
    io:format("Running UHFT latency test (target ≤1000ns P99)...~n"),
    
    try
        %% Create test actor using optimized spawn
        {ok, Actor, SpawnLatency} = bitactor_server_optimized:spawn_actor_fast(uhft_test, #{}),
        
        %% Warm up - critical for accurate latency measurement
        [bitactor_server_optimized:send_message_fast(Actor, <<"warmup">>) || _ <- lists:seq(1, 100)],
        timer:sleep(10), % Allow warmup processing
        
        %% Measure 10,000 message latencies for statistical significance
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server_optimized:send_message_fast(Actor, <<"uhft_test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 10000)],
        
        %% Calculate comprehensive statistics
        SortedLatencies = lists:sort(Latencies),
        Len = length(SortedLatencies),
        P50 = lists:nth(Len div 2, SortedLatencies),
        P95 = lists:nth(round(Len * 0.95), SortedLatencies),
        P99 = lists:nth(round(Len * 0.99), SortedLatencies),
        P999 = lists:nth(round(Len * 0.999), SortedLatencies),
        
        Mean = lists:sum(SortedLatencies) / Len,
        Min = lists:min(SortedLatencies),
        Max = lists:max(SortedLatencies),
        
        %% Cleanup
        bitactor_server_optimized:kill_actor_fast(Actor),
        
        %% UHFT compliance check
        UHFTCompliant = P99 =< 1000,
        UHFTElite = P99 =< 500,
        
        Result = #{
            status => passed,
            test_type => uhft_latency_validation,
            sample_count => Len,
            spawn_latency_ns => SpawnLatency,
            min_latency_ns => Min,
            mean_latency_ns => round(Mean),
            p50_latency_ns => P50,
            p95_latency_ns => P95,
            p99_latency_ns => P99,
            p999_latency_ns => P999,
            max_latency_ns => Max,
            uhft_compliant => UHFTCompliant,
            uhft_elite => UHFTElite,
            optimization_success => P99 < 2916  % vs baseline
        },
        
        %% Report results
        io:format("  Spawn Latency: ~p ns~n", [SpawnLatency]),
        io:format("  P50: ~p ns, P95: ~p ns, P99: ~p ns~n", [P50, P95, P99]),
        io:format("  UHFT Compliant (≤1000ns): ~p~n", [UHFTCompliant]),
        io:format("  UHFT Elite (≤500ns): ~p~n", [UHFTElite]),
        
        Result
        
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => uhft_latency_validation,
                error => {Error, Reason, Stacktrace}
            }
    end.

%% OPTIMIZED: High-throughput test
-spec run_throughput_test_optimized() -> #{atom() => term()}.
run_throughput_test_optimized() ->
    io:format("Running optimized throughput test...~n"),
    
    try
        %% Create multiple target actors using fast spawn
        ActorCount = 20,
        TargetActors = [begin
            {ok, A, _} = bitactor_server_optimized:spawn_actor_fast(throughput_test, #{id => N}),
            A
        end || N <- lists:seq(1, ActorCount)],
        
        %% Burst test - 100k messages
        MessageCount = 100000,
        StartTime = erlang:monotonic_time(millisecond),
        
        %% Parallel message sending using fast path
        [begin
            Actor = lists:nth(rand:uniform(ActorCount), TargetActors),
            bitactor_server_optimized:send_message_fast(Actor, <<"throughput_test">>)
        end || _ <- lists:seq(1, MessageCount)],
        
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        
        %% Calculate throughput metrics
        ThroughputMsgsSec = (MessageCount * 1000) / max(1, Duration),
        LatencyPerMessage = (Duration * 1000000) / MessageCount, % nanoseconds
        
        %% Cleanup
        [bitactor_server_optimized:kill_actor_fast(A) || A <- TargetActors],
        
        Result = #{
            status => passed,
            test_type => uhft_throughput_validation,
            messages_sent => MessageCount,
            duration_ms => Duration,
            throughput_msgs_sec => ThroughputMsgsSec,
            avg_latency_per_msg_ns => LatencyPerMessage,
            target_met => ThroughputMsgsSec >= 100000, % 100k msgs/sec minimum
            uhft_capable => ThroughputMsgsSec >= 1000000 % 1M msgs/sec target
        },
        
        io:format("  Throughput: ~.0f msgs/sec~n", [ThroughputMsgsSec]),
        io:format("  Average latency per message: ~.0f ns~n", [LatencyPerMessage]),
        
        Result
        
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => uhft_throughput_validation,
                error => {Error, Reason, Stacktrace}
            }
    end.

%% Memory efficiency test
-spec run_memory_efficiency_test() -> #{atom() => term()}.
run_memory_efficiency_test() ->
    io:format("Running memory efficiency test...~n"),
    
    try
        InitialMemory = erlang:memory(total),
        
        %% Create many actors to test memory efficiency
        ActorCount = 5000,
        StartTime = erlang:system_time(millisecond),
        
        Actors = [begin
            {ok, A, _} = bitactor_server_optimized:spawn_actor_fast(memory_test, #{id => N}),
            A
        end || N <- lists:seq(1, ActorCount)],
        
        PeakMemory = erlang:memory(total),
        
        %% Send messages to measure memory pressure
        [bitactor_server_optimized:send_message_fast(A, <<"memory_test">>) || A <- Actors],
        
        %% Force GC and measure
        erlang:garbage_collect(),
        PostGCMemory = erlang:memory(total),
        
        %% Cleanup
        [bitactor_server_optimized:kill_actor_fast(A) || A <- Actors],
        erlang:garbage_collect(),
        
        EndTime = erlang:system_time(millisecond),
        FinalMemory = erlang:memory(total),
        
        #{
            status => passed,
            test_type => memory_efficiency,
            actors_created => ActorCount,
            duration_ms => EndTime - StartTime,
            initial_memory_mb => InitialMemory / (1024 * 1024),
            peak_memory_mb => PeakMemory / (1024 * 1024),
            post_gc_memory_mb => PostGCMemory / (1024 * 1024),
            final_memory_mb => FinalMemory / (1024 * 1024),
            memory_per_actor_bytes => (PeakMemory - InitialMemory) / ActorCount,
            memory_efficiency => (FinalMemory - InitialMemory) < (PeakMemory - InitialMemory) * 0.1
        }
        
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => memory_efficiency,
                error => {Error, Reason, Stacktrace}
            }
    end.

%% NIF performance validation
-spec run_nif_performance_test() -> #{atom() => term()}.
run_nif_performance_test() ->
    io:format("Running NIF performance validation...~n"),
    
    try
        %% Test NIF loading and basic functions
        NIFLoaded = erlang:function_exported(bitactor_nif, measure_latency, 0),
        
        case NIFLoaded of
            true ->
                %% Measure NIF overhead
                {ok, MinNIF, AvgNIF, MaxNIF} = bitactor_nif:measure_latency(),
                {ok, ActorCount, TotalMessages, TotalTicks, NSPerTick} = bitactor_nif:get_stats(),
                
                #{
                    status => passed,
                    test_type => nif_performance,
                    nif_loaded => true,
                    nif_min_overhead_ns => MinNIF,
                    nif_avg_overhead_ns => AvgNIF,
                    nif_max_overhead_ns => MaxNIF,
                    nif_actor_count => ActorCount,
                    nif_total_messages => TotalMessages,
                    nif_total_ticks => TotalTicks,
                    nif_ns_per_tick => NSPerTick,
                    performance_rating => case AvgNIF of
                        N when N < 50 -> excellent;
                        N when N < 100 -> good;
                        N when N < 200 -> acceptable;
                        _ -> poor
                    end
                };
            false ->
                #{
                    status => passed,
                    test_type => nif_performance,
                    nif_loaded => false,
                    fallback_mode => true,
                    performance_impact => "Using Erlang fallback (higher latency expected)"
                }
        end
        
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => nif_performance,
                error => {Error, Reason, Stacktrace}
            }
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

-spec ensure_optimized_system_ready() -> ok.
ensure_optimized_system_ready() ->
    %% Start optimized application
    case application:ensure_all_started(bitactor) of
        {ok, _} -> 
            %% Switch to optimized server if not already running
            case whereis(bitactor_server_optimized) of
                undefined ->
                    {ok, _} = bitactor_server_optimized:start_link();
                _ -> ok
            end,
            ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec generate_uhft_performance_report(#{atom() => term()}) -> ok.
generate_uhft_performance_report(Results) ->
    io:format("~n=== UHFT Performance Report (Mermaid) ===~n"),
    
    LatencyResult = maps:get(latency_test_optimized, Results),
    ThroughputResult = maps:get(throughput_test_optimized, Results),
    MemoryResult = maps:get(memory_efficiency_test, Results),
    NIFResult = maps:get(nif_performance_test, Results),
    
    io:format("```mermaid~n"),
    io:format("graph TB~n"),
    io:format("    A[BitActor UHFT 80/20 Optimized] --> B[Performance Metrics]~n"),
    
    case maps:get(status, LatencyResult) of
        passed ->
            P99 = maps:get(p99_latency_ns, LatencyResult, 0),
            UHFTCompliant = maps:get(uhft_compliant, LatencyResult, false),
            io:format("    B --> C[Latency P99: ~p ns]~n", [P99]),
            case UHFTCompliant of
                true -> io:format("    C --> C1[✓ UHFT Compliant ≤1000ns]~n");
                false -> io:format("    C --> C1[✗ Exceeds UHFT limit]~n")
            end;
        failed -> io:format("    B --> C[✗ Latency Test Failed]~n")
    end,
    
    case maps:get(status, ThroughputResult) of
        passed ->
            Throughput = maps:get(throughput_msgs_sec, ThroughputResult, 0),
            UHFTCapable = maps:get(uhft_capable, ThroughputResult, false),
            io:format("    B --> D[Throughput: ~.0f msgs/sec]~n", [Throughput]),
            case UHFTCapable of
                true -> io:format("    D --> D1[✓ UHFT Capable ≥1M msgs/sec]~n");
                false -> io:format("    D --> D1[⚠ Below UHFT target]~n")
            end;
        failed -> io:format("    B --> D[✗ Throughput Test Failed]~n")
    end,
    
    case maps:get(status, MemoryResult) of
        passed ->
            MemPerActor = maps:get(memory_per_actor_bytes, MemoryResult, 0),
            io:format("    B --> E[Memory: ~.0f bytes/actor]~n", [MemPerActor]);
        failed -> io:format("    B --> E[✗ Memory Test Failed]~n")
    end,
    
    case maps:get(status, NIFResult) of
        passed ->
            NIFLoaded = maps:get(nif_loaded, NIFResult, false),
            case NIFLoaded of
                true ->
                    AvgOverhead = maps:get(nif_avg_overhead_ns, NIFResult, 0),
                    io:format("    B --> F[NIF: ~p ns overhead]~n", [AvgOverhead]),
                    io:format("    F --> F1[✓ C NIF Active]~n");
                false ->
                    io:format("    B --> F[NIF: Fallback Mode]~n"),
                    io:format("    F --> F1[⚠ Erlang Fallback]~n")
            end;
        failed -> io:format("    B --> F[✗ NIF Test Failed]~n")
    end,
    
    io:format("```~n"),
    ok.