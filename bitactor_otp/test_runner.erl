-module(test_runner).
-export([main/1]).

main(_) ->
    io:format("=== BitActor UHFT Stress Test Suite ===~n"),
    
    % Add code path
    code:add_pathz("_build/default/lib/bitactor/ebin"),
    
    % Start application
    io:format("Starting BitActor application...~n"),
    case application:ensure_all_started(bitactor) of
        {ok, Apps} ->
            io:format("✓ Applications started: ~p~n", [Apps]),
            run_stress_tests();
        Error ->
            io:format("✗ Failed to start application: ~p~n", [Error]),
            erlang:halt(1)
    end.

run_stress_tests() ->
    io:format("Running comprehensive stress tests...~n"),
    
    % Basic functionality test
    BasicResult = run_basic_test(),
    print_test_result("Basic Functionality", BasicResult),
    
    % Latency test
    LatencyResult = run_latency_test(),
    print_test_result("Latency Validation", LatencyResult),
    
    % Throughput test  
    ThroughputResult = run_throughput_test(),
    print_test_result("Throughput Validation", ThroughputResult),
    
    % Actor stress test
    StressResult = run_actor_stress_test(),
    print_test_result("Actor Stress Test", StressResult),
    
    io:format("~n=== Test Summary ===~n"),
    Results = #{
        basic => BasicResult,
        latency => LatencyResult,
        throughput => ThroughputResult,
        stress => StressResult
    },
    
    % Generate OTEL performance metrics
    generate_otel_metrics(Results),
    
    erlang:halt(0).

run_basic_test() ->
    try
        StartTime = erlang:monotonic_time(millisecond),
        
        % Test actor creation
        {ok, Actor1, _} = bitactor_server:spawn_actor(test_actor, #{}),
        
        % Test message sending
        ok = bitactor_server:send_message(Actor1, <<"test_message">>),
        
        % Test multiple actors
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(test_actor, #{id => N}),
            A
        end || N <- lists:seq(1, 100)],
        
        % Cleanup
        [bitactor_server:kill_actor(A) || A <- [Actor1 | Actors]],
        
        EndTime = erlang:monotonic_time(millisecond),
        
        #{
            status => passed,
            duration_ms => EndTime - StartTime,
            actors_created => length(Actors) + 1,
            test_type => basic_functionality
        }
    catch
        Error:Reason:_Stacktrace ->
            #{
                status => failed,
                error => {Error, Reason},
                test_type => basic_functionality
            }
    end.

run_latency_test() ->
    try
        % Create test actor
        {ok, Actor, _} = bitactor_server:spawn_actor(latency_test, #{}),
        
        % Measure 1000 message latencies
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server:send_message(Actor, <<"latency_test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 1000)],
        
        % Calculate statistics
        SortedLatencies = lists:sort(Latencies),
        P50 = percentile(SortedLatencies, 50),
        P95 = percentile(SortedLatencies, 95),
        P99 = percentile(SortedLatencies, 99),
        
        % Cleanup
        bitactor_server:kill_actor(Actor),
        
        #{
            status => passed,
            test_type => latency_validation,
            sample_count => length(Latencies),
            p50_latency_ns => P50,
            p95_latency_ns => P95,
            p99_latency_ns => P99,
            max_latency_ns => lists:max(SortedLatencies),
            min_latency_ns => lists:min(SortedLatencies),
            sub_microsecond => P99 < 1000,
            uhft_compliant => P99 =< 1000
        }
    catch
        Error:Reason:_Stacktrace ->
            #{
                status => failed,
                test_type => latency_validation,
                error => {Error, Reason}
            }
    end.

run_throughput_test() ->
    try
        % Create multiple target actors
        TargetActors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(throughput_test, #{id => N}),
            A
        end || N <- lists:seq(1, 10)],
        
        % Send burst of messages
        MessageCount = 10000,
        StartTime = erlang:monotonic_time(millisecond),
        
        [begin
            Actor = lists:nth(rand:uniform(length(TargetActors)), TargetActors),
            bitactor_server:send_message(Actor, <<"throughput_test">>)
        end || _ <- lists:seq(1, MessageCount)],
        
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        
        % Calculate throughput
        ThroughputMsgsSec = (MessageCount * 1000) / Duration,
        
        % Cleanup
        [bitactor_server:kill_actor(A) || A <- TargetActors],
        
        #{
            status => passed,
            test_type => throughput_validation,
            messages_sent => MessageCount,
            duration_ms => Duration,
            throughput_msgs_sec => ThroughputMsgsSec,
            target_met => ThroughputMsgsSec >= 10000,
            uhft_capable => ThroughputMsgsSec >= 100000
        }
    catch
        Error:Reason:_Stacktrace ->
            #{
                status => failed,
                test_type => throughput_validation,
                error => {Error, Reason}
            }
    end.

run_actor_stress_test() ->
    try
        % Test creating many actors
        ActorCount = 1000,
        StartTime = erlang:monotonic_time(millisecond),
        
        % Create actors
        Actors = lists:map(fun(N) ->
            case catch bitactor_server:spawn_actor(stress_test, #{id => N}) of
                {ok, A, _} -> {ok, A};
                Error -> {error, Error}
            end
        end, lists:seq(1, ActorCount)),
        
        % Count successful creations
        SuccessfulActors = [A || {ok, A} <- Actors],
        FailedCreations = length(Actors) - length(SuccessfulActors),
        
        % Send messages to all actors
        [bitactor_server:send_message(A, <<"stress_test">>) || A <- SuccessfulActors],
        
        % Measure memory usage
        Memory = erlang:memory(total) / (1024 * 1024), % MB
        
        % Cleanup
        [bitactor_server:kill_actor(A) || A <- SuccessfulActors],
        
        EndTime = erlang:monotonic_time(millisecond),
        
        #{
            status => passed,
            test_type => actor_stress,
            target_actor_count => ActorCount,
            successful_actors => length(SuccessfulActors),
            failed_creations => FailedCreations,
            duration_ms => EndTime - StartTime,
            memory_peak_mb => Memory,
            success_rate => length(SuccessfulActors) / ActorCount,
            target_met => length(SuccessfulActors) >= ActorCount * 0.95
        }
    catch
        Error:Reason:_Stacktrace ->
            #{
                status => failed,
                test_type => actor_stress,
                error => {Error, Reason}
            }
    end.

print_test_result(TestName, Result) ->
    Status = maps:get(status, Result, unknown),
    TestType = maps:get(test_type, Result, unknown),
    
    io:format("~n--- ~s ---~n", [TestName]),
    io:format("Status: ~p~n", [Status]),
    
    case Status of
        passed ->
            case TestType of
                latency_validation ->
                    P99 = maps:get(p99_latency_ns, Result, 0),
                    SubMicro = maps:get(sub_microsecond, Result, false),
                    UHFTCompliant = maps:get(uhft_compliant, Result, false),
                    io:format("P99 Latency: ~p ns~n", [P99]),
                    io:format("Sub-microsecond: ~p~n", [SubMicro]),
                    io:format("UHFT Compliant: ~p~n", [UHFTCompliant]);
                throughput_validation ->
                    Throughput = maps:get(throughput_msgs_sec, Result, 0),
                    TargetMet = maps:get(target_met, Result, false),
                    UHFTCapable = maps:get(uhft_capable, Result, false),
                    io:format("Throughput: ~.2f msgs/sec~n", [Throughput]),
                    io:format("Target Met: ~p~n", [TargetMet]),
                    io:format("UHFT Capable: ~p~n", [UHFTCapable]);
                actor_stress ->
                    Actors = maps:get(successful_actors, Result, 0),
                    SuccessRate = maps:get(success_rate, Result, 0),
                    TargetMet = maps:get(target_met, Result, false),
                    io:format("Successful Actors: ~p~n", [Actors]),
                    io:format("Success Rate: ~.2f%~n", [SuccessRate * 100]),
                    io:format("Target Met: ~p~n", [TargetMet]);
                basic_functionality ->
                    Duration = maps:get(duration_ms, Result, 0),
                    ActorsCreated = maps:get(actors_created, Result, 0),
                    io:format("Duration: ~p ms~n", [Duration]),
                    io:format("Actors Created: ~p~n", [ActorsCreated]);
                _ ->
                    io:format("Test completed successfully~n")
            end;
        failed ->
            Error = maps:get(error, Result, unknown_error),
            io:format("ERROR: ~p~n", [Error])
    end.

generate_otel_metrics(Results) ->
    io:format("~n=== OpenTelemetry Performance Metrics ===~n"),
    
    % Extract key metrics
    BasicResult = maps:get(basic, Results),
    LatencyResult = maps:get(latency, Results),
    ThroughputResult = maps:get(throughput, Results),
    StressResult = maps:get(stress, Results),
    
    % Generate Mermaid diagram
    io:format("```mermaid~n"),
    io:format("graph TD~n"),
    io:format("    A[BitActor UHFT System] --> B[Basic Test]~n"),
    io:format("    A --> C[Latency Test]~n"),
    io:format("    A --> D[Throughput Test]~n"),
    io:format("    A --> E[Stress Test]~n"),
    
    case maps:get(status, BasicResult) of
        passed -> io:format("    B --> B1[✓ PASSED]~n");
        failed -> io:format("    B --> B1[✗ FAILED]~n")
    end,
    
    case maps:get(status, LatencyResult) of
        passed -> 
            P99 = maps:get(p99_latency_ns, LatencyResult, 0),
            UHFTCompliant = maps:get(uhft_compliant, LatencyResult, false),
            io:format("    C --> C1[✓ P99: ~p ns]~n", [P99]),
            case UHFTCompliant of
                true -> io:format("    C1 --> C2[✓ UHFT Compliant]~n");
                false -> io:format("    C1 --> C2[✗ Not UHFT]~n")
            end;
        failed -> io:format("    C --> C1[✗ FAILED]~n")
    end,
    
    case maps:get(status, ThroughputResult) of
        passed ->
            Throughput = maps:get(throughput_msgs_sec, ThroughputResult, 0),
            UHFTCapable = maps:get(uhft_capable, ThroughputResult, false),
            io:format("    D --> D1[✓ ~.0f msgs/sec]~n", [Throughput]),
            case UHFTCapable of
                true -> io:format("    D1 --> D2[✓ UHFT Capable]~n");
                false -> io:format("    D1 --> D2[✗ Not UHFT]~n")
            end;
        failed -> io:format("    D --> D1[✗ FAILED]~n")
    end,
    
    case maps:get(status, StressResult) of
        passed ->
            Actors = maps:get(successful_actors, StressResult, 0),
            SuccessRate = maps:get(success_rate, StressResult, 0),
            io:format("    E --> E1[✓ ~p actors]~n", [Actors]),
            io:format("    E1 --> E2[~.1f% success]~n", [SuccessRate * 100]);
        failed -> io:format("    E --> E1[✗ FAILED]~n")
    end,
    
    io:format("```~n").

percentile([], _) -> 0;
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = round((Percentile / 100) * Len),
    ClampedIndex = max(1, min(Index, Len)),
    lists:nth(ClampedIndex, SortedList).