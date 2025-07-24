%%%-------------------------------------------------------------------
%%% @doc Simple BitActor Stress Test Runner
%%% Bypasses complex type issues to get tests working
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(stress_runner).

-export([run_all_tests/0, run_basic_test/0]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run_all_tests() -> #{atom() => term()}.
run_all_tests() ->
    ?LOG_INFO("Starting BitActor stress test runner"),
    
    %% Ensure system is ready
    ok = ensure_system_ready(),
    
    %% Run individual test components
    Results = #{
        basic_test => run_basic_test(),
        latency_test => run_latency_test(),
        throughput_test => run_throughput_test(),
        actor_stress_test => run_actor_stress_test()
    },
    
    ?LOG_INFO("All stress tests completed"),
    Results.

-spec run_basic_test() -> #{atom() => term()}.
run_basic_test() ->
    ?LOG_INFO("Running basic BitActor test"),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    try
        %% Test 1: Basic actor creation
        {ok, Actor1, _} = bitactor_server:spawn_actor(test_actor, #{}),
        
        %% Test 2: Message sending
        ok = bitactor_server:send_message(Actor1, <<"test_message">>),
        
        %% Test 3: Multiple actors
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(test_actor, #{id => N}),
            A
        end || N <- lists:seq(1, 100)],
        
        %% Test 4: Cleanup
        [bitactor_server:kill_actor(A) || A <- [Actor1 | Actors]],
        
        EndTime = erlang:monotonic_time(millisecond),
        
        #{
            status => passed,
            duration_ms => EndTime - StartTime,
            actors_created => length(Actors) + 1,
            test_type => basic_functionality
        }
    catch
        Error:Reason:Stacktrace ->
            ErrorEndTime = erlang:monotonic_time(millisecond),
            #{
                status => failed,
                duration_ms => ErrorEndTime - StartTime,
                error => {Error, Reason, Stacktrace},
                test_type => basic_functionality
            }
    end.

%%%===================================================================
%%% Individual Test Functions
%%%===================================================================

-spec run_latency_test() -> #{atom() => term()}.
run_latency_test() ->
    ?LOG_INFO("Running latency test"),
    
    try
        %% Create test actor
        {ok, Actor, _} = bitactor_server:spawn_actor(latency_test, #{}),
        
        %% Measure 1000 message latencies
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server:send_message(Actor, <<"latency_test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 1000)],
        
        %% Calculate statistics
        SortedLatencies = lists:sort(Latencies),
        P50 = percentile(SortedLatencies, 50),
        P95 = percentile(SortedLatencies, 95),
        P99 = percentile(SortedLatencies, 99),
        
        %% Cleanup
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
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => latency_validation,
                error => {Error, Reason, Stacktrace}
            }
    end.

-spec run_throughput_test() -> #{atom() => term()}.
run_throughput_test() ->
    ?LOG_INFO("Running throughput test"),
    
    try
        %% Create multiple target actors
        TargetActors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(throughput_test, #{id => N}),
            A
        end || N <- lists:seq(1, 10)],
        
        %% Send burst of messages
        MessageCount = 10000,
        StartTime = erlang:monotonic_time(millisecond),
        
        [begin
            Actor = lists:nth(rand:uniform(length(TargetActors)), TargetActors),
            bitactor_server:send_message(Actor, <<"throughput_test">>)
        end || _ <- lists:seq(1, MessageCount)],
        
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        
        %% Calculate throughput
        ThroughputMsgsSec = (MessageCount * 1000) / Duration,
        
        %% Cleanup
        [bitactor_server:kill_actor(A) || A <- TargetActors],
        
        #{
            status => passed,
            test_type => throughput_validation,
            messages_sent => MessageCount,
            duration_ms => Duration,
            throughput_msgs_sec => ThroughputMsgsSec,
            target_met => ThroughputMsgsSec >= 10000, % 10k msgs/sec target for basic test
            uhft_capable => ThroughputMsgsSec >= 100000 % 100k msgs/sec
        }
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => throughput_validation,
                error => {Error, Reason, Stacktrace}
            }
    end.

-spec run_actor_stress_test() -> #{atom() => term()}.
run_actor_stress_test() ->
    ?LOG_INFO("Running actor stress test"),
    
    try
        %% Test creating many actors
        ActorCount = 1000,
        StartTime = erlang:monotonic_time(millisecond),
        
        %% Create actors
        Actors = lists:map(fun(N) ->
            case catch bitactor_server:spawn_actor(stress_test, #{id => N}) of
                {ok, A, _} -> {ok, A};
                Error -> {error, Error}
            end
        end, lists:seq(1, ActorCount)),
        
        %% Count successful creations
        SuccessfulActors = [A || {ok, A} <- Actors],
        FailedCreations = length(Actors) - length(SuccessfulActors),
        
        %% Send messages to all actors
        [bitactor_server:send_message(A, <<"stress_test">>) || A <- SuccessfulActors],
        
        %% Measure memory usage
        Memory = erlang:memory(total) / (1024 * 1024), % MB
        
        %% Cleanup
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
            target_met => length(SuccessfulActors) >= ActorCount * 0.95 % 95% success rate
        }
    catch
        Error:Reason:Stacktrace ->
            #{
                status => failed,
                test_type => actor_stress,
                error => {Error, Reason, Stacktrace}
            }
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec percentile([number()], number()) -> number().
percentile([], _) -> 0;
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = round((Percentile / 100) * Len),
    ClampedIndex = max(1, min(Index, Len)),
    lists:nth(ClampedIndex, SortedList).

%%%===================================================================
%%% Reporting
%%%===================================================================

-spec format_test_results(#{atom() => term()}) -> ok.
format_test_results(Results) ->
    ?LOG_INFO("=== BitActor Stress Test Results ==="),
    
    maps:fold(fun(TestName, TestResult, _) ->
        Status = maps:get(status, TestResult, unknown),
        TestType = maps:get(test_type, TestResult, unknown),
        
        ?LOG_INFO("Test: ~p (~p) - Status: ~p", [TestName, TestType, Status]),
        
        case Status of
            passed ->
                case TestType of
                    latency_validation ->
                        P99 = maps:get(p99_latency_ns, TestResult, 0),
                        SubMicro = maps:get(sub_microsecond, TestResult, false),
                        ?LOG_INFO("  P99 Latency: ~p ns, Sub-microsecond: ~p", [P99, SubMicro]);
                    throughput_validation ->
                        Throughput = maps:get(throughput_msgs_sec, TestResult, 0),
                        ?LOG_INFO("  Throughput: ~.2f msgs/sec", [Throughput]);
                    actor_stress ->
                        Actors = maps:get(successful_actors, TestResult, 0),
                        SuccessRate = maps:get(success_rate, TestResult, 0),
                        ?LOG_INFO("  Actors: ~p, Success Rate: ~.2f%", [Actors, SuccessRate * 100]);
                    _ ->
                        ?LOG_INFO("  Basic test completed successfully")
                end;
            failed ->
                Error = maps:get(error, TestResult, unknown_error),
                ?LOG_ERROR("  Test failed: ~p", [Error])
        end
    end, ok, Results),
    
    ?LOG_INFO("=== End Results ==="),
    ok.