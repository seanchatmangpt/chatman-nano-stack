#!/usr/bin/env escript

main(_Args) ->
    code:add_pathz("_build/default/lib/bitactor/ebin"),
    
    io:format("Starting BitActor application...~n"),
    case application:ensure_all_started(bitactor) of
        {ok, Apps} ->
            io:format("Applications started: ~p~n", [Apps]),
            
            io:format("Running stress tests...~n"),
            case catch stress_runner:run_all_tests() of
                Results when is_map(Results) ->
                    io:format("=== STRESS TEST RESULTS ===~n"),
                    print_results(Results),
                    halt(0);
                Error ->
                    io:format("Test execution error: ~p~n", [Error]),
                    halt(1)
            end;
        Error ->
            io:format("Failed to start application: ~p~n", [Error]),
            halt(1)
    end.

print_results(Results) ->
    maps:fold(fun(TestName, TestResult, _) ->
        Status = maps:get(status, TestResult, unknown),
        TestType = maps:get(test_type, TestResult, unknown),
        
        io:format("Test: ~p (~p) - Status: ~p~n", [TestName, TestType, Status]),
        
        case Status of
            passed ->
                case TestType of
                    latency_validation ->
                        P99 = maps:get(p99_latency_ns, TestResult, 0),
                        SubMicro = maps:get(sub_microsecond, TestResult, false),
                        UHFTCompliant = maps:get(uhft_compliant, TestResult, false),
                        io:format("  P99 Latency: ~p ns, Sub-microsecond: ~p, UHFT: ~p~n", [P99, SubMicro, UHFTCompliant]);
                    throughput_validation ->
                        Throughput = maps:get(throughput_msgs_sec, TestResult, 0),
                        TargetMet = maps:get(target_met, TestResult, false),
                        UHFTCapable = maps:get(uhft_capable, TestResult, false),
                        io:format("  Throughput: ~.2f msgs/sec, Target: ~p, UHFT: ~p~n", [Throughput, TargetMet, UHFTCapable]);
                    actor_stress ->
                        Actors = maps:get(successful_actors, TestResult, 0),
                        SuccessRate = maps:get(success_rate, TestResult, 0),
                        TargetMet = maps:get(target_met, TestResult, false),
                        io:format("  Actors: ~p, Success Rate: ~.2f%, Target: ~p~n", [Actors, SuccessRate * 100, TargetMet]);
                    basic_functionality ->
                        Duration = maps:get(duration_ms, TestResult, 0),
                        ActorsCreated = maps:get(actors_created, TestResult, 0),
                        io:format("  Duration: ~p ms, Actors Created: ~p~n", [Duration, ActorsCreated]);
                    _ ->
                        io:format("  Test completed successfully~n")
                end;
            failed ->
                Error = maps:get(error, TestResult, unknown_error),
                io:format("  ERROR: ~p~n", [Error])
        end
    end, ok, Results).