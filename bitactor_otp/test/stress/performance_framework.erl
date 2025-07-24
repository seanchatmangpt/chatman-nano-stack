%%%-------------------------------------------------------------------
%%% @doc BitActor Performance Validation Framework
%%% Ultra-precise stress testing for UHFT systems
%%% Agent 1: Performance Test Orchestrator
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(performance_framework).

-export([
    start/0, start/1, stop/0,
    run_test_suite/0, run_test_suite/1,
    validate_performance/2,
    get_metrics/0, reset_metrics/0
]).

-export([
    register_test/2,
    run_test/1,
    collect_results/1,
    generate_report/1
]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_DURATION_MS, 60000).  % 1 minute default
-define(UHFT_LATENCY_TARGET_NS, 1000). % 1 microsecond
-define(UHFT_THROUGHPUT_TARGET, 1000000). % 1M msgs/sec

-record(test_config, {
    name :: atom(),
    description :: string(),
    duration_ms = ?DEFAULT_DURATION_MS :: pos_integer(),
    target_latency_ns = ?UHFT_LATENCY_TARGET_NS :: pos_integer(),
    target_throughput = ?UHFT_THROUGHPUT_TARGET :: pos_integer(),
    success_criteria :: #{atom() => term()},
    timeout_ms :: pos_integer(),
    parallel_workers = 1 :: pos_integer()
}).

-record(test_result, {
    test_name :: atom(),
    status :: passed | failed | error,
    start_time :: integer(),
    end_time :: integer(),
    duration_ms :: integer(),
    metrics :: #{atom() => term()},
    errors = [] :: [term()],
    performance_grade :: atom() % excellent | good | acceptable | poor
}).

-record(performance_metrics, {
    latency_p50_ns :: integer(),
    latency_p95_ns :: integer(),
    latency_p99_ns :: integer(),
    latency_p999_ns :: integer(),
    latency_max_ns :: integer(),
    throughput_msgs_sec :: float(),
    error_rate :: float(),
    memory_peak_mb :: integer(),
    cpu_usage_percent :: float(),
    gc_count :: integer(),
    gc_time_ms :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start(#{}).

-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn_link(fun() -> init_framework(Config) end),
            register(?MODULE, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

-spec stop() -> ok.
stop() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            unregister(?MODULE),
            ok
    end.

-spec run_test_suite() -> {ok, [test_result()]}.
run_test_suite() ->
    run_test_suite(#{}).

-spec run_test_suite(map()) -> {ok, [test_result()]}.
run_test_suite(Options) ->
    ?LOG_INFO("Starting BitActor UHFT stress test suite"),
    
    %% Ensure framework is started
    {ok, _} = start(),
    
    %% Reset metrics
    ok = reset_metrics(),
    
    %% Define test suite
    Tests = [
        #test_config{
            name = latency_validation,
            description = "Sub-microsecond latency validation",
            duration_ms = 30000,
            target_latency_ns = 1000,
            success_criteria = #{
                p99_latency_ns => {less_than, 1000},
                p999_latency_ns => {less_than, 2000}
            }
        },
        #test_config{
            name = throughput_validation,
            description = "1M+ messages/second throughput",
            duration_ms = 60000,
            target_throughput = 1000000,
            success_criteria = #{
                throughput_msgs_sec => {greater_than, 1000000},
                error_rate => {less_than, 0.001}
            }
        },
        #test_config{
            name = concurrent_actors,
            description = "10k concurrent actors stress test",
            duration_ms = 120000,
            success_criteria = #{
                actor_count => {greater_than, 10000},
                memory_per_actor_kb => {less_than, 100}
            }
        },
        #test_config{
            name = sustained_load,
            description = "24-hour simulation (compressed to 10min)",
            duration_ms = 600000,
            success_criteria = #{
                uptime_percent => {greater_than, 99.99},
                performance_degradation => {less_than, 0.05}
            }
        },
        #test_config{
            name = chaos_resilience,
            description = "Chaos engineering validation",
            duration_ms = 180000,
            success_criteria = #{
                recovery_time_ms => {less_than, 1000},
                data_consistency => {equals, true}
            }
        }
    ],
    
    %% Run tests in parallel for faster execution
    Results = run_tests_parallel(Tests, Options),
    
    %% Generate comprehensive report
    Report = generate_comprehensive_report(Results),
    
    %% Log summary
    log_test_summary(Results),
    
    {ok, Results, Report}.

%%%===================================================================
%%% Test Execution Engine
%%%===================================================================

-spec run_tests_parallel([test_config()], map()) -> [test_result()].
run_tests_parallel(Tests, Options) ->
    MaxParallel = maps:get(max_parallel, Options, 3),
    
    %% Create test workers
    Workers = lists:map(fun(Test) ->
        spawn_monitor(fun() -> run_single_test(Test) end)
    end, Tests),
    
    %% Collect results with timeout
    collect_worker_results(Workers, []).

-spec run_single_test(test_config()) -> test_result().
run_single_test(Config) ->
    ?LOG_INFO("Starting test: ~p", [Config#test_config.name]),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    try
        %% Execute the test
        Result = execute_test(Config),
        
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        
        %% Validate results against criteria
        Status = validate_test_result(Result, Config#test_config.success_criteria),
        Grade = calculate_performance_grade(Result, Config),
        
        #test_result{
            test_name = Config#test_config.name,
            status = Status,
            start_time = StartTime,
            end_time = EndTime,
            duration_ms = Duration,
            metrics = Result,
            performance_grade = Grade
        }
    catch
        Error:Reason:Stacktrace ->
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,
            
            #test_result{
                test_name = Config#test_config.name,
                status = error,
                start_time = StartTime,
                end_time = EndTime,
                duration_ms = Duration,
                errors = [{Error, Reason, Stacktrace}],
                performance_grade = poor
            }
    end.

-spec execute_test(test_config()) -> #{atom() => term()}.
execute_test(#test_config{name = latency_validation} = Config) ->
    latency_test_engine:run(Config);
execute_test(#test_config{name = throughput_validation} = Config) ->
    throughput_test_engine:run(Config);
execute_test(#test_config{name = concurrent_actors} = Config) ->
    actor_stress_engine:run(Config);
execute_test(#test_config{name = sustained_load} = Config) ->
    sustained_load_engine:run(Config);
execute_test(#test_config{name = chaos_resilience} = Config) ->
    chaos_test_engine:run(Config);
execute_test(Config) ->
    ?LOG_WARNING("Unknown test type: ~p", [Config#test_config.name]),
    #{error => unknown_test_type}.

%%%===================================================================
%%% Result Validation
%%%===================================================================

-spec validate_test_result(map(), #{atom() => term()}) -> passed | failed.
validate_test_result(Result, Criteria) ->
    Validations = maps:fold(fun(Key, {Operator, Expected}, Acc) ->
        Actual = maps:get(Key, Result, undefined),
        Valid = validate_criterion(Actual, Operator, Expected),
        [{Key, Valid, Actual, Expected} | Acc]
    end, [], Criteria),
    
    case lists:all(fun({_, Valid, _, _}) -> Valid end, Validations) of
        true -> passed;
        false ->
            ?LOG_WARNING("Test failed validation: ~p", [Validations]),
            failed
    end.

-spec validate_criterion(term(), atom(), term()) -> boolean().
validate_criterion(Actual, less_than, Expected) when is_number(Actual), is_number(Expected) ->
    Actual < Expected;
validate_criterion(Actual, greater_than, Expected) when is_number(Actual), is_number(Expected) ->
    Actual > Expected;
validate_criterion(Actual, equals, Expected) ->
    Actual =:= Expected;
validate_criterion(_, _, _) ->
    false.

-spec calculate_performance_grade(map(), test_config()) -> atom().
calculate_performance_grade(Result, Config) ->
    LatencyScore = case maps:get(p99_latency_ns, Result, undefined) of
        undefined -> 0;
        Latency when Latency < Config#test_config.target_latency_ns * 0.5 -> 10;
        Latency when Latency < Config#test_config.target_latency_ns * 0.8 -> 8;
        Latency when Latency < Config#test_config.target_latency_ns -> 6;
        Latency when Latency < Config#test_config.target_latency_ns * 2 -> 4;
        _ -> 0
    end,
    
    ThroughputScore = case maps:get(throughput_msgs_sec, Result, undefined) of
        undefined -> 0;
        Throughput when Throughput > Config#test_config.target_throughput * 1.5 -> 10;
        Throughput when Throughput > Config#test_config.target_throughput * 1.2 -> 8;
        Throughput when Throughput > Config#test_config.target_throughput -> 6;
        Throughput when Throughput > Config#test_config.target_throughput * 0.8 -> 4;
        _ -> 0
    end,
    
    ErrorScore = case maps:get(error_rate, Result, 0) of
        ErrorRate when ErrorRate < 0.001 -> 10;
        ErrorRate when ErrorRate < 0.01 -> 6;
        ErrorRate when ErrorRate < 0.1 -> 3;
        _ -> 0
    end,
    
    OverallScore = (LatencyScore + ThroughputScore + ErrorScore) / 3,
    
    case OverallScore of
        Score when Score >= 9 -> excellent;
        Score when Score >= 7 -> good;
        Score when Score >= 5 -> acceptable;
        _ -> poor
    end.

%%%===================================================================
%%% Metrics Collection
%%%===================================================================

-spec get_metrics() -> #{atom() => term()}.
get_metrics() ->
    case whereis(?MODULE) of
        undefined -> #{};
        Pid ->
            Pid ! {get_metrics, self()},
            receive
                {metrics, Metrics} -> Metrics
            after 5000 -> #{}
            end
    end.

-spec reset_metrics() -> ok.
reset_metrics() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid ->
            Pid ! reset_metrics,
            ok
    end.

%%%===================================================================
%%% Report Generation
%%%===================================================================

-spec generate_comprehensive_report([test_result()]) -> map().
generate_comprehensive_report(Results) ->
    PassedTests = length([R || R <- Results, R#test_result.status =:= passed]),
    TotalTests = length(Results),
    OverallSuccess = PassedTests =:= TotalTests,
    
    PerformanceGrades = [R#test_result.performance_grade || R <- Results],
    AverageGrade = calculate_average_grade(PerformanceGrades),
    
    #{
        summary => #{
            total_tests => TotalTests,
            passed_tests => PassedTests,
            success_rate => PassedTests / TotalTests,
            overall_success => OverallSuccess,
            average_grade => AverageGrade
        },
        test_results => Results,
        recommendations => generate_recommendations(Results),
        timestamp => erlang:system_time(second)
    }.

-spec generate_recommendations([test_result()]) -> [string()].
generate_recommendations(Results) ->
    Recommendations = [],
    
    %% Check for latency issues
    LatencyRecs = case [R || R <- Results, 
                       R#test_result.test_name =:= latency_validation,
                       R#test_result.status =/= passed] of
        [] -> [];
        _ -> ["Optimize critical path for sub-microsecond latency",
              "Consider CPU affinity and NUMA optimization",
              "Review GC settings and memory allocation patterns"]
    end,
    
    %% Check for throughput issues
    ThroughputRecs = case [R || R <- Results,
                          R#test_result.test_name =:= throughput_validation,
                          R#test_result.status =/= passed] of
        [] -> [];
        _ -> ["Scale horizontally with more actor instances",
              "Optimize message serialization",
              "Consider lock-free data structures"]
    end,
    
    %% Check for memory issues
    MemoryRecs = case [R || R <- Results,
                       maps:get(memory_per_actor_kb, R#test_result.metrics, 0) > 100] of
        [] -> [];
        _ -> ["Optimize actor memory footprint",
              "Implement more aggressive GC tuning",
              "Review data structure choices"]
    end,
    
    Recommendations ++ LatencyRecs ++ ThroughputRecs ++ MemoryRecs.

%%%===================================================================
%%% Private Functions
%%%===================================================================

init_framework(Config) ->
    ?LOG_INFO("Initializing BitActor performance framework"),
    
    %% Initialize metrics storage
    ets:new(performance_metrics, [named_table, public, set]),
    
    %% Start metric collection
    spawn_link(fun() -> metrics_collector_loop() end),
    
    framework_loop(Config).

framework_loop(Config) ->
    receive
        {get_metrics, From} ->
            Metrics = collect_current_metrics(),
            From ! {metrics, Metrics},
            framework_loop(Config);
        reset_metrics ->
            ets:delete_all_objects(performance_metrics),
            framework_loop(Config);
        stop ->
            ok;
        _ ->
            framework_loop(Config)
    end.

metrics_collector_loop() ->
    timer:sleep(1000), % Collect every second
    
    %% Collect system metrics
    Metrics = #{
        memory_total => erlang:memory(total),
        memory_processes => erlang:memory(processes),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        timestamp => erlang:monotonic_time(millisecond)
    },
    
    ets:insert(performance_metrics, {erlang:monotonic_time(), Metrics}),
    metrics_collector_loop().

collect_current_metrics() ->
    case ets:last(performance_metrics) of
        '$end_of_table' -> #{};
        Key ->
            [{_, Metrics}] = ets:lookup(performance_metrics, Key),
            Metrics
    end.

collect_worker_results([], Acc) ->
    lists:reverse(Acc);
collect_worker_results([{Pid, Ref} | Workers], Acc) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            %% Worker completed successfully, get result
            receive
                Result when is_record(Result, test_result) ->
                    collect_worker_results(Workers, [Result | Acc])
            after 1000 ->
                collect_worker_results(Workers, Acc)
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            %% Worker failed
            ErrorResult = #test_result{
                test_name = unknown,
                status = error,
                errors = [Reason]
            },
            collect_worker_results(Workers, [ErrorResult | Acc])
    after 300000 -> % 5 minute timeout per test
        %% Kill remaining workers
        [exit(P, kill) || {P, _} <- Workers],
        lists:reverse(Acc)
    end.

calculate_average_grade(Grades) ->
    GradeValues = #{excellent => 4, good => 3, acceptable => 2, poor => 1},
    Values = [maps:get(Grade, GradeValues, 1) || Grade <- Grades],
    AvgValue = lists:sum(Values) / length(Values),
    
    case AvgValue of
        Avg when Avg >= 3.5 -> excellent;
        Avg when Avg >= 2.5 -> good;
        Avg when Avg >= 1.5 -> acceptable;
        _ -> poor
    end.

log_test_summary(Results) ->
    Passed = length([R || R <- Results, R#test_result.status =:= passed]),
    Total = length(Results),
    
    ?LOG_INFO("Test Suite Summary: ~p/~p tests passed", [Passed, Total]),
    
    lists:foreach(fun(Result) ->
        Status = case Result#test_result.status of
            passed -> "✓ PASSED";
            failed -> "✗ FAILED";
            error -> "⚠ ERROR"
        end,
        ?LOG_INFO("  ~s: ~s (~p grade)", [
            Result#test_result.test_name,
            Status,
            Result#test_result.performance_grade
        ])
    end, Results).

%%%===================================================================
%%% Validation API
%%%===================================================================

-spec validate_performance(atom(), map()) -> {passed | failed, map()}.
validate_performance(TestType, Metrics) ->
    Criteria = get_test_criteria(TestType),
    Status = validate_test_result(Metrics, Criteria),
    {Status, Criteria}.

get_test_criteria(uhft_latency) ->
    #{
        p99_latency_ns => {less_than, 1000},
        p999_latency_ns => {less_than, 2000}
    };
get_test_criteria(uhft_throughput) ->
    #{
        throughput_msgs_sec => {greater_than, 1000000},
        error_rate => {less_than, 0.001}
    };
get_test_criteria(memory_efficiency) ->
    #{
        memory_per_actor_kb => {less_than, 100},
        gc_impact_percent => {less_than, 5}
    };
get_test_criteria(_) ->
    #{}.

%%%===================================================================
%%% Test Registration
%%%===================================================================

-spec register_test(atom(), test_config()) -> ok.
register_test(Name, Config) ->
    ets:insert(performance_metrics, {test_config, Name, Config}),
    ok.

-spec run_test(atom()) -> test_result().
run_test(TestName) ->
    case ets:lookup(performance_metrics, {test_config, TestName}) of
        [{_, _, Config}] ->
            run_single_test(Config);
        [] ->
            #test_result{
                test_name = TestName,
                status = error,
                errors = [test_not_found]
            }
    end.