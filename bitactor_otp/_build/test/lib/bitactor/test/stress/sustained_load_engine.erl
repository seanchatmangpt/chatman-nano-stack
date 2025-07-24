%%%-------------------------------------------------------------------
%%% @doc BitActor Sustained Load Test Engine
%%% Agent 6: Long-term stability and performance degradation testing
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(sustained_load_engine).

-export([run/1]).
-export([monitor_performance_degradation/2, detect_memory_leaks/1, validate_stability/2]).

-include_lib("kernel/include/logger.hrl").

-define(SUSTAINED_DURATION_MS, 600000). % 10 minutes (24-hour simulation compressed)
-define(PERFORMANCE_SAMPLE_INTERVAL_MS, 5000). % 5 second intervals
-define(MEMORY_LEAK_THRESHOLD_MB, 100). % 100MB growth considered a leak
-define(DEGRADATION_THRESHOLD_PERCENT, 5). % 5% degradation threshold
-define(STABILITY_TARGET_UPTIME, 99.99). % 99.99% uptime requirement

-record(sustained_config, {
    duration_ms = ?SUSTAINED_DURATION_MS :: pos_integer(),
    baseline_load_actors = 5000 :: pos_integer(),
    load_variation_percent = 20 :: pos_integer(), % ±20% load variation
    sample_interval_ms = ?PERFORMANCE_SAMPLE_INTERVAL_MS :: pos_integer(),
    memory_leak_threshold_mb = ?MEMORY_LEAK_THRESHOLD_MB :: float(),
    degradation_threshold_percent = ?DEGRADATION_THRESHOLD_PERCENT :: float(),
    target_uptime_percent = ?STABILITY_TARGET_UPTIME :: float()
}).

-record(sustained_result, {
    test_duration_ms :: non_neg_integer(),
    baseline_performance :: #{atom() => term()},
    final_performance :: #{atom() => term()},
    performance_samples :: [#{atom() => term()}],
    uptime_percent :: float(),
    memory_leak_detected :: boolean(),
    memory_growth_mb :: float(),
    performance_degradation_percent :: float(),
    stability_violations :: non_neg_integer(),
    system_recoveries :: non_neg_integer(),
    consistency_maintained :: boolean()
}).

-record(performance_sample, {
    timestamp :: integer(),
    latency_p99_ns :: non_neg_integer(),
    throughput_msgs_sec :: float(),
    memory_total_mb :: float(),
    memory_processes_mb :: float(),
    actor_count :: non_neg_integer(),
    gc_count :: non_neg_integer(),
    gc_time_ms :: non_neg_integer(),
    process_count :: non_neg_integer(),
    run_queue_length :: non_neg_integer(),
    system_health :: atom()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run(map()) -> #{atom() => term()}.
run(Config) ->
    ?LOG_INFO("Starting sustained load test engine"),
    
    %% Ensure system is ready
    ok = ensure_system_ready(),
    
    %% Configure sustained test
    SustainedConfig = configure_sustained_test(Config),
    
    %% Execute sustained load test
    Results = execute_sustained_load_test(SustainedConfig),
    
    %% Format results
    format_sustained_results(Results, SustainedConfig).

%%%===================================================================
%%% Core Sustained Testing
%%%===================================================================

-spec execute_sustained_load_test(sustained_config()) -> sustained_result().
execute_sustained_load_test(Config) ->
    ?LOG_INFO("Executing sustained load test for ~p ms with ~p baseline actors", 
             [Config#sustained_config.duration_ms, Config#sustained_config.baseline_load_actors]),
    
    %% Initialize tracking
    ets:new(sustained_tracking, [named_table, public, ordered_set]),
    ets:insert(sustained_tracking, {config, Config}),
    
    %% Establish baseline
    {BaselineActors, BaselinePerformance} = establish_sustained_baseline(Config),
    
    %% Start monitoring processes
    MonitorPid = spawn_link(fun() -> sustained_monitor_loop(Config) end),
    LoadManagerPid = spawn_link(fun() -> load_manager_loop(BaselineActors, Config) end),
    
    %% Execute sustained load
    TestStartTime = erlang:monotonic_time(millisecond),
    run_sustained_load(Config, TestStartTime),
    TestEndTime = erlang:monotonic_time(millisecond),
    
    %% Stop monitoring
    exit(MonitorPid, shutdown),
    exit(LoadManagerPid, shutdown),
    
    %% Measure final performance
    FinalPerformance = measure_current_performance(BaselineActors),
    
    %% Collect and analyze results
    PerformanceSamples = collect_performance_samples(),
    SustainedResult = analyze_sustained_results(TestStartTime, TestEndTime, 
                                              BaselinePerformance, FinalPerformance, 
                                              PerformanceSamples, Config),
    
    %% Cleanup
    cleanup_sustained_test(BaselineActors),
    
    SustainedResult.

-spec establish_sustained_baseline(sustained_config()) -> {[term()], #{atom() => term()}}.
establish_sustained_baseline(Config) ->
    ?LOG_INFO("Establishing sustained load baseline"),
    
    %% Spawn baseline actor load
    BaselineActors = spawn_sustained_actors(Config#sustained_config.baseline_load_actors),
    
    %% Let system stabilize and warm up
    warmup_sustained_system(BaselineActors),
    
    %% Measure baseline performance
    BaselinePerformance = measure_current_performance(BaselineActors),
    
    ?LOG_INFO("Baseline established: ~p actors, ~.2f p99 latency ns", 
             [length(BaselineActors), maps:get(latency_p99_ns, BaselinePerformance)]),
    
    {BaselineActors, BaselinePerformance}.

-spec spawn_sustained_actors(pos_integer()) -> [term()].
spawn_sustained_actors(Count) ->
    ?LOG_DEBUG("Spawning ~p sustained load actors", [Count]),
    
    %% Spawn different types of actors for realistic load
    ActorTypes = [sustained_worker, sustained_coordinator, sustained_storage, sustained_monitor],
    
    lists:filtermap(fun(N) ->
        ActorType = lists:nth((N rem length(ActorTypes)) + 1, ActorTypes),
        
        InitState = #{
            id => N,
            type => ActorType,
            startup_time => erlang:monotonic_time(nanosecond),
            message_count => 0,
            last_activity => erlang:monotonic_time(millisecond),
            load_level => baseline
        },
        
        case catch bitactor_server:spawn_actor(ActorType, InitState) of
            {ok, ActorRef, _} -> {true, ActorRef};
            Error -> 
                ?LOG_WARNING("Failed to spawn sustained actor ~p: ~p", [N, Error]),
                false
        end
    end, lists:seq(1, Count)).

-spec warmup_sustained_system([term()]) -> ok.
warmup_sustained_system(Actors) ->
    ?LOG_DEBUG("Warming up sustained system with ~p actors", [length(Actors)]),
    
    %% Send warmup messages to all actors
    WarmupMessage = <<"sustained_warmup">>,
    [catch bitactor_server:send_message(A, WarmupMessage) || A <- Actors],
    
    %% Let system stabilize
    timer:sleep(5000),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    ok.

%%%===================================================================
%%% Load Management
%%%===================================================================

-spec run_sustained_load(sustained_config(), integer()) -> ok.
run_sustained_load(Config, StartTime) ->
    EndTime = StartTime + Config#sustained_config.duration_ms,
    ?LOG_INFO("Running sustained load until ~p", [EndTime]),
    
    sustained_load_loop(EndTime).

sustained_load_loop(EndTime) when erlang:monotonic_time(millisecond) >= EndTime ->
    ?LOG_INFO("Sustained load duration complete"),
    ok;
sustained_load_loop(EndTime) ->
    %% Continue running - monitoring and load management happen in separate processes
    timer:sleep(10000), % Check every 10 seconds
    sustained_load_loop(EndTime).

-spec load_manager_loop([term()], sustained_config()) -> ok.
load_manager_loop(BaselineActors, Config) ->
    %% Manage varying load levels to simulate real-world conditions
    LoadVariation = Config#sustained_config.load_variation_percent,
    
    %% Generate load pattern: baseline ± variation
    LoadMultiplier = 1.0 + (rand:uniform(LoadVariation * 2) - LoadVariation) / 100.0,
    
    %% Apply load
    apply_sustained_load(BaselineActors, LoadMultiplier),
    
    %% Wait before next load adjustment
    timer:sleep(30000), % Adjust load every 30 seconds
    
    load_manager_loop(BaselineActors, Config).

-spec apply_sustained_load([term()], float()) -> ok.
apply_sustained_load(Actors, LoadMultiplier) when length(Actors) < 10 ->
    ok; % Too few actors to apply meaningful load
apply_sustained_load(Actors, LoadMultiplier) ->
    %% Calculate message burst based on load multiplier
    BaseMessagesPerActor = 10,
    MessagesPerActor = round(BaseMessagesPerActor * LoadMultiplier),
    
    %% Send messages to random subset of actors
    ActorSubset = select_random_actors(Actors, min(1000, length(Actors) div 2)),
    
    %% Generate realistic workload
    [send_sustained_workload(Actor, MessagesPerActor) || Actor <- ActorSubset],
    
    ok.

-spec select_random_actors([term()], pos_integer()) -> [term()].
select_random_actors(Actors, Count) when Count >= length(Actors) ->
    Actors;
select_random_actors(Actors, Count) ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), A} || A <- Actors])],
    lists:sublist(Shuffled, Count).

-spec send_sustained_workload(term(), pos_integer()) -> ok.
send_sustained_workload(_Actor, 0) -> ok;
send_sustained_workload(Actor, MessageCount) ->
    %% Generate different types of workload messages
    Message = case rand:uniform(4) of
        1 -> generate_compute_task();
        2 -> generate_storage_task();
        3 -> generate_coordination_task();
        _ -> generate_monitoring_task()
    end,
    
    catch bitactor_server:send_message(Actor, Message),
    send_sustained_workload(Actor, MessageCount - 1).

%%%===================================================================
%%% Performance Monitoring
%%%===================================================================

-spec sustained_monitor_loop(sustained_config()) -> ok.
sustained_monitor_loop(Config) ->
    Interval = Config#sustained_config.sample_interval_ms,
    sustained_monitor_loop_impl(Config, Interval).

sustained_monitor_loop_impl(Config, Interval) ->
    %% Collect performance sample
    Sample = collect_performance_sample(),
    
    %% Store sample with timestamp as key for ordering
    Timestamp = Sample#performance_sample.timestamp,
    ets:insert(sustained_tracking, {Timestamp, Sample}),
    
    %% Check for immediate issues
    check_system_health(Sample, Config),
    
    timer:sleep(Interval),
    sustained_monitor_loop_impl(Config, Interval).

-spec collect_performance_sample() -> performance_sample().
collect_performance_sample() ->
    %% System metrics
    Memory = erlang:memory(),
    {GCCount, GCTime, _} = erlang:statistics(garbage_collection),
    
    %% Measure current latency
    LatencyP99 = measure_current_latency(),
    
    %% Estimate throughput
    Throughput = estimate_current_throughput(),
    
    %% System health assessment
    Health = assess_system_health(),
    
    #performance_sample{
        timestamp = erlang:monotonic_time(millisecond),
        latency_p99_ns = LatencyP99,
        throughput_msgs_sec = Throughput,
        memory_total_mb = maps:get(total, Memory) / (1024 * 1024),
        memory_processes_mb = maps:get(processes, Memory) / (1024 * 1024),
        actor_count = get_current_actor_count(),
        gc_count = GCCount,
        gc_time_ms = GCTime,
        process_count = erlang:system_info(process_count),
        run_queue_length = erlang:statistics(run_queue),
        system_health = Health
    }.

-spec measure_current_latency() -> non_neg_integer().
measure_current_latency() ->
    %% Quick latency measurement using NIF
    case catch bitactor_nif:measure_latency() of
        {ok, _Min, _Avg, Max} -> Max;
        _ -> 0 % Fallback
    end.

-spec estimate_current_throughput() -> float().
estimate_current_throughput() ->
    %% Estimate throughput from system statistics
    case catch bitactor_nif:get_stats() of
        {ok, _ActorCount, TotalMessages, _TotalTicks, _} ->
            %% Simple throughput estimation (messages per second)
            %% This is a rough estimate - real implementation would track deltas
            TotalMessages / 60.0; % Assume 1-minute average
        _ -> 0.0
    end.

-spec get_current_actor_count() -> non_neg_integer().
get_current_actor_count() ->
    case catch bitactor_nif:get_stats() of
        {ok, ActorCount, _, _, _} -> ActorCount;
        _ -> 0
    end.

-spec assess_system_health() -> atom().
assess_system_health() ->
    Memory = erlang:memory(total) / (1024 * 1024), % MB
    ProcessCount = erlang:system_info(process_count),
    RunQueue = erlang:statistics(run_queue),
    
    case {Memory < 2000, ProcessCount < 100000, RunQueue < 200} of
        {true, true, true} -> healthy;
        {false, _, _} -> memory_pressure;
        {_, false, _} -> process_overload;
        {_, _, false} -> scheduler_overload
    end.

-spec check_system_health(performance_sample(), sustained_config()) -> ok.
check_system_health(Sample, Config) ->
    %% Check for immediate stability issues
    case Sample#performance_sample.system_health of
        healthy -> ok;
        _ ->
            %% Log health issue
            ?LOG_WARNING("System health issue detected: ~p", [Sample#performance_sample.system_health]),
            
            %% Update stability violation counter
            increment_stability_violations(),
            
            %% Check if system needs recovery
            case severe_health_issue(Sample) of
                true -> trigger_system_recovery(Sample);
                false -> ok
            end
    end.

-spec severe_health_issue(performance_sample()) -> boolean().
severe_health_issue(Sample) ->
    Sample#performance_sample.memory_total_mb > 4000 orelse % > 4GB
    Sample#performance_sample.run_queue_length > 1000 orelse
    Sample#performance_sample.latency_p99_ns > 10000000. % > 10ms

-spec trigger_system_recovery(performance_sample()) -> ok.
trigger_system_recovery(Sample) ->
    ?LOG_WARNING("Triggering system recovery due to: ~p", [Sample#performance_sample.system_health]),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Brief pause to allow recovery
    timer:sleep(5000),
    
    %% Increment recovery counter
    increment_system_recoveries(),
    
    ok.

%%%===================================================================
%%% Analysis and Results
%%%===================================================================

-spec analyze_sustained_results(integer(), integer(), #{atom() => term()}, 
                               #{atom() => term()}, [performance_sample()], 
                               sustained_config()) -> sustained_result().
analyze_sustained_results(StartTime, EndTime, BaselinePerformance, FinalPerformance, 
                         Samples, Config) ->
    TestDuration = EndTime - StartTime,
    
    %% Analyze performance degradation
    DegradationPercent = calculate_performance_degradation(BaselinePerformance, FinalPerformance),
    
    %% Analyze memory growth
    {MemoryLeakDetected, MemoryGrowthMB} = analyze_memory_growth(Samples),
    
    %% Calculate uptime
    UptimePercent = calculate_uptime(Samples),
    
    %% Get violation counters
    StabilityViolations = get_stability_violations(),
    SystemRecoveries = get_system_recoveries(),
    
    %% Check consistency
    ConsistencyMaintained = validate_data_consistency(Samples),
    
    #sustained_result{
        test_duration_ms = TestDuration,
        baseline_performance = BaselinePerformance,
        final_performance = FinalPerformance,
        performance_samples = Samples,
        uptime_percent = UptimePercent,
        memory_leak_detected = MemoryLeakDetected,
        memory_growth_mb = MemoryGrowthMB,
        performance_degradation_percent = DegradationPercent,
        stability_violations = StabilityViolations,
        system_recoveries = SystemRecoveries,
        consistency_maintained = ConsistencyMaintained
    }.

-spec calculate_performance_degradation(#{atom() => term()}, #{atom() => term()}) -> float().
calculate_performance_degradation(Baseline, Final) ->
    BaselineLatency = maps:get(latency_p99_ns, Baseline, 1),
    FinalLatency = maps:get(latency_p99_ns, Final, 1),
    
    BaselineThroughput = maps:get(throughput_msgs_sec, Baseline, 1),
    FinalThroughput = maps:get(throughput_msgs_sec, Final, 1),
    
    %% Calculate degradation percentages
    LatencyDegradation = ((FinalLatency - BaselineLatency) / BaselineLatency) * 100,
    ThroughputDegradation = ((BaselineThroughput - FinalThroughput) / BaselineThroughput) * 100,
    
    %% Return worst degradation
    max(LatencyDegradation, ThroughputDegradation).

-spec analyze_memory_growth([performance_sample()]) -> {boolean(), float()}.
analyze_memory_growth(Samples) when length(Samples) < 2 ->
    {false, 0.0};
analyze_memory_growth(Samples) ->
    SortedSamples = lists:sort(fun(A, B) -> 
        A#performance_sample.timestamp =< B#performance_sample.timestamp 
    end, Samples),
    
    InitialMemory = (hd(SortedSamples))#performance_sample.memory_total_mb,
    FinalMemory = (lists:last(SortedSamples))#performance_sample.memory_total_mb,
    
    MemoryGrowth = FinalMemory - InitialMemory,
    LeakDetected = MemoryGrowth > ?MEMORY_LEAK_THRESHOLD_MB,
    
    {LeakDetected, MemoryGrowth}.

-spec calculate_uptime([performance_sample()]) -> float().
calculate_uptime(Samples) ->
    HealthySamples = length([S || S <- Samples, S#performance_sample.system_health =:= healthy]),
    TotalSamples = length(Samples),
    
    case TotalSamples of
        0 -> 0.0;
        _ -> (HealthySamples / TotalSamples) * 100
    end.

-spec validate_data_consistency([performance_sample()]) -> boolean().
validate_data_consistency(Samples) ->
    %% Simple consistency check: actor count should not drop dramatically
    ActorCounts = [S#performance_sample.actor_count || S <- Samples],
    
    case {ActorCounts, length(ActorCounts)} of
        {[], _} -> true;
        {_, N} when N < 2 -> true;
        _ ->
            MinCount = lists:min(ActorCounts),
            MaxCount = lists:max(ActorCounts),
            
            %% Consistency maintained if count doesn't drop more than 50%
            MinCount >= MaxCount * 0.5
    end.

%%%===================================================================
%%% Metrics Storage and Retrieval
%%%===================================================================

-spec collect_performance_samples() -> [performance_sample()].
collect_performance_samples() ->
    %% Extract all samples from ETS table
    AllEntries = ets:tab2list(sustained_tracking),
    Samples = [Sample || {Timestamp, Sample} <- AllEntries, 
                        is_integer(Timestamp), is_record(Sample, performance_sample)],
    
    %% Sort by timestamp
    lists:sort(fun(A, B) -> 
        A#performance_sample.timestamp =< B#performance_sample.timestamp 
    end, Samples).

-spec increment_stability_violations() -> ok.
increment_stability_violations() ->
    case ets:lookup(sustained_tracking, stability_violations) of
        [{stability_violations, Count}] ->
            ets:insert(sustained_tracking, {stability_violations, Count + 1});
        [] ->
            ets:insert(sustained_tracking, {stability_violations, 1})
    end,
    ok.

-spec increment_system_recoveries() -> ok.
increment_system_recoveries() ->
    case ets:lookup(sustained_tracking, system_recoveries) of
        [{system_recoveries, Count}] ->
            ets:insert(sustained_tracking, {system_recoveries, Count + 1});
        [] ->
            ets:insert(sustained_tracking, {system_recoveries, 1})
    end,
    ok.

-spec get_stability_violations() -> non_neg_integer().
get_stability_violations() ->
    case ets:lookup(sustained_tracking, stability_violations) of
        [{stability_violations, Count}] -> Count;
        [] -> 0
    end.

-spec get_system_recoveries() -> non_neg_integer().
get_system_recoveries() ->
    case ets:lookup(sustained_tracking, system_recoveries) of
        [{system_recoveries, Count}] -> Count;
        [] -> 0
    end.

%%%===================================================================
%%% Performance Measurement
%%%===================================================================

-spec measure_current_performance([term()]) -> #{atom() => term()}.
measure_current_performance(Actors) ->
    %% Measure latency
    LatencyMeasurements = measure_latency_sample(Actors),
    P99Latency = calculate_percentile(LatencyMeasurements, 99),
    
    %% Estimate throughput
    Throughput = estimate_current_throughput(),
    
    %% System metrics
    Memory = erlang:memory(),
    
    #{
        latency_p99_ns => P99Latency,
        throughput_msgs_sec => Throughput,
        memory_total_mb => maps:get(total, Memory) / (1024 * 1024),
        memory_processes_mb => maps:get(processes, Memory) / (1024 * 1024),
        actor_count => length(Actors),
        timestamp => erlang:monotonic_time(millisecond)
    }.

-spec measure_latency_sample([term()]) -> [non_neg_integer()].
measure_latency_sample(Actors) when length(Actors) < 10 ->
    [0]; % Fallback for small actor counts
measure_latency_sample(Actors) ->
    %% Sample 100 actors for latency measurement
    SampleActors = lists:sublist(Actors, 100),
    
    lists:map(fun(Actor) ->
        StartTime = erlang:monotonic_time(nanosecond),
        case catch bitactor_server:send_message(Actor, <<"latency_test">>) of
            ok ->
                EndTime = erlang:monotonic_time(nanosecond),
                EndTime - StartTime;
            _ ->
                5000000 % 5ms penalty for failed message
        end
    end, SampleActors).

-spec calculate_percentile([number()], number()) -> number().
calculate_percentile([], _) -> 0;
calculate_percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = round((Percentile / 100) * Len),
    ClampedIndex = max(1, min(Index, Len)),
    lists:nth(ClampedIndex, Sorted).

%%%===================================================================
%%% Workload Generation
%%%===================================================================

-spec generate_compute_task() -> binary().
generate_compute_task() ->
    TaskData = #{
        type => compute,
        operation => math_heavy,
        iterations => rand:uniform(1000),
        timestamp => erlang:monotonic_time(nanosecond)
    },
    term_to_binary(TaskData).

-spec generate_storage_task() -> binary().
generate_storage_task() ->
    TaskData = #{
        type => storage,
        operation => data_write,
        data => crypto:strong_rand_bytes(rand:uniform(1024)),
        timestamp => erlang:monotonic_time(nanosecond)
    },
    term_to_binary(TaskData).

-spec generate_coordination_task() -> binary().
generate_coordination_task() ->
    TaskData = #{
        type => coordination,
        operation => sync_request,
        participants => rand:uniform(10),
        timestamp => erlang:monotonic_time(nanosecond)
    },
    term_to_binary(TaskData).

-spec generate_monitoring_task() -> binary().
generate_monitoring_task() ->
    TaskData = #{
        type => monitoring,
        operation => health_check,
        metrics => [latency, throughput, memory],
        timestamp => erlang:monotonic_time(nanosecond)
    },
    term_to_binary(TaskData).

%%%===================================================================
%%% System Management
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec cleanup_sustained_test([term()]) -> ok.
cleanup_sustained_test(BaselineActors) ->
    ?LOG_INFO("Cleaning up sustained test with ~p actors", [length(BaselineActors)]),
    
    %% Gracefully shutdown actors
    [catch bitactor_server:kill_actor(A) || A <- BaselineActors],
    
    %% Cleanup tracking table
    ets:delete(sustained_tracking),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    ok.

%%%===================================================================
%%% Configuration and Formatting
%%%===================================================================

-spec configure_sustained_test(map()) -> sustained_config().
configure_sustained_test(Config) ->
    #sustained_config{
        duration_ms = maps:get(duration_ms, Config, ?SUSTAINED_DURATION_MS),
        baseline_load_actors = maps:get(baseline_load_actors, Config, 5000),
        load_variation_percent = maps:get(load_variation_percent, Config, 20),
        sample_interval_ms = maps:get(sample_interval_ms, Config, ?PERFORMANCE_SAMPLE_INTERVAL_MS),
        memory_leak_threshold_mb = maps:get(memory_leak_threshold_mb, Config, ?MEMORY_LEAK_THRESHOLD_MB),
        degradation_threshold_percent = maps:get(degradation_threshold_percent, Config, ?DEGRADATION_THRESHOLD_PERCENT),
        target_uptime_percent = maps:get(target_uptime_percent, Config, ?STABILITY_TARGET_UPTIME)
    }.

-spec format_sustained_results(sustained_result(), sustained_config()) -> #{atom() => term()}.
format_sustained_results(Result, Config) ->
    #{
        test_type => sustained_load,
        duration_ms => Result#sustained_result.test_duration_ms,
        target_uptime_percent => Config#sustained_config.target_uptime_percent,
        
        %% Performance metrics
        uptime_percent => Result#sustained_result.uptime_percent,
        performance_degradation => Result#sustained_result.performance_degradation_percent,
        
        %% Memory analysis
        memory_leak_detected => Result#sustained_result.memory_leak_detected,
        memory_growth_mb => Result#sustained_result.memory_growth_mb,
        
        %% Stability metrics
        stability_violations => Result#sustained_result.stability_violations,
        system_recoveries => Result#sustained_result.system_recoveries,
        consistency_maintained => Result#sustained_result.consistency_maintained,
        
        %% Performance comparison
        baseline_latency_ns => maps:get(latency_p99_ns, Result#sustained_result.baseline_performance, 0),
        final_latency_ns => maps:get(latency_p99_ns, Result#sustained_result.final_performance, 0),
        baseline_throughput => maps:get(throughput_msgs_sec, Result#sustained_result.baseline_performance, 0),
        final_throughput => maps:get(throughput_msgs_sec, Result#sustained_result.final_performance, 0),
        
        %% Validation flags
        target_met => Result#sustained_result.uptime_percent >= Config#sustained_config.target_uptime_percent,
        performance_stable => Result#sustained_result.performance_degradation_percent <= 
                             Config#sustained_config.degradation_threshold_percent,
        memory_stable => not Result#sustained_result.memory_leak_detected,
        uhft_stable => Result#sustained_result.uptime_percent >= 99.99 andalso
                      Result#sustained_result.performance_degradation_percent <= 1.0,
        
        %% Success criteria
        error_rate => (Result#sustained_result.stability_violations + 
                      Result#sustained_result.system_recoveries) / 
                     max(Result#sustained_result.test_duration_ms div 1000, 1), % Errors per second
        
        timestamp => erlang:system_time(second)
    }.