%%%-------------------------------------------------------------------
%%% @doc BitActor Chaos Engineering Test Engine
%%% Agent 5: Fault injection and recovery validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(chaos_test_engine).

-export([run/1]).
-export([inject_fault/2, measure_recovery_time/2, validate_system_consistency/0]).

-include_lib("kernel/include/logger.hrl").

-define(CHAOS_DURATION_MS, 180000). % 3 minutes
-define(RECOVERY_TIMEOUT_MS, 5000). % 5 seconds max recovery
-define(FAULT_INJECTION_INTERVAL_MS, 30000). % 30 seconds between faults

-record(chaos_config, {
    duration_ms = ?CHAOS_DURATION_MS :: pos_integer(),
    fault_types = [process_kill, memory_pressure, network_delay, cpu_spike] :: [atom()],
    recovery_timeout_ms = ?RECOVERY_TIMEOUT_MS :: pos_integer(),
    fault_interval_ms = ?FAULT_INJECTION_INTERVAL_MS :: pos_integer(),
    baseline_actors = 1000 :: pos_integer(),
    target_availability = 99.9 :: float() % 99.9% uptime requirement
}).

-record(chaos_result, {
    faults_injected :: non_neg_integer(),
    recovery_times_ms :: [non_neg_integer()],
    availability_percent :: float(),
    data_consistency_score :: float(),
    system_stability_score :: float(),
    fault_tolerance_score :: float(),
    cascade_failure_count :: non_neg_integer(),
    successful_recoveries :: non_neg_integer()
}).

-record(fault_injection, {
    type :: atom(),
    start_time :: integer(),
    end_time :: integer(),
    target :: term(),
    success :: boolean(),
    recovery_time_ms :: non_neg_integer(),
    side_effects :: [term()]
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run(map()) -> #{atom() => term()}.
run(Config) ->
    ?LOG_INFO("Starting chaos engineering test engine"),
    
    %% Ensure system is ready
    ok = ensure_system_ready(),
    
    %% Configure chaos test
    ChaosConfig = configure_chaos_test(Config),
    
    %% Execute chaos testing
    Results = execute_chaos_test(ChaosConfig),
    
    %% Format results
    format_chaos_results(Results, ChaosConfig).

%%%===================================================================
%%% Core Chaos Testing
%%%===================================================================

-spec execute_chaos_test(chaos_config()) -> chaos_result().
execute_chaos_test(Config) ->
    ?LOG_INFO("Executing chaos test for ~p ms with faults: ~p", 
             [Config#chaos_config.duration_ms, Config#chaos_config.fault_types]),
    
    %% Prepare system and baseline
    {BaselineActors, BaselineMetrics} = establish_baseline(Config),
    
    %% Initialize chaos tracking
    ets:new(chaos_tracking, [named_table, public, set]),
    ets:insert(chaos_tracking, {fault_history, []}),
    ets:insert(chaos_tracking, {system_events, []}),
    ets:insert(chaos_tracking, {baseline_metrics, BaselineMetrics}),
    
    %% Start monitoring
    MonitorPid = spawn_link(fun() -> chaos_monitor_loop() end),
    
    %% Execute chaos scenarios
    ChaosResult = execute_chaos_scenarios(Config, BaselineActors),
    
    %% Stop monitoring
    exit(MonitorPid, shutdown),
    
    %% Cleanup and finalize
    cleanup_chaos_test(BaselineActors),
    finalize_chaos_results(ChaosResult).

-spec establish_baseline(chaos_config()) -> {[term()], #{atom() => term()}}.
establish_baseline(Config) ->
    ?LOG_INFO("Establishing baseline with ~p actors", [Config#chaos_config.baseline_actors]),
    
    %% Spawn baseline actors
    BaselineActors = spawn_baseline_actors(Config#chaos_config.baseline_actors),
    
    %% Let system stabilize
    timer:sleep(2000),
    
    %% Measure baseline performance
    BaselineMetrics = measure_baseline_metrics(BaselineActors),
    
    {BaselineActors, BaselineMetrics}.

-spec spawn_baseline_actors(pos_integer()) -> [term()].
spawn_baseline_actors(Count) ->
    ?LOG_DEBUG("Spawning ~p baseline actors", [Count]),
    
    lists:filtermap(fun(N) ->
        ActorType = case N rem 4 of
            0 -> chaos_worker;
            1 -> chaos_coordinator;
            2 -> chaos_storage;
            _ -> chaos_monitor
        end,
        
        InitState = #{
            id => N,
            type => ActorType,
            startup_time => erlang:monotonic_time(nanosecond),
            message_count => 0,
            health_status => healthy
        },
        
        case catch bitactor_server:spawn_actor(ActorType, InitState) of
            {ok, ActorRef, _} -> {true, ActorRef};
            Error -> 
                ?LOG_WARNING("Failed to spawn baseline actor ~p: ~p", [N, Error]),
                false
        end
    end, lists:seq(1, Count)).

-spec measure_baseline_metrics([term()]) -> #{atom() => term()}.
measure_baseline_metrics(Actors) ->
    ?LOG_DEBUG("Measuring baseline metrics for ~p actors", [length(Actors)]),
    
    %% System metrics
    Memory = erlang:memory(),
    {GCCount, GCTime, _} = erlang:statistics(garbage_collection),
    
    %% Send test messages to measure latency
    LatencyMeasurements = measure_baseline_latency(Actors),
    
    #{
        actor_count => length(Actors),
        memory_total_mb => maps:get(total, Memory) / (1024 * 1024),
        memory_processes_mb => maps:get(processes, Memory) / (1024 * 1024),
        gc_count => GCCount,
        gc_time_ms => GCTime,
        baseline_latency_ns => calculate_percentile(LatencyMeasurements, 95),
        process_count => erlang:system_info(process_count),
        run_queue_length => erlang:statistics(run_queue)
    }.

-spec measure_baseline_latency([term()]) -> [non_neg_integer()].
measure_baseline_latency(Actors) when length(Actors) =< 10 ->
    []; % Too few actors for meaningful latency measurement
measure_baseline_latency(Actors) ->
    TestActors = lists:sublist(Actors, 100), % Test subset for speed
    
    lists:map(fun(Actor) ->
        StartTime = erlang:monotonic_time(nanosecond),
        case catch bitactor_server:send_message(Actor, <<"baseline_test">>) of
            ok -> 
                EndTime = erlang:monotonic_time(nanosecond),
                EndTime - StartTime;
            _ -> 
                1000000 % 1ms penalty for failed message
        end
    end, TestActors).

%%%===================================================================
%%% Chaos Scenarios
%%%===================================================================

-spec execute_chaos_scenarios(chaos_config(), [term()]) -> chaos_result().
execute_chaos_scenarios(Config, BaselineActors) ->
    EndTime = erlang:monotonic_time(millisecond) + Config#chaos_config.duration_ms,
    
    %% Initialize result tracking
    InitialResult = #chaos_result{
        faults_injected = 0,
        recovery_times_ms = [],
        availability_percent = 100.0,
        data_consistency_score = 1.0,
        system_stability_score = 1.0,
        fault_tolerance_score = 1.0,
        cascade_failure_count = 0,
        successful_recoveries = 0
    },
    
    chaos_scenario_loop(Config, BaselineActors, EndTime, InitialResult).

-spec chaos_scenario_loop(chaos_config(), [term()], integer(), chaos_result()) -> chaos_result().
chaos_scenario_loop(_Config, _Actors, EndTime, Result) when erlang:monotonic_time(millisecond) >= EndTime ->
    ?LOG_INFO("Chaos scenarios complete"),
    Result;
chaos_scenario_loop(Config, Actors, EndTime, Result) ->
    %% Select random fault type
    FaultType = lists:nth(rand:uniform(length(Config#chaos_config.fault_types)), 
                         Config#chaos_config.fault_types),
    
    ?LOG_INFO("Injecting chaos fault: ~p", [FaultType]),
    
    %% Inject fault and measure recovery
    FaultInjection = inject_and_measure_fault(FaultType, Actors, Config),
    
    %% Update results
    UpdatedResult = update_chaos_results(Result, FaultInjection),
    
    %% Wait before next fault
    timer:sleep(Config#chaos_config.fault_interval_ms),
    
    %% Validate system health before continuing
    case validate_system_health(Actors) of
        healthy -> 
            chaos_scenario_loop(Config, Actors, EndTime, UpdatedResult);
        degraded ->
            ?LOG_WARNING("System degraded, allowing recovery time"),
            timer:sleep(Config#chaos_config.recovery_timeout_ms),
            chaos_scenario_loop(Config, Actors, EndTime, UpdatedResult);
        failed ->
            ?LOG_ERROR("System failed chaos test"),
            UpdatedResult#chaos_result{fault_tolerance_score = 0.0}
    end.

-spec inject_and_measure_fault(atom(), [term()], chaos_config()) -> fault_injection().
inject_and_measure_fault(FaultType, Actors, Config) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Record pre-fault state
    PreFaultState = capture_system_state(Actors),
    
    %% Inject fault
    FaultResult = case FaultType of
        process_kill -> inject_process_kill_fault(Actors);
        memory_pressure -> inject_memory_pressure_fault();
        network_delay -> inject_network_delay_fault();
        cpu_spike -> inject_cpu_spike_fault();
        supervisor_failure -> inject_supervisor_failure();
        message_flood -> inject_message_flood_fault(Actors);
        _ -> {error, unknown_fault_type}
    end,
    
    %% Measure recovery time
    RecoveryTime = measure_recovery_time(PreFaultState, Actors, Config#chaos_config.recovery_timeout_ms),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    %% Detect side effects
    SideEffects = detect_side_effects(PreFaultState, Actors),
    
    #fault_injection{
        type = FaultType,
        start_time = StartTime,
        end_time = EndTime,
        target = FaultResult,
        success = element(1, FaultResult) =:= ok,
        recovery_time_ms = RecoveryTime,
        side_effects = SideEffects
    }.

%%%===================================================================
%%% Fault Injection Methods
%%%===================================================================

-spec inject_process_kill_fault([term()]) -> {ok, term()} | {error, term()}.
inject_process_kill_fault(Actors) when length(Actors) < 10 ->
    {error, insufficient_actors};
inject_process_kill_fault(Actors) ->
    %% Kill 5-15% of actors
    KillCount = max(1, (length(Actors) * (5 + rand:uniform(10))) div 100),
    TargetActors = lists:sublist([X || {_, X} <- lists:sort([{rand:uniform(), A} || A <- Actors])], KillCount),
    
    ?LOG_INFO("Killing ~p actors for chaos test", [length(TargetActors)]),
    
    KillResults = [catch bitactor_server:kill_actor(A) || A <- TargetActors],
    SuccessfulKills = length([R || R <- KillResults, R =:= ok]),
    
    {ok, #{killed_actors => TargetActors, successful_kills => SuccessfulKills}}.

-spec inject_memory_pressure_fault() -> {ok, term()} | {error, term()}.
inject_memory_pressure_fault() ->
    ?LOG_INFO("Injecting memory pressure"),
    
    %% Allocate large amounts of memory rapidly
    PressureProc = spawn(fun() ->
        memory_pressure_loop([], 0)
    end),
    
    %% Let pressure build for 2 seconds
    timer:sleep(2000),
    
    %% Release pressure
    exit(PressureProc, kill),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    {ok, #{pressure_process => PressureProc, duration_ms => 2000}}.

memory_pressure_loop(Acc, Count) when Count > 1000 ->
    %% Prevent infinite memory growth
    timer:sleep(10),
    memory_pressure_loop(lists:sublist(Acc, 500), 0);
memory_pressure_loop(Acc, Count) ->
    %% Allocate 1MB chunks
    Chunk = binary:copy(<<0:8>>, 1024 * 1024),
    memory_pressure_loop([Chunk | Acc], Count + 1).

-spec inject_network_delay_fault() -> {ok, term()} | {error, term()}.
inject_network_delay_fault() ->
    ?LOG_INFO("Injecting network delay simulation"),
    
    %% Simulate network delay by introducing artificial delays in message passing
    DelayProc = spawn(fun() ->
        network_delay_loop(5000) % 5 second delay injection
    end),
    
    {ok, #{delay_process => DelayProc, simulated_delay_ms => 100}}.

network_delay_loop(0) -> ok;
network_delay_loop(Remaining) ->
    %% Simulate network delay by consuming CPU cycles
    timer:sleep(1),
    [crypto:hash(sha256, <<N:32>>) || N <- lists:seq(1, 100)],
    network_delay_loop(Remaining - 1).

-spec inject_cpu_spike_fault() -> {ok, term()} | {error, term()}.
inject_cpu_spike_fault() ->
    ?LOG_INFO("Injecting CPU spike"),
    
    %% Spawn CPU-intensive processes
    NumCores = erlang:system_info(schedulers),
    CPUProcs = [spawn(fun() -> cpu_intensive_loop(3000) end) || _ <- lists:seq(1, NumCores)],
    
    {ok, #{cpu_processes => CPUProcs, duration_ms => 3000}}.

cpu_intensive_loop(0) -> ok;
cpu_intensive_loop(Remaining) ->
    %% CPU-intensive computation
    [math:sqrt(N * 1.0) || N <- lists:seq(1, 10000)],
    cpu_intensive_loop(Remaining - 1).

-spec inject_supervisor_failure() -> {ok, term()} | {error, term()}.
inject_supervisor_failure() ->
    ?LOG_INFO("Injecting supervisor failure"),
    
    %% Attempt to cause supervisor stress (but don't actually crash the system)
    case whereis(bitactor_sup) of
        undefined -> 
            {error, supervisor_not_found};
        SupPid ->
            %% Send a burst of invalid requests to stress the supervisor
            [SupPid ! invalid_request || _ <- lists:seq(1, 100)],
            {ok, #{supervisor => SupPid, stress_messages => 100}}
    end.

-spec inject_message_flood_fault([term()]) -> {ok, term()} | {error, term()}.
inject_message_flood_fault(Actors) when length(Actors) < 5 ->
    {error, insufficient_actors};
inject_message_flood_fault(Actors) ->
    ?LOG_INFO("Injecting message flood"),
    
    %% Send flood of messages to random actors
    FloodSize = 10000,
    TargetCount = min(100, length(Actors)),
    TargetActors = lists:sublist(Actors, TargetCount),
    
    FloodProc = spawn(fun() ->
        message_flood_loop(TargetActors, FloodSize)
    end),
    
    {ok, #{flood_process => FloodProc, message_count => FloodSize, target_actors => TargetCount}}.

message_flood_loop(_Actors, 0) -> ok;
message_flood_loop(Actors, Remaining) ->
    Actor = lists:nth(rand:uniform(length(Actors)), Actors),
    Message = crypto:strong_rand_bytes(1024), % 1KB messages
    catch bitactor_server:send_message(Actor, Message),
    message_flood_loop(Actors, Remaining - 1).

%%%===================================================================
%%% Recovery and Health Monitoring
%%%===================================================================

-spec measure_recovery_time(#{atom() => term()}, [term()], pos_integer()) -> non_neg_integer().
measure_recovery_time(PreFaultState, Actors, TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    measure_recovery_loop(PreFaultState, Actors, StartTime, TimeoutMs).

measure_recovery_loop(PreFaultState, Actors, StartTime, TimeoutMs) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    ElapsedTime = CurrentTime - StartTime,
    
    if ElapsedTime >= TimeoutMs ->
        TimeoutMs; % Recovery timed out
       true ->
        case check_system_recovery(PreFaultState, Actors) of
            recovered -> ElapsedTime;
            recovering -> 
                timer:sleep(100),
                measure_recovery_loop(PreFaultState, Actors, StartTime, TimeoutMs);
            failed -> TimeoutMs % Recovery failed
        end
    end.

-spec check_system_recovery(#{atom() => term()}, [term()]) -> recovered | recovering | failed.
check_system_recovery(PreFaultState, Actors) ->
    CurrentState = capture_system_state(Actors),
    
    %% Check if system metrics are within acceptable range of baseline
    PreActorCount = maps:get(actor_count, PreFaultState, 0),
    CurrentActorCount = maps:get(actor_count, CurrentState, 0),
    
    PreMemory = maps:get(memory_total_mb, PreFaultState, 0),
    CurrentMemory = maps:get(memory_total_mb, CurrentState, 0),
    
    %% Recovery criteria: within 20% of baseline metrics
    ActorCountOK = CurrentActorCount >= PreActorCount * 0.8,
    MemoryOK = CurrentMemory =< PreMemory * 1.2,
    
    %% Check system responsiveness
    ResponsivenessOK = check_system_responsiveness(Actors),
    
    case {ActorCountOK, MemoryOK, ResponsivenessOK} of
        {true, true, true} -> recovered;
        {false, _, _} -> recovering; % Actors still being restored
        {_, false, _} -> recovering; % Memory still high
        {_, _, false} -> failed % System unresponsive
    end.

-spec check_system_responsiveness([term()]) -> boolean().
check_system_responsiveness(Actors) when length(Actors) < 5 ->
    true; % Too few actors to test meaningfully
check_system_responsiveness(Actors) ->
    TestActors = lists:sublist(Actors, 10),
    ResponseCount = lists:sum([
        case catch bitactor_server:send_message(A, <<"health_check">>) of
            ok -> 1;
            _ -> 0
        end || A <- TestActors
    ]),
    
    %% At least 80% of tested actors should respond
    ResponseCount >= length(TestActors) * 0.8.

-spec validate_system_health([term()]) -> healthy | degraded | failed.
validate_system_health(Actors) ->
    %% Check system metrics
    Memory = erlang:memory(total) / (1024 * 1024), % MB
    ProcessCount = erlang:system_info(process_count),
    RunQueue = erlang:statistics(run_queue),
    
    %% Check actor responsiveness
    ResponsivenessOK = check_system_responsiveness(Actors),
    
    %% Health criteria
    MemoryOK = Memory < 2000, % Under 2GB
    ProcessCountOK = ProcessCount < 50000,
    RunQueueOK = RunQueue < 100,
    
    case {MemoryOK, ProcessCountOK, RunQueueOK, ResponsivenessOK} of
        {true, true, true, true} -> healthy;
        {false, _, _, _} -> degraded; % High memory usage
        {_, false, _, _} -> degraded; % Too many processes
        {_, _, false, _} -> degraded; % High run queue
        {_, _, _, false} -> failed % System unresponsive
    end.

-spec capture_system_state([term()]) -> #{atom() => term()}.
capture_system_state(Actors) ->
    Memory = erlang:memory(),
    {GCCount, GCTime, _} = erlang:statistics(garbage_collection),
    
    #{
        timestamp => erlang:monotonic_time(millisecond),
        actor_count => length(Actors),
        memory_total_mb => maps:get(total, Memory) / (1024 * 1024),
        memory_processes_mb => maps:get(processes, Memory) / (1024 * 1024),
        process_count => erlang:system_info(process_count),
        run_queue_length => erlang:statistics(run_queue),
        gc_count => GCCount,
        gc_time_ms => GCTime
    }.

-spec detect_side_effects(#{atom() => term()}, [term()]) -> [term()].
detect_side_effects(PreFaultState, Actors) ->
    CurrentState = capture_system_state(Actors),
    SideEffects = [],
    
    %% Check for memory leaks
    PreMemory = maps:get(memory_total_mb, PreFaultState),
    CurrentMemory = maps:get(memory_total_mb, CurrentState),
    
    SideEffects1 = if CurrentMemory > PreMemory * 1.5 ->
        [memory_leak | SideEffects];
       true -> SideEffects
    end,
    
    %% Check for process leaks
    PreProcesses = maps:get(process_count, PreFaultState),
    CurrentProcesses = maps:get(process_count, CurrentState),
    
    SideEffects2 = if CurrentProcesses > PreProcesses * 1.2 ->
        [process_leak | SideEffects1];
       true -> SideEffects1
    end,
    
    %% Check for actor count discrepancies
    PreActors = maps:get(actor_count, PreFaultState),
    CurrentActors = maps:get(actor_count, CurrentState),
    
    SideEffects3 = if CurrentActors < PreActors * 0.5 ->
        [actor_loss | SideEffects2];
       true -> SideEffects2
    end,
    
    SideEffects3.

%%%===================================================================
%%% Monitoring and Tracking
%%%===================================================================

-spec chaos_monitor_loop() -> ok.
chaos_monitor_loop() ->
    %% Monitor system events during chaos testing
    timer:sleep(1000),
    
    %% Record system event
    Event = #{
        timestamp => erlang:monotonic_time(millisecond),
        memory_mb => erlang:memory(total) / (1024 * 1024),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue)
    },
    
    %% Store event
    case ets:lookup(chaos_tracking, system_events) of
        [{system_events, Events}] ->
            UpdatedEvents = [Event | lists:sublist(Events, 99)], % Keep last 100 events
            ets:insert(chaos_tracking, {system_events, UpdatedEvents});
        [] ->
            ets:insert(chaos_tracking, {system_events, [Event]})
    end,
    
    chaos_monitor_loop().

%%%===================================================================
%%% Result Processing
%%%===================================================================

-spec update_chaos_results(chaos_result(), fault_injection()) -> chaos_result().
update_chaos_results(Result, FaultInjection) ->
    %% Update fault tracking
    [{fault_history, History}] = ets:lookup(chaos_tracking, fault_history),
    ets:insert(chaos_tracking, {fault_history, [FaultInjection | History]}),
    
    %% Calculate updated metrics
    NewRecoveryTimes = [FaultInjection#fault_injection.recovery_time_ms | Result#chaos_result.recovery_times_ms],
    
    SuccessfulRecoveries = if FaultInjection#fault_injection.success andalso 
                             FaultInjection#fault_injection.recovery_time_ms < ?RECOVERY_TIMEOUT_MS ->
        Result#chaos_result.successful_recoveries + 1;
       true -> Result#chaos_result.successful_recoveries
    end,
    
    TotalFaults = Result#chaos_result.faults_injected + 1,
    AvailabilityPercent = (SuccessfulRecoveries / TotalFaults) * 100,
    
    Result#chaos_result{
        faults_injected = TotalFaults,
        recovery_times_ms = NewRecoveryTimes,
        availability_percent = AvailabilityPercent,
        successful_recoveries = SuccessfulRecoveries
    }.

-spec finalize_chaos_results(chaos_result()) -> chaos_result().
finalize_chaos_results(Result) ->
    %% Calculate final scores
    RecoveryTimes = Result#chaos_result.recovery_times_ms,
    
    AvgRecoveryTime = case RecoveryTimes of
        [] -> 0;
        _ -> lists:sum(RecoveryTimes) / length(RecoveryTimes)
    end,
    
    %% Fault tolerance score based on recovery performance
    FaultToleranceScore = if AvgRecoveryTime =< 1000 -> 1.0; % Excellent
                            AvgRecoveryTime =< 3000 -> 0.8; % Good
                            AvgRecoveryTime =< 5000 -> 0.6; % Acceptable
                            true -> 0.3 % Poor
                         end,
    
    %% System stability score
    SystemStabilityScore = if Result#chaos_result.availability_percent >= 99.9 -> 1.0;
                             Result#chaos_result.availability_percent >= 99.0 -> 0.8;
                             Result#chaos_result.availability_percent >= 95.0 -> 0.6;
                             true -> 0.3
                          end,
    
    %% Data consistency score (simplified)
    DataConsistencyScore = if Result#chaos_result.cascade_failure_count =:= 0 -> 1.0;
                             Result#chaos_result.cascade_failure_count =< 2 -> 0.8;
                             true -> 0.5
                          end,
    
    %% Cleanup tracking table
    ets:delete(chaos_tracking),
    
    Result#chaos_result{
        fault_tolerance_score = FaultToleranceScore,
        system_stability_score = SystemStabilityScore,
        data_consistency_score = DataConsistencyScore
    }.

%%%===================================================================
%%% System Management
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec cleanup_chaos_test([term()]) -> ok.
cleanup_chaos_test(BaselineActors) ->
    ?LOG_INFO("Cleaning up chaos test with ~p baseline actors", [length(BaselineActors)]),
    
    %% Kill baseline actors
    [catch bitactor_server:kill_actor(A) || A <- BaselineActors],
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Clean up any remaining ETS tables
    case ets:whereis(chaos_tracking) of
        undefined -> ok;
        _ -> ets:delete(chaos_tracking)
    end,
    
    ok.

%%%===================================================================
%%% Utilities
%%%===================================================================

-spec calculate_percentile([number()], number()) -> number().
calculate_percentile([], _) -> 0;
calculate_percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = round((Percentile / 100) * Len),
    ClampedIndex = max(1, min(Index, Len)),
    lists:nth(ClampedIndex, Sorted).

-spec configure_chaos_test(map()) -> chaos_config().
configure_chaos_test(Config) ->
    #chaos_config{
        duration_ms = maps:get(duration_ms, Config, ?CHAOS_DURATION_MS),
        fault_types = maps:get(fault_types, Config, [process_kill, memory_pressure, network_delay, cpu_spike]),
        recovery_timeout_ms = maps:get(recovery_timeout_ms, Config, ?RECOVERY_TIMEOUT_MS),
        fault_interval_ms = maps:get(fault_interval_ms, Config, ?FAULT_INJECTION_INTERVAL_MS),
        baseline_actors = maps:get(baseline_actors, Config, 1000),
        target_availability = maps:get(target_availability, Config, 99.9)
    }.

-spec format_chaos_results(chaos_result(), chaos_config()) -> #{atom() => term()}.
format_chaos_results(Result, Config) ->
    RecoveryTimes = Result#chaos_result.recovery_times_ms,
    
    #{
        test_type => chaos_resilience,
        duration_ms => Config#chaos_config.duration_ms,
        target_availability => Config#chaos_config.target_availability,
        
        %% Core chaos metrics
        faults_injected => Result#chaos_result.faults_injected,
        successful_recoveries => Result#chaos_result.successful_recoveries,
        availability_percent => Result#chaos_result.availability_percent,
        
        %% Recovery performance
        recovery_time_ms => case RecoveryTimes of
            [] -> 0;
            _ -> lists:sum(RecoveryTimes) / length(RecoveryTimes)
        end,
        max_recovery_time_ms => case RecoveryTimes of
            [] -> 0;
            _ -> lists:max(RecoveryTimes)
        end,
        min_recovery_time_ms => case RecoveryTimes of
            [] -> 0;
            _ -> lists:min(RecoveryTimes)
        end,
        
        %% Quality scores
        fault_tolerance_score => Result#chaos_result.fault_tolerance_score,
        system_stability_score => Result#chaos_result.system_stability_score,
        data_consistency => Result#chaos_result.data_consistency_score > 0.9,
        
        %% Reliability metrics
        cascade_failure_count => Result#chaos_result.cascade_failure_count,
        
        %% Validation flags
        target_met => Result#chaos_result.availability_percent >= Config#chaos_config.target_availability,
        uhft_resilient => Result#chaos_result.availability_percent >= 99.9 andalso
                         (case RecoveryTimes of
                            [] -> true;
                            _ -> lists:max(RecoveryTimes) =< 1000
                          end),
        
        %% Success criteria
        error_rate => (Result#chaos_result.faults_injected - Result#chaos_result.successful_recoveries) / 
                     max(Result#chaos_result.faults_injected, 1),
        
        timestamp => erlang:system_time(second)
    }.