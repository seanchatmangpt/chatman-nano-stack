%%%-------------------------------------------------------------------
%%% @doc BitActor Lifecycle Stress Test Engine
%%% Agent 4: 10k+ concurrent actors with spawn/kill cycles
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(actor_stress_engine).

-export([run/1]).
-export([stress_test_actors/3, monitor_memory_usage/2, validate_actor_consistency/1]).

-include_lib("kernel/include/logger.hrl").

-define(TARGET_ACTOR_COUNT, 10000).
-define(STRESS_DURATION_MS, 120000). % 2 minutes
-define(SPAWN_BATCH_SIZE, 100).
-define(MAX_MEMORY_PER_ACTOR_KB, 100).
-define(MEMORY_SAMPLE_INTERVAL_MS, 500).

-record(actor_stress_config, {
    target_actor_count = ?TARGET_ACTOR_COUNT :: pos_integer(),
    duration_ms = ?STRESS_DURATION_MS :: pos_integer(),
    spawn_batch_size = ?SPAWN_BATCH_SIZE :: pos_integer(),
    kill_rate_percent = 10 :: pos_integer(), % Percentage of actors to kill per cycle
    spawn_rate_msgs_sec = 1000 :: pos_integer(),
    message_burst_size = 50 :: pos_integer(),
    memory_check_interval_ms = ?MEMORY_SAMPLE_INTERVAL_MS :: pos_integer()
}).

-record(actor_stress_result, {
    max_concurrent_actors :: non_neg_integer(),
    total_actors_spawned :: non_neg_integer(),
    total_actors_killed :: non_neg_integer(),
    actor_spawn_failures :: non_neg_integer(),
    memory_peak_mb :: float(),
    memory_per_actor_kb :: float(),
    gc_pressure_events :: non_neg_integer(),
    supervisor_restarts :: non_neg_integer(),
    message_delivery_failures :: non_neg_integer(),
    consistency_violations :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run(map()) -> #{atom() => term()}.
run(Config) ->
    ?LOG_INFO("Starting actor lifecycle stress test engine"),
    
    %% Ensure system is ready
    ok = ensure_system_ready(),
    
    %% Configure stress test
    StressConfig = configure_actor_stress_test(Config),
    
    %% Execute stress test
    Results = execute_actor_stress_test(StressConfig),
    
    %% Format results
    format_actor_stress_results(Results, StressConfig).

%%%===================================================================
%%% Core Stress Testing
%%%===================================================================

-spec execute_actor_stress_test(actor_stress_config()) -> actor_stress_result().
execute_actor_stress_test(Config) ->
    ?LOG_INFO("Executing actor stress test: ~p actors for ~p ms", 
             [Config#actor_stress_config.target_actor_count, Config#actor_stress_config.duration_ms]),
    
    %% Prepare system
    prepare_system_for_stress_test(),
    
    %% Start monitoring processes
    MemoryMonitor = spawn_link(fun() -> memory_monitor_loop(Config) end),
    SupervisorMonitor = spawn_link(fun() -> supervisor_monitor_loop() end),
    
    %% Execute stress phases
    StressResult = execute_stress_phases(Config),
    
    %% Stop monitoring
    exit(MemoryMonitor, shutdown),
    exit(SupervisorMonitor, shutdown),
    
    %% Collect final metrics
    finalize_stress_results(StressResult, Config).

-spec execute_stress_phases(actor_stress_config()) -> actor_stress_result().
execute_stress_phases(Config) ->
    ?LOG_INFO("Starting stress test phases"),
    
    %% Initialize tracking
    ets:new(actor_stress_tracking, [named_table, public, set]),
    ets:insert(actor_stress_tracking, {active_actors, []}),
    ets:insert(actor_stress_tracking, {metrics, initialize_metrics()}),
    
    %% Phase 1: Ramp up to target actor count
    ramp_up_phase(Config),
    
    %% Phase 2: Sustained load with lifecycle churn
    sustained_churn_phase(Config),
    
    %% Phase 3: Memory pressure testing
    memory_pressure_phase(Config),
    
    %% Phase 4: Controlled shutdown
    shutdown_phase(),
    
    %% Extract final results
    extract_final_results().

-spec ramp_up_phase(actor_stress_config()) -> ok.
ramp_up_phase(Config) ->
    ?LOG_INFO("Phase 1: Ramping up to ~p actors", [Config#actor_stress_config.target_actor_count]),
    
    TargetCount = Config#actor_stress_config.target_actor_count,
    BatchSize = Config#actor_stress_config.spawn_batch_size,
    
    ramp_up_loop(0, TargetCount, BatchSize, []).

-spec ramp_up_loop(non_neg_integer(), pos_integer(), pos_integer(), [term()]) -> ok.
ramp_up_loop(CurrentCount, TargetCount, _BatchSize, ActiveActors) when CurrentCount >= TargetCount ->
    ets:insert(actor_stress_tracking, {active_actors, ActiveActors}),
    update_metrics(max_concurrent_actors, length(ActiveActors)),
    ?LOG_INFO("Ramp up complete: ~p actors spawned", [length(ActiveActors)]),
    ok;
ramp_up_loop(CurrentCount, TargetCount, BatchSize, ActiveActors) ->
    SpawnCount = min(BatchSize, TargetCount - CurrentCount),
    
    %% Spawn batch of actors
    {NewActors, SpawnFailures} = spawn_actor_batch(SpawnCount),
    
    %% Update tracking
    UpdatedActors = NewActors ++ ActiveActors,
    update_metrics(total_actors_spawned, length(NewActors)),
    update_metrics(actor_spawn_failures, SpawnFailures),
    
    %% Brief pause to allow system to stabilize
    timer:sleep(100),
    
    %% Continue ramping
    ramp_up_loop(CurrentCount + length(NewActors), TargetCount, BatchSize, UpdatedActors).

-spec sustained_churn_phase(actor_stress_config()) -> ok.
sustained_churn_phase(Config) ->
    ?LOG_INFO("Phase 2: Sustained churn with ~p%% kill rate", 
             [Config#actor_stress_config.kill_rate_percent]),
    
    ChurnDuration = Config#actor_stress_config.duration_ms div 2, % Half the total duration
    EndTime = erlang:monotonic_time(millisecond) + ChurnDuration,
    
    sustained_churn_loop(Config, EndTime).

-spec sustained_churn_loop(actor_stress_config(), integer()) -> ok.
sustained_churn_loop(_Config, EndTime) when erlang:monotonic_time(millisecond) >= EndTime ->
    ?LOG_INFO("Sustained churn phase complete"),
    ok;
sustained_churn_loop(Config, EndTime) ->
    [{active_actors, ActiveActors}] = ets:lookup(actor_stress_tracking, active_actors),
    
    %% Calculate kill count
    KillCount = max(1, (length(ActiveActors) * Config#actor_stress_config.kill_rate_percent) div 100),
    
    %% Kill random actors
    {KilledActors, RemainingActors} = kill_random_actors(ActiveActors, KillCount),
    
    %% Spawn replacement actors plus some extra for growth
    SpawnCount = length(KilledActors) + rand:uniform(10),
    {NewActors, SpawnFailures} = spawn_actor_batch(SpawnCount),
    
    %% Update state
    UpdatedActors = NewActors ++ RemainingActors,
    ets:insert(actor_stress_tracking, {active_actors, UpdatedActors}),
    
    %% Update metrics
    update_metrics(total_actors_killed, length(KilledActors)),
    update_metrics(total_actors_spawned, length(NewActors)),
    update_metrics(actor_spawn_failures, SpawnFailures),
    update_metrics(max_concurrent_actors, max(length(UpdatedActors), get_metric(max_concurrent_actors))),
    
    %% Send message bursts to random actors
    send_message_bursts(UpdatedActors, Config),
    
    %% Brief pause before next cycle
    timer:sleep(1000),
    
    sustained_churn_loop(Config, EndTime).

-spec memory_pressure_phase(actor_stress_config()) -> ok.
memory_pressure_phase(Config) ->
    ?LOG_INFO("Phase 3: Memory pressure testing"),
    
    [{active_actors, ActiveActors}] = ets:lookup(actor_stress_tracking, active_actors),
    
    %% Create memory pressure by spawning additional actors rapidly
    PressureActors = create_memory_pressure_actors(length(ActiveActors)),
    
    %% Monitor for OOM or GC pressure
    monitor_memory_pressure(5000), % 5 second monitoring
    
    %% Cleanup pressure actors
    cleanup_actors(PressureActors),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(1000),
    
    ?LOG_INFO("Memory pressure phase complete"),
    ok.

-spec shutdown_phase() -> ok.
shutdown_phase() ->
    ?LOG_INFO("Phase 4: Controlled shutdown"),
    
    [{active_actors, ActiveActors}] = ets:lookup(actor_stress_tracking, active_actors),
    
    %% Graceful shutdown in batches
    shutdown_actors_gracefully(ActiveActors),
    
    ets:insert(actor_stress_tracking, {active_actors, []}),
    
    ?LOG_INFO("Shutdown phase complete"),
    ok.

%%%===================================================================
%%% Actor Management
%%%===================================================================

-spec spawn_actor_batch(pos_integer()) -> {[term()], non_neg_integer()}.
spawn_actor_batch(Count) ->
    Results = lists:map(fun(N) ->
        ActorType = case rand:uniform(4) of
            1 -> stress_worker;
            2 -> stress_calculator;
            3 -> stress_storage;
            _ -> stress_coordinator
        end,
        
        InitState = #{
            id => N,
            spawn_time => erlang:monotonic_time(nanosecond),
            message_count => 0,
            state_data => crypto:strong_rand_bytes(rand:uniform(1024))
        },
        
        case catch bitactor_server:spawn_actor(ActorType, InitState) of
            {ok, ActorRef, _} -> {ok, ActorRef};
            Error -> {error, Error}
        end
    end, lists:seq(1, Count)),
    
    Successful = [Actor || {ok, Actor} <- Results],
    Failures = length([E || {error, E} <- Results]),
    
    {Successful, Failures}.

-spec kill_random_actors([term()], pos_integer()) -> {[term()], [term()]}.
kill_random_actors(Actors, KillCount) when KillCount >= length(Actors) ->
    %% Kill all actors
    [catch bitactor_server:kill_actor(A) || A <- Actors],
    {Actors, []};
kill_random_actors(Actors, KillCount) ->
    %% Randomly select actors to kill
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), A} || A <- Actors])],
    {ToKill, ToKeep} = lists:split(KillCount, Shuffled),
    
    %% Kill selected actors
    KillResults = [catch bitactor_server:kill_actor(A) || A <- ToKill],
    
    %% Track successful kills
    SuccessfulKills = [A || {A, ok} <- lists:zip(ToKill, KillResults)],
    
    {SuccessfulKills, ToKeep}.

-spec create_memory_pressure_actors(pos_integer()) -> [term()].
create_memory_pressure_actors(BaseCount) ->
    %% Spawn 50% more actors to create memory pressure
    ExtraCount = BaseCount div 2,
    
    {PressureActors, _} = spawn_actor_batch(ExtraCount),
    
    %% Send large payloads to pressure actors
    LargePayload = crypto:strong_rand_bytes(10240), % 10KB payloads
    [catch bitactor_server:send_message(A, LargePayload) || A <- PressureActors],
    
    PressureActors.

-spec cleanup_actors([term()]) -> ok.
cleanup_actors(Actors) ->
    [catch bitactor_server:kill_actor(A) || A <- Actors],
    ok.

-spec shutdown_actors_gracefully([term()]) -> ok.
shutdown_actors_gracefully(Actors) ->
    BatchSize = 50,
    shutdown_in_batches(Actors, BatchSize).

-spec shutdown_in_batches([term()], pos_integer()) -> ok.
shutdown_in_batches([], _) -> ok;
shutdown_in_batches(Actors, BatchSize) ->
    {Batch, Remaining} = case length(Actors) > BatchSize of
        true -> lists:split(BatchSize, Actors);
        false -> {Actors, []}
    end,
    
    %% Shutdown batch
    [catch bitactor_server:kill_actor(A) || A <- Batch],
    
    %% Brief pause
    timer:sleep(100),
    
    shutdown_in_batches(Remaining, BatchSize).

%%%===================================================================
%%% Message Testing
%%%===================================================================

-spec send_message_bursts([term()], actor_stress_config()) -> ok.
send_message_bursts(Actors, Config) when length(Actors) < 10 ->
    ok; % Skip if too few actors
send_message_bursts(Actors, Config) ->
    BurstSize = Config#actor_stress_config.message_burst_size,
    TargetCount = min(BurstSize, length(Actors)),
    
    %% Select random actors for message burst
    TargetActors = [lists:nth(rand:uniform(length(Actors)), Actors) || _ <- lists:seq(1, TargetCount)],
    
    %% Send burst messages
    MessagePayload = generate_stress_message(),
    DeliveryFailures = lists:sum([
        case catch bitactor_server:send_message(Actor, MessagePayload) of
            ok -> 0;
            _ -> 1
        end || Actor <- TargetActors
    ]),
    
    update_metrics(message_delivery_failures, DeliveryFailures),
    ok.

-spec generate_stress_message() -> binary().
generate_stress_message() ->
    Timestamp = erlang:monotonic_time(nanosecond),
    SeqNum = rand:uniform(1000000),
    Payload = crypto:strong_rand_bytes(rand:uniform(512)),
    
    <<Timestamp:64/native, SeqNum:32/native, Payload/binary>>.

%%%===================================================================
%%% Monitoring
%%%===================================================================

-spec memory_monitor_loop(actor_stress_config()) -> ok.
memory_monitor_loop(Config) ->
    Interval = Config#actor_stress_config.memory_check_interval_ms,
    memory_monitor_loop_impl(Interval).

memory_monitor_loop_impl(Interval) ->
    %% Sample memory usage
    Memory = erlang:memory(),
    Total = maps:get(total, Memory),
    Processes = maps:get(processes, Memory),
    
    %% Get actor count
    ActorCount = case ets:lookup(actor_stress_tracking, active_actors) of
        [{_, Actors}] -> length(Actors);
        [] -> 0
    end,
    
    %% Calculate memory per actor
    MemoryPerActorKB = if ActorCount > 0 -> 
        (Processes / 1024) / ActorCount; 
        true -> 0 
    end,
    
    %% Update metrics
    CurrentPeak = get_metric(memory_peak_mb),
    update_metrics(memory_peak_mb, max(Total / (1024 * 1024), CurrentPeak)),
    update_metrics(memory_per_actor_kb, MemoryPerActorKB),
    
    %% Check for GC pressure
    {GCCount, GCTime, _} = erlang:statistics(garbage_collection),
    PrevGCCount = get_metric(prev_gc_count),
    
    if GCCount > PrevGCCount + 100 -> % More than 100 GCs in interval
        update_metrics(gc_pressure_events, get_metric(gc_pressure_events) + 1);
       true -> ok
    end,
    
    update_metrics(prev_gc_count, GCCount),
    
    timer:sleep(Interval),
    memory_monitor_loop_impl(Interval).

-spec supervisor_monitor_loop() -> ok.
supervisor_monitor_loop() ->
    %% Monitor supervisor restarts
    case catch supervisor:which_children(bitactor_sup) of
        Children when is_list(Children) ->
            RestartCount = length([C || {_, _, _, _} <- Children]),
            PrevRestarts = get_metric(supervisor_restarts),
            
            if RestartCount > PrevRestarts ->
                update_metrics(supervisor_restarts, RestartCount);
               true -> ok
            end;
        _ -> ok
    end,
    
    timer:sleep(1000),
    supervisor_monitor_loop().

-spec monitor_memory_pressure(pos_integer()) -> ok.
monitor_memory_pressure(Duration) ->
    EndTime = erlang:monotonic_time(millisecond) + Duration,
    monitor_memory_pressure_loop(EndTime).

monitor_memory_pressure_loop(EndTime) when erlang:monotonic_time(millisecond) >= EndTime ->
    ok;
monitor_memory_pressure_loop(EndTime) ->
    Memory = erlang:memory(total),
    
    %% Check if approaching system limits (example threshold)
    SystemMemory = Memory / (1024 * 1024), % MB
    
    if SystemMemory > 1000 -> % 1GB threshold
        update_metrics(gc_pressure_events, get_metric(gc_pressure_events) + 1);
       true -> ok
    end,
    
    timer:sleep(100),
    monitor_memory_pressure_loop(EndTime).

%%%===================================================================
%%% Metrics Management
%%%===================================================================

-spec initialize_metrics() -> #{atom() => term()}.
initialize_metrics() ->
    #{
        max_concurrent_actors => 0,
        total_actors_spawned => 0,
        total_actors_killed => 0,
        actor_spawn_failures => 0,
        memory_peak_mb => 0.0,
        memory_per_actor_kb => 0.0,
        gc_pressure_events => 0,
        supervisor_restarts => 0,
        message_delivery_failures => 0,
        consistency_violations => 0,
        prev_gc_count => 0
    }.

-spec update_metrics(atom(), term()) -> ok.
update_metrics(Key, Value) ->
    [{metrics, Metrics}] = ets:lookup(actor_stress_tracking, metrics),
    UpdatedMetrics = maps:put(Key, Value, Metrics),
    ets:insert(actor_stress_tracking, {metrics, UpdatedMetrics}),
    ok.

-spec get_metric(atom()) -> term().
get_metric(Key) ->
    case ets:lookup(actor_stress_tracking, metrics) of
        [{metrics, Metrics}] -> maps:get(Key, Metrics, 0);
        [] -> 0
    end.

%%%===================================================================
%%% Result Processing
%%%===================================================================

-spec extract_final_results() -> actor_stress_result().
extract_final_results() ->
    [{metrics, Metrics}] = ets:lookup(actor_stress_tracking, metrics),
    
    #actor_stress_result{
        max_concurrent_actors = maps:get(max_concurrent_actors, Metrics),
        total_actors_spawned = maps:get(total_actors_spawned, Metrics),
        total_actors_killed = maps:get(total_actors_killed, Metrics),
        actor_spawn_failures = maps:get(actor_spawn_failures, Metrics),
        memory_peak_mb = maps:get(memory_peak_mb, Metrics),
        memory_per_actor_kb = maps:get(memory_per_actor_kb, Metrics),
        gc_pressure_events = maps:get(gc_pressure_events, Metrics),
        supervisor_restarts = maps:get(supervisor_restarts, Metrics),
        message_delivery_failures = maps:get(message_delivery_failures, Metrics),
        consistency_violations = maps:get(consistency_violations, Metrics)
    }.

-spec finalize_stress_results(actor_stress_result(), actor_stress_config()) -> actor_stress_result().
finalize_stress_results(Result, _Config) ->
    %% Cleanup tracking table
    ets:delete(actor_stress_tracking),
    
    %% Final garbage collection
    erlang:garbage_collect(),
    
    Result.

%%%===================================================================
%%% System Preparation
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec prepare_system_for_stress_test() -> ok.
prepare_system_for_stress_test() ->
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Reset telemetry
    catch bitactor_telemetry:reset_metrics(),
    
    %% Clear any existing tracking
    case ets:whereis(actor_stress_tracking) of
        undefined -> ok;
        _ -> ets:delete(actor_stress_tracking)
    end,
    
    ok.

%%%===================================================================
%%% Configuration and Formatting
%%%===================================================================

-spec configure_actor_stress_test(map()) -> actor_stress_config().
configure_actor_stress_test(Config) ->
    #actor_stress_config{
        target_actor_count = maps:get(target_actor_count, Config, ?TARGET_ACTOR_COUNT),
        duration_ms = maps:get(duration_ms, Config, ?STRESS_DURATION_MS),
        spawn_batch_size = maps:get(spawn_batch_size, Config, ?SPAWN_BATCH_SIZE),
        kill_rate_percent = maps:get(kill_rate_percent, Config, 10),
        spawn_rate_msgs_sec = maps:get(spawn_rate_msgs_sec, Config, 1000),
        message_burst_size = maps:get(message_burst_size, Config, 50),
        memory_check_interval_ms = maps:get(memory_check_interval_ms, Config, ?MEMORY_SAMPLE_INTERVAL_MS)
    }.

-spec format_actor_stress_results(actor_stress_result(), actor_stress_config()) -> #{atom() => term()}.
format_actor_stress_results(Result, Config) ->
    #{
        test_type => concurrent_actors,
        target_actor_count => Config#actor_stress_config.target_actor_count,
        
        %% Core actor metrics
        actor_count => Result#actor_stress_result.max_concurrent_actors,
        max_concurrent_actors => Result#actor_stress_result.max_concurrent_actors,
        total_actors_spawned => Result#actor_stress_result.total_actors_spawned,
        total_actors_killed => Result#actor_stress_result.total_actors_killed,
        
        %% Performance metrics
        memory_peak_mb => Result#actor_stress_result.memory_peak_mb,
        memory_per_actor_kb => Result#actor_stress_result.memory_per_actor_kb,
        
        %% Reliability metrics
        actor_spawn_failures => Result#actor_stress_result.actor_spawn_failures,
        spawn_success_rate => case Result#actor_stress_result.total_actors_spawned of
            0 -> 0.0;
            Total -> (Total - Result#actor_stress_result.actor_spawn_failures) / Total
        end,
        
        message_delivery_failures => Result#actor_stress_result.message_delivery_failures,
        supervisor_restarts => Result#actor_stress_result.supervisor_restarts,
        gc_pressure_events => Result#actor_stress_result.gc_pressure_events,
        consistency_violations => Result#actor_stress_result.consistency_violations,
        
        %% Validation flags
        target_met => Result#actor_stress_result.max_concurrent_actors >= Config#actor_stress_config.target_actor_count,
        memory_efficient => Result#actor_stress_result.memory_per_actor_kb =< ?MAX_MEMORY_PER_ACTOR_KB,
        stable_supervision => Result#actor_stress_result.supervisor_restarts =< 1,
        low_failure_rate => (Result#actor_stress_result.actor_spawn_failures / 
                            max(Result#actor_stress_result.total_actors_spawned, 1)) < 0.01,
        
        %% Success criteria
        error_rate => (Result#actor_stress_result.actor_spawn_failures + 
                      Result#actor_stress_result.message_delivery_failures) / 
                     max(Result#actor_stress_result.total_actors_spawned, 1),
        
        timestamp => erlang:system_time(second)
    }.