%%%-------------------------------------------------------------------
%%% @doc BitActor Throughput Test Engine
%%% Agent 3: 1M+ messages/second load generation and validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(throughput_test_engine).

-export([run/1]).
-export([measure_throughput/3, generate_load/4, calculate_throughput_stats/2]).

-include_lib("kernel/include/logger.hrl").

-define(TARGET_THROUGHPUT, 1000000). % 1M msgs/sec
-define(MEASUREMENT_DURATION_MS, 10000). % 10 second bursts
-define(MAX_PRODUCERS, 16).
-define(BATCH_SIZE, 1000).

-record(throughput_config, {
    target_msgs_sec = ?TARGET_THROUGHPUT :: pos_integer(),
    duration_ms = ?MEASUREMENT_DURATION_MS :: pos_integer(),
    producer_count = 4 :: pos_integer(),
    batch_size = ?BATCH_SIZE :: pos_integer(),
    message_size = 256 :: pos_integer(),
    burst_count = 5 :: pos_integer()
}).

-record(throughput_result, {
    msgs_sent :: non_neg_integer(),
    msgs_received :: non_neg_integer(),
    duration_ms :: non_neg_integer(),
    throughput_msgs_sec :: float(),
    peak_throughput_msgs_sec :: float(),
    sustained_throughput_msgs_sec :: float(),
    error_count :: non_neg_integer(),
    queue_depths :: [non_neg_integer()],
    backpressure_events :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run(map()) -> #{atom() => term()}.
run(Config) ->
    ?LOG_INFO("Starting high-throughput validation engine"),
    
    %% Ensure BitActor system is ready
    ok = ensure_system_ready(),
    
    %% Configure throughput test
    ThroughputConfig = configure_throughput_test(Config),
    
    %% Execute throughput measurements
    Results = execute_throughput_tests(ThroughputConfig),
    
    %% Format results for framework
    format_throughput_results(Results, ThroughputConfig).

%%%===================================================================
%%% Core Throughput Testing
%%%===================================================================

-spec execute_throughput_tests(throughput_config()) -> throughput_result().
execute_throughput_tests(Config) ->
    ?LOG_INFO("Executing throughput tests with ~p producers, target ~p msgs/sec", 
             [Config#throughput_config.producer_count, Config#throughput_config.target_msgs_sec]),
    
    %% Prepare system for high throughput
    prepare_system_for_throughput(),
    
    %% Create target actors for message sending
    TargetActors = create_target_actors(Config#throughput_config.producer_count * 2),
    
    %% Execute multiple bursts to measure sustained throughput
    BurstResults = execute_throughput_bursts(Config, TargetActors),
    
    %% Cleanup
    cleanup_target_actors(TargetActors),
    
    %% Aggregate results
    aggregate_burst_results(BurstResults, Config).

-spec execute_throughput_bursts(throughput_config(), [term()]) -> [throughput_result()].
execute_throughput_bursts(Config, TargetActors) ->
    BurstCount = Config#throughput_config.burst_count,
    
    lists:map(fun(BurstNum) ->
        ?LOG_DEBUG("Executing throughput burst ~p/~p", [BurstNum, BurstCount]),
        
        %% Reset system state
        reset_throughput_metrics(),
        
        %% Execute single burst
        Result = execute_single_burst(Config, TargetActors),
        
        %% Cool down between bursts
        timer:sleep(2000),
        
        Result
    end, lists:seq(1, BurstCount)).

-spec execute_single_burst(throughput_config(), [term()]) -> throughput_result().
execute_single_burst(Config, TargetActors) ->
    ProducerCount = Config#throughput_config.producer_count,
    Duration = Config#throughput_config.duration_ms,
    
    %% Start metrics collection
    MetricsCollector = spawn_link(fun() -> collect_throughput_metrics() end),
    
    %% Launch producer processes
    Producers = launch_producers(ProducerCount, Config, TargetActors),
    
    %% Measure for specified duration
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Let producers run
    timer:sleep(Duration),
    
    %% Stop producers
    [exit(Pid, shutdown) || {Pid, _} <- Producers],
    
    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = EndTime - StartTime,
    
    %% Stop metrics collection
    exit(MetricsCollector, shutdown),
    
    %% Collect results from producers
    collect_producer_results(Producers, ActualDuration).

-spec launch_producers(pos_integer(), throughput_config(), [term()]) -> [{pid(), reference()}].
launch_producers(ProducerCount, Config, TargetActors) ->
    MessagesPerProducer = (Config#throughput_config.target_msgs_sec * 
                          Config#throughput_config.duration_ms div 1000) div ProducerCount,
    
    lists:map(fun(ProducerId) ->
        Pid = spawn_monitor(fun() ->
            producer_loop(ProducerId, Config, TargetActors, MessagesPerProducer)
        end),
        Pid
    end, lists:seq(1, ProducerCount)).

-spec producer_loop(pos_integer(), throughput_config(), [term()], pos_integer()) -> ok.
producer_loop(ProducerId, Config, TargetActors, TotalMessages) ->
    ?LOG_DEBUG("Producer ~p starting with ~p total messages", [ProducerId, TotalMessages]),
    
    BatchSize = Config#throughput_config.batch_size,
    MessageSize = Config#throughput_config.message_size,
    
    %% Generate message payload once
    MessagePayload = generate_test_payload(MessageSize),
    
    %% Send messages in batches
    send_messages_batched(ProducerId, TargetActors, MessagePayload, TotalMessages, BatchSize, 0, 0).

-spec send_messages_batched(pos_integer(), [term()], binary(), pos_integer(), 
                           pos_integer(), non_neg_integer(), non_neg_integer()) -> ok.
send_messages_batched(_ProducerId, _TargetActors, _Payload, TotalMessages, _BatchSize, SentCount, _ErrorCount) 
  when SentCount >= TotalMessages ->
    ok;
send_messages_batched(ProducerId, TargetActors, Payload, TotalMessages, BatchSize, SentCount, ErrorCount) ->
    BatchStart = erlang:monotonic_time(microsecond),
    
    %% Send batch of messages
    {NewSentCount, NewErrorCount} = send_message_batch(TargetActors, Payload, 
                                                       min(BatchSize, TotalMessages - SentCount), 
                                                       SentCount, ErrorCount),
    
    BatchEnd = erlang:monotonic_time(microsecond),
    BatchDuration = BatchEnd - BatchStart,
    
    %% Adaptive rate limiting to maintain target throughput
    TargetBatchTime = (BatchSize * 1000000) div (?TARGET_THROUGHPUT div length(TargetActors)),
    
    if BatchDuration < TargetBatchTime ->
        timer:sleep((TargetBatchTime - BatchDuration) div 1000);
       true -> ok
    end,
    
    send_messages_batched(ProducerId, TargetActors, Payload, TotalMessages, BatchSize, 
                         NewSentCount, NewErrorCount).

-spec send_message_batch([term()], binary(), pos_integer(), non_neg_integer(), non_neg_integer()) 
                        -> {non_neg_integer(), non_neg_integer()}.
send_message_batch(TargetActors, Payload, BatchSize, SentCount, ErrorCount) ->
    lists:foldl(fun(_, {Sent, Errors}) ->
        Actor = lists:nth(rand:uniform(length(TargetActors)), TargetActors),
        
        case catch bitactor_server:send_message(Actor, Payload) of
            ok -> {Sent + 1, Errors};
            _ -> {Sent, Errors + 1}
        end
    end, {SentCount, ErrorCount}, lists:seq(1, BatchSize)).

%%%===================================================================
%%% Metrics Collection
%%%===================================================================

-spec collect_throughput_metrics() -> ok.
collect_throughput_metrics() ->
    ets:new(throughput_metrics, [named_table, public, set]),
    throughput_metrics_loop().

throughput_metrics_loop() ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Collect system metrics
    {ok, ActorCount, TotalMessages, TotalTicks, _} = bitactor_nif:get_stats(),
    
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    
    Metrics = #{
        timestamp => StartTime,
        actor_count => ActorCount,
        total_messages => TotalMessages,
        total_ticks => TotalTicks,
        memory_total => maps:get(total, Memory),
        memory_processes => maps:get(processes, Memory),
        process_count => ProcessCount,
        run_queue => erlang:statistics(run_queue)
    },
    
    ets:insert(throughput_metrics, {StartTime, Metrics}),
    
    timer:sleep(100), % Sample every 100ms for high resolution
    throughput_metrics_loop().

-spec reset_throughput_metrics() -> ok.
reset_throughput_metrics() ->
    case ets:whereis(throughput_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(throughput_metrics)
    end,
    ok.

%%%===================================================================
%%% Result Processing
%%%===================================================================

-spec collect_producer_results([{pid(), reference()}], pos_integer()) -> throughput_result().
collect_producer_results(Producers, Duration) ->
    Results = collect_all_producer_results(Producers, []),
    
    TotalSent = lists:sum([Sent || {Sent, _} <- Results]),
    TotalErrors = lists:sum([Errors || {_, Errors} <- Results]),
    
    ThroughputMsgsSec = (TotalSent * 1000) / Duration,
    
    %% Get metrics from collection
    Metrics = get_collected_metrics(),
    
    #throughput_result{
        msgs_sent = TotalSent,
        msgs_received = TotalSent - TotalErrors,
        duration_ms = Duration,
        throughput_msgs_sec = ThroughputMsgsSec,
        peak_throughput_msgs_sec = calculate_peak_throughput(Metrics),
        sustained_throughput_msgs_sec = ThroughputMsgsSec,
        error_count = TotalErrors,
        queue_depths = extract_queue_depths(Metrics),
        backpressure_events = count_backpressure_events(Metrics)
    }.

-spec collect_all_producer_results([{pid(), reference()}], [{non_neg_integer(), non_neg_integer()}]) 
                                  -> [{non_neg_integer(), non_neg_integer()}].
collect_all_producer_results([], Acc) ->
    Acc;
collect_all_producer_results([{Pid, Ref} | Rest], Acc) ->
    Result = receive
        {'DOWN', Ref, process, Pid, normal} ->
            {0, 0}; % Normal shutdown
        {'DOWN', Ref, process, Pid, {producer_result, Sent, Errors}} ->
            {Sent, Errors};
        {'DOWN', Ref, process, Pid, _} ->
            {0, 1} % Error case
    after 5000 ->
        {0, 1} % Timeout
    end,
    collect_all_producer_results(Rest, [Result | Acc]).

-spec get_collected_metrics() -> [#{atom() => term()}].
get_collected_metrics() ->
    case ets:whereis(throughput_metrics) of
        undefined -> [];
        _ ->
            Keys = ets:select(throughput_metrics, [{'$1', [], ['$1']}]),
            [element(2, hd(ets:lookup(throughput_metrics, Key))) || Key <- Keys]
    end.

-spec calculate_peak_throughput([#{atom() => term()}]) -> float().
calculate_peak_throughput(Metrics) when length(Metrics) < 2 ->
    0.0;
calculate_peak_throughput(Metrics) ->
    SortedMetrics = lists:sort(fun(A, B) -> 
        maps:get(timestamp, A) =< maps:get(timestamp, B) 
    end, Metrics),
    
    MessageDeltas = calculate_message_deltas(SortedMetrics),
    case MessageDeltas of
        [] -> 0.0;
        _ -> lists:max(MessageDeltas)
    end.

-spec calculate_message_deltas([#{atom() => term()}]) -> [float()].
calculate_message_deltas([_]) -> [];
calculate_message_deltas([M1, M2 | Rest]) ->
    TimeDelta = maps:get(timestamp, M2) - maps:get(timestamp, M1),
    MsgDelta = maps:get(total_messages, M2) - maps:get(total_messages, M1),
    
    ThroughputMsgsSec = if TimeDelta > 0 -> (MsgDelta * 1000) / TimeDelta; true -> 0.0 end,
    
    [ThroughputMsgsSec | calculate_message_deltas([M2 | Rest])].

-spec extract_queue_depths([#{atom() => term()}]) -> [non_neg_integer()].
extract_queue_depths(Metrics) ->
    [maps:get(run_queue, M, 0) || M <- Metrics].

-spec count_backpressure_events([#{atom() => term()}]) -> non_neg_integer().
count_backpressure_events(Metrics) ->
    length([M || M <- Metrics, maps:get(run_queue, M, 0) > 10]).

-spec aggregate_burst_results([throughput_result()], throughput_config()) -> throughput_result().
aggregate_burst_results(Results, _Config) ->
    TotalSent = lists:sum([R#throughput_result.msgs_sent || R <- Results]),
    TotalReceived = lists:sum([R#throughput_result.msgs_received || R <- Results]),
    TotalErrors = lists:sum([R#throughput_result.error_count || R <- Results]),
    AvgDuration = lists:sum([R#throughput_result.duration_ms || R <- Results]) / length(Results),
    
    Throughputs = [R#throughput_result.throughput_msgs_sec || R <- Results],
    PeakThroughputs = [R#throughput_result.peak_throughput_msgs_sec || R <- Results],
    
    #throughput_result{
        msgs_sent = TotalSent,
        msgs_received = TotalReceived,
        duration_ms = round(AvgDuration),
        throughput_msgs_sec = lists:sum(Throughputs) / length(Throughputs),
        peak_throughput_msgs_sec = lists:max(PeakThroughputs),
        sustained_throughput_msgs_sec = lists:min(Throughputs),
        error_count = TotalErrors,
        queue_depths = lists:flatten([R#throughput_result.queue_depths || R <- Results]),
        backpressure_events = lists:sum([R#throughput_result.backpressure_events || R <- Results])
    }.

%%%===================================================================
%%% System Preparation and Cleanup
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end.

-spec prepare_system_for_throughput() -> ok.
prepare_system_for_throughput() ->
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Reset telemetry
    catch bitactor_telemetry:reset_metrics(),
    
    %% Warm up allocators
    warmup_for_throughput(),
    
    ok.

-spec warmup_for_throughput() -> ok.
warmup_for_throughput() ->
    %% Create and destroy actors to warm up memory allocators
    WarmupActors = [begin
        {ok, A, _} = bitactor_server:spawn_actor(warmup, #{}),
        A
    end || _ <- lists:seq(1, 100)],
    
    %% Send warm-up messages
    [catch bitactor_server:send_message(A, <<"warmup">>) || A <- WarmupActors],
    
    %% Cleanup
    [catch bitactor_server:kill_actor(A) || A <- WarmupActors],
    
    timer:sleep(500),
    erlang:garbage_collect(),
    ok.

-spec create_target_actors(pos_integer()) -> [term()].
create_target_actors(Count) ->
    ?LOG_DEBUG("Creating ~p target actors for throughput testing", [Count]),
    
    lists:map(fun(N) ->
        {ok, Actor, _} = bitactor_server:spawn_actor(throughput_target, #{id => N}),
        Actor
    end, lists:seq(1, Count)).

-spec cleanup_target_actors([term()]) -> ok.
cleanup_target_actors(Actors) ->
    ?LOG_DEBUG("Cleaning up ~p target actors", [length(Actors)]),
    [catch bitactor_server:kill_actor(A) || A <- Actors],
    ok.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

-spec configure_throughput_test(map()) -> throughput_config().
configure_throughput_test(Config) ->
    #throughput_config{
        target_msgs_sec = maps:get(target_throughput, Config, ?TARGET_THROUGHPUT),
        duration_ms = maps:get(duration_ms, Config, ?MEASUREMENT_DURATION_MS),
        producer_count = maps:get(producer_count, Config, 4),
        batch_size = maps:get(batch_size, Config, ?BATCH_SIZE),
        message_size = maps:get(message_size, Config, 256),
        burst_count = maps:get(burst_count, Config, 5)
    }.

-spec generate_test_payload(pos_integer()) -> binary().
generate_test_payload(Size) when Size >= 16 ->
    Timestamp = erlang:monotonic_time(nanosecond),
    SequenceNum = rand:uniform(1000000),
    HeaderSize = 16,
    PayloadSize = Size - HeaderSize,
    
    Payload = if PayloadSize > 0 -> crypto:strong_rand_bytes(PayloadSize); true -> <<>> end,
    
    <<Timestamp:64/native, SequenceNum:32/native, 0:32, Payload/binary>>;
generate_test_payload(_Size) ->
    Timestamp = erlang:monotonic_time(nanosecond),
    <<Timestamp:64/native>>.

-spec format_throughput_results(throughput_result(), throughput_config()) -> #{atom() => term()}.
format_throughput_results(Result, Config) ->
    #{
        test_type => throughput_validation,
        target_throughput => Config#throughput_config.target_msgs_sec,
        
        %% Core throughput metrics
        throughput_msgs_sec => Result#throughput_result.throughput_msgs_sec,
        peak_throughput_msgs_sec => Result#throughput_result.peak_throughput_msgs_sec,
        sustained_throughput_msgs_sec => Result#throughput_result.sustained_throughput_msgs_sec,
        
        %% Message statistics
        msgs_sent => Result#throughput_result.msgs_sent,
        msgs_received => Result#throughput_result.msgs_received,
        error_count => Result#throughput_result.error_count,
        error_rate => Result#throughput_result.error_count / max(Result#throughput_result.msgs_sent, 1),
        
        %% System performance
        duration_ms => Result#throughput_result.duration_ms,
        backpressure_events => Result#throughput_result.backpressure_events,
        avg_queue_depth => case Result#throughput_result.queue_depths of
            [] -> 0;
            QueueDepths -> lists:sum(QueueDepths) / length(QueueDepths)
        end,
        max_queue_depth => case Result#throughput_result.queue_depths of
            [] -> 0;
            QueueDepths -> lists:max(QueueDepths)
        end,
        
        %% Validation flags
        target_met => Result#throughput_result.throughput_msgs_sec >= Config#throughput_config.target_msgs_sec,
        uhft_compliant => Result#throughput_result.throughput_msgs_sec >= ?TARGET_THROUGHPUT andalso
                         Result#throughput_result.error_rate < 0.001,
        
        timestamp => erlang:system_time(second)
    }.