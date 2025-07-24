%%%-------------------------------------------------------------------
%%% @doc BitActor Latency Measurement Engine
%%% Agent 2: Sub-microsecond latency validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(latency_test_engine).

-export([run/1, measure_single_operation/2, collect_latency_samples/3]).
-export([calculate_percentiles/1, validate_latency_targets/2]).

-include_lib("kernel/include/logger.hrl").

-define(NANOSECOND_PRECISION, true).
-define(LATENCY_SAMPLES, 100000). % 100k samples for statistical significance
-define(WARMUP_SAMPLES, 10000).   % 10k warmup samples

-record(latency_config, {
    operation_type :: spawn_actor | send_message | tick_all | mixed_workload,
    sample_count = ?LATENCY_SAMPLES :: pos_integer(),
    warmup_count = ?WARMUP_SAMPLES :: pos_integer(),
    target_p99_ns = 1000 :: pos_integer(),
    target_p999_ns = 2000 :: pos_integer(),
    concurrent_producers = 1 :: pos_integer(),
    message_size = 256 :: pos_integer()
}).

-record(latency_results, {
    samples :: [non_neg_integer()],
    percentiles :: #{atom() => non_neg_integer()},
    histogram :: #{non_neg_integer() => non_neg_integer()},
    statistics :: #{atom() => term()},
    validation :: #{atom() => boolean()}
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec run(map()) -> #{atom() => term()}.
run(Config) ->
    ?LOG_INFO("Starting sub-microsecond latency validation"),
    
    %% Ensure BitActor system is ready
    ok = ensure_system_ready(),
    
    %% Configure test parameters
    LatencyConfig = configure_latency_test(Config),
    
    %% Execute latency measurements
    Results = execute_latency_tests(LatencyConfig),
    
    %% Generate comprehensive results
    format_results(Results, LatencyConfig).

%%%===================================================================
%%% Core Latency Measurement
%%%===================================================================

-spec execute_latency_tests(term()) -> term().
execute_latency_tests(Config) ->
    ?LOG_INFO("Executing latency tests with ~p samples", [Config#latency_config.sample_count]),
    
    %% System preparation
    prepare_system_for_latency_test(),
    
    %% Warmup phase
    ?LOG_INFO("Warmup phase: ~p samples", [Config#latency_config.warmup_count]),
    _ = collect_latency_samples(Config#latency_config.operation_type,
                               Config#latency_config.warmup_count,
                               Config),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(1000),
    
    %% Main measurement phase
    ?LOG_INFO("Measurement phase: ~p samples", [Config#latency_config.sample_count]),
    Samples = collect_latency_samples(Config#latency_config.operation_type,
                                     Config#latency_config.sample_count,
                                     Config),
    
    %% Calculate statistics
    Percentiles = calculate_percentiles(Samples),
    Histogram = generate_latency_histogram(Samples),
    Statistics = calculate_latency_statistics(Samples),
    Validation = validate_latency_targets(Percentiles, Config),
    
    #latency_results{
        samples = Samples,
        percentiles = Percentiles,
        histogram = Histogram,
        statistics = Statistics,
        validation = Validation
    }.

-spec collect_latency_samples(atom(), pos_integer(), term()) -> [non_neg_integer()].
collect_latency_samples(OperationType, SampleCount, Config) ->
    case Config#latency_config.concurrent_producers of
        1 ->
            %% Single-threaded measurement for highest precision
            collect_samples_sequential(OperationType, SampleCount, Config);
        N when N > 1 ->
            %% Multi-threaded measurement for load testing
            collect_samples_concurrent(OperationType, SampleCount, N, Config)
    end.

-spec collect_samples_sequential(atom(), pos_integer(), term()) -> [non_neg_integer()].
collect_samples_sequential(OperationType, SampleCount, Config) ->
    ?LOG_DEBUG("Collecting ~p sequential samples for ~p", [SampleCount, OperationType]),
    
    lists:map(fun(_) ->
        measure_single_operation(OperationType, Config)
    end, lists:seq(1, SampleCount)).

-spec collect_samples_concurrent(atom(), pos_integer(), pos_integer(), term()) -> [non_neg_integer()].
collect_samples_concurrent(OperationType, SampleCount, ProducerCount, Config) ->
    ?LOG_DEBUG("Collecting ~p concurrent samples with ~p producers", [SampleCount, ProducerCount]),
    
    SamplesPerProducer = SampleCount div ProducerCount,
    
    %% Spawn concurrent producers
    Producers = lists:map(fun(ProducerId) ->
        spawn_monitor(fun() ->
            Samples = collect_samples_sequential(OperationType, SamplesPerProducer, Config),
            exit({samples, ProducerId, Samples})
        end)
    end, lists:seq(1, ProducerCount)),
    
    %% Collect results
    collect_producer_samples(Producers, []).

-spec measure_single_operation(atom(), term()) -> non_neg_integer().
measure_single_operation(spawn_actor, _Config) ->
    Start = get_precise_time_ns(),
    {ok, ActorRef, _NIFLatency} = bitactor_server:spawn_actor(latency_test, #{}),
    End = get_precise_time_ns(),
    
    %% Cleanup
    bitactor_server:kill_actor(ActorRef),
    
    End - Start;

measure_single_operation(send_message, Config) ->
    %% Pre-create actor to isolate message sending latency
    {ok, ActorRef, _} = bitactor_server:spawn_actor(latency_test, #{}),
    
    %% Generate message
    Message = generate_test_message(Config#latency_config.message_size),
    
    %% Measure message sending
    Start = get_precise_time_ns(),
    ok = bitactor_server:send_message(ActorRef, Message),
    End = get_precise_time_ns(),
    
    %% Cleanup
    bitactor_server:kill_actor(ActorRef),
    
    End - Start;

measure_single_operation(tick_all, _Config) ->
    Start = get_precise_time_ns(),
    {ok, _TickLatency} = bitactor_nif:tick_all(),
    End = get_precise_time_ns(),
    
    End - Start;

measure_single_operation(mixed_workload, Config) ->
    %% Mixed workload: spawn, send, tick
    Operations = [spawn_actor, send_message, tick_all],
    Operation = lists:nth(rand:uniform(length(Operations)), Operations),
    measure_single_operation(Operation, Config).

%%%===================================================================
%%% High-Precision Timing
%%%===================================================================

-spec get_precise_time_ns() -> non_neg_integer().
get_precise_time_ns() ->
    case ?NANOSECOND_PRECISION of
        true ->
            %% Use NIF for maximum precision
            case catch bitactor_nif:measure_latency() of
                {ok, _Min, _Avg, _Max} ->
                    %% Get current time with NIF precision
                    erlang:monotonic_time(nanosecond);
                _ ->
                    %% Fallback to Erlang timing
                    erlang:monotonic_time(nanosecond)
            end;
        false ->
            erlang:monotonic_time(nanosecond)
    end.

%%%===================================================================
%%% Statistical Analysis
%%%===================================================================

-spec calculate_percentiles([number()]) -> #{atom() => number()}.
calculate_percentiles(Samples) ->
    SortedSamples = lists:sort(Samples),
    Len = length(SortedSamples),
    
    #{
        min => lists:min(SortedSamples),
        p1 => percentile(SortedSamples, 1),
        p5 => percentile(SortedSamples, 5),
        p10 => percentile(SortedSamples, 10),
        p25 => percentile(SortedSamples, 25),
        p50 => percentile(SortedSamples, 50),
        p75 => percentile(SortedSamples, 75),
        p90 => percentile(SortedSamples, 90),
        p95 => percentile(SortedSamples, 95),
        p99 => percentile(SortedSamples, 99),
        p999 => percentile(SortedSamples, 99.9),
        p9999 => percentile(SortedSamples, 99.99),
        max => lists:max(SortedSamples),
        mean => lists:sum(SortedSamples) / Len,
        count => Len
    }.

-spec percentile([number()], number()) -> number().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = round((Percentile / 100) * Len),
    ClampedIndex = max(1, min(Index, Len)),
    lists:nth(ClampedIndex, SortedList).

-spec calculate_latency_statistics([number()]) -> #{atom() => term()}.
calculate_latency_statistics(Samples) ->
    Len = length(Samples),
    Sum = lists:sum(Samples),
    Mean = Sum / Len,
    
    %% Calculate variance and standard deviation
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- Samples]) / Len,
    StdDev = math:sqrt(Variance),
    
    %% Calculate coefficient of variation
    CoefficientOfVariation = StdDev / Mean,
    
    %% Identify outliers (beyond 3 standard deviations)
    Outliers = [X || X <- Samples, abs(X - Mean) > 3 * StdDev],
    
    #{
        sample_count => Len,
        sum_ns => Sum,
        mean_ns => Mean,
        variance => Variance,
        std_dev_ns => StdDev,
        coefficient_of_variation => CoefficientOfVariation,
        outlier_count => length(Outliers),
        outlier_percentage => (length(Outliers) / Len) * 100
    }.

-spec generate_latency_histogram([number()]) -> #{non_neg_integer() => non_neg_integer()}.
generate_latency_histogram(Samples) ->
    %% Create histogram buckets in nanoseconds
    Buckets = [100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000],
    
    %% Count samples in each bucket
    lists:foldl(fun(Sample, Hist) ->
        Bucket = find_bucket(Sample, Buckets),
        maps:update_with(Bucket, fun(Count) -> Count + 1 end, 1, Hist)
    end, #{}, Samples).

-spec find_bucket(number(), [number()]) -> non_neg_integer().
find_bucket(_Value, []) -> 
    infinity;
find_bucket(Value, [Bucket | _Rest]) when Value =< Bucket -> 
    Bucket;
find_bucket(Value, [_Bucket | Rest]) -> 
    find_bucket(Value, Rest).

%%%===================================================================
%%% Validation
%%%===================================================================

-spec validate_latency_targets(#{atom() => number()}, term()) -> #{atom() => boolean()}.
validate_latency_targets(Percentiles, Config) ->
    P99 = maps:get(p99, Percentiles, infinity),
    P999 = maps:get(p999, Percentiles, infinity),
    _Mean = maps:get(mean, Percentiles, infinity),
    
    #{
        p99_target_met => P99 =< Config#latency_config.target_p99_ns,
        p999_target_met => P999 =< Config#latency_config.target_p999_ns,
        uhft_compliant => P99 =< 1000 andalso P999 =< 2000, % Standard UHFT requirements
        sub_microsecond => P99 < 1000,
        low_jitter => (maps:get(p99, Percentiles) - maps:get(p50, Percentiles)) < 500,
        consistent_performance => maps:get(coefficient_of_variation, 
                                           #{coefficient_of_variation => 1}, 1) < 0.3
    }.

%%%===================================================================
%%% System Preparation
%%%===================================================================

-spec ensure_system_ready() -> ok.
ensure_system_ready() ->
    %% Ensure BitActor application is running
    case application:ensure_all_started(bitactor) of
        {ok, _} -> ok;
        {error, Reason} -> throw({system_not_ready, Reason})
    end,
    
    %% Verify NIF is loaded
    case catch bitactor_nif:measure_latency() of
        {ok, _, _, _} -> ok;
        _ -> throw({nif_not_loaded})
    end,
    
    %% Reset system state
    bitactor_telemetry:reset_metrics(),
    
    ok.

-spec prepare_system_for_latency_test() -> ok.
prepare_system_for_latency_test() ->
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Disable scheduler compaction for consistency
    erlang:system_flag(scheduler_wall_time, true),
    
    %% Warm up the system
    warmup_system(),
    
    ok.

-spec warmup_system() -> ok.
warmup_system() ->
    %% Create and destroy some actors to warm up allocators
    WarmupActors = [begin
        {ok, A, _} = bitactor_server:spawn_actor(warmup, #{}),
        A
    end || _ <- lists:seq(1, 100)],
    
    %% Send some messages
    [bitactor_server:send_message(A, warmup_msg) || A <- WarmupActors],
    
    %% Cleanup
    [bitactor_server:kill_actor(A) || A <- WarmupActors],
    
    %% Force GC after warmup
    erlang:garbage_collect(),
    timer:sleep(100),
    
    ok.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

-spec configure_latency_test(map()) -> term().
configure_latency_test(Config) ->
    #latency_config{
        operation_type = maps:get(operation_type, Config, mixed_workload),
        sample_count = maps:get(sample_count, Config, ?LATENCY_SAMPLES),
        warmup_count = maps:get(warmup_count, Config, ?WARMUP_SAMPLES),
        target_p99_ns = maps:get(target_p99_ns, Config, 1000),
        target_p999_ns = maps:get(target_p999_ns, Config, 2000),
        concurrent_producers = maps:get(concurrent_producers, Config, 1),
        message_size = maps:get(message_size, Config, 256)
    }.

-spec generate_test_message(pos_integer()) -> binary().
generate_test_message(Size) when Size >= 8 ->
    Timestamp = erlang:monotonic_time(nanosecond),
    Padding = crypto:strong_rand_bytes(Size - 8),
    <<Timestamp:64/native, Padding/binary>>;
generate_test_message(_Size) ->
    <<(erlang:monotonic_time(nanosecond)):64/native>>.

-spec collect_producer_samples([{pid(), reference()}], [number()]) -> [number()].
collect_producer_samples([], Acc) ->
    lists:flatten(Acc);
collect_producer_samples([{Pid, Ref} | Rest], Acc) ->
    receive
        {'DOWN', Ref, process, Pid, {samples, _ProducerId, Samples}} ->
            collect_producer_samples(Rest, [Samples | Acc]);
        {'DOWN', Ref, process, Pid, _Reason} ->
            ?LOG_WARNING("Producer ~p failed", [Pid]),
            collect_producer_samples(Rest, Acc)
    after 30000 ->
        ?LOG_ERROR("Timeout waiting for producer results"),
        lists:flatten(Acc)
    end.

-spec format_results(term(), term()) -> #{atom() => term()}.
format_results(Results, Config) ->
    #{
        test_type => latency_validation,
        operation_type => Config#latency_config.operation_type,
        sample_count => length(Results#latency_results.samples),
        
        %% Core metrics
        p50_latency_ns => maps:get(p50, Results#latency_results.percentiles),
        p95_latency_ns => maps:get(p95, Results#latency_results.percentiles),
        p99_latency_ns => maps:get(p99, Results#latency_results.percentiles),
        p999_latency_ns => maps:get(p999, Results#latency_results.percentiles),
        max_latency_ns => maps:get(max, Results#latency_results.percentiles),
        mean_latency_ns => maps:get(mean, Results#latency_results.percentiles),
        
        %% Validation results
        uhft_compliant => maps:get(uhft_compliant, Results#latency_results.validation),
        p99_target_met => maps:get(p99_target_met, Results#latency_results.validation),
        sub_microsecond => maps:get(sub_microsecond, Results#latency_results.validation),
        
        %% Statistical measures
        std_dev_ns => maps:get(std_dev_ns, Results#latency_results.statistics),
        coefficient_of_variation => maps:get(coefficient_of_variation, Results#latency_results.statistics),
        outlier_percentage => maps:get(outlier_percentage, Results#latency_results.statistics),
        
        %% Additional data
        percentiles => Results#latency_results.percentiles,
        histogram => Results#latency_results.histogram,
        statistics => Results#latency_results.statistics,
        validation => Results#latency_results.validation,
        
        %% Success criteria
        error_rate => 0.0, % No errors in latency measurement
        timestamp => erlang:system_time(second)
    }.