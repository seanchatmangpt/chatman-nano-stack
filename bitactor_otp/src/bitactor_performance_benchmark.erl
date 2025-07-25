%%%-------------------------------------------------------------------
%%% @doc BitActor Performance Benchmark Suite
%%% Comprehensive performance testing with OTEL metrics
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_performance_benchmark).

%% API
-export([run_all/0, run_all/1]).
-export([run_benchmark/1, run_benchmark/2]).
-export([stress_test/1]).
-export([generate_report/0]).

%% Individual benchmarks
-export([latency_benchmark/1]).
-export([throughput_benchmark/1]).
-export([scalability_benchmark/1]).
-export([memory_benchmark/1]).
-export([uhft_benchmark/1]).

-record(benchmark_result, {
    name :: atom(),
    category :: atom(),
    metrics :: map(),
    otel_traces :: [map()],
    status :: passed | failed | degraded,
    details :: map()
}).

-record(benchmark_report, {
    timestamp :: integer(),
    duration_ms :: integer(),
    results :: [#benchmark_result{}],
    summary :: map(),
    otel_aggregated :: map()
}).

-define(DEFAULT_ITERATIONS, 10000).
-define(WARMUP_ITERATIONS, 1000).
-define(UHFT_LATENCY_TARGET_NS, 500).
-define(UHFT_THROUGHPUT_TARGET, 1000000). % 1M ops/sec

%%%===================================================================
%%% API
%%%===================================================================

-spec run_all() -> {ok, #benchmark_report{}} | {error, term()}.
run_all() ->
    run_all(#{}).

-spec run_all(map()) -> {ok, #benchmark_report{}} | {error, term()}.
run_all(Options) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Initialize telemetry (graceful handling)
    case catch bitactor_telemetry:reset_metrics() of
        ok -> ok;
        _ -> io:format("  Warning: telemetry not available~n")
    end,
    
    %% Enable OTEL tracing
    setup_otel_tracing(),
    
    io:format("~n=== BitActor Performance Benchmark Suite ===~n"),
    io:format("Starting comprehensive performance analysis...~n~n"),
    
    %% Run warmup
    warmup_system(),
    
    %% Run all benchmarks
    Benchmarks = [
        {latency, fun latency_benchmark/1},
        {throughput, fun throughput_benchmark/1},
        {scalability, fun scalability_benchmark/1},
        {memory, fun memory_benchmark/1},
        {uhft, fun uhft_benchmark/1}
    ],
    
    Results = lists:map(fun({Name, Fun}) ->
        io:format("Running ~p benchmark...~n", [Name]),
        Result = Fun(Options),
        io:format("~p benchmark complete.~n~n", [Name]),
        Result
    end, Benchmarks),
    
    %% Run stress test
    StressResult = stress_test(Options),
    AllResults = Results ++ [StressResult],
    
    EndTime = erlang:monotonic_time(millisecond),
    
    %% Generate report
    Report = compile_report(AllResults, EndTime - StartTime),
    
    %% Save report
    erlang:put(last_benchmark_report, Report),
    {ok, Report}.

-spec run_benchmark(atom()) -> #benchmark_result{}.
run_benchmark(BenchmarkName) ->
    run_benchmark(BenchmarkName, #{}).

-spec run_benchmark(atom(), map()) -> #benchmark_result{}.
run_benchmark(BenchmarkName, Options) ->
    case BenchmarkName of
        latency -> latency_benchmark(Options);
        throughput -> throughput_benchmark(Options);
        scalability -> scalability_benchmark(Options);
        memory -> memory_benchmark(Options);
        uhft -> uhft_benchmark(Options);
        _ -> error({unknown_benchmark, BenchmarkName})
    end.

-spec stress_test(map()) -> #benchmark_result{}.
stress_test(Options) ->
    io:format("Running stress test...~n"),
    
    %% Configuration
    ActorCount = maps:get(actor_count, Options, 5000),
    MessageCount = maps:get(message_count, Options, 1000000),
    Duration = maps:get(duration_ms, Options, 60000), % 60 seconds
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(stress_test),
    
    %% Phase 1: Spawn actors rapidly
    SpawnStart = erlang:monotonic_time(millisecond),
    Actors = spawn_actors_batch(ActorCount),
    SpawnDuration = erlang:monotonic_time(millisecond) - SpawnStart,
    
    %% Phase 2: Message bombardment
    MessageStart = erlang:monotonic_time(millisecond),
    send_messages_concurrent(Actors, MessageCount),
    MessageDuration = erlang:monotonic_time(millisecond) - MessageStart,
    
    %% Phase 3: Sustained load
    _SustainedStart = erlang:monotonic_time(millisecond),
    SustainedStats = sustained_load_test(Actors, Duration),
    
    %% Phase 4: Cleanup under load
    CleanupStart = erlang:monotonic_time(millisecond),
    cleanup_actors_concurrent(Actors),
    CleanupDuration = erlang:monotonic_time(millisecond) - CleanupStart,
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Calculate metrics
    Metrics = #{
        actors_spawned => ActorCount,
        spawn_rate => (ActorCount * 1000) div SpawnDuration,
        messages_sent => MessageCount,
        message_rate => (MessageCount * 1000) div MessageDuration,
        sustained_throughput => maps:get(throughput, SustainedStats),
        cleanup_rate => (ActorCount * 1000) div CleanupDuration,
        total_duration_ms => erlang:monotonic_time(millisecond) - SpawnStart
    },
    
    %% Determine status
    Status = evaluate_stress_test(Metrics),
    
    #benchmark_result{
        name = stress_test,
        category = stress,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            phases => [spawn, message, sustained, cleanup],
            sustained_stats => SustainedStats
        }
    }.

-spec generate_report() -> {ok, binary()} | {error, no_report}.
generate_report() ->
    case erlang:get(last_benchmark_report) of
        undefined -> {error, no_report};
        Report -> {ok, format_benchmark_report(Report)}
    end.

%%%===================================================================
%%% Individual Benchmarks
%%%===================================================================

-spec latency_benchmark(map()) -> #benchmark_result{}.
latency_benchmark(Options) ->
    Iterations = maps:get(iterations, Options, ?DEFAULT_ITERATIONS),
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(latency_benchmark),
    
    %% Measure spawn latency
    SpawnLatencies = measure_spawn_latency(Iterations),
    
    %% Measure message latency
    MessageLatencies = measure_message_latency(Iterations),
    
    %% Measure tick latency
    TickLatencies = measure_tick_latency(Iterations),
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Calculate statistics
    Metrics = #{
        spawn_latency => calculate_latency_stats(SpawnLatencies),
        message_latency => calculate_latency_stats(MessageLatencies),
        tick_latency => calculate_latency_stats(TickLatencies),
        iterations => Iterations
    },
    
    %% Evaluate against targets
    Status = evaluate_latency_metrics(Metrics),
    
    #benchmark_result{
        name = latency,
        category = performance,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            targets => #{
                spawn => <<"< 1ms">>,
                message => <<"< 10μs">>,
                tick => <<"< 100ns">>
            }
        }
    }.

-spec throughput_benchmark(map()) -> #benchmark_result{}.
throughput_benchmark(Options) ->
    %% Configuration
    ActorCounts = maps:get(actor_counts, Options, [10, 100, 1000, 5000]),
    MessageBatch = maps:get(message_batch, Options, 10000),
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(throughput_benchmark),
    
    %% Test different actor counts
    Results = lists:map(fun(ActorCount) ->
        %% Spawn actors
        Actors = spawn_actors_batch(ActorCount),
        
        %% Measure message throughput
        Start = erlang:monotonic_time(millisecond),
        send_messages_uniform(Actors, MessageBatch),
        Duration = erlang:monotonic_time(millisecond) - Start,
        
        Throughput = (MessageBatch * 1000) div max(Duration, 1),
        
        %% Measure tick throughput
        TickStart = erlang:monotonic_time(millisecond),
        [bitactor_nif:tick_all() || _ <- lists:seq(1, 1000)],
        TickDuration = erlang:monotonic_time(millisecond) - TickStart,
        
        TickThroughput = (ActorCount * 1000 * 1000) div max(TickDuration, 1),
        
        %% Cleanup
        cleanup_actors_concurrent(Actors),
        
        #{
            actor_count => ActorCount,
            message_throughput => Throughput,
            tick_throughput => TickThroughput,
            messages_per_actor => MessageBatch div ActorCount
        }
    end, ActorCounts),
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Aggregate metrics
    Metrics = #{
        configurations => Results,
        max_message_throughput => lists:max([maps:get(message_throughput, R) || R <- Results]),
        max_tick_throughput => lists:max([maps:get(tick_throughput, R) || R <- Results])
    },
    
    %% Evaluate
    Status = evaluate_throughput_metrics(Metrics),
    
    #benchmark_result{
        name = throughput,
        category = performance,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            test_configurations => length(ActorCounts),
            message_batch_size => MessageBatch
        }
    }.

-spec scalability_benchmark(map()) -> #benchmark_result{}.
scalability_benchmark(Options) ->
    %% Test scaling characteristics
    ScalePoints = maps:get(scale_points, Options, [10, 50, 100, 500, 1000, 5000, 10000]),
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(scalability_benchmark),
    
    Results = lists:map(fun(ActorCount) ->
        %% Measure spawn scalability
        SpawnStart = erlang:monotonic_time(millisecond),
        Actors = spawn_actors_batch(ActorCount),
        SpawnDuration = erlang:monotonic_time(millisecond) - SpawnStart,
        
        %% Measure message scalability
        MessageCount = min(ActorCount * 100, 100000),
        MessageStart = erlang:monotonic_time(millisecond),
        send_messages_uniform(Actors, MessageCount),
        MessageDuration = erlang:monotonic_time(millisecond) - MessageStart,
        
        %% Measure memory usage
        MemoryBefore = erlang:memory(total),
        timer:sleep(100), % Let system stabilize
        MemoryAfter = erlang:memory(total),
        
        %% Cleanup
        CleanupStart = erlang:monotonic_time(millisecond),
        cleanup_actors_concurrent(Actors),
        CleanupDuration = erlang:monotonic_time(millisecond) - CleanupStart,
        
        #{
            actor_count => ActorCount,
            spawn_time_ms => SpawnDuration,
            spawn_rate => (ActorCount * 1000) div max(SpawnDuration, 1),
            message_time_ms => MessageDuration,
            message_rate => (MessageCount * 1000) div max(MessageDuration, 1),
            memory_per_actor => (MemoryAfter - MemoryBefore) div ActorCount,
            cleanup_time_ms => CleanupDuration
        }
    end, ScalePoints),
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Analyze scaling characteristics
    ScalingAnalysis = analyze_scaling(Results),
    
    Metrics = #{
        scale_points => Results,
        scaling_analysis => ScalingAnalysis
    },
    
    Status = evaluate_scalability_metrics(ScalingAnalysis),
    
    #benchmark_result{
        name = scalability,
        category = performance,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            max_tested_actors => lists:max(ScalePoints),
            scaling_efficiency => maps:get(efficiency, ScalingAnalysis)
        }
    }.

-spec memory_benchmark(map()) -> #benchmark_result{}.
memory_benchmark(_Options) ->
    %% Configuration
    TestConfigs = [
        #{name => small_actors, count => 10000, message_size => 100},
        #{name => medium_actors, count => 1000, message_size => 1024},
        #{name => large_actors, count => 100, message_size => 10240},
        #{name => xlarge_actors, count => 10, message_size => 102400}
    ],
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(memory_benchmark),
    
    %% Force GC before starting
    erlang:garbage_collect(),
    timer:sleep(100),
    BaselineMemory = erlang:memory(total),
    
    Results = lists:map(fun(Config) ->
        #{name := Name, count := Count, message_size := MsgSize} = Config,
        
        %% Spawn actors
        Actors = spawn_actors_batch(Count),
        AfterSpawnMemory = erlang:memory(total),
        
        %% Send messages
        Messages = [crypto:strong_rand_bytes(MsgSize) || _ <- lists:seq(1, 100)],
        [send_message_to_random(Actors, {data, Msg}) || Msg <- Messages],
        
        %% Let system stabilize
        timer:sleep(100),
        AfterMessageMemory = erlang:memory(total),
        
        %% Cleanup
        cleanup_actors_concurrent(Actors),
        erlang:garbage_collect(),
        timer:sleep(100),
        AfterCleanupMemory = erlang:memory(total),
        
        #{
            config => Name,
            actor_count => Count,
            message_size => MsgSize,
            spawn_memory => AfterSpawnMemory - BaselineMemory,
            message_memory => AfterMessageMemory - AfterSpawnMemory,
            memory_per_actor => (AfterSpawnMemory - BaselineMemory) div Count,
            cleanup_efficiency => (AfterCleanupMemory - BaselineMemory) / (AfterMessageMemory - BaselineMemory)
        }
    end, TestConfigs),
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Memory analysis
    MemoryAnalysis = analyze_memory_usage(Results),
    
    Metrics = #{
        configurations => Results,
        memory_analysis => MemoryAnalysis,
        baseline_memory => BaselineMemory
    },
    
    Status = evaluate_memory_metrics(MemoryAnalysis),
    
    #benchmark_result{
        name = memory,
        category = performance,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            test_configurations => length(TestConfigs),
            memory_efficiency => maps:get(efficiency_score, MemoryAnalysis)
        }
    }.

-spec uhft_benchmark(map()) -> #benchmark_result{}.
uhft_benchmark(_Options) ->
    %% UHFT-specific scenarios
    io:format("Running UHFT-specific benchmarks...~n"),
    
    %% Start OTEL span
    SpanCtx = otel_tracer:start_span(uhft_benchmark),
    
    %% UC1: Market Data Feed Handler
    UC1Result = uhft_market_data_benchmark(),
    
    %% UC2: Order Book Aggregator
    UC2Result = uhft_order_book_benchmark(),
    
    %% UC3: Alpha Calculator
    UC3Result = uhft_alpha_calculator_benchmark(),
    
    %% UC4: Risk Manager
    UC4Result = uhft_risk_manager_benchmark(),
    
    %% UC5: Execution Gateway
    UC5Result = uhft_execution_gateway_benchmark(),
    
    %% End OTEL span
    otel_tracer:end_span(SpanCtx),
    
    %% Aggregate results
    AllResults = [UC1Result, UC2Result, UC3Result, UC4Result, UC5Result],
    
    Metrics = #{
        scenarios => AllResults,
        all_targets_met => lists:all(fun(#{target_met := Met}) -> Met end, AllResults),
        average_latency => lists:sum([maps:get(avg_latency_ns, R) || R <- AllResults]) div length(AllResults)
    },
    
    Status = case maps:get(all_targets_met, Metrics) of
        true -> passed;
        false -> failed
    end,
    
    #benchmark_result{
        name = uhft,
        category = specialized,
        metrics = Metrics,
        otel_traces = get_otel_traces(SpanCtx),
        status = Status,
        details = #{
            use_cases => 5,
            critical_path => <<"Market Data -> Alpha -> Risk -> Execution">>
        }
    }.

%%%===================================================================
%%% UHFT Scenario Benchmarks
%%%===================================================================

uhft_market_data_benchmark() ->
    %% Create specialized actors
    {ok, Parser, _} = bitactor_server:spawn_actor(market_parser, #{}),
    {ok, Distributor, _} = bitactor_server:spawn_actor(feed_distributor, #{}),
    
    %% Generate realistic market data
    Symbols = [<<"AAPL">>, <<"GOOGL">>, <<"MSFT">>, <<"AMZN">>, <<"TSLA">>],
    TickCount = 100000,
    
    Ticks = [#{
        symbol => lists:nth((I rem length(Symbols)) + 1, Symbols),
        price => 100.0 + (I rem 100) / 100,
        volume => 100 + (I rem 1000),
        timestamp => erlang:monotonic_time(nanosecond)
    } || I <- lists:seq(1, TickCount)],
    
    %% Measure processing latency
    Latencies = lists:map(fun(Tick) ->
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(Parser, {parse, Tick}),
        ok = bitactor_server:send_message(Distributor, {distribute, Tick}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Ticks),
    
    %% Cleanup
    bitactor_server:kill_actor(Parser),
    bitactor_server:kill_actor(Distributor),
    
    %% Calculate stats
    Stats = calculate_latency_stats(Latencies),
    
    #{
        scenario => market_data_handler,
        ticks_processed => TickCount,
        avg_latency_ns => maps:get(avg, Stats),
        p99_latency_ns => maps:get(p99, Stats),
        target_met => maps:get(p99, Stats) < ?UHFT_LATENCY_TARGET_NS,
        throughput => (TickCount * 1000000000) div lists:sum(Latencies)
    }.

uhft_order_book_benchmark() ->
    %% Create exchange actors
    Exchanges = [nasdaq, nyse, bats, iex, arca],
    ExchangeActors = lists:map(fun(Ex) ->
        {ok, Actor, _} = bitactor_server:spawn_actor(exchange_feed, #{exchange => Ex}),
        {Ex, Actor}
    end, Exchanges),
    
    {ok, Aggregator, _} = bitactor_server:spawn_actor(book_aggregator, #{}),
    
    %% Generate order book updates
    UpdateCount = 50000,
    Updates = [#{
        exchange => lists:nth((I rem length(Exchanges)) + 1, Exchanges),
        side => case I rem 2 of 0 -> bid; 1 -> ask end,
        price => 100.0 + (I rem 20) / 10,
        quantity => 100 * (1 + (I rem 10)),
        timestamp => erlang:monotonic_time(nanosecond)
    } || I <- lists:seq(1, UpdateCount)],
    
    %% Measure aggregation latency
    Latencies = lists:map(fun(Update) ->
        Start = erlang:monotonic_time(nanosecond),
        Exchange = maps:get(exchange, Update),
        {Exchange, Actor} = lists:keyfind(Exchange, 1, ExchangeActors),
        ok = bitactor_server:send_message(Actor, {book_update, Update}),
        ok = bitactor_server:send_message(Aggregator, {aggregate, Exchange, Update}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Updates),
    
    %% Cleanup
    [bitactor_server:kill_actor(A) || {_, A} <- ExchangeActors],
    bitactor_server:kill_actor(Aggregator),
    
    Stats = calculate_latency_stats(Latencies),
    
    #{
        scenario => order_book_aggregator,
        updates_processed => UpdateCount,
        exchanges => length(Exchanges),
        avg_latency_ns => maps:get(avg, Stats),
        p99_latency_ns => maps:get(p99, Stats),
        target_met => maps:get(p99, Stats) < 1000, % 1μs target
        throughput => (UpdateCount * 1000000000) div lists:sum(Latencies)
    }.

uhft_alpha_calculator_benchmark() ->
    {ok, AlphaEngine, _} = bitactor_server:spawn_actor(alpha_engine, #{
        models => [momentum, mean_reversion, arbitrage, sentiment]
    }),
    
    %% Generate feature vectors
    FeatureCount = 10000,
    Features = [#{
        timestamp => erlang:monotonic_time(nanosecond),
        price_features => [rand:uniform() || _ <- lists:seq(1, 20)],
        volume_features => [rand:uniform() || _ <- lists:seq(1, 10)],
        market_features => [rand:uniform() || _ <- lists:seq(1, 15)],
        technical_features => [rand:uniform() || _ <- lists:seq(1, 25)]
    } || _ <- lists:seq(1, FeatureCount)],
    
    %% Measure calculation latency
    Latencies = lists:map(fun(Feature) ->
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(AlphaEngine, {calculate_alpha, Feature}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Features),
    
    bitactor_server:kill_actor(AlphaEngine),
    
    Stats = calculate_latency_stats(Latencies),
    
    #{
        scenario => alpha_calculator,
        calculations => FeatureCount,
        feature_dimensions => 70,
        avg_latency_ns => maps:get(avg, Stats),
        p99_latency_ns => maps:get(p99, Stats),
        target_met => maps:get(p99, Stats) < 10000, % 10μs target
        throughput => (FeatureCount * 1000000000) div lists:sum(Latencies)
    }.

uhft_risk_manager_benchmark() ->
    {ok, RiskEngine, _} = bitactor_server:spawn_actor(risk_engine, #{
        limits => #{
            position => 1000000,
            exposure => 5000000,
            var => 250000
        }
    }),
    
    %% Generate risk check requests
    RequestCount = 20000,
    Requests = [#{
        type => lists:nth((I rem 3) + 1, [position_check, exposure_check, var_check]),
        symbol => lists:nth((I rem 5) + 1, [<<"AAPL">>, <<"GOOGL">>, <<"MSFT">>, <<"AMZN">>, <<"TSLA">>]),
        quantity => 100 * (1 + (I rem 10)),
        price => 100.0 + (I rem 50),
        account => <<"ACC", (integer_to_binary(I rem 10))/binary>>,
        timestamp => erlang:monotonic_time(nanosecond)
    } || I <- lists:seq(1, RequestCount)],
    
    %% Measure risk check latency
    Latencies = lists:map(fun(Request) ->
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(RiskEngine, {check_risk, Request}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Requests),
    
    bitactor_server:kill_actor(RiskEngine),
    
    Stats = calculate_latency_stats(Latencies),
    
    #{
        scenario => risk_manager,
        checks_performed => RequestCount,
        check_types => 3,
        avg_latency_ns => maps:get(avg, Stats),
        p99_latency_ns => maps:get(p99, Stats),
        target_met => maps:get(p99, Stats) < 5000, % 5μs target
        throughput => (RequestCount * 1000000000) div lists:sum(Latencies)
    }.

uhft_execution_gateway_benchmark() ->
    {ok, Gateway, _} = bitactor_server:spawn_actor(execution_gateway, #{
        venues => [nasdaq, nyse, bats, iex, arca],
        routing_logic => smart_order_routing
    }),
    
    %% Generate orders
    OrderCount = 25000,
    Orders = [#{
        order_id => <<"ORD", (integer_to_binary(I))/binary>>,
        symbol => lists:nth((I rem 5) + 1, [<<"AAPL">>, <<"GOOGL">>, <<"MSFT">>, <<"AMZN">>, <<"TSLA">>]),
        side => case I rem 2 of 0 -> buy; 1 -> sell end,
        quantity => 100 * (1 + (I rem 10)),
        order_type => lists:nth((I rem 3) + 1, [market, limit, stop]),
        price => case (I rem 3) of 1 -> 100.0 + (I rem 20); _ -> undefined end,
        timestamp => erlang:monotonic_time(nanosecond)
    } || I <- lists:seq(1, OrderCount)],
    
    %% Measure routing latency
    Latencies = lists:map(fun(Order) ->
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(Gateway, {route_order, Order}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Orders),
    
    bitactor_server:kill_actor(Gateway),
    
    Stats = calculate_latency_stats(Latencies),
    
    #{
        scenario => execution_gateway,
        orders_routed => OrderCount,
        venues => 5,
        avg_latency_ns => maps:get(avg, Stats),
        p99_latency_ns => maps:get(p99, Stats),
        target_met => maps:get(p99, Stats) < 2000, % 2μs target
        throughput => (OrderCount * 1000000000) div lists:sum(Latencies)
    }.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

-spec warmup_system() -> ok.
warmup_system() ->
    io:format("Warming up system...~n"),
    %% Spawn and exercise actors to warm up the system
    WarmupActors = spawn_actors_batch(100),
    [send_message_to_random(WarmupActors, warmup) || _ <- lists:seq(1, 1000)],
    cleanup_actors_concurrent(WarmupActors),
    timer:sleep(100),
    ok.

-spec setup_otel_tracing() -> ok.
setup_otel_tracing() ->
    %% Configure OpenTelemetry for performance tracing (graceful handling)
    case catch telemetry:attach_many(
        performance_benchmark_handler,
        [
            [bitactor, actor, spawn],
            [bitactor, message, send],
            [bitactor, actor, kill],
            [bitactor, benchmark, complete]
        ],
        fun handle_telemetry_event/4,
        #{}
    ) of
        ok -> ok;
        _ -> io:format("  Warning: OTEL telemetry not available~n")
    end,
    ok.

-spec handle_telemetry_event(list(), map(), map(), map()) -> ok.
handle_telemetry_event(EventName, Measurements, Metadata, _Config) ->
    %% Record telemetry events for analysis
    Event = #{
        event => EventName,
        measurements => Measurements,
        metadata => Metadata,
        timestamp => erlang:monotonic_time(nanosecond)
    },
    
    %% Store in process dictionary for collection
    Events = erlang:get(telemetry_events, []),
    erlang:put(telemetry_events, [Event | Events]),
    ok.

-spec spawn_actors_batch(non_neg_integer()) -> [reference()].
spawn_actors_batch(Count) ->
    lists:map(fun(I) ->
        {ok, Ref, _} = bitactor_server:spawn_actor(benchmark, #{id => I}),
        Ref
    end, lists:seq(1, Count)).

-spec send_messages_uniform([reference()], non_neg_integer()) -> ok.
send_messages_uniform(Actors, TotalMessages) ->
    ActorCount = length(Actors),
    MessagesPerActor = TotalMessages div ActorCount,
    
    lists:foreach(fun({Idx, Actor}) ->
        lists:foreach(fun(MsgIdx) ->
            bitactor_server:send_message(Actor, {msg, Idx, MsgIdx})
        end, lists:seq(1, MessagesPerActor))
    end, lists:zip(lists:seq(1, ActorCount), Actors)).

-spec send_messages_concurrent([reference()], non_neg_integer()) -> ok.
send_messages_concurrent(Actors, TotalMessages) ->
    Parent = self(),
    NumWorkers = min(10, length(Actors)),
    MessagesPerWorker = TotalMessages div NumWorkers,
    
    _Workers = [spawn(fun() ->
        [send_message_to_random(Actors, {concurrent, W, I}) 
         || I <- lists:seq(1, MessagesPerWorker)],
        Parent ! {worker_done, W}
    end) || W <- lists:seq(1, NumWorkers)],
    
    %% Wait for workers
    [receive {worker_done, W} -> ok after 30000 -> error(timeout) end || W <- lists:seq(1, NumWorkers)],
    ok.

-spec send_message_to_random([reference()], term()) -> ok.
send_message_to_random(Actors, Message) ->
    Actor = lists:nth(rand:uniform(length(Actors)), Actors),
    bitactor_server:send_message(Actor, Message).

-spec cleanup_actors_concurrent([reference()]) -> ok.
cleanup_actors_concurrent(Actors) ->
    Parent = self(),
    
    %% Kill actors in parallel
    lists:foreach(fun(Actor) ->
        spawn(fun() ->
            bitactor_server:kill_actor(Actor),
            Parent ! {killed, Actor}
        end)
    end, Actors),
    
    %% Wait for all kills
    lists:foreach(fun(Actor) ->
        receive {killed, Actor} -> ok after 5000 -> error({cleanup_timeout, Actor}) end
    end, Actors),
    ok.

-spec sustained_load_test([reference()], non_neg_integer()) -> map().
sustained_load_test(Actors, DurationMs) ->
    EndTime = erlang:monotonic_time(millisecond) + DurationMs,
    sustained_load_loop(Actors, EndTime, 0, []).

sustained_load_loop(Actors, EndTime, Count, Latencies) ->
    case erlang:monotonic_time(millisecond) >= EndTime of
        true ->
            #{
                messages_sent => Count,
                avg_latency => case Latencies of
                    [] -> 0;
                    _ -> lists:sum(Latencies) div length(Latencies)
                end,
                throughput => (Count * 1000) div max(EndTime - erlang:monotonic_time(millisecond) + 5000, 1)
            };
        false ->
            Start = erlang:monotonic_time(nanosecond),
            send_message_to_random(Actors, {sustained, Count}),
            Latency = erlang:monotonic_time(nanosecond) - Start,
            
            %% Sample latencies (every 100th message)
            NewLatencies = case Count rem 100 of
                0 -> [Latency | Latencies];
                _ -> Latencies
            end,
            
            sustained_load_loop(Actors, EndTime, Count + 1, NewLatencies)
    end.

-spec measure_spawn_latency(non_neg_integer()) -> [non_neg_integer()].
measure_spawn_latency(Iterations) ->
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(nanosecond),
        {ok, Actor, _} = bitactor_server:spawn_actor(latency_test, #{}),
        End = erlang:monotonic_time(nanosecond),
        bitactor_server:kill_actor(Actor),
        End - Start
    end, lists:seq(1, Iterations)).

-spec measure_message_latency(non_neg_integer()) -> [non_neg_integer()].
measure_message_latency(Iterations) ->
    {ok, Actor, _} = bitactor_server:spawn_actor(message_latency, #{}),
    
    Latencies = lists:map(fun(I) ->
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(Actor, {latency_test, I}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, lists:seq(1, Iterations)),
    
    bitactor_server:kill_actor(Actor),
    Latencies.

-spec measure_tick_latency(non_neg_integer()) -> [non_neg_integer()].
measure_tick_latency(Iterations) ->
    %% Create actors for tick testing
    Actors = spawn_actors_batch(100),
    
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(nanosecond),
        bitactor_nif:tick_all(),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, lists:seq(1, Iterations)),
    
    cleanup_actors_concurrent(Actors),
    Latencies.

-spec calculate_latency_stats([non_neg_integer()]) -> map().
calculate_latency_stats(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    #{
        min => hd(Sorted),
        max => lists:last(Sorted),
        avg => lists:sum(Sorted) div Len,
        p50 => lists:nth(max(1, Len div 2), Sorted),
        p95 => lists:nth(max(1, (Len * 95) div 100), Sorted),
        p99 => lists:nth(max(1, (Len * 99) div 100), Sorted),
        p999 => lists:nth(max(1, (Len * 999) div 1000), Sorted),
        count => Len
    }.

-spec analyze_scaling([map()]) -> map().
analyze_scaling(Results) ->
    %% Extract scaling data
    ActorCounts = [maps:get(actor_count, R) || R <- Results],
    SpawnRates = [maps:get(spawn_rate, R) || R <- Results],
    MessageRates = [maps:get(message_rate, R) || R <- Results],
    
    %% Calculate scaling efficiency
    %% Perfect linear scaling = 1.0, sublinear < 1.0
    SpawnEfficiency = calculate_scaling_efficiency(ActorCounts, SpawnRates),
    MessageEfficiency = calculate_scaling_efficiency(ActorCounts, MessageRates),
    
    #{
        spawn_scaling => SpawnEfficiency,
        message_scaling => MessageEfficiency,
        efficiency => (SpawnEfficiency + MessageEfficiency) / 2,
        linear_scaling => (SpawnEfficiency > 0.8) and (MessageEfficiency > 0.8)
    }.

-spec calculate_scaling_efficiency([number()], [number()]) -> float().
calculate_scaling_efficiency(Inputs, Outputs) when length(Inputs) > 1 ->
    %% Calculate correlation coefficient
    N = length(Inputs),
    SumX = lists:sum(Inputs),
    SumY = lists:sum(Outputs),
    SumXY = lists:sum([X * Y || {X, Y} <- lists:zip(Inputs, Outputs)]),
    SumX2 = lists:sum([X * X || X <- Inputs]),
    SumY2 = lists:sum([Y * Y || Y <- Outputs]),
    
    Numerator = N * SumXY - SumX * SumY,
    Denominator = math:sqrt((N * SumX2 - SumX * SumX) * (N * SumY2 - SumY * SumY)),
    
    case Denominator of
        0 -> 0.0;
        _ -> abs(Numerator / Denominator)
    end;
calculate_scaling_efficiency(_, _) -> 1.0.

-spec analyze_memory_usage([map()]) -> map().
analyze_memory_usage(Results) ->
    %% Calculate memory efficiency metrics
    MemoryPerActor = [maps:get(memory_per_actor, R) || R <- Results],
    CleanupEfficiencies = [maps:get(cleanup_efficiency, R, 1.0) || R <- Results],
    
    #{
        avg_memory_per_actor => lists:sum(MemoryPerActor) div length(MemoryPerActor),
        max_memory_per_actor => lists:max(MemoryPerActor),
        min_memory_per_actor => lists:min(MemoryPerActor),
        avg_cleanup_efficiency => lists:sum(CleanupEfficiencies) / length(CleanupEfficiencies),
        efficiency_score => calculate_memory_efficiency_score(MemoryPerActor)
    }.

-spec calculate_memory_efficiency_score([number()]) -> float().
calculate_memory_efficiency_score(MemoryPerActor) ->
    %% Score based on memory usage (lower is better)
    %% Target: < 1KB per actor = 1.0, > 10KB = 0.0
    AvgMemory = lists:sum(MemoryPerActor) div length(MemoryPerActor),
    case AvgMemory of
        M when M < 1024 -> 1.0;
        M when M < 10240 -> 1.0 - (M - 1024) / 9216;
        _ -> 0.0
    end.

-spec evaluate_stress_test(map()) -> passed | failed | degraded.
evaluate_stress_test(Metrics) ->
    SpawnRate = maps:get(spawn_rate, Metrics),
    MessageRate = maps:get(message_rate, Metrics),
    SustainedThroughput = maps:get(sustained_throughput, Metrics),
    
    if
        SpawnRate > 5000 andalso MessageRate > 100000 andalso SustainedThroughput > 50000 ->
            passed;
        SpawnRate > 1000 andalso MessageRate > 50000 andalso SustainedThroughput > 25000 ->
            degraded;
        true ->
            failed
    end.

-spec evaluate_latency_metrics(map()) -> passed | failed | degraded.
evaluate_latency_metrics(Metrics) ->
    SpawnP99 = maps:get(p99, maps:get(spawn_latency, Metrics)),
    MessageP99 = maps:get(p99, maps:get(message_latency, Metrics)),
    TickP99 = maps:get(p99, maps:get(tick_latency, Metrics)),
    
    if
        SpawnP99 < 1000000 andalso MessageP99 < 10000 andalso TickP99 < 100 ->
            passed;
        SpawnP99 < 5000000 andalso MessageP99 < 50000 andalso TickP99 < 500 ->
            degraded;
        true ->
            failed
    end.

-spec evaluate_throughput_metrics(map()) -> passed | failed | degraded.
evaluate_throughput_metrics(Metrics) ->
    MaxMessageThroughput = maps:get(max_message_throughput, Metrics),
    MaxTickThroughput = maps:get(max_tick_throughput, Metrics),
    
    if
        MaxMessageThroughput > 100000 andalso MaxTickThroughput > ?UHFT_THROUGHPUT_TARGET ->
            passed;
        MaxMessageThroughput > 50000 andalso MaxTickThroughput > 500000 ->
            degraded;
        true ->
            failed
    end.

-spec evaluate_scalability_metrics(map()) -> passed | failed | degraded.
evaluate_scalability_metrics(Analysis) ->
    Efficiency = maps:get(efficiency, Analysis),
    LinearScaling = maps:get(linear_scaling, Analysis),
    
    if
        Efficiency > 0.9 andalso LinearScaling ->
            passed;
        Efficiency > 0.7 ->
            degraded;
        true ->
            failed
    end.

-spec evaluate_memory_metrics(map()) -> passed | failed | degraded.
evaluate_memory_metrics(Analysis) ->
    EfficiencyScore = maps:get(efficiency_score, Analysis),
    CleanupEfficiency = maps:get(avg_cleanup_efficiency, Analysis),
    
    if
        EfficiencyScore > 0.9 andalso CleanupEfficiency > 0.9 ->
            passed;
        EfficiencyScore > 0.7 andalso CleanupEfficiency > 0.7 ->
            degraded;
        true ->
            failed
    end.

-spec get_otel_traces(term()) -> [map()].
get_otel_traces(_SpanCtx) ->
    %% Collect telemetry events
    Events = erlang:get(telemetry_events, []),
    erlang:put(telemetry_events, []), % Clear for next run
    
    %% Format as traces
    lists:reverse(Events).

-spec compile_report([#benchmark_result{}], non_neg_integer()) -> #benchmark_report{}.
compile_report(Results, DurationMs) ->
    %% Aggregate OTEL metrics
    AllOtelTraces = lists:flatten([R#benchmark_result.otel_traces || R <- Results]),
    OtelAggregated = aggregate_otel_metrics(AllOtelTraces),
    
    %% Create summary
    {PassedCount, FailedCount, DegradedCount} = count_statuses(Results),
    
    Summary = #{
        total_benchmarks => length(Results),
        passed => PassedCount,
        failed => FailedCount,
        degraded => DegradedCount,
        overall_status => determine_overall_status(PassedCount, FailedCount, DegradedCount),
        key_findings => extract_key_findings(Results)
    },
    
    #benchmark_report{
        timestamp = erlang:system_time(seconds),
        duration_ms = DurationMs,
        results = Results,
        summary = Summary,
        otel_aggregated = OtelAggregated
    }.

-spec count_statuses([#benchmark_result{}]) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
count_statuses(Results) ->
    lists:foldl(fun(#benchmark_result{status = Status}, {P, F, D}) ->
        case Status of
            passed -> {P + 1, F, D};
            failed -> {P, F + 1, D};
            degraded -> {P, F, D + 1}
        end
    end, {0, 0, 0}, Results).

-spec determine_overall_status(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> atom().
determine_overall_status(_, Failed, _) when Failed > 0 -> failed;
determine_overall_status(_, _, Degraded) when Degraded > 2 -> degraded;
determine_overall_status(Passed, 0, _) when Passed > 0 -> passed;
determine_overall_status(_, _, _) -> unknown.

-spec extract_key_findings([#benchmark_result{}]) -> [binary()].
extract_key_findings(Results) ->
    Findings = lists:foldl(fun(#benchmark_result{name = Name, status = Status, metrics = Metrics}, Acc) ->
        Finding = case {Name, Status} of
            {uhft, passed} -> <<"✅ UHFT requirements met">>;
            {uhft, failed} -> <<"❌ UHFT requirements NOT met">>;
            {latency, passed} -> <<"✅ Latency targets achieved">>;
            {throughput, passed} -> <<"✅ Throughput targets achieved">>;
            {scalability, passed} -> <<"✅ Linear scaling confirmed">>;
            {memory, passed} -> <<"✅ Memory efficiency optimal">>;
            _ -> format_benchmark_finding(Name, Status, Metrics)
        end,
        [Finding | Acc]
    end, [], Results),
    lists:reverse(Findings).

-spec format_benchmark_finding(atom(), atom(), map()) -> binary().
format_benchmark_finding(Name, Status, _Metrics) ->
    StatusIcon = case Status of
        passed -> <<"✅">>;
        failed -> <<"❌">>;
        degraded -> <<"⚠️">>
    end,
    iolist_to_binary([StatusIcon, <<" ">>, atom_to_binary(Name), <<" benchmark ">>, atom_to_binary(Status)]).

-spec aggregate_otel_metrics([map()]) -> map().
aggregate_otel_metrics(Traces) ->
    %% Group by event type
    EventGroups = lists:foldl(fun(#{event := Event} = Trace, Acc) ->
        maps:update_with(Event, fun(List) -> [Trace | List] end, [Trace], Acc)
    end, #{}, Traces),
    
    %% Calculate aggregates
    maps:map(fun(_Event, EventTraces) ->
        #{
            count => length(EventTraces),
            total_duration => lists:sum([maps:get(duration, M, 0) || #{measurements := M} <- EventTraces])
        }
    end, EventGroups).

-spec format_benchmark_report(#benchmark_report{}) -> binary().
format_benchmark_report(#benchmark_report{
    timestamp = Timestamp,
    duration_ms = DurationMs,
    results = Results,
    summary = Summary,
    otel_aggregated = OtelMetrics
}) ->
    %% Generate Mermaid diagrams
    PerformanceDiagram = generate_performance_diagram(Results),
    OtelDiagram = generate_otel_diagram(OtelMetrics),
    
    %% Format individual results
    ResultSections = [format_benchmark_result(R) || R <- Results],
    
    iolist_to_binary([
        <<"# BitActor Performance Benchmark Report\n\n">>,
        <<"Generated: ">>, format_timestamp(Timestamp), <<"\n">>,
        <<"Duration: ">>, integer_to_binary(DurationMs), <<" ms\n\n">>,
        <<"## Summary\n\n">>,
        format_summary(Summary), <<"\n\n">>,
        <<"## Performance Overview\n\n">>,
        PerformanceDiagram, <<"\n\n">>,
        <<"## OpenTelemetry Metrics\n\n">>,
        OtelDiagram, <<"\n\n">>,
        <<"## Detailed Results\n\n">>,
        ResultSections
    ]).

-spec format_timestamp(integer()) -> binary().
format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S])).

-spec format_summary(map()) -> binary().
format_summary(#{
    total_benchmarks := Total,
    passed := Passed,
    failed := Failed,
    degraded := Degraded,
    overall_status := Status,
    key_findings := Findings
}) ->
    StatusText = case Status of
        passed -> <<"✅ ALL BENCHMARKS PASSED">>;
        failed -> <<"❌ BENCHMARKS FAILED">>;
        degraded -> <<"⚠️ PERFORMANCE DEGRADED">>;
        unknown -> <<"❓ STATUS UNKNOWN">>
    end,
    
    FindingsText = [<<" - ", F/binary, "\n">> || F <- Findings],
    
    iolist_to_binary([
        StatusText, <<"\n\n">>,
        <<"- Total Benchmarks: ">>, integer_to_binary(Total), <<"\n">>,
        <<"- Passed: ">>, integer_to_binary(Passed), <<"\n">>,
        <<"- Failed: ">>, integer_to_binary(Failed), <<"\n">>,
        <<"- Degraded: ">>, integer_to_binary(Degraded), <<"\n\n">>,
        <<"### Key Findings:\n">>,
        FindingsText
    ]).

-spec generate_performance_diagram([#benchmark_result{}]) -> binary().
generate_performance_diagram(Results) ->
    %% Extract latency data for diagram
    LatencyData = lists:foldl(fun(#benchmark_result{name = Name, metrics = Metrics}, Acc) ->
        case Name of
            latency ->
                SpawnP99 = maps:get(p99, maps:get(spawn_latency, Metrics, #{}), 0),
                MessageP99 = maps:get(p99, maps:get(message_latency, Metrics, #{}), 0),
                [{<<"Spawn P99">>, SpawnP99 div 1000}, {<<"Message P99">>, MessageP99 div 1000} | Acc];
            uhft ->
                case maps:get(scenarios, Metrics, []) of
                    [] -> Acc;
                    Scenarios ->
                        UhftLatencies = [{atom_to_binary(maps:get(scenario, S)), maps:get(p99_latency_ns, S, 0) div 1000} 
                                        || S <- Scenarios],
                        UhftLatencies ++ Acc
                end;
            _ -> Acc
        end
    end, [], Results),
    
    Lines = [iolist_to_binary([<<"    ">>, Name, <<" : ">>, integer_to_binary(Value), <<"\n">>]) 
             || {Name, Value} <- LatencyData],
    
    iolist_to_binary([
        <<"```mermaid\n">>,
        <<"graph LR\n">>,
        <<"    subgraph \"Latency (μs)\"\n">>,
        Lines,
        <<"    end\n">>,
        <<"```">>
    ]).

-spec generate_otel_diagram(map()) -> binary().
generate_otel_diagram(OtelMetrics) ->
    %% Format OTEL metrics as pie chart
    Lines = maps:fold(fun(Event, #{count := Count}, Acc) ->
        EventName = iolist_to_binary([io_lib:format("~p", [Event])]),
        Line = iolist_to_binary([<<"    \"">>, EventName, <<"\" : ">>, integer_to_binary(Count), <<"\n">>]),
        [Line | Acc]
    end, [], OtelMetrics),
    
    iolist_to_binary([
        <<"```mermaid\n">>,
        <<"pie title OpenTelemetry Event Distribution\n">>,
        Lines,
        <<"```">>
    ]).

-spec format_benchmark_result(#benchmark_result{}) -> binary().
format_benchmark_result(#benchmark_result{
    name = Name,
    category = Category,
    status = Status,
    metrics = Metrics,
    details = Details
}) ->
    StatusIcon = case Status of
        passed -> <<"✅">>;
        failed -> <<"❌">>;
        degraded -> <<"⚠️">>
    end,
    
    MetricsText = format_metrics(Name, Metrics),
    DetailsText = format_details(Details),
    
    iolist_to_binary([
        <<"### ">>, StatusIcon, <<" ">>, atom_to_binary(Name), <<" (">>, atom_to_binary(Category), <<")\n\n">>,
        MetricsText,
        DetailsText,
        <<"\n">>
    ]).

-spec format_metrics(atom(), map()) -> binary().
format_metrics(uhft, #{scenarios := Scenarios}) ->
    ScenarioLines = [format_uhft_scenario(S) || S <- Scenarios],
    iolist_to_binary([<<"#### UHFT Scenarios:\n">>, ScenarioLines]);
format_metrics(_, Metrics) ->
    Lines = maps:fold(fun(K, V, Acc) ->
        Line = iolist_to_binary([<<"- ">>, format_metric_key(K), <<": ">>, format_metric_value(V), <<"\n">>]),
        [Line | Acc]
    end, [], Metrics),
    iolist_to_binary(Lines).

-spec format_uhft_scenario(map()) -> binary().
format_uhft_scenario(#{
    scenario := Scenario,
    target_met := Met,
    avg_latency_ns := AvgLatency,
    p99_latency_ns := P99Latency
}) ->
    Icon = case Met of true -> <<"✅">>; false -> <<"❌">> end,
    iolist_to_binary([
        <<"- ">>, Icon, <<" ">>, atom_to_binary(Scenario), 
        <<": Avg=">>, integer_to_binary(AvgLatency), <<"ns, P99=">>, 
        integer_to_binary(P99Latency), <<"ns\n">>
    ]).

-spec format_metric_key(term()) -> binary().
format_metric_key(Key) when is_atom(Key) -> atom_to_binary(Key);
format_metric_key(Key) -> iolist_to_binary(io_lib:format("~p", [Key])).

-spec format_metric_value(term()) -> binary().
format_metric_value(Value) when is_integer(Value) -> integer_to_binary(Value);
format_metric_value(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 2}]);
format_metric_value(Value) when is_map(Value) -> <<"[complex data]">>;
format_metric_value(Value) when is_list(Value) -> <<"[list data]">>;
format_metric_value(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).

-spec format_details(map()) -> binary().
format_details(Details) when map_size(Details) =:= 0 -> <<>>;
format_details(Details) ->
    Lines = maps:fold(fun(K, V, Acc) ->
        Line = iolist_to_binary([<<"  - ">>, atom_to_binary(K), <<": ">>, format_metric_value(V), <<"\n">>]),
        [Line | Acc]
    end, [], Details),
    iolist_to_binary([<<"#### Details:\n">>, Lines]).