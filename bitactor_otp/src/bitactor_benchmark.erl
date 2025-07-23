%%%-------------------------------------------------------------------
%%% @doc BitActor Performance Benchmarks
%%% Real measurements for UHFT validation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_benchmark).

%% API
-export([run_all/0, run_all/1]).
-export([measure_spawn_latency/0, measure_spawn_latency/1]).
-export([measure_message_latency/0, measure_message_latency/1]).
-export([measure_tick_throughput/0, measure_tick_throughput/1]).
-export([stress_test_actors/1]).
-export([uhft_scenario_test/0]).

-define(ITERATIONS, 10000).
-define(WARMUP, 1000).

%%%===================================================================
%%% API
%%%===================================================================

-spec run_all() -> [{atom(), map()}].
run_all() ->
    run_all(#{}).

-spec run_all(map()) -> [{atom(), map()}].
run_all(Options) ->
    io:format("~n=== BitActor UHFT Performance Benchmarks ===~n~n"),
    
    %% Warm up the system
    warmup(),
    
    %% Run benchmarks
    Results = [
        {spawn_latency, measure_spawn_latency(Options)},
        {message_latency, measure_message_latency(Options)},
        {tick_throughput, measure_tick_throughput(Options)},
        {stress_test, stress_test_actors(maps:get(actors, Options, 1000))},
        {uhft_scenarios, uhft_scenario_test()}
    ],
    
    %% Print summary
    print_summary(Results),
    Results.

%%%===================================================================
%%% Spawn Latency Benchmark
%%%===================================================================

-spec measure_spawn_latency() -> map().
measure_spawn_latency() ->
    measure_spawn_latency(#{}).

-spec measure_spawn_latency(map()) -> map().
measure_spawn_latency(Options) ->
    Iterations = maps:get(iterations, Options, ?ITERATIONS),
    
    io:format("Measuring actor spawn latency (~p iterations)...~n", [Iterations]),
    
    %% Measure NIF overhead first
    {ok, MinOverhead, AvgOverhead, MaxOverhead} = bitactor_nif:measure_latency(),
    
    %% Collect spawn measurements
    Measurements = lists:map(fun(_) ->
        Start = erlang:monotonic_time(nanosecond),
        {ok, _ActorRef, NIFLatency} = bitactor_server:spawn_actor(market_data, #{}),
        End = erlang:monotonic_time(nanosecond),
        TotalLatency = End - Start,
        {TotalLatency, NIFLatency}
    end, lists:seq(1, Iterations)),
    
    %% Calculate statistics
    {TotalLatencies, NIFLatencies} = lists:unzip(Measurements),
    
    Stats = #{
        total => calculate_stats(TotalLatencies),
        nif => calculate_stats(NIFLatencies),
        overhead => #{
            min => MinOverhead,
            avg => AvgOverhead,
            max => MaxOverhead
        },
        iterations => Iterations
    },
    
    print_latency_stats("Spawn Latency", Stats),
    Stats.

%%%===================================================================
%%% Message Latency Benchmark
%%%===================================================================

-spec measure_message_latency() -> map().
measure_message_latency() ->
    measure_message_latency(#{}).

-spec measure_message_latency(map()) -> map().
measure_message_latency(Options) ->
    Iterations = maps:get(iterations, Options, ?ITERATIONS),
    
    io:format("~nMeasuring message send latency (~p iterations)...~n", [Iterations]),
    
    %% Create test actor
    {ok, ActorRef} = bitactor_server:spawn_actor(test, #{}),
    
    %% Warm up
    [bitactor_server:send_message(ActorRef, warmup) || _ <- lists:seq(1, ?WARMUP)],
    
    %% Measure
    Measurements = lists:map(fun(I) ->
        Msg = {test_message, I, erlang:monotonic_time(nanosecond)},
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(ActorRef, Msg),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, lists:seq(1, Iterations)),
    
    bitactor_server:kill_actor(ActorRef),
    
    Stats = #{
        latency => calculate_stats(Measurements),
        iterations => Iterations
    },
    
    print_latency_stats("Message Send Latency", Stats),
    Stats.

%%%===================================================================
%%% Tick Throughput Benchmark
%%%===================================================================

-spec measure_tick_throughput() -> map().
measure_tick_throughput() ->
    measure_tick_throughput(#{}).

-spec measure_tick_throughput(map()) -> map().
measure_tick_throughput(Options) ->
    ActorCounts = maps:get(actor_counts, Options, [10, 100, 1000, 5000]),
    
    io:format("~nMeasuring tick throughput for different actor counts...~n"),
    
    Results = lists:map(fun(Count) ->
        %% Spawn actors
        Actors = [begin
            {ok, Ref} = bitactor_server:spawn_actor(benchmark, #{id => I}),
            Ref
        end || I <- lists:seq(1, Count)],
        
        %% Measure tick performance
        TickCount = 1000,
        Start = erlang:monotonic_time(nanosecond),
        
        [bitactor_nif:tick_all() || _ <- lists:seq(1, TickCount)],
        
        End = erlang:monotonic_time(nanosecond),
        Duration = End - Start,
        
        %% Clean up
        [bitactor_server:kill_actor(Ref) || Ref <- Actors],
        
        ThroughputMicros = (Count * TickCount * 1000) div (Duration div 1000),
        
        #{
            actor_count => Count,
            total_ticks => Count * TickCount,
            duration_ms => Duration div 1000000,
            throughput_per_ms => ThroughputMicros div 1000,
            ns_per_tick => Duration div (Count * TickCount)
        }
    end, ActorCounts),
    
    print_throughput_results(Results),
    #{results => Results}.

%%%===================================================================
%%% Stress Test
%%%===================================================================

-spec stress_test_actors(pos_integer()) -> map().
stress_test_actors(ActorCount) ->
    io:format("~nRunning stress test with ~p actors...~n", [ActorCount]),
    
    %% Spawn all actors
    SpawnStart = erlang:monotonic_time(millisecond),
    Actors = [begin
        {ok, Ref} = bitactor_server:spawn_actor(stress_test, #{id => I}),
        Ref
    end || I <- lists:seq(1, ActorCount)],
    SpawnDuration = erlang:monotonic_time(millisecond) - SpawnStart,
    
    %% Send messages to all actors
    MessageCount = 100,
    MessageStart = erlang:monotonic_time(millisecond),
    
    [bitactor_server:send_message(
        lists:nth(rand:uniform(ActorCount), Actors),
        {stress_message, I}
    ) || I <- lists:seq(1, ActorCount * MessageCount)],
    
    MessageDuration = erlang:monotonic_time(millisecond) - MessageStart,
    
    %% Tick test
    TickStart = erlang:monotonic_time(millisecond),
    [bitactor_nif:tick_all() || _ <- lists:seq(1, 100)],
    TickDuration = erlang:monotonic_time(millisecond) - TickStart,
    
    %% Get stats
    Stats = bitactor_server:get_stats(),
    {ok, NIFStats} = bitactor_nif:get_stats(),
    
    %% Clean up
    CleanupStart = erlang:monotonic_time(millisecond),
    [bitactor_server:kill_actor(Ref) || Ref <- Actors],
    CleanupDuration = erlang:monotonic_time(millisecond) - CleanupStart,
    
    Results = #{
        actor_count => ActorCount,
        spawn_duration_ms => SpawnDuration,
        spawn_rate => ActorCount * 1000 div SpawnDuration,
        message_duration_ms => MessageDuration,
        message_rate => (ActorCount * MessageCount * 1000) div MessageDuration,
        tick_duration_ms => TickDuration,
        cleanup_duration_ms => CleanupDuration,
        server_stats => Stats,
        nif_stats => NIFStats
    },
    
    print_stress_results(Results),
    Results.

%%%===================================================================
%%% UHFT Scenario Tests
%%%===================================================================

-spec uhft_scenario_test() -> map().
uhft_scenario_test() ->
    io:format("~n=== UHFT Scenario Tests ===~n"),
    
    Scenarios = [
        uhft_market_data_handler(),
        uhft_order_book_aggregator(),
        uhft_alpha_calculator(),
        uhft_risk_manager(),
        uhft_execution_gateway()
    ],
    
    #{scenarios => Scenarios}.

%% UC1: Market Data Feed Handler
uhft_market_data_handler() ->
    io:format("~nUC1: Market Data Feed Handler (Target: <500ns)~n"),
    
    %% Create market data actors
    {ok, Parser} = bitactor_server:spawn_actor(parser, #{}),
    {ok, Distributor} = bitactor_server:spawn_actor(distributor, #{}),
    
    %% Simulate 1M ticks/second for 100ms
    TickData = [{{symbol, <<"AAPL">>}, {price, 150.25 + I/100}, {volume, 100}}
                || I <- lists:seq(1, 100000)],
    
    Start = erlang:monotonic_time(nanosecond),
    
    [bitactor_server:send_message(Parser, Tick) || Tick <- TickData],
    
    End = erlang:monotonic_time(nanosecond),
    Duration = End - Start,
    AvgLatency = Duration div length(TickData),
    
    bitactor_server:kill_actor(Parser),
    bitactor_server:kill_actor(Distributor),
    
    Result = #{
        scenario => market_data_handler,
        ticks_processed => length(TickData),
        total_duration_ms => Duration div 1000000,
        avg_latency_ns => AvgLatency,
        target_met => AvgLatency < 500,
        throughput => (length(TickData) * 1000000000) div Duration
    },
    
    io:format("  Average latency: ~p ns (Target met: ~p)~n", 
              [AvgLatency, AvgLatency < 500]),
    Result.

%% UC2: Order Book Aggregator
uhft_order_book_aggregator() ->
    io:format("~nUC2: Order Book Aggregator (Target: <1μs)~n"),
    
    %% Create exchange feed actors
    Exchanges = [binance, coinbase, kraken, ftx, okx],
    ExchangeActors = [{Exchange, element(2, bitactor_server:spawn_actor(exchange, #{name => Exchange}))} 
                      || Exchange <- Exchanges],
    
    {ok, Aggregator} = bitactor_server:spawn_actor(book_aggregator, #{}),
    
    %% Simulate order book updates
    Updates = [{{Exchange, Side, Price, Volume}, erlang:monotonic_time(nanosecond)}
               || Exchange <- Exchanges,
                  Side <- [bid, ask],
                  Price <- [100.0, 100.1, 100.2],
                  Volume <- [1000]],
    
    Latencies = lists:map(fun({Update, _}) ->
        Start = erlang:monotonic_time(nanosecond),
        bitactor_server:send_message(Aggregator, Update),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Updates),
    
    %% Cleanup
    [bitactor_server:kill_actor(Actor) || {_, Actor} <- ExchangeActors],
    bitactor_server:kill_actor(Aggregator),
    
    Stats = calculate_stats(Latencies),
    AvgLatency = maps:get(avg, Stats),
    
    Result = #{
        scenario => order_book_aggregator,
        exchanges => length(Exchanges),
        updates => length(Updates),
        latency_stats => Stats,
        target_met => AvgLatency < 1000
    },
    
    io:format("  Average latency: ~p ns (Target met: ~p)~n", 
              [AvgLatency, AvgLatency < 1000]),
    Result.

%% UC3: Alpha Signal Calculator
uhft_alpha_calculator() ->
    io:format("~nUC3: Alpha Signal Calculator (Target: <10μs)~n"),
    
    {ok, AlphaEngine} = bitactor_server:spawn_actor(alpha_engine, #{}),
    
    %% Generate feature vectors
    Features = [{features, [rand:uniform() || _ <- lists:seq(1, 50)]} 
                || _ <- lists:seq(1, 1000)],
    
    Latencies = lists:map(fun(Feature) ->
        Start = erlang:monotonic_time(nanosecond),
        bitactor_server:send_message(AlphaEngine, Feature),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Features),
    
    bitactor_server:kill_actor(AlphaEngine),
    
    Stats = calculate_stats(Latencies),
    AvgLatency = maps:get(avg, Stats),
    
    Result = #{
        scenario => alpha_calculator,
        calculations => length(Features),
        latency_stats => Stats,
        target_met => AvgLatency < 10000
    },
    
    io:format("  Average latency: ~p ns (Target met: ~p)~n", 
              [AvgLatency, AvgLatency < 10000]),
    Result.

%% UC4: Risk Manager
uhft_risk_manager() ->
    io:format("~nUC4: Risk Manager (Target: <5μs)~n"),
    
    {ok, RiskEngine} = bitactor_server:spawn_actor(risk_engine, #{}),
    
    %% Risk check requests
    Requests = [{check_position, #{
        symbol => <<"AAPL">>,
        quantity => 1000,
        price => 150.0,
        account => <<"ACC001">>
    }} || _ <- lists:seq(1, 1000)],
    
    Latencies = lists:map(fun(Request) ->
        Start = erlang:monotonic_time(nanosecond),
        bitactor_server:send_message(RiskEngine, Request),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Requests),
    
    bitactor_server:kill_actor(RiskEngine),
    
    Stats = calculate_stats(Latencies),
    AvgLatency = maps:get(avg, Stats),
    
    Result = #{
        scenario => risk_manager,
        checks => length(Requests),
        latency_stats => Stats,
        target_met => AvgLatency < 5000
    },
    
    io:format("  Average latency: ~p ns (Target met: ~p)~n", 
              [AvgLatency, AvgLatency < 5000]),
    Result.

%% UC5: Execution Gateway
uhft_execution_gateway() ->
    io:format("~nUC5: Execution Gateway (Target: <2μs)~n"),
    
    {ok, Gateway} = bitactor_server:spawn_actor(exec_gateway, #{}),
    
    %% Order routing requests
    Orders = [{route_order, #{
        symbol => <<"AAPL">>,
        side => buy,
        quantity => 100,
        order_type => market
    }} || _ <- lists:seq(1, 1000)],
    
    Latencies = lists:map(fun(Order) ->
        Start = erlang:monotonic_time(nanosecond),
        bitactor_server:send_message(Gateway, Order),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end, Orders),
    
    bitactor_server:kill_actor(Gateway),
    
    Stats = calculate_stats(Latencies),
    AvgLatency = maps:get(avg, Stats),
    
    Result = #{
        scenario => execution_gateway,
        orders => length(Orders),
        latency_stats => Stats,
        target_met => AvgLatency < 2000
    },
    
    io:format("  Average latency: ~p ns (Target met: ~p)~n", 
              [AvgLatency, AvgLatency < 2000]),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec warmup() -> ok.
warmup() ->
    io:format("Warming up system...~n"),
    %% Spawn and kill some actors to warm up the system
    Refs = [element(2, bitactor_server:spawn_actor(warmup, #{})) || _ <- lists:seq(1, 100)],
    [bitactor_server:send_message(Ref, warmup) || Ref <- Refs, _ <- lists:seq(1, 10)],
    [bitactor_server:kill_actor(Ref) || Ref <- Refs],
    ok.

-spec calculate_stats([number()]) -> map().
calculate_stats(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    
    Min = hd(Sorted),
    Max = lists:last(Sorted),
    Sum = lists:sum(Sorted),
    Avg = Sum div Len,
    
    P50 = lists:nth(Len div 2, Sorted),
    P95 = lists:nth((Len * 95) div 100, Sorted),
    P99 = lists:nth((Len * 99) div 100, Sorted),
    P999 = lists:nth((Len * 999) div 1000, Sorted),
    
    #{
        min => Min,
        max => Max,
        avg => Avg,
        p50 => P50,
        p95 => P95,
        p99 => P99,
        p999 => P999,
        count => Len
    }.

-spec print_latency_stats(string(), map()) -> ok.
print_latency_stats(Title, Stats) ->
    io:format("~n~s Results:~n", [Title]),
    
    case maps:get(total, Stats, undefined) of
        undefined ->
            Latency = maps:get(latency, Stats),
            print_stats_table(Latency);
        TotalStats ->
            io:format("  Total Latency (end-to-end):~n"),
            print_stats_table(TotalStats),
            io:format("  NIF Latency (C code only):~n"),
            print_stats_table(maps:get(nif, Stats))
    end,
    ok.

-spec print_stats_table(map()) -> ok.
print_stats_table(Stats) ->
    io:format("    Min:  ~p ns~n", [maps:get(min, Stats)]),
    io:format("    Avg:  ~p ns~n", [maps:get(avg, Stats)]),
    io:format("    P50:  ~p ns~n", [maps:get(p50, Stats)]),
    io:format("    P95:  ~p ns~n", [maps:get(p95, Stats)]),
    io:format("    P99:  ~p ns~n", [maps:get(p99, Stats)]),
    io:format("    P999: ~p ns~n", [maps:get(p999, Stats)]),
    io:format("    Max:  ~p ns~n", [maps:get(max, Stats)]).

-spec print_throughput_results([map()]) -> ok.
print_throughput_results(Results) ->
    io:format("~nTick Throughput Results:~n"),
    io:format("~-12s ~-15s ~-20s ~-15s~n", 
              ["Actors", "Total Ticks", "Throughput/ms", "ns/tick"]),
    io:format("~s~n", [lists:duplicate(65, "-")]),
    
    [io:format("~-12b ~-15b ~-20b ~-15b~n", 
               [maps:get(actor_count, R),
                maps:get(total_ticks, R),
                maps:get(throughput_per_ms, R),
                maps:get(ns_per_tick, R)])
     || R <- Results],
    ok.

-spec print_stress_results(map()) -> ok.
print_stress_results(Results) ->
    io:format("~nStress Test Results:~n"),
    io:format("  Actors spawned:    ~p in ~p ms (~p/sec)~n",
              [maps:get(actor_count, Results),
               maps:get(spawn_duration_ms, Results),
               maps:get(spawn_rate, Results)]),
    io:format("  Messages sent:     ~p/sec~n",
              [maps:get(message_rate, Results)]),
    io:format("  Tick duration:     ~p ms for 100 ticks~n",
              [maps:get(tick_duration_ms, Results)]),
    io:format("  Cleanup duration:  ~p ms~n",
              [maps:get(cleanup_duration_ms, Results)]).

-spec print_summary([{atom(), map()}]) -> ok.
print_summary(Results) ->
    io:format("~n~n=== BENCHMARK SUMMARY ===~n"),
    
    %% Extract key metrics
    SpawnStats = proplists:get_value(spawn_latency, Results),
    MessageStats = proplists:get_value(message_latency, Results),
    StressStats = proplists:get_value(stress_test, Results),
    UHFTStats = proplists:get_value(uhft_scenarios, Results),
    
    %% Print spawn performance
    case SpawnStats of
        #{total := #{avg := AvgSpawn}} ->
            io:format("~nActor Spawn: ~p ns average (~s)~n", 
                      [AvgSpawn, performance_rating(AvgSpawn, 10000)]);
        _ -> ok
    end,
    
    %% Print message performance
    case MessageStats of
        #{latency := #{avg := AvgMsg}} ->
            io:format("Message Send: ~p ns average (~s)~n", 
                      [AvgMsg, performance_rating(AvgMsg, 1000)]);
        _ -> ok
    end,
    
    %% Print UHFT results
    case UHFTStats of
        #{scenarios := Scenarios} ->
            io:format("~nUHFT Scenarios:~n"),
            [io:format("  ~-25s: ~s~n", 
                       [maps:get(scenario, S), 
                        case maps:get(target_met, S) of
                            true -> "✓ PASSED";
                            false -> "✗ FAILED"
                        end])
             || S <- Scenarios];
        _ -> ok
    end,
    
    io:format("~n=========================~n").

-spec performance_rating(number(), number()) -> string().
performance_rating(Value, Target) when Value < Target ->
    "EXCELLENT";
performance_rating(Value, Target) when Value < Target * 2 ->
    "GOOD";
performance_rating(Value, Target) when Value < Target * 5 ->
    "ACCEPTABLE";
performance_rating(_, _) ->
    "NEEDS OPTIMIZATION".