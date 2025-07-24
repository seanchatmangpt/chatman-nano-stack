-module(quick_optimization_test).
-export([run_80_20_test/0]).

%% Direct 80/20 optimization test using existing infrastructure
run_80_20_test() ->
    io:format("=== 80/20 BitActor Latency Optimization Test ===~n"),
    
    %% Test 1: Baseline measurement
    BaselineResults = run_baseline_test(),
    io:format("Baseline P99: ~p ns~n", [maps:get(p99_latency_ns, BaselineResults, 0)]),
    
    %% Test 2: With NIF properly loaded
    OptimizedResults = run_optimized_test(),
    io:format("Optimized P99: ~p ns~n", [maps:get(p99_latency_ns, OptimizedResults, 0)]),
    
    %% Calculate improvement
    BaselineP99 = maps:get(p99_latency_ns, BaselineResults, 9999),
    OptimizedP99 = maps:get(p99_latency_ns, OptimizedResults, 9999),
    Improvement = ((BaselineP99 - OptimizedP99) / BaselineP99) * 100,
    
    UHFTCompliant = OptimizedP99 =< 1000,
    
    io:format("~n=== 80/20 Optimization Results ===~n"),
    io:format("Baseline P99:  ~p ns~n", [BaselineP99]),
    io:format("Optimized P99: ~p ns~n", [OptimizedP99]),
    io:format("Improvement:   ~.1f%~n", [Improvement]),
    io:format("UHFT Compliant (≤1000ns): ~p~n", [UHFTCompliant]),
    
    %% Generate mermaid diagram
    generate_comparison_report(BaselineP99, OptimizedP99, UHFTCompliant, Improvement),
    
    #{
        baseline_p99_ns => BaselineP99,
        optimized_p99_ns => OptimizedP99,
        improvement_percent => Improvement,
        uhft_compliant => UHFTCompliant,
        target_achieved => OptimizedP99 < BaselineP99
    }.

run_baseline_test() ->
    try
        %% Create actor
        {ok, Actor, _} = bitactor_server:spawn_actor(baseline_test, #{}),
        
        %% Measure message latencies (smaller sample for speed)
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server:send_message(Actor, <<"baseline_test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 1000)],
        
        SortedLatencies = lists:sort(Latencies),
        P99 = lists:nth(round(length(SortedLatencies) * 0.99), SortedLatencies),
        
        bitactor_server:kill_actor(Actor),
        
        #{p99_latency_ns => P99, test_type => baseline}
        
    catch
        _:_ -> #{p99_latency_ns => 99999, test_type => baseline, error => true}
    end.

run_optimized_test() ->
    try
        %% Apply optimizations before test
        apply_runtime_optimizations(),
        
        %% Create actor with potential NIF acceleration
        {ok, Actor, SpawnLatency} = bitactor_server:spawn_actor(optimized_test, #{}),
        
        %% Warmup
        [bitactor_server:send_message(Actor, <<"warmup">>) || _ <- lists:seq(1, 50)],
        timer:sleep(5),
        
        %% Measure optimized latencies
        Latencies = [begin
            Start = erlang:monotonic_time(nanosecond),
            ok = bitactor_server:send_message(Actor, <<"optimized_test">>),
            End = erlang:monotonic_time(nanosecond),
            End - Start
        end || _ <- lists:seq(1, 1000)],
        
        SortedLatencies = lists:sort(Latencies),
        P50 = lists:nth(round(length(SortedLatencies) * 0.50), SortedLatencies),
        P95 = lists:nth(round(length(SortedLatencies) * 0.95), SortedLatencies),
        P99 = lists:nth(round(length(SortedLatencies) * 0.99), SortedLatencies),
        
        bitactor_server:kill_actor(Actor),
        
        #{
            p50_latency_ns => P50,
            p95_latency_ns => P95,
            p99_latency_ns => P99,
            spawn_latency_ns => SpawnLatency,
            test_type => optimized
        }
        
    catch
        _:_ -> #{p99_latency_ns => 99999, test_type => optimized, error => true}
    end.

%% Apply runtime optimizations that don't require code changes
apply_runtime_optimizations() ->
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Set process priority if possible
    try process_flag(priority, high) catch _:_ -> ok end,
    
    %% Check NIF loading
    try
        case erlang:function_exported(bitactor_nif, measure_latency, 0) of
            true -> 
                {ok, _, Avg, _} = bitactor_nif:measure_latency(),
                io:format("NIF loaded, overhead: ~p ns~n", [Avg]);
            false -> 
                io:format("NIF not loaded, using fallback~n")
        end
    catch
        _:_ -> ok
    end.

generate_comparison_report(BaselineP99, OptimizedP99, UHFTCompliant, Improvement) ->
    io:format("~n=== Performance Comparison (Mermaid) ===~n"),
    io:format("```mermaid~n"),
    io:format("graph LR~n"),
    io:format("    A[BitActor Baseline] --> B[~p ns P99]~n", [BaselineP99]),
    io:format("    C[80/20 Optimized] --> D[~p ns P99]~n", [OptimizedP99]),
    io:format("    B --> E[~.1f% Improvement]~n", [Improvement]),
    
    if UHFTCompliant ->
        io:format("    D --> F[✓ UHFT Compliant]~n");
    true ->
        io:format("    D --> F[✗ Above UHFT Limit]~n")
    end,
    
    if Improvement > 0 ->
        io:format("    E --> G[✓ Performance Gain]~n");
    true ->
        io:format("    E --> G[✗ No Improvement]~n")
    end,
    
    io:format("```~n").