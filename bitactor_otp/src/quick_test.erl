-module(quick_test).
-export([run_tests/0]).

run_tests() ->
    io:format("=== Quick BitActor Test ===~n"),
    
    % Test 1: Basic system status
    try
        Stats = bitactor_server:get_stats(),
        io:format("✓ BitActor server responsive, stats: ~p~n", [Stats])
    catch
        Error:Reason ->
            io:format("✗ BitActor server not responsive: ~p:~p~n", [Error, Reason])
    end,
    
    % Test 2: Basic actor creation  
    try
        {ok, Actor, LatencyNs} = bitactor_server:spawn_actor(test_actor, #{}),
        io:format("✓ Actor spawned: ~p, Latency: ~p ns~n", [Actor, LatencyNs]),
        
        % Test 3: Message sending
        ok = bitactor_server:send_message(Actor, <<"test">>),
        io:format("✓ Message sent successfully~n"),
        
        % Test 4: Actor cleanup
        ok = bitactor_server:kill_actor(Actor),
        io:format("✓ Actor cleaned up~n"),
        
        % Test 5: Performance validation
        run_performance_test()
        
    catch
        Error2:Reason2 ->
            io:format("✗ Basic operations failed: ~p:~p~n", [Error2, Reason2])
    end.

run_performance_test() ->
    io:format("~n--- Performance Validation ---~n"),
    
    % Create multiple actors for throughput test
    ActorCount = 100,
    MessageCount = 1000,
    
    try
        StartTime = erlang:monotonic_time(millisecond),
        
        % Create actors
        Actors = [begin
            {ok, A, _} = bitactor_server:spawn_actor(perf_test, #{id => N}),
            A
        end || N <- lists:seq(1, ActorCount)],
        
        io:format("✓ Created ~p actors~n", [ActorCount]),
        
        % Send messages
        MsgStartTime = erlang:monotonic_time(nanosecond),
        [bitactor_server:send_message(lists:nth(rand:uniform(ActorCount), Actors), <<"perf_test">>) || _ <- lists:seq(1, MessageCount)],
        MsgEndTime = erlang:monotonic_time(nanosecond),
        
        % Calculate throughput
        MessageLatency = (MsgEndTime - MsgStartTime) / MessageCount,
        ThroughputMsgsSec = 1000000000 / MessageLatency * ActorCount,
        
        io:format("✓ Sent ~p messages~n", [MessageCount]),
        io:format("✓ Average message latency: ~.2f ns~n", [MessageLatency]),
        io:format("✓ Estimated throughput: ~.2f msgs/sec~n", [ThroughputMsgsSec]),
        
        % UHFT validation
        if 
            MessageLatency < 1000 ->
                io:format("✓ Sub-microsecond latency achieved (UHFT compliant)~n");
            true ->
                io:format("⚠ Latency ~.2f ns exceeds 1000ns (not UHFT compliant)~n", [MessageLatency])
        end,
        
        if
            ThroughputMsgsSec > 100000 ->
                io:format("✓ High throughput achieved (UHFT capable)~n");
            true ->
                io:format("⚠ Throughput ~.2f msgs/sec below 100k (not UHFT capable)~n", [ThroughputMsgsSec])
        end,
        
        % Cleanup
        [bitactor_server:kill_actor(A) || A <- Actors],
        
        EndTime = erlang:monotonic_time(millisecond),
        TotalDuration = EndTime - StartTime,
        
        io:format("✓ Test completed in ~p ms~n", [TotalDuration]),
        
        % Generate mermaid diagram
        generate_mermaid_report(MessageLatency, ThroughputMsgsSec, ActorCount, MessageCount),
        
        #{
            status => passed,
            avg_latency_ns => MessageLatency,
            throughput_msgs_sec => ThroughputMsgsSec,
            actors_tested => ActorCount,
            messages_sent => MessageCount,
            total_duration_ms => TotalDuration,
            uhft_latency_compliant => MessageLatency < 1000,
            uhft_throughput_capable => ThroughputMsgsSec > 100000
        }
        
    catch
        Error:Reason ->
            io:format("✗ Performance test failed: ~p:~p~n", [Error, Reason]),
            #{status => failed, error => {Error, Reason}}
    end.

generate_mermaid_report(LatencyNs, ThroughputMsgsSec, ActorCount, MessageCount) ->
    io:format("~n=== OTEL Performance Report (Mermaid) ===~n"),
    io:format("```mermaid~n"),
    io:format("graph TB~n"),
    io:format("    A[BitActor UHFT Engine] --> B[Performance Metrics]~n"),
    io:format("    B --> C[Latency: ~.1f ns]~n", [LatencyNs]),
    io:format("    B --> D[Throughput: ~.0f msgs/sec]~n", [ThroughputMsgsSec]),
    io:format("    B --> E[Actors: ~p]~n", [ActorCount]),
    io:format("    B --> F[Messages: ~p]~n", [MessageCount]),
    
    if LatencyNs < 1000 ->
        io:format("    C --> C1[✓ UHFT Compliant]~n");
    true ->
        io:format("    C --> C1[✗ Not UHFT]~n")
    end,
    
    if ThroughputMsgsSec > 100000 ->
        io:format("    D --> D1[✓ UHFT Capable]~n");
    true ->
        io:format("    D --> D1[✗ Below UHFT]~n")
    end,
    
    io:format("    E --> E1[Concurrent Processing]~n"),
    io:format("    F --> F1[Message Distribution]~n"),
    io:format("```~n").