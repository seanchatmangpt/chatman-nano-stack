#!/usr/bin/env escript
%%! -pa .

main(_) ->
    % Compile the module
    case compile:file("test_bitactor", [binary, return_errors]) of
        {ok, _, Binary} ->
            code:load_binary(test_bitactor, "test_bitactor.beam", Binary),
            io:format("✅ Module compiled~n"),
            
            % Start the server
            {ok, Pid} = test_bitactor:start_link(),
            io:format("✅ Server started: ~p~n", [Pid]),
            
            % Send signals
            Start = erlang:monotonic_time(),
            send_signals(100000),
            End = erlang:monotonic_time(),
            
            % Get stats
            Stats = test_bitactor:get_stats(),
            Count = maps:get(signals_processed, Stats, 0),
            ElapsedMs = erlang:convert_time_unit(End - Start, native, millisecond),
            
            io:format("✅ Processed ~p signals~n", [Count]),
            io:format("Time: ~p ms~n", [ElapsedMs]),
            io:format("Throughput: ~.2f signals/sec~n", [Count * 1000 / ElapsedMs]),
            
            test_bitactor:stop();
        {error, Errors, _} ->
            io:format("❌ Compilation failed: ~p~n", [Errors])
    end.

send_signals(0) -> ok;
send_signals(N) ->
    test_bitactor:send_signal(semanticsignal, N),
    send_signals(N - 1).
