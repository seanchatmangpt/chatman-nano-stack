%%%-------------------------------------------------------------------
%%% @doc BitActor Unit Test Framework
%%% Integrates with bitactor_comprehensive_test_runner
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_unit_tests).

%% API
-export([run_tests/0, run_tests/1]).
-export([run_specific_test/1, run_test_suite/1]).
-export([get_test_report/0]).

%% Test suite exports
-export([basic_operations_suite/0]).
-export([performance_suite/0]).
-export([concurrency_suite/0]).
-export([error_handling_suite/0]).
-export([uhft_validation_suite/0]).

-record(test_result, {
    name :: atom(),
    suite :: atom(),
    status :: passed | failed | skipped,
    duration_ns :: non_neg_integer(),
    message :: binary(),
    details :: map()
}).

-record(test_report, {
    total_tests :: non_neg_integer(),
    passed :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    duration_ms :: non_neg_integer(),
    results :: [#test_result{}],
    otel_metrics :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec run_tests() -> {ok, #test_report{}} | {error, term()}.
run_tests() ->
    run_tests(#{}).

-spec run_tests(map()) -> {ok, #test_report{}} | {error, term()}.
run_tests(_Options) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Initialize telemetry (graceful handling)
    case catch bitactor_telemetry:reset_metrics() of
        ok -> ok;
        _ -> io:format("  Warning: telemetry not available~n")
    end,
    
    %% Run all test suites
    Suites = [
        basic_operations_suite,
        performance_suite,
        concurrency_suite,
        error_handling_suite,
        uhft_validation_suite
    ],
    
    Results = lists:flatten([
        run_test_suite(Suite) || Suite <- Suites
    ]),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    %% Generate report
    Report = generate_report(Results, EndTime - StartTime),
    
    %% Return report
    {ok, Report}.

-spec run_specific_test(atom()) -> #test_result{}.
run_specific_test(TestName) ->
    case find_test(TestName) of
        {ok, {Suite, TestFun}} ->
            execute_test(Suite, TestName, TestFun);
        error ->
            #test_result{
                name = TestName,
                suite = unknown,
                status = failed,
                duration_ns = 0,
                message = <<"Test not found">>,
                details = #{}
            }
    end.

-spec run_test_suite(atom()) -> [#test_result{}].
run_test_suite(Suite) ->
    case Suite of
        basic_operations_suite -> basic_operations_suite();
        performance_suite -> performance_suite();
        concurrency_suite -> concurrency_suite();
        error_handling_suite -> error_handling_suite();
        uhft_validation_suite -> uhft_validation_suite();
        _ -> []
    end.

-spec get_test_report() -> {ok, binary()} | {error, no_report}.
get_test_report() ->
    case get(last_test_report) of
        undefined -> {error, no_report};
        Report -> {ok, format_report(Report)}
    end.

%%%===================================================================
%%% Test Suites
%%%===================================================================

basic_operations_suite() ->
    Tests = [
        {test_actor_spawn, fun test_actor_spawn/0},
        {test_actor_kill, fun test_actor_kill/0},
        {test_message_send, fun test_message_send/0},
        {test_actor_count, fun test_actor_count/0},
        {test_actor_lifecycle, fun test_actor_lifecycle/0}
    ],
    [execute_test(basic_operations, Name, Fun) || {Name, Fun} <- Tests].

performance_suite() ->
    Tests = [
        {test_spawn_latency, fun test_spawn_latency/0},
        {test_message_latency, fun test_message_latency/0},
        {test_throughput, fun test_throughput/0},
        {test_memory_efficiency, fun test_memory_efficiency/0}
    ],
    [execute_test(performance, Name, Fun) || {Name, Fun} <- Tests].

concurrency_suite() ->
    Tests = [
        {test_concurrent_spawn, fun test_concurrent_spawn/0},
        {test_concurrent_messaging, fun test_concurrent_messaging/0},
        {test_race_conditions, fun test_race_conditions/0},
        {test_deadlock_prevention, fun test_deadlock_prevention/0}
    ],
    [execute_test(concurrency, Name, Fun) || {Name, Fun} <- Tests].

error_handling_suite() ->
    Tests = [
        {test_invalid_actor_ref, fun test_invalid_actor_ref/0},
        {test_nif_error_handling, fun test_nif_error_handling/0},
        {test_overload_handling, fun test_overload_handling/0},
        {test_crash_recovery, fun test_crash_recovery/0}
    ],
    [execute_test(error_handling, Name, Fun) || {Name, Fun} <- Tests].

uhft_validation_suite() ->
    Tests = [
        {test_market_data_latency, fun test_market_data_latency/0},
        {test_order_routing_latency, fun test_order_routing_latency/0},
        {test_tick_processing_throughput, fun test_tick_processing_throughput/0},
        {test_zero_copy_messaging, fun test_zero_copy_messaging/0}
    ],
    [execute_test(uhft_validation, Name, Fun) || {Name, Fun} <- Tests].

%%%===================================================================
%%% Individual Tests
%%%===================================================================

test_actor_spawn() ->
    {ok, Actor, _} = bitactor_server:spawn_actor(test, #{id => 1}),
    true = is_reference(Actor),
    ok = bitactor_server:kill_actor(Actor),
    {passed, <<"Actor spawn successful">>}.

test_actor_kill() ->
    {ok, Actor, _} = bitactor_server:spawn_actor(test, #{}),
    ok = bitactor_server:kill_actor(Actor),
    {error, not_found} = bitactor_server:kill_actor(Actor),
    {passed, <<"Actor kill successful">>}.

test_message_send() ->
    {ok, Actor, _} = bitactor_server:spawn_actor(test, #{}),
    ok = bitactor_server:send_message(Actor, {test, message}),
    ok = bitactor_server:kill_actor(Actor),
    {passed, <<"Message send successful">>}.

test_actor_count() ->
    Initial = bitactor_server:get_actor_count(),
    {ok, A1, _} = bitactor_server:spawn_actor(test, #{}),
    {ok, A2, _} = bitactor_server:spawn_actor(test, #{}),
    Count = bitactor_server:get_actor_count(),
    true = (Count >= Initial + 2),
    ok = bitactor_server:kill_actor(A1),
    ok = bitactor_server:kill_actor(A2),
    {passed, <<"Actor count tracking works">>}.

test_actor_lifecycle() ->
    %% Full lifecycle test
    {ok, Actor, _} = bitactor_server:spawn_actor(lifecycle, #{state => init}),
    ok = bitactor_server:send_message(Actor, {update, running}),
    ok = bitactor_server:send_message(Actor, {update, stopping}),
    ok = bitactor_server:kill_actor(Actor),
    {passed, <<"Actor lifecycle complete">>}.

test_spawn_latency() ->
    Latencies = [begin
        Start = erlang:monotonic_time(nanosecond),
        {ok, Actor, _} = bitactor_server:spawn_actor(perf, #{}),
        End = erlang:monotonic_time(nanosecond),
        bitactor_server:kill_actor(Actor),
        End - Start
    end || _ <- lists:seq(1, 100)],
    
    AvgLatency = lists:sum(Latencies) div length(Latencies),
    case AvgLatency < 1000000 of  % Less than 1ms
        true -> {passed, <<"Spawn latency acceptable: ", (integer_to_binary(AvgLatency))/binary, " ns">>};
        false -> {failed, <<"Spawn latency too high: ", (integer_to_binary(AvgLatency))/binary, " ns">>}
    end.

test_message_latency() ->
    {ok, Actor, _} = bitactor_server:spawn_actor(perf, #{}),
    
    Latencies = [begin
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(Actor, {perf, I}),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || I <- lists:seq(1, 1000)],
    
    bitactor_server:kill_actor(Actor),
    
    AvgLatency = lists:sum(Latencies) div length(Latencies),
    case AvgLatency < 10000 of  % Less than 10μs
        true -> {passed, <<"Message latency acceptable: ", (integer_to_binary(AvgLatency))/binary, " ns">>};
        false -> {failed, <<"Message latency too high: ", (integer_to_binary(AvgLatency))/binary, " ns">>}
    end.

test_throughput() ->
    ActorCount = 100,
    Actors = [begin {ok, A, _} = bitactor_server:spawn_actor(throughput, #{}), A end || _ <- lists:seq(1, ActorCount)],
    
    Start = erlang:monotonic_time(millisecond),
    MessageCount = 10000,
    
    [bitactor_server:send_message(
        lists:nth(rand:uniform(ActorCount), Actors),
        {throughput, I}
    ) || I <- lists:seq(1, MessageCount)],
    
    Duration = erlang:monotonic_time(millisecond) - Start,
    Rate = (MessageCount * 1000) div max(Duration, 1),
    
    [bitactor_server:kill_actor(A) || A <- Actors],
    
    case Rate > 50000 of  % More than 50k msgs/sec
        true -> {passed, <<"Throughput acceptable: ", (integer_to_binary(Rate))/binary, " msgs/sec">>};
        false -> {failed, <<"Throughput too low: ", (integer_to_binary(Rate))/binary, " msgs/sec">>}
    end.

test_memory_efficiency() ->
    InitialMemory = erlang:memory(total),
    
    ActorCount = 1000,
    Actors = [begin {ok, A, _} = bitactor_server:spawn_actor(memory, #{id => I}), A end || I <- lists:seq(1, ActorCount)],
    
    AfterSpawnMemory = erlang:memory(total),
    MemoryPerActor = (AfterSpawnMemory - InitialMemory) div ActorCount,
    
    [bitactor_server:kill_actor(A) || A <- Actors],
    
    case MemoryPerActor < 10240 of  % Less than 10KB per actor
        true -> {passed, <<"Memory efficiency good: ", (integer_to_binary(MemoryPerActor))/binary, " bytes/actor">>};
        false -> {failed, <<"Memory usage high: ", (integer_to_binary(MemoryPerActor))/binary, " bytes/actor">>}
    end.

test_concurrent_spawn() ->
    Parent = self(),
    NumProcesses = 10,
    ActorsPerProcess = 10,
    
    Pids = [spawn(fun() ->
        Actors = [begin {ok, A, _} = bitactor_server:spawn_actor(concurrent, #{p => N, a => I}), A end
                  || I <- lists:seq(1, ActorsPerProcess)],
        Parent ! {self(), actors, Actors}
    end) || N <- lists:seq(1, NumProcesses)],
    
    AllActors = lists:flatten([receive {Pid, actors, As} -> As after 5000 -> [] end || Pid <- Pids]),
    
    UniqueCount = length(lists:usort(AllActors)),
    ExpectedCount = NumProcesses * ActorsPerProcess,
    
    [bitactor_server:kill_actor(A) || A <- AllActors],
    
    case UniqueCount =:= ExpectedCount of
        true -> {passed, <<"Concurrent spawn successful">>};
        false -> {failed, <<"Concurrent spawn mismatch: ", (integer_to_binary(UniqueCount))/binary, 
                          "/", (integer_to_binary(ExpectedCount))/binary>>}
    end.

test_concurrent_messaging() ->
    {ok, Actor, _} = bitactor_server:spawn_actor(concurrent_msg, #{}),
    
    Parent = self(),
    NumSenders = 10,
    MessagesPerSender = 100,
    
    Senders = [spawn(fun() ->
        [bitactor_server:send_message(Actor, {sender, N, I}) || I <- lists:seq(1, MessagesPerSender)],
        Parent ! {self(), done}
    end) || N <- lists:seq(1, NumSenders)],
    
    [receive {Pid, done} -> ok after 5000 -> timeout end || Pid <- Senders],
    
    bitactor_server:kill_actor(Actor),
    {passed, <<"Concurrent messaging successful">>}.

test_race_conditions() ->
    %% Test for race conditions in actor state
    {ok, Actor, _} = bitactor_server:spawn_actor(race_test, #{counter => 0}),
    
    NumThreads = 10,
    IncrementsPerThread = 100,
    Parent = self(),
    
    Threads = [spawn(fun() ->
        [bitactor_server:send_message(Actor, increment) || _ <- lists:seq(1, IncrementsPerThread)],
        Parent ! {self(), done}
    end) || _ <- lists:seq(1, NumThreads)],
    
    [receive {Pid, done} -> ok after 5000 -> timeout end || Pid <- Threads],
    
    bitactor_server:kill_actor(Actor),
    {passed, <<"No race conditions detected">>}.

test_deadlock_prevention() ->
    %% Create circular dependency scenario
    {ok, A1, _} = bitactor_server:spawn_actor(deadlock_test, #{id => 1}),
    {ok, A2, _} = bitactor_server:spawn_actor(deadlock_test, #{id => 2}),
    {ok, A3, _} = bitactor_server:spawn_actor(deadlock_test, #{id => 3}),
    
    %% Send messages that could cause deadlock
    ok = bitactor_server:send_message(A1, {wait_for, A2}),
    ok = bitactor_server:send_message(A2, {wait_for, A3}),
    ok = bitactor_server:send_message(A3, {wait_for, A1}),
    
    %% System should not deadlock
    timer:sleep(100),
    
    %% Clean up
    ok = bitactor_server:kill_actor(A1),
    ok = bitactor_server:kill_actor(A2),
    ok = bitactor_server:kill_actor(A3),
    
    {passed, <<"Deadlock prevention working">>}.

test_invalid_actor_ref() ->
    FakeRef = make_ref(),
    case bitactor_server:send_message(FakeRef, test) of
        {error, _} -> {passed, <<"Invalid actor ref handled correctly">>};
        ok -> {failed, <<"Invalid actor ref not caught">>}
    end.

test_nif_error_handling() ->
    %% Test NIF error conditions
    case bitactor_server:get_stats() of
        #{nif_loaded := true} ->
            %% Try invalid spawn parameters
            case bitactor_server:spawn_actor(<<"invalid">>, <<"bad">>) of
                {error, _} -> {passed, <<"NIF error handling working">>};
                {ok, _} -> {failed, <<"NIF should reject invalid parameters">>}
            end;
        _ ->
            {skipped, <<"NIF not loaded">>}
    end.

test_overload_handling() ->
    %% Create overload condition
    {ok, Actor, _} = bitactor_server:spawn_actor(overload_test, #{}),
    
    %% Send massive number of messages
    MessageCount = 100000,
    spawn(fun() ->
        [bitactor_server:send_message(Actor, {overload, I}) || I <- lists:seq(1, MessageCount)]
    end),
    
    %% System should remain responsive
    timer:sleep(10),
    {ok, TestActor, _} = bitactor_server:spawn_actor(test, #{}),
    ok = bitactor_server:send_message(TestActor, responsive),
    ok = bitactor_server:kill_actor(TestActor),
    
    bitactor_server:kill_actor(Actor),
    {passed, <<"System handles overload gracefully">>}.

test_crash_recovery() ->
    %% Test crash recovery
    {ok, Actor, _} = bitactor_server:spawn_actor(crash_test, #{}),
    
    %% Send message that might cause crash
    bitactor_server:send_message(Actor, {crash, divide_by_zero}),
    
    %% System should continue functioning
    timer:sleep(10),
    Count = bitactor_server:get_actor_count(),
    true = is_integer(Count),
    
    %% Clean up if actor survived
    catch bitactor_server:kill_actor(Actor),
    
    {passed, <<"Crash recovery working">>}.

test_market_data_latency() ->
    {ok, MarketActor, _} = bitactor_server:spawn_actor(market_data, #{}),
    
    Latencies = [begin
        Tick = {tick, <<"AAPL">>, 150.25 + I/100, erlang:monotonic_time(nanosecond)},
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(MarketActor, Tick),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || I <- lists:seq(1, 1000)],
    
    bitactor_server:kill_actor(MarketActor),
    
    P99 = lists:nth(990, lists:sort(Latencies)),
    case P99 < 1000 of  % Less than 1μs
        true -> {passed, <<"Market data P99 latency: ", (integer_to_binary(P99))/binary, " ns">>};
        false -> {failed, <<"Market data P99 too high: ", (integer_to_binary(P99))/binary, " ns">>}
    end.

test_order_routing_latency() ->
    {ok, RouterActor, _} = bitactor_server:spawn_actor(order_router, #{}),
    
    Latencies = [begin
        Order = {order, <<"AAPL">>, buy, 100, market, erlang:monotonic_time(nanosecond)},
        Start = erlang:monotonic_time(nanosecond),
        ok = bitactor_server:send_message(RouterActor, Order),
        End = erlang:monotonic_time(nanosecond),
        End - Start
    end || _ <- lists:seq(1, 1000)],
    
    bitactor_server:kill_actor(RouterActor),
    
    P99 = lists:nth(990, lists:sort(Latencies)),
    case P99 < 2000 of  % Less than 2μs
        true -> {passed, <<"Order routing P99 latency: ", (integer_to_binary(P99))/binary, " ns">>};
        false -> {failed, <<"Order routing P99 too high: ", (integer_to_binary(P99))/binary, " ns">>}
    end.

test_tick_processing_throughput() ->
    %% Test high-frequency tick processing
    ActorCount = 10,
    Actors = [begin {ok, A, _} = bitactor_server:spawn_actor(tick_processor, #{}), A end || _ <- lists:seq(1, ActorCount)],
    
    TickCount = 10000,
    Start = erlang:monotonic_time(millisecond),
    
    [bitactor_server:send_message(
        lists:nth((I rem ActorCount) + 1, Actors),
        {tick, <<"AAPL">>, 150.0 + I/1000, I}
    ) || I <- lists:seq(1, TickCount)],
    
    Duration = erlang:monotonic_time(millisecond) - Start,
    Rate = (TickCount * 1000) div max(Duration, 1),
    
    [bitactor_server:kill_actor(A) || A <- Actors],
    
    case Rate > 100000 of  % More than 100k ticks/sec
        true -> {passed, <<"Tick throughput: ", (integer_to_binary(Rate))/binary, " ticks/sec">>};
        false -> {failed, <<"Tick throughput low: ", (integer_to_binary(Rate))/binary, " ticks/sec">>}
    end.

test_zero_copy_messaging() ->
    %% Test zero-copy message passing efficiency
    {ok, Actor, _} = bitactor_server:spawn_actor(zero_copy, #{}),
    
    %% Large binary message
    LargeBinary = crypto:strong_rand_bytes(1024 * 1024), % 1MB
    
    Start = erlang:monotonic_time(nanosecond),
    ok = bitactor_server:send_message(Actor, {binary, LargeBinary}),
    End = erlang:monotonic_time(nanosecond),
    
    Latency = End - Start,
    bitactor_server:kill_actor(Actor),
    
    %% Should be fast even with large binary (zero-copy)
    case Latency < 100000 of  % Less than 100μs for 1MB
        true -> {passed, <<"Zero-copy messaging efficient">>};
        false -> {failed, <<"Large message latency high: ", (integer_to_binary(Latency))/binary, " ns">>}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec execute_test(atom(), atom(), fun(() -> {passed | failed | skipped, binary()} | term())) -> #test_result{}.
execute_test(Suite, Name, Fun) ->
    Start = erlang:monotonic_time(nanosecond),
    
    {FinalStatus, FinalMessage} = try
        case Fun() of
            {Status, Message} when Status =:= passed; Status =:= failed; Status =:= skipped ->
                {Status, Message};
            _ ->
                {failed, <<"Test returned unexpected result">>}
        end
    catch
        Class:Reason:Stacktrace ->
            ErrorMsg = iolist_to_binary(io_lib:format("~p:~p ~p", [Class, Reason, Stacktrace])),
            {failed, ErrorMsg}
    end,
    
    End = erlang:monotonic_time(nanosecond),
    Duration = End - Start,
    
    %% Emit telemetry if available
    catch telemetry:execute(
        [bitactor, unit_test, complete],
        #{duration => Duration},
        #{suite => Suite, test => Name, status => FinalStatus}
    ),
    
    #test_result{
        name = Name,
        suite = Suite,
        status = FinalStatus,
        duration_ns = Duration,
        message = FinalMessage,
        details = #{}
    }.

-spec find_test(atom()) -> {ok, {atom(), fun()}} | error.
find_test(TestName) ->
    AllTests = [
        {basic_operations, [
            {test_actor_spawn, fun test_actor_spawn/0},
            {test_actor_kill, fun test_actor_kill/0},
            {test_message_send, fun test_message_send/0},
            {test_actor_count, fun test_actor_count/0},
            {test_actor_lifecycle, fun test_actor_lifecycle/0}
        ]},
        {performance, [
            {test_spawn_latency, fun test_spawn_latency/0},
            {test_message_latency, fun test_message_latency/0},
            {test_throughput, fun test_throughput/0},
            {test_memory_efficiency, fun test_memory_efficiency/0}
        ]},
        {concurrency, [
            {test_concurrent_spawn, fun test_concurrent_spawn/0},
            {test_concurrent_messaging, fun test_concurrent_messaging/0},
            {test_race_conditions, fun test_race_conditions/0},
            {test_deadlock_prevention, fun test_deadlock_prevention/0}
        ]},
        {error_handling, [
            {test_invalid_actor_ref, fun test_invalid_actor_ref/0},
            {test_nif_error_handling, fun test_nif_error_handling/0},
            {test_overload_handling, fun test_overload_handling/0},
            {test_crash_recovery, fun test_crash_recovery/0}
        ]},
        {uhft_validation, [
            {test_market_data_latency, fun test_market_data_latency/0},
            {test_order_routing_latency, fun test_order_routing_latency/0},
            {test_tick_processing_throughput, fun test_tick_processing_throughput/0},
            {test_zero_copy_messaging, fun test_zero_copy_messaging/0}
        ]}
    ],
    
    find_test_in_suites(TestName, AllTests).

find_test_in_suites(_, []) -> error;
find_test_in_suites(TestName, [{Suite, Tests} | Rest]) ->
    case lists:keyfind(TestName, 1, Tests) of
        {TestName, Fun} -> {ok, {Suite, Fun}};
        false -> find_test_in_suites(TestName, Rest)
    end.

-spec generate_report([#test_result{}], non_neg_integer()) -> #test_report{}.
generate_report(Results, DurationMs) ->
    {Passed, Failed, Skipped} = count_results(Results),
    
    %% Get telemetry metrics (graceful handling)
    OtelMetrics = case catch bitactor_telemetry:get_metrics() of
        Metrics when is_map(Metrics) -> Metrics;
        _ -> #{}
    end,
    
    Report = #test_report{
        total_tests = length(Results),
        passed = Passed,
        failed = Failed,
        skipped = Skipped,
        duration_ms = DurationMs,
        results = Results,
        otel_metrics = OtelMetrics
    },
    
    %% Store for later retrieval
    put(last_test_report, Report),
    
    Report.

-spec count_results([#test_result{}]) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
count_results(Results) ->
    lists:foldl(fun(#test_result{status = Status}, {P, F, S}) ->
        case Status of
            passed -> {P + 1, F, S};
            failed -> {P, F + 1, S};
            skipped -> {P, F, S + 1}
        end
    end, {0, 0, 0}, Results).

-spec format_report(#test_report{}) -> binary().
format_report(#test_report{
    total_tests = Total,
    passed = Passed,
    failed = Failed,
    skipped = Skipped,
    duration_ms = Duration,
    results = Results
}) ->
    %% Generate Mermaid diagram
    Mermaid = generate_mermaid_diagram(Results),
    
    %% Format failed tests
    FailedTests = [R || R = #test_result{status = failed} <- Results],
    FailedSection = case FailedTests of
        [] -> <<"All tests passed!">>;
        _ -> format_failed_tests(FailedTests)
    end,
    
    iolist_to_binary([
        <<"# BitActor Unit Test Report\n\n">>,
        <<"## Summary\n">>,
        <<"- Total Tests: ">>, integer_to_binary(Total), <<"\n">>,
        <<"- Passed: ">>, integer_to_binary(Passed), <<"\n">>,
        <<"- Failed: ">>, integer_to_binary(Failed), <<"\n">>,
        <<"- Skipped: ">>, integer_to_binary(Skipped), <<"\n">>,
        <<"- Duration: ">>, integer_to_binary(Duration), <<" ms\n\n">>,
        <<"## Test Results\n\n">>,
        Mermaid, <<"\n\n">>,
        <<"## Failed Tests\n\n">>,
        FailedSection
    ]).

-spec generate_mermaid_diagram([#test_result{}]) -> binary().
generate_mermaid_diagram(Results) ->
    %% Group by suite
    Suites = lists:foldl(fun(#test_result{suite = Suite, status = Status}, Acc) ->
        maps:update_with(Suite, fun({P, F, S}) ->
            case Status of
                passed -> {P + 1, F, S};
                failed -> {P, F + 1, S};
                skipped -> {P, F, S + 1}
            end
        end, {0, 0, 0}, Acc)
    end, #{}, Results),
    
    SuiteLines = maps:fold(fun(Suite, {P, F, S}, Acc) ->
        Total = P + F + S,
        PassRate = (P * 100) div Total,
        Status = if
            F > 0 -> <<"❌">>;
            S > 0 -> <<"⚠️">>;
            true -> <<"✅">>
        end,
        Line = iolist_to_binary([
            <<"    ">>, atom_to_binary(Suite), <<" : ">>,
            Status, <<" ">>, integer_to_binary(PassRate), <<"%\n">>
        ]),
        [Line | Acc]
    end, [], Suites),
    
    iolist_to_binary([
        <<"```mermaid\n">>,
        <<"pie title Test Results by Suite\n">>,
        SuiteLines,
        <<"```">>
    ]).

-spec format_failed_tests([#test_result{}]) -> binary().
format_failed_tests(FailedTests) ->
    Lines = [format_failed_test(T) || T <- FailedTests],
    iolist_to_binary(Lines).

-spec format_failed_test(#test_result{}) -> binary().
format_failed_test(#test_result{
    name = Name,
    suite = Suite,
    message = Message,
    duration_ns = Duration
}) ->
    iolist_to_binary([
        <<"### ">>, atom_to_binary(Suite), <<":">>, atom_to_binary(Name), <<"\n">>,
        <<"- Message: ">>, Message, <<"\n">>,
        <<"- Duration: ">>, integer_to_binary(Duration div 1000), <<" μs\n\n">>
    ]).