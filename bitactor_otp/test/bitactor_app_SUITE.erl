%%%-------------------------------------------------------------------
%%% @doc BitActor Application Test Suite
%%% Comprehensive unit tests for bitactor_app module
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_app_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Common Test Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        test_start_stop,
        test_env_functions,
        test_uhft_optimizations,
        test_telemetry_initialization,
        test_memory_management,
        test_scheduler_optimization,
        test_error_handling,
        test_supervision_tree,
        test_configuration_validation,
        test_performance_metrics
    ].

init_per_suite(Config) ->
    %% Stop application if running
    application:stop(bitactor),
    Config.

end_per_suite(_Config) ->
    application:stop(bitactor),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure clean state
    application:stop(bitactor),
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(bitactor),
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_start_stop(_Config) ->
    %% Test normal startup
    {ok, Pid} = bitactor_app:start(normal, []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    
    %% Test application is registered
    ?assertMatch(Pid, whereis(bitactor_sup)),
    
    %% Test stop
    ok = bitactor_app:stop([]),
    
    %% Verify cleanup
    timer:sleep(100),
    ?assertEqual(undefined, whereis(bitactor_sup)),
    ok.

test_env_functions(_Config) ->
    %% Test get_env/1
    application:load(bitactor),
    application:set_env(bitactor, test_key, test_value),
    
    ?assertEqual(test_value, bitactor_app:get_env(test_key)),
    ?assertEqual(undefined, bitactor_app:get_env(nonexistent_key)),
    
    %% Test get_env/2 with default
    ?assertEqual(default_value, bitactor_app:get_env(nonexistent_key, default_value)),
    ?assertEqual(test_value, bitactor_app:get_env(test_key, default_value)),
    
    ok.

test_uhft_optimizations(_Config) ->
    %% Mock system_flag calls to avoid affecting test environment
    meck:new(erlang, [unstick]),
    meck:expect(erlang, system_flag, fun(_, _) -> ok end),
    meck:expect(erlang, garbage_collect, fun() -> ok end),
    
    %% Test UHFT optimizations are applied
    application:load(bitactor),
    application:set_env(bitactor, process_flags, [{priority, high}, {fullsweep_after, 1000}]),
    application:set_env(bitactor, memory_pool_size, 1000000),
    
    %% Start application - this should trigger optimizations
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    %% Verify system_flag was called for optimizations
    ?assert(meck:called(erlang, system_flag, [priority, high])),
    ?assert(meck:called(erlang, system_flag, [fullsweep_after, 1000])),
    ?assert(meck:called(erlang, system_flag, [scheduler_bind_type, thread_spread])),
    
    meck:unload(erlang),
    ok.

test_telemetry_initialization(_Config) ->
    %% Test with telemetry enabled
    application:load(bitactor),
    application:set_env(bitactor, enable_telemetry, true),
    
    %% Mock telemetry module
    meck:new(telemetry, []),
    meck:expect(telemetry, attach_many, fun(_, _, _, _) -> ok end),
    
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    %% Verify telemetry was initialized
    ?assert(meck:called(telemetry, attach_many, ['_', '_', '_', '_'])),
    
    meck:unload(telemetry),
    
    %% Test with telemetry disabled
    application:stop(bitactor),
    application:set_env(bitactor, enable_telemetry, false),
    
    {ok, _Pid2} = bitactor_app:start(normal, []),
    
    ok.

test_memory_management(_Config) ->
    %% Test memory pool allocation
    application:load(bitactor),
    application:set_env(bitactor, memory_pool_size, 1000000),
    
    %% Mock binary operations
    meck:new(binary, []),
    meck:expect(binary, copy, fun(_, _) -> <<0:8000000>> end),
    meck:expect(erlang, garbage_collect, fun() -> ok end),
    
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    %% Verify memory operations
    ?assert(meck:called(binary, copy, [<<0>>, 1000])),
    ?assert(meck:called(erlang, garbage_collect, [])),
    
    meck:unload([binary]),
    ok.

test_scheduler_optimization(_Config) ->
    %% Test scheduler binding and CPU optimization
    meck:new(erlang, [unstick]),
    meck:expect(erlang, system_flag, fun(_, _) -> ok end),
    meck:expect(erlang, system_info, fun(schedulers) -> 8 end),
    
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    %% Verify scheduler optimizations
    ?assert(meck:called(erlang, system_flag, [scheduler_bind_type, thread_spread])),
    ?assert(meck:called(erlang, system_flag, [dirty_cpu_schedulers_online, 8])),
    
    meck:unload(erlang),
    ok.

test_error_handling(_Config) ->
    %% Test startup with invalid supervisor
    meck:new(bitactor_sup, []),
    meck:expect(bitactor_sup, start_link, fun() -> {error, test_error} end),
    
    ?assertMatch({error, test_error}, bitactor_app:start(normal, [])),
    
    meck:unload(bitactor_sup),
    ok.

test_supervision_tree(_Config) ->
    %% Test supervision tree structure
    {ok, Pid} = bitactor_app:start(normal, []),
    
    %% Check supervisor is running
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(bitactor_sup)),
    
    %% Get supervisor children
    Children = supervisor:which_children(bitactor_sup),
    ?assert(length(Children) > 0),
    
    %% Check for expected children
    ChildNames = [Name || {Name, _, _, _} <- Children],
    ?assert(lists:member(bitactor_server, ChildNames)),
    
    ok.

test_configuration_validation(_Config) ->
    %% Test various configuration scenarios
    application:load(bitactor),
    
    %% Test default configuration
    application:unset_env(bitactor, process_flags),
    application:unset_env(bitactor, memory_pool_size),
    application:unset_env(bitactor, enable_telemetry),
    
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    %% Should start successfully with defaults
    ?assertNotEqual(undefined, whereis(bitactor_sup)),
    
    ok.

test_performance_metrics(_Config) ->
    %% Test performance monitoring setup
    StartTime = erlang:monotonic_time(millisecond),
    
    {ok, _Pid} = bitactor_app:start(normal, []),
    
    EndTime = erlang:monotonic_time(millisecond),
    StartupTime = EndTime - StartTime,
    
    %% Startup should be reasonably fast (under 5 seconds)
    ?assert(StartupTime < 5000),
    
    %% Check memory usage is reasonable
    MemoryUsage = erlang:memory(total),
    ?assert(MemoryUsage < 100 * 1024 * 1024), % Under 100MB is reasonable
    
    ok.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

%% Utility function to wait for process
wait_for_process(Name, Timeout) ->
    wait_for_process(Name, Timeout, 10).

wait_for_process(_Name, 0, _Interval) ->
    false;
wait_for_process(Name, Timeout, Interval) ->
    case whereis(Name) of
        undefined ->
            timer:sleep(Interval),
            wait_for_process(Name, Timeout - Interval, Interval);
        _Pid ->
            true
    end.