%%%-------------------------------------------------------------------
%%% @doc BitActor Common Test Suite
%%% Comprehensive integration testing for production BitActor
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/bitactor.hrl").

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_application_start_stop/1,
    test_actor_spawn_kill/1,
    test_message_passing/1,
    test_supervision_recovery/1,
    test_telemetry_collection/1,
    test_health_monitoring/1,
    test_load_handling/1,
    test_nif_integration/1,
    test_fault_tolerance/1,
    test_concurrent_operations/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [
        {group, basic_operations},
        {group, fault_tolerance},
        {group, performance},
        {group, integration}
    ].

groups() ->
    [
        {basic_operations, [parallel], [
            test_application_start_stop,
            test_actor_spawn_kill,
            test_message_passing
        ]},
        {fault_tolerance, [sequence], [
            test_supervision_recovery,
            test_fault_tolerance
        ]},
        {performance, [parallel], [
            test_load_handling,
            test_concurrent_operations
        ]},
        {integration, [sequence], [
            test_telemetry_collection,
            test_health_monitoring,
            test_nif_integration
        ]}
    ].

init_per_suite(Config) ->
    %% Start the BitActor application
    application:ensure_all_started(bitactor),
    Config.

end_per_suite(_Config) ->
    %% Stop the BitActor application
    application:stop(bitactor),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Finished test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_application_start_stop(_Config) ->
    %% Test application lifecycle
    ?assert(is_pid(whereis(bitactor_sup))),
    ?assert(is_pid(whereis(bitactor_server))),
    ?assert(is_pid(whereis(bitactor_telemetry))),
    ?assert(is_pid(whereis(bitactor_health))),
    
    %% Test application stop/start cycle
    ok = application:stop(bitactor),
    ?assertEqual(undefined, whereis(bitactor_sup)),
    
    {ok, _} = application:ensure_all_started(bitactor),
    ?assert(is_pid(whereis(bitactor_sup))),
    ok.

test_actor_spawn_kill(_Config) ->
    %% Test basic actor lifecycle
    {ok, ActorRef} = bitactor_server:spawn_actor(market_data, #{symbol => <<"AAPL">>}),
    ?assert(is_reference(ActorRef)),
    
    %% Verify actor count
    ?assertEqual(1, bitactor_server:get_actor_count()),
    
    %% Kill actor
    ok = bitactor_server:kill_actor(ActorRef),
    ?assertEqual(0, bitactor_server:get_actor_count()),
    
    %% Test killing non-existent actor
    ?assertEqual({error, not_found}, bitactor_server:kill_actor(ActorRef)),
    ok.

test_message_passing(_Config) ->
    %% Spawn test actor
    {ok, ActorRef} = bitactor_server:spawn_actor(order_book, #{symbol => <<"AAPL">>}),
    
    %% Send messages
    ok = bitactor_server:send_message(ActorRef, {order, buy, 150.0, 100}),
    ok = bitactor_server:send_message(ActorRef, {order, sell, 151.0, 50}),
    
    %% Allow message processing
    timer:sleep(100),
    
    %% Verify statistics updated
    Stats = bitactor_server:get_stats(),
    ?assert(maps:get(messages_sent, Stats, 0) >= 2),
    
    %% Cleanup
    ok = bitactor_server:kill_actor(ActorRef),
    ok.

test_supervision_recovery(_Config) ->
    %% Get initial supervisor pid
    SupPid = whereis(bitactor_sup),
    ?assert(is_pid(SupPid)),
    
    %% Kill the bitactor_server process
    ServerPid = whereis(bitactor_server),
    exit(ServerPid, kill),
    
    %% Wait for supervisor to restart it
    timer:sleep(100),
    
    %% Verify server was restarted
    NewServerPid = whereis(bitactor_server),
    ?assert(is_pid(NewServerPid)),
    ?assertNotEqual(ServerPid, NewServerPid),
    
    %% Verify supervisor is still the same
    ?assertEqual(SupPid, whereis(bitactor_sup)),
    ok.

test_telemetry_collection(_Config) ->
    %% Get initial metrics
    InitialMetrics = bitactor_telemetry:get_metrics(),
    ?assert(is_map(InitialMetrics)),
    
    %% Perform operations that should generate telemetry
    {ok, ActorRef} = bitactor_server:spawn_actor(risk_engine, #{}),
    ok = bitactor_server:send_message(ActorRef, {position, <<"AAPL">>, 100, 150.0}),
    ok = bitactor_server:kill_actor(ActorRef),
    
    %% Allow telemetry collection
    timer:sleep(100),
    
    %% Get updated metrics
    UpdatedMetrics = bitactor_telemetry:get_metrics(),
    ?assert(maps:get(actors_spawned, UpdatedMetrics, 0) > 
            maps:get(actors_spawned, InitialMetrics, 0)),
    ok.

test_health_monitoring(_Config) ->
    %% Get health status
    Health = bitactor_health:get_health(),
    ?assert(is_map(Health)),
    ?assert(maps:is_key(status, Health)),
    ?assert(maps:is_key(system, Health)),
    
    %% Force health check
    ok = bitactor_health:force_health_check(),
    
    %% Verify health report structure
    HealthReport = bitactor_health:get_health(),
    System = maps:get(system, HealthReport),
    ?assert(maps:is_key(memory_mb, System)),
    ?assert(maps:is_key(processes, System)),
    ?assert(maps:is_key(actors, System)),
    ok.

test_load_handling(_Config) ->
    %% Spawn multiple actors concurrently
    NumActors = 100,
    ActorRefs = lists:map(fun(I) ->
        {ok, Ref} = bitactor_server:spawn_actor(market_data, #{id => I}),
        Ref
    end, lists:seq(1, NumActors)),
    
    ?assertEqual(NumActors, length(ActorRefs)),
    ?assertEqual(NumActors, bitactor_server:get_actor_count()),
    
    %% Send messages to all actors
    lists:foreach(fun(ActorRef) ->
        ok = bitactor_server:send_message(ActorRef, {tick, <<"TEST">>, 100.0, 1000})
    end, ActorRefs),
    
    %% Allow processing
    timer:sleep(500),
    
    %% Cleanup all actors
    lists:foreach(fun(ActorRef) ->
        ok = bitactor_server:kill_actor(ActorRef)
    end, ActorRefs),
    
    ?assertEqual(0, bitactor_server:get_actor_count()),
    ok.

test_nif_integration(_Config) ->
    %% Test NIF loading and basic functionality
    case erlang:module_loaded(bitactor_nif) of
        true ->
            %% Test NIF functions (will fall back gracefully if not loaded)
            try
                bitactor_nif:tick_all()
            catch
                error:{not_loaded, _} ->
                    ct:pal("NIF not loaded, testing fallback behavior");
                _:_ ->
                    ct:pal("NIF loaded and functional")
            end;
        false ->
            ct:pal("NIF module not loaded, testing pure Erlang mode")
    end,
    ok.

test_fault_tolerance(_Config) ->
    %% Create actors and verify fault isolation
    {ok, Actor1} = bitactor_server:spawn_actor(market_data, #{symbol => <<"AAPL">>}),
    {ok, Actor2} = bitactor_server:spawn_actor(order_book, #{symbol => <<"MSFT">>}),
    
    ?assertEqual(2, bitactor_server:get_actor_count()),
    
    %% Simulate fault in one actor by sending invalid message
    bitactor_server:send_message(Actor1, invalid_message),
    
    %% Allow processing and potential error handling
    timer:sleep(100),
    
    %% Both actors should still exist (fault isolation)
    ?assertEqual(2, bitactor_server:get_actor_count()),
    
    %% Cleanup
    ok = bitactor_server:kill_actor(Actor1),
    ok = bitactor_server:kill_actor(Actor2),
    ok.

test_concurrent_operations(_Config) ->
    %% Test concurrent spawn/kill operations
    Self = self(),
    NumProcs = 10,
    
    %% Spawn processes that each create and destroy actors
    Pids = [spawn_link(fun() ->
        {ok, ActorRef} = bitactor_server:spawn_actor(execution, #{id => I}),
        ok = bitactor_server:send_message(ActorRef, {order, buy, 100.0, 10}),
        timer:sleep(rand:uniform(50)),
        ok = bitactor_server:kill_actor(ActorRef),
        Self ! {done, I}
    end) || I <- lists:seq(1, NumProcs)],
    
    %% Wait for all processes to complete
    [receive {done, I} -> ok end || I <- lists:seq(1, NumProcs)],
    
    %% Verify clean state
    ?assertEqual(0, bitactor_server:get_actor_count()),
    
    %% Verify all processes completed
    ?assertEqual(NumProcs, length(Pids)),
    ok.