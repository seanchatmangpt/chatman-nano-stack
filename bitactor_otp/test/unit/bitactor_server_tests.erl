%%%-------------------------------------------------------------------
%%% @doc BitActor Server Unit Tests
%%% EUnit tests for bitactor_server module
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/bitactor.hrl").

%%%===================================================================
%%% Test Generator Functions
%%%===================================================================

bitactor_server_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_server_start_stop/0,
         fun test_actor_lifecycle/0,
         fun test_message_sending/0,
         fun test_statistics_tracking/0,
         fun test_error_handling/0
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    %% Start minimal application dependencies
    application:ensure_all_started(lager),
    {ok, Pid} = bitactor_server:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> bitactor_server:stop();
        false -> ok
    end,
    application:stop(lager).

%%%===================================================================
%%% Unit Tests
%%%===================================================================

test_server_start_stop() ->
    %% Server should be running
    ?assert(is_pid(whereis(bitactor_server))),
    
    %% Initial state checks
    ?assertEqual(0, bitactor_server:get_actor_count()),
    Stats = bitactor_server:get_stats(),
    ?assert(is_map(Stats)),
    ?assertEqual(0, maps:get(actors_spawned, Stats, 0)).

test_actor_lifecycle() ->
    %% Test spawning different actor types
    ActorTypes = [market_data, order_book, risk_engine, execution, position],
    
    ActorRefs = lists:map(fun(Type) ->
        {ok, Ref} = bitactor_server:spawn_actor(Type, #{type => Type}),
        ?assert(is_reference(Ref)),
        Ref
    end, ActorTypes),
    
    %% Verify actor count
    ?assertEqual(length(ActorTypes), bitactor_server:get_actor_count()),
    
    %% Kill all actors
    lists:foreach(fun(Ref) ->
        ?assertEqual(ok, bitactor_server:kill_actor(Ref))
    end, ActorRefs),
    
    %% Verify clean state
    ?assertEqual(0, bitactor_server:get_actor_count()).

test_message_sending() ->
    %% Spawn test actors
    {ok, MarketActor} = bitactor_server:spawn_actor(market_data, #{symbol => <<"TEST">>}),
    {ok, OrderActor} = bitactor_server:spawn_actor(order_book, #{symbol => <<"TEST">>}),
    
    %% Send various message types
    Messages = [
        {MarketActor, {tick, <<"TEST">>, 100.0, 1000}},
        {OrderActor, {order, buy, 99.0, 100}},
        {OrderActor, {order, sell, 101.0, 50}}
    ],
    
    lists:foreach(fun({ActorRef, Message}) ->
        ?assertEqual(ok, bitactor_server:send_message(ActorRef, Message))
    end, Messages),
    
    %% Allow message processing
    timer:sleep(50),
    
    %% Verify statistics updated
    Stats = bitactor_server:get_stats(),
    ?assert(maps:get(messages_sent, Stats, 0) >= length(Messages)),
    
    %% Cleanup
    bitactor_server:kill_actor(MarketActor),
    bitactor_server:kill_actor(OrderActor).

test_statistics_tracking() ->
    InitialStats = bitactor_server:get_stats(),
    
    %% Perform operations
    {ok, Actor1} = bitactor_server:spawn_actor(market_data, #{}),
    {ok, Actor2} = bitactor_server:spawn_actor(order_book, #{}),
    
    bitactor_server:send_message(Actor1, test_message),
    bitactor_server:send_message(Actor2, test_message),
    
    bitactor_server:kill_actor(Actor1),
    bitactor_server:kill_actor(Actor2),
    
    %% Allow processing
    timer:sleep(50),
    
    %% Verify statistics
    FinalStats = bitactor_server:get_stats(),
    
    ?assert(maps:get(actors_spawned, FinalStats, 0) > 
            maps:get(actors_spawned, InitialStats, 0)),
    ?assert(maps:get(actors_killed, FinalStats, 0) > 
            maps:get(actors_killed, InitialStats, 0)),
    ?assert(maps:get(messages_sent, FinalStats, 0) > 
            maps:get(messages_sent, InitialStats, 0)).

test_error_handling() ->
    %% Test killing non-existent actor
    FakeRef = make_ref(),
    ?assertEqual({error, not_found}, bitactor_server:kill_actor(FakeRef)),
    
    %% Test sending message to non-existent actor
    %% This should not crash the server
    bitactor_server:send_message(FakeRef, test_message),
    
    %% Server should still be responsive
    ?assertEqual(0, bitactor_server:get_actor_count()),
    ?assert(is_pid(whereis(bitactor_server))).