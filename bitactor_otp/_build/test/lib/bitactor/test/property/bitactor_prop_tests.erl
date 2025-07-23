%%%-------------------------------------------------------------------
%%% @doc BitActor Property-Based Tests  
%%% PropEr tests for BitActor system properties
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../include/bitactor.hrl").

%%%===================================================================
%%% Property Test Generators
%%%===================================================================

%% Actor type generator
actor_type() ->
    oneof([market_data, order_book, risk_engine, execution, position]).

%% Message generator
actor_message() ->
    oneof([
        {tick, binary(), float(), pos_integer()},
        {order, oneof([buy, sell]), float(), pos_integer()},
        {cancel, binary()},
        {position, binary(), integer(), float()}
    ]).

%% Actor initialization data generator
actor_init_data() ->
    oneof([
        #{symbol => binary()},
        #{id => pos_integer()},
        #{type => actor_type()},
        #{}
    ]).

%%%===================================================================
%%% Property Tests
%%%===================================================================

%% Property: Actor spawning and killing maintains count consistency
prop_actor_count_consistency() ->
    ?FORALL({NumSpawn, NumKill}, {range(1, 20), range(0, 20)},
        begin
            %% Setup
            application:ensure_all_started(bitactor),
            InitialCount = bitactor_server:get_actor_count(),
            
            %% Spawn actors
            ActorRefs = [begin
                {ok, Ref} = bitactor_server:spawn_actor(actor_type(), actor_init_data()),
                Ref
            end || _ <- lists:seq(1, NumSpawn)],
            
            CountAfterSpawn = bitactor_server:get_actor_count(),
            
            %% Kill some actors
            {ToKill, ToKeep} = lists:split(min(NumKill, length(ActorRefs)), ActorRefs),
            lists:foreach(fun(Ref) ->
                ok = bitactor_server:kill_actor(Ref)
            end, ToKill),
            
            FinalCount = bitactor_server:get_actor_count(),
            
            %% Cleanup remaining actors
            lists:foreach(fun(Ref) ->
                bitactor_server:kill_actor(Ref)
            end, ToKeep),
            
            %% Properties to verify
            CountAfterSpawn =:= InitialCount + NumSpawn andalso
            FinalCount =:= InitialCount + length(ToKeep)
        end).

%% Property: Message sending never crashes the system
prop_message_sending_safety() ->
    ?FORALL({ActorType, InitData, Messages}, 
            {actor_type(), actor_init_data(), list(actor_message())},
        begin
            %% Setup
            application:ensure_all_started(bitactor),
            {ok, ActorRef} = bitactor_server:spawn_actor(ActorType, InitData),
            
            %% Send all messages
            lists:foreach(fun(Message) ->
                bitactor_server:send_message(ActorRef, Message)
            end, Messages),
            
            %% Allow processing
            timer:sleep(100),
            
            %% System should still be responsive
            IsResponsive = try
                bitactor_server:get_actor_count(),
                true
            catch
                _:_ -> false
            end,
            
            %% Cleanup
            bitactor_server:kill_actor(ActorRef),
            
            IsResponsive
        end).

%% Property: Statistics are monotonic (only increase)
prop_statistics_monotonic() ->
    ?FORALL(Operations, list(oneof([spawn, kill, message])),
        begin
            %% Setup
            application:ensure_all_started(bitactor),
            InitialStats = bitactor_server:get_stats(),
            
            %% Execute operations while tracking an actor
            {ok, ActorRef} = bitactor_server:spawn_actor(market_data, #{}),
            
            %% Apply operations
            lists:foldl(fun(Op, Acc) ->
                case Op of
                    spawn ->
                        {ok, NewRef} = bitactor_server:spawn_actor(order_book, #{}),
                        [NewRef | Acc];
                    kill when Acc =/= [] ->
                        [RefToKill | Rest] = Acc,
                        bitactor_server:kill_actor(RefToKill),
                        Rest;
                    kill ->
                        Acc;
                    message ->
                        bitactor_server:send_message(ActorRef, {tick, <<"TEST">>, 100.0, 100}),
                        Acc
                end
            end, [], Operations),
            
            %% Allow processing
            timer:sleep(100),
            
            FinalStats = bitactor_server:get_stats(),
            
            %% Cleanup
            bitactor_server:kill_actor(ActorRef),
            
            %% Properties: stats should only increase
            maps:get(actors_spawned, FinalStats, 0) >= 
                maps:get(actors_spawned, InitialStats, 0) andalso
            maps:get(messages_sent, FinalStats, 0) >= 
                maps:get(messages_sent, InitialStats, 0)
        end).

%% Property: System recovers from supervisor restarts
prop_supervisor_recovery() ->
    ?FORALL(NumActors, range(1, 10),
        begin
            %% Setup
            application:ensure_all_started(bitactor),
            
            %% Spawn actors
            ActorRefs = [begin
                {ok, Ref} = bitactor_server:spawn_actor(actor_type(), #{}),
                Ref
            end || _ <- lists:seq(1, NumActors)],
            
            InitialCount = bitactor_server:get_actor_count(),
            ServerPid = whereis(bitactor_server),
            
            %% Kill the server process
            exit(ServerPid, kill),
            
            %% Wait for supervisor to restart
            timer:sleep(200),
            
            %% System should be responsive again
            IsRecovered = try
                NewServerPid = whereis(bitactor_server),
                is_pid(NewServerPid) andalso NewServerPid =/= ServerPid
            catch
                _:_ -> false
            end,
            
            %% Note: actors will be lost after restart (expected behavior)
            %% This tests that the system structure recovers
            
            IsRecovered
        end).

%% Property: Concurrent operations don't cause race conditions
prop_concurrent_safety() ->
    ?FORALL(NumProcs, range(2, 10),
        begin
            %% Setup
            application:ensure_all_started(bitactor),
            Self = self(),
            
            %% Spawn concurrent processes
            Pids = [spawn_link(fun() ->
                try
                    {ok, ActorRef} = bitactor_server:spawn_actor(market_data, #{id => I}),
                    bitactor_server:send_message(ActorRef, {tick, <<"TEST">>, 100.0, 100}),
                    timer:sleep(rand:uniform(50)),
                    bitactor_server:kill_actor(ActorRef),
                    Self ! {ok, I}
                catch
                    Error:Reason ->
                        Self ! {error, I, Error, Reason}
                end
            end) || I <- lists:seq(1, NumProcs)],
            
            %% Collect results
            Results = [receive
                {ok, I} -> ok;
                {error, I, Error, Reason} -> {error, I, Error, Reason}
            end || I <- lists:seq(1, NumProcs)],
            
            %% All operations should succeed
            AllSucceeded = lists:all(fun(R) -> R =:= ok end, Results),
            
            %% System should be clean
            FinalCount = bitactor_server:get_actor_count(),
            
            AllSucceeded andalso FinalCount =:= 0
        end).

%%%===================================================================
%%% EUnit Integration
%%%===================================================================

proper_test_() ->
    {timeout, 60, [
        ?_assert(proper:quickcheck(prop_actor_count_consistency(), 
                                  [{to_file, user}, {numtests, 50}])),
        ?_assert(proper:quickcheck(prop_message_sending_safety(), 
                                  [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_statistics_monotonic(), 
                                  [{to_file, user}, {numtests, 50}])),
        ?_assert(proper:quickcheck(prop_supervisor_recovery(), 
                                  [{to_file, user}, {numtests, 20}])),
        ?_assert(proper:quickcheck(prop_concurrent_safety(), 
                                  [{to_file, user}, {numtests, 30}]))
    ]}.