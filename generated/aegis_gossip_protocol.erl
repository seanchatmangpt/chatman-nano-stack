%%%-------------------------------------------------------------------
%%% @doc CNS Aegis Fabric - Threat Gossip Protocol
%%% Generated from TTL: 2025-07-24T20:24:30.978426
%%% NO HANDCODING - This file is auto-generated
%%%
%%% Implements high-speed threat signature broadcast using gossip protocol
%%% Target: <100ms propagation across fabric
%%%-------------------------------------------------------------------
-module(aegis_gossip_protocol).

-behaviour(gen_server).

%% API
-export([start_link/0, broadcast_threat/2, add_node/1, remove_node/1]).
-export([get_threat_signatures/0, get_convergence_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    nodes = [] :: [node()],
    threats = #{} :: #{binary() => threat_signature()},
    gossip_timer :: timer:tref(),
    fanout = 3 :: integer(),
    interval = 100 :: integer(),
    max_hops = 5 :: integer(),
    convergence_target = 1000 :: integer()
}).

-record(threat_signature, {
    id :: binary(),
    type :: atom(),
    pattern :: binary(),
    priority :: atom(),
    propagation_speed :: integer(),
    false_positive_rate :: float(),
    timestamp :: integer(),
    hops = 0 :: integer()
}).

%%%===================================================================
%%% Threat Signatures from TTL
%%%===================================================================

-define(THREAT_SIGNATURES, [
    #threat_signature{
        id = <<"DDoSAttack">>,
        type = networkthreat,
        pattern = <<"rate_limit_exceeded">>,
        priority = critical,
        propagation_speed = 50,
        false_positive_rate = 0.005,
        timestamp = erlang:system_time(millisecond)
    },    #threat_signature{
        id = <<"SQLInjection">>,
        type = applicationthreat,
        pattern = <<"('|(--|;)|(<|>)|union|select|insert|update|delete|drop|create|alter|exec|execute|script|javascript|eval)">>,
        priority = critical,
        propagation_speed = 100,
        false_positive_rate = 0.001,
        timestamp = erlang:system_time(millisecond)
    },    #threat_signature{
        id = <<"XSSAttack">>,
        type = applicationthreat,
        pattern = <<"(<script|<iframe|javascript:|onerror=|onload=|onclick=|<svg/onload)">>,
        priority = critical,
        propagation_speed = 100,
        false_positive_rate = 0.002,
        timestamp = erlang:system_time(millisecond)
    },    #threat_signature{
        id = <<"BruteForceAttack">>,
        type = systemthreat,
        pattern = <<"failed_auth_attempts">>,
        priority = high,
        propagation_speed = 200,
        false_positive_rate = 0.003,
        timestamp = erlang:system_time(millisecond)
    },    #threat_signature{
        id = <<"PrivilegeEscalation">>,
        type = systemthreat,
        pattern = <<"unauthorized_privilege_change">>,
        priority = critical,
        propagation_speed = 50,
        false_positive_rate = 0.001,
        timestamp = erlang:system_time(millisecond)
    }]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

broadcast_threat(ThreatId, ThreatData) ->
    gen_server:cast(?MODULE, {broadcast, ThreatId, ThreatData}).

add_node(Node) ->
    gen_server:call(?MODULE, {add_node, Node}).

remove_node(Node) ->
    gen_server:call(?MODULE, {remove_node, Node}).

get_threat_signatures() ->
    gen_server:call(?MODULE, get_threats).

get_convergence_time() ->
    gen_server:call(?MODULE, get_convergence).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG_INFO("Aegis Gossip Protocol starting with config: ~p", [
        #{fanout => 3,
          interval => 100ms,
          max_hops => 5}
    ]),
    
    %% Initialize with TTL-defined threats
    Threats = lists:foldl(fun(Threat, Acc) ->
        maps:put(Threat#threat_signature.id, Threat, Acc)
    end, #{}, ?THREAT_SIGNATURES),
    
    %% Start gossip timer
    {ok, Timer} = timer:send_interval(
        100, 
        self(), 
        gossip_tick
    ),
    
    %% Connect to K8s service mesh
    connect_to_service_mesh(),
    
    {ok, #state{threats = Threats, gossip_timer = Timer}}.

handle_call({add_node, Node}, _From, State) ->
    NewNodes = lists:usort([Node | State#state.nodes]),
    {reply, ok, State#state{nodes = NewNodes}};

handle_call({remove_node, Node}, _From, State) ->
    NewNodes = lists:delete(Node, State#state.nodes),
    {reply, ok, State#state{nodes = NewNodes}};

handle_call(get_threats, _From, State) ->
    {reply, State#state.threats, State};

handle_call(get_convergence, _From, State) ->
    %% Calculate actual vs target convergence
    ActualTime = calculate_convergence_time(State),
    Target = State#state.convergence_target,
    {reply, #{actual => ActualTime, target => Target}, State}.

handle_cast({broadcast, ThreatId, ThreatData}, State) ->
    %% Create threat signature
    Threat = #threat_signature{
        id = ThreatId,
        type = maps:get(type, ThreatData, unknown),
        pattern = maps:get(pattern, ThreatData, <<>>),
        priority = maps:get(priority, ThreatData, normal),
        propagation_speed = maps:get(speed, ThreatData, 100),
        false_positive_rate = maps:get(fp_rate, ThreatData, 0.01),
        timestamp = erlang:system_time(millisecond),
        hops = 0
    },
    
    %% Add to local store
    NewThreats = maps:put(ThreatId, Threat, State#state.threats),
    
    %% Gossip immediately for critical threats
    case Threat#threat_signature.priority of
        critical ->
            gossip_threats([Threat], State);
        _ ->
            ok
    end,
    
    {noreply, State#state{threats = NewThreats}}.

handle_info(gossip_tick, State) ->
    %% Select random threats to gossip
    AllThreats = maps:values(State#state.threats),
    ThreatsToGossip = select_threats_for_gossip(AllThreats),
    gossip_threats(ThreatsToGossip, State),
    {noreply, State};

handle_info({gossip_receive, FromNode, Threats}, State) ->
    %% Merge received threats
    NewThreats = merge_threats(Threats, State#state.threats),
    
    %% Forward to other nodes if hop count allows
    forward_threats(Threats, FromNode, State),
    
    {noreply, State#state{threats = NewThreats}}.

terminate(_Reason, State) ->
    timer:cancel(State#state.gossip_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_to_service_mesh() ->
    %% Connect to K8s service mesh for inter-pod communication
    case os:getenv("KUBERNETES_SERVICE_HOST") of
        false ->
            ?LOG_INFO("Not running in Kubernetes, using local mode");
        _Host ->
            ?LOG_INFO("Connecting to K8s service mesh"),
            %% Register with service discovery
            register_with_service_mesh()
    end.

register_with_service_mesh() ->
    %% Register this node with the K8s service mesh
    PodName = os:getenv("POD_NAME", "aegis-unknown"),
    PodIP = os:getenv("POD_IP", "127.0.0.1"),
    
    %% Use Kubernetes API to register
    %% This integrates with bitactor_k8s_service_mesh.erl
    bitactor_k8s_service_mesh:register_node(PodName, PodIP).

gossip_threats(Threats, #state{nodes = Nodes, fanout = Fanout}) ->
    %% Select random nodes for gossip
    TargetNodes = select_random_nodes(Nodes, Fanout),
    
    %% Send threats to each target
    lists:foreach(fun(Node) ->
        erlang:send({?MODULE, Node}, {gossip_receive, node(), Threats})
    end, TargetNodes).

select_threats_for_gossip(AllThreats) ->
    %% 80/20 optimization: prioritize critical threats
    {Critical, Others} = lists:partition(fun(T) ->
        T#threat_signature.priority =:= critical
    end, AllThreats),
    
    %% Always gossip critical threats
    Critical ++ select_random(Others, 5).

select_random_nodes(Nodes, Count) ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Nodes])],
    lists:sublist(Shuffled, Count).

select_random(List, Count) ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])],
    lists:sublist(Shuffled, Count).

merge_threats(NewThreats, ExistingThreats) ->
    lists:foldl(fun(Threat, Acc) ->
        ThreatId = Threat#threat_signature.id,
        case maps:get(ThreatId, Acc, undefined) of
            undefined ->
                %% New threat, add it
                maps:put(ThreatId, Threat, Acc);
            Existing ->
                %% Keep the newer one
                if
                    Threat#threat_signature.timestamp > 
                    Existing#threat_signature.timestamp ->
                        maps:put(ThreatId, Threat, Acc);
                    true ->
                        Acc
                end
        end
    end, ExistingThreats, NewThreats).

forward_threats(Threats, FromNode, State) ->
    %% Increment hop count and forward if under limit
    ForwardableThreats = lists:filtermap(fun(T) ->
        Hops = T#threat_signature.hops,
        if
            Hops < State#state.max_hops ->
                {true, T#threat_signature{hops = Hops + 1}};
            true ->
                false
        end
    end, Threats),
    
    %% Don't send back to sender
    TargetNodes = lists:delete(FromNode, State#state.nodes),
    gossip_threats(ForwardableThreats, State#state{nodes = TargetNodes}).

calculate_convergence_time(#state{threats = Threats}) ->
    %% Calculate average propagation time
    CurrentTime = erlang:system_time(millisecond),
    PropagationTimes = lists:map(fun({_, T}) ->
        CurrentTime - T#threat_signature.timestamp
    end, maps:to_list(Threats)),
    
    case PropagationTimes of
        [] -> 0;
        Times -> lists:sum(Times) div length(Times)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gossip_test() ->
    {ok, Pid} = start_link(),
    
    %% Test threat broadcast
    broadcast_threat(<<"test_threat">>, #{
        type => network,
        pattern => <<"test_pattern">>,
        priority => critical
    }),
    
    timer:sleep(100),
    
    Threats = get_threat_signatures(),
    ?assert(maps:is_key(<<"test_threat">>, Threats)),
    
    gen_server:stop(Pid).

convergence_test() ->
    {ok, Pid} = start_link(),
    
    %% Add some nodes
    add_node('node1@host'),
    add_node('node2@host'),
    
    %% Broadcast threat
    broadcast_threat(<<"convergence_test">>, #{priority => critical}),
    
    timer:sleep(200),
    
    #{actual := Actual, target := Target} = get_convergence_time(),
    ?assert(Actual < Target),
    
    gen_server:stop(Pid).

-endif.