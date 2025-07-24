%%%-------------------------------------------------------------------
%%% @doc BitActor Distributed Clustering Module
%%% Ultra-low latency distributed BitActor deployment
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_cluster).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0]).
-export([join_cluster/1, leave_cluster/0, get_cluster_status/0]).
-export([spawn_distributed_actor/3, broadcast_semantic_update/2]).
-export([get_node_load/0, get_cluster_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DISCOVERY_PORT, 4369).
-define(HEARTBEAT_INTERVAL, 1000). % 1 second
-define(NODE_TIMEOUT, 5000). % 5 seconds

-record(state, {
    cluster_nodes = [],           % Active cluster nodes
    node_loads = #{},             % Node load information
    semantic_replicas = #{},      % Semantic data distribution
    heartbeat_timer,              % Heartbeat timer reference
    node_monitors = #{},          % Node monitoring references
    cluster_id                    % Unique cluster identifier
}).

-record(node_info, {
    node,                         % Node name
    cpu_load,                     % CPU utilization
    memory_usage,                 % Memory usage in MB
    actor_count,                  % Number of active actors
    semantic_domains = [],        % Loaded semantic domains
    last_heartbeat,               % Last heartbeat timestamp
    rtt_ns                        % Round-trip time in nanoseconds
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec join_cluster(node()) -> ok | {error, term()}.
join_cluster(Node) ->
    gen_server:call(?SERVER, {join_cluster, Node}).

-spec leave_cluster() -> ok.
leave_cluster() ->
    gen_server:call(?SERVER, leave_cluster).

-spec get_cluster_status() -> map().
get_cluster_status() ->
    gen_server:call(?SERVER, get_cluster_status).

-spec spawn_distributed_actor(atom(), atom(), map()) -> {ok, reference(), node()} | {error, term()}.
spawn_distributed_actor(Domain, Type, Config) ->
    gen_server:call(?SERVER, {spawn_distributed_actor, Domain, Type, Config}).

-spec broadcast_semantic_update(atom(), term()) -> ok.
broadcast_semantic_update(Domain, Update) ->
    gen_server:cast(?SERVER, {broadcast_semantic_update, Domain, Update}).

-spec get_node_load() -> map().
get_node_load() ->
    gen_server:call(?SERVER, get_node_load).

-spec get_cluster_nodes() -> [node()].
get_cluster_nodes() ->
    gen_server:call(?SERVER, get_cluster_nodes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {ok, #state{}}.
init(Options) ->
    process_flag(trap_exit, true),
    
    %% Generate unique cluster ID
    ClusterID = generate_cluster_id(),
    
    %% Set up node monitoring
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    
    %% Start heartbeat timer
    HeartbeatTimer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    
    %% Initialize state
    State = #state{
        cluster_nodes = [node()],
        node_loads = #{},
        semantic_replicas = #{},
        heartbeat_timer = HeartbeatTimer,
        node_monitors = #{},
        cluster_id = ClusterID
    },
    
    %% Start semantic data replication
    init_semantic_replication(State),
    
    error_logger:info_msg("BitActor cluster initialized on ~p with ID ~p", [node(), ClusterID]),
    {ok, State}.

handle_call({join_cluster, Node}, _From, State) ->
    case net_adm:ping(Node) of
        pong ->
            %% Successfully connected to node
            case connect_to_cluster_node(Node, State) of
                {ok, NewState} ->
                    error_logger:info_msg("Joined cluster node: ~p", [Node]),
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        pang ->
            {reply, {error, node_unreachable}, State}
    end;

handle_call(leave_cluster, _From, State) ->
    %% Gracefully leave the cluster
    NewState = disconnect_from_cluster(State),
    {reply, ok, NewState};

handle_call(get_cluster_status, _From, State) ->
    Status = #{
        cluster_id => State#state.cluster_id,
        local_node => node(),
        cluster_nodes => State#state.cluster_nodes,
        node_count => length(State#state.cluster_nodes),
        node_loads => State#state.node_loads,
        semantic_replicas => maps:keys(State#state.semantic_replicas)
    },
    {reply, Status, State};

handle_call({spawn_distributed_actor, Domain, Type, Config}, _From, State) ->
    %% Select optimal node for actor spawning
    case select_optimal_node(Domain, State) of
        {ok, TargetNode} ->
            case spawn_remote_actor(TargetNode, Domain, Type, Config) of
                {ok, ActorRef} ->
                    %% Update node load tracking
                    NewState = update_node_load(TargetNode, +1, State),
                    {reply, {ok, ActorRef, TargetNode}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_node_load, _From, State) ->
    LocalLoad = calculate_local_load(),
    {reply, LocalLoad, State};

handle_call(get_cluster_nodes, _From, State) ->
    {reply, State#state.cluster_nodes, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({broadcast_semantic_update, Domain, Update}, State) ->
    %% Broadcast semantic update to all cluster nodes
    lists:foreach(fun(Node) ->
        case Node =/= node() of
            true ->
                spawn(fun() ->
                    rpc:cast(Node, bitactor_semantic, handle_remote_update, [Domain, Update])
                end);
            false ->
                ok
        end
    end, State#state.cluster_nodes),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(heartbeat, State) ->
    %% Send heartbeat to all cluster nodes
    NewState = send_cluster_heartbeat(State),
    
    %% Schedule next heartbeat
    HeartbeatTimer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {noreply, NewState#state{heartbeat_timer = HeartbeatTimer}};

handle_info({nodeup, Node, _}, State) ->
    error_logger:info_msg("Node up detected: ~p", [Node]),
    %% Attempt to add node to cluster if it's running BitActor
    NewState = maybe_add_cluster_node(Node, State),
    {noreply, NewState};

handle_info({nodedown, Node, Reason}, State) ->
    error_logger:warning_msg("Node down detected: ~p, reason: ~p", [Node, Reason]),
    %% Remove node from cluster
    NewState = remove_cluster_node(Node, State),
    {noreply, NewState};

handle_info({cluster_heartbeat, Node, NodeInfo}, State) ->
    %% Process heartbeat from cluster node
    NewState = process_cluster_heartbeat(Node, NodeInfo, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel heartbeat timer
    case State#state.heartbeat_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    
    %% Gracefully leave cluster
    disconnect_from_cluster(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate_cluster_id() -> binary().
generate_cluster_id() ->
    <<ClusterID:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(ClusterID, 16).

-spec connect_to_cluster_node(node(), #state{}) -> {ok, #state{}} | {error, term()}.
connect_to_cluster_node(Node, State) ->
    try
        %% Attempt to call cluster service on remote node
        case rpc:call(Node, ?MODULE, get_cluster_status, [], 5000) of
            {badrpc, Reason} ->
                {error, {rpc_failed, Reason}};
            RemoteStatus ->
                %% Add node to cluster
                Nodes = lists:usort([Node | State#state.cluster_nodes]),
                
                %% Set up monitoring
                MonitorRef = monitor_node(Node, true),
                Monitors = maps:put(Node, MonitorRef, State#state.node_monitors),
                
                %% Sync semantic data with new node
                sync_semantic_data(Node, State),
                
                NewState = State#state{
                    cluster_nodes = Nodes,
                    node_monitors = Monitors
                },
                
                {ok, NewState}
        end
    catch
        _Type:Error ->
            {error, {connection_failed, Error}}
    end.

-spec disconnect_from_cluster(#state{}) -> #state{}.
disconnect_from_cluster(State) ->
    %% Stop monitoring all nodes
    lists:foreach(fun(Node) ->
        case maps:find(Node, State#state.node_monitors) of
            {ok, MonitorRef} ->
                demonitor(MonitorRef, [flush]);
            error ->
                ok
        end
    end, State#state.cluster_nodes),
    
    %% Clear cluster state
    State#state{
        cluster_nodes = [node()],
        node_monitors = #{},
        node_loads = #{}
    }.

-spec select_optimal_node(atom(), #state{}) -> {ok, node()} | {error, term()}.
select_optimal_node(Domain, State) ->
    %% Find nodes with lowest load for the given domain
    NodeScores = lists:map(fun(Node) ->
        Load = maps:get(Node, State#state.node_loads, #{cpu_load => 0.5, actor_count => 0}),
        
        %% Calculate score based on CPU load and actor count
        CPULoad = maps:get(cpu_load, Load, 0.5),
        ActorCount = maps:get(actor_count, Load, 0),
        
        %% Prefer nodes with semantic domain already loaded
        DomainBonus = case has_semantic_domain(Node, Domain) of
            true -> -0.2;  % Bonus for having domain
            false -> 0.0
        end,
        
        Score = CPULoad + (ActorCount / 1000.0) + DomainBonus,
        {Node, Score}
    end, State#state.cluster_nodes),
    
    %% Select node with lowest score
    case lists:keysort(2, NodeScores) of
        [] ->
            {error, no_nodes_available};
        [{BestNode, _Score} | _] ->
            {ok, BestNode}
    end.

-spec spawn_remote_actor(node(), atom(), atom(), map()) -> {ok, reference()} | {error, term()}.
spawn_remote_actor(Node, Domain, Type, Config) ->
    case rpc:call(Node, bitactor_semantic, spawn_semantic_actor, [Domain, Type, Config], 10000) of
        {ok, ActorRef} ->
            {ok, ActorRef};
        {error, Reason} ->
            {error, Reason};
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

-spec calculate_local_load() -> map().
calculate_local_load() ->
    %% Get system statistics
    CPULoad = case cpu_sup:avg1() of
        {ok, Load} -> Load / 256.0;  % Convert to 0-1 range
        _ -> 0.5  % Default if unavailable
    end,
    
    MemoryUsage = case erlang:memory(total) of
        Memory -> Memory / (1024 * 1024)  % Convert to MB
    end,
    
    ActorCount = case catch bitactor_server:get_actor_count() of
        Count when is_integer(Count) -> Count;
        _ -> 0
    end,
    
    #{
        cpu_load => CPULoad,
        memory_usage => MemoryUsage,
        actor_count => ActorCount,
        node => node(),
        timestamp => erlang:monotonic_time(millisecond)
    }.

-spec send_cluster_heartbeat(#state{}) -> #state{}.
send_cluster_heartbeat(State) ->
    LocalLoad = calculate_local_load(),
    
    %% Send heartbeat to all cluster nodes
    lists:foreach(fun(Node) ->
        case Node =/= node() of
            true ->
                spawn(fun() ->
                    Node ! {cluster_heartbeat, node(), LocalLoad}
                end);
            false ->
                ok
        end
    end, State#state.cluster_nodes),
    
    %% Update local load in state
    NodeLoads = maps:put(node(), LocalLoad, State#state.node_loads),
    State#state{node_loads = NodeLoads}.

-spec process_cluster_heartbeat(node(), map(), #state{}) -> #state{}.
process_cluster_heartbeat(Node, NodeInfo, State) ->
    %% Update node load information
    NodeLoads = maps:put(Node, NodeInfo, State#state.node_loads),
    State#state{node_loads = NodeLoads}.

-spec maybe_add_cluster_node(node(), #state{}) -> #state{}.
maybe_add_cluster_node(Node, State) ->
    %% Check if node is already in cluster
    case lists:member(Node, State#state.cluster_nodes) of
        true ->
            State;
        false ->
            %% Check if node is running BitActor cluster service
            case rpc:call(Node, erlang, whereis, [?SERVER], 1000) of
                Pid when is_pid(Pid) ->
                    %% Add to cluster
                    case connect_to_cluster_node(Node, State) of
                        {ok, NewState} ->
                            error_logger:info_msg("Auto-discovered and joined cluster node: ~p", [Node]),
                            NewState;
                        {error, _Reason} ->
                            State
                    end;
                _ ->
                    State
            end
    end.

-spec remove_cluster_node(node(), #state{}) -> #state{}.
remove_cluster_node(Node, State) ->
    %% Remove from cluster nodes
    Nodes = lists:delete(Node, State#state.cluster_nodes),
    
    %% Remove monitoring
    case maps:find(Node, State#state.node_monitors) of
        {ok, MonitorRef} ->
            demonitor(MonitorRef, [flush]);
        error ->
            ok
    end,
    Monitors = maps:remove(Node, State#state.node_monitors),
    
    %% Remove load information
    NodeLoads = maps:remove(Node, State#state.node_loads),
    
    State#state{
        cluster_nodes = Nodes,
        node_monitors = Monitors,
        node_loads = NodeLoads
    }.

-spec init_semantic_replication(#state{}) -> ok.
init_semantic_replication(_State) ->
    %% Initialize semantic data replication system
    %% This would sync ontologies, SPARQL queries, and SHACL rules
    ok.

-spec sync_semantic_data(node(), #state{}) -> ok.
sync_semantic_data(Node, _State) ->
    %% Sync semantic data with newly joined node
    spawn(fun() ->
        try
            %% Get local semantic domains
            LocalDomains = case catch bitactor_semantic:get_semantic_stats() of
                Stats when is_map(Stats) ->
                    maps:keys(Stats);
                _ ->
                    []
            end,
            
            %% Send domains to remote node
            lists:foreach(fun(Domain) ->
                rpc:cast(Node, bitactor_semantic, sync_domain, [Domain, node()])
            end, LocalDomains)
        catch
            _:_ ->
                ok
        end
    end),
    ok.

-spec has_semantic_domain(node(), atom()) -> boolean().
has_semantic_domain(Node, Domain) ->
    case rpc:call(Node, bitactor_semantic, has_domain, [Domain], 1000) of
        true -> true;
        _ -> false
    end.

-spec update_node_load(node(), integer(), #state{}) -> #state{}.
update_node_load(Node, ActorDelta, State) ->
    NodeLoads = maps:update_with(Node, fun(Load) ->
        CurrentCount = maps:get(actor_count, Load, 0),
        maps:put(actor_count, CurrentCount + ActorDelta, Load)
    end, #{actor_count => ActorDelta}, State#state.node_loads),
    
    State#state{node_loads = NodeLoads}.

monitor_node(Node, Flag) ->
    monitor(process, {?SERVER, Node}).