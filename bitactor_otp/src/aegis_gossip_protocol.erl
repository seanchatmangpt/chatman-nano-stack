%%%-------------------------------------------------------------------
%%% @doc Aegis Fabric Threat Gossip Protocol
%%% High-speed threat signature propagation for inter-pod communication
%%% Implements sub-microsecond threat broadcast across K8s mesh
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(aegis_gossip_protocol).

%% API
-export([start_link/0, start_link/1]).
-export([broadcast_threat/2, handle_threat_signature/2]).
-export([join_fabric/2, leave_fabric/1]).
-export([get_fabric_status/0, get_node_metrics/1]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Gossip protocol exports
-export([gossip_loop/1, handle_gossip_message/2]).
-export([compress_signature/1, decompress_signature/1]).
-export([encrypt_gossip/2, decrypt_gossip/2]).

-include_lib("kernel/include/logger.hrl").

-record(aegis_node, {
    node_id :: binary(),
    pod_name :: binary(),
    namespace :: binary(),
    ip_address :: inet:ip_address(),
    port :: inet:port_number(),
    status :: active | inactive | suspected,
    last_seen :: integer(),
    threat_count :: non_neg_integer(),
    latency_ns :: non_neg_integer()
}).

-record(threat_signature, {
    signature_id :: binary(),
    threat_type :: atom(),
    severity :: critical | high | medium | low,
    source_node :: binary(),
    timestamp_ns :: integer(),
    ttl :: non_neg_integer(),
    payload :: binary(),
    hash :: binary()
}).

-record(gossip_state, {
    node_id :: binary(),
    nodes :: #{binary() => #aegis_node{}},
    seen_signatures :: #{binary() => integer()},
    pending_broadcasts :: queue:queue(),
    metrics :: map(),
    config :: map()
}).

-define(GOSSIP_PORT, 4369).
-define(GOSSIP_INTERVAL_MS, 10).
-define(GOSSIP_FANOUT, 3).
-define(MAX_GOSSIP_HOPS, 5).
-define(SIGNATURE_TTL_MS, 5000).
-define(NODE_TIMEOUT_MS, 1000).
-define(COMPRESSION_LEVEL, 1). % LZ4 fast compression

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec broadcast_threat(#threat_signature{}, map()) -> ok | {error, term()}.
broadcast_threat(ThreatSig, Options) ->
    gen_server:call(?MODULE, {broadcast_threat, ThreatSig, Options}).

-spec handle_threat_signature(binary(), binary()) -> ok | {error, term()}.
handle_threat_signature(SignatureData, FromNode) ->
    gen_server:cast(?MODULE, {handle_threat, SignatureData, FromNode}).

-spec join_fabric(binary(), #aegis_node{}) -> ok | {error, term()}.
join_fabric(NodeId, NodeInfo) ->
    gen_server:call(?MODULE, {join_fabric, NodeId, NodeInfo}).

-spec leave_fabric(binary()) -> ok.
leave_fabric(NodeId) ->
    gen_server:cast(?MODULE, {leave_fabric, NodeId}).

-spec get_fabric_status() -> {ok, map()}.
get_fabric_status() ->
    gen_server:call(?MODULE, get_fabric_status).

-spec get_node_metrics(binary()) -> {ok, map()} | {error, not_found}.
get_node_metrics(NodeId) ->
    gen_server:call(?MODULE, {get_node_metrics, NodeId}).

%%%===================================================================
%%% Gen_server Callbacks
%%%===================================================================

init(Config) ->
    ?LOG_INFO("🚀 Initializing Aegis Gossip Protocol"),
    
    % Generate unique node ID
    NodeId = generate_node_id(),
    
    % Setup UDP socket for gossip
    {ok, Socket} = gen_udp:open(?GOSSIP_PORT, [
        binary,
        {active, true},
        {reuseaddr, true},
        {recbuf, 65536},
        {sndbuf, 65536}
    ]),
    
    % Initialize state
    State = #gossip_state{
        node_id = NodeId,
        nodes = #{},
        seen_signatures = #{},
        pending_broadcasts = queue:new(),
        metrics = init_metrics(),
        config = maps:merge(default_config(), Config)
    },
    
    % Start gossip loop
    erlang:send_after(?GOSSIP_INTERVAL_MS, self(), gossip_tick),
    
    % Join cluster if seed nodes provided
    case maps:get(seed_nodes, Config, []) of
        [] -> ok;
        Seeds -> join_seed_nodes(Seeds, State)
    end,
    
    ?LOG_INFO("Aegis Gossip Protocol initialized with Node ID: ~s", [NodeId]),
    {ok, State}.

handle_call({broadcast_threat, ThreatSig, Options}, _From, State) ->
    StartTime = erlang:monotonic_time(nanosecond),
    
    % Add to pending broadcasts
    NewState = add_to_broadcast_queue(ThreatSig, Options, State),
    
    % Trigger immediate gossip
    self() ! gossip_tick,
    
    EndTime = erlang:monotonic_time(nanosecond),
    Latency = EndTime - StartTime,
    
    % Update metrics
    UpdatedState = update_broadcast_metrics(Latency, NewState),
    
    {reply, ok, UpdatedState};

handle_call({join_fabric, NodeId, NodeInfo}, _From, State) ->
    ?LOG_INFO("Node ~s joining fabric", [NodeId]),
    
    % Add node to fabric
    NewNodes = maps:put(NodeId, NodeInfo#aegis_node{
        last_seen = erlang:system_time(millisecond),
        status = active
    }, State#gossip_state.nodes),
    
    NewState = State#gossip_state{nodes = NewNodes},
    
    % Send welcome packet with current fabric state
    send_welcome_packet(NodeId, NodeInfo, NewState),
    
    {reply, ok, NewState};

handle_call(get_fabric_status, _From, State) ->
    Status = #{
        node_id => State#gossip_state.node_id,
        active_nodes => count_active_nodes(State),
        total_nodes => maps:size(State#gossip_state.nodes),
        signatures_seen => maps:size(State#gossip_state.seen_signatures),
        pending_broadcasts => queue:len(State#gossip_state.pending_broadcasts),
        metrics => State#gossip_state.metrics
    },
    {reply, {ok, Status}, State};

handle_call({get_node_metrics, NodeId}, _From, State) ->
    case maps:find(NodeId, State#gossip_state.nodes) of
        {ok, Node} ->
            Metrics = #{
                node_id => NodeId,
                status => Node#aegis_node.status,
                last_seen => Node#aegis_node.last_seen,
                threat_count => Node#aegis_node.threat_count,
                latency_ns => Node#aegis_node.latency_ns
            },
            {reply, {ok, Metrics}, State};
        error ->
            {reply, {error, not_found}, State}
    end.

handle_cast({handle_threat, SignatureData, FromNode}, State) ->
    % Decompress and decrypt
    case decrypt_and_decompress(SignatureData, State) of
        {ok, ThreatSig} ->
            NewState = process_threat_signature(ThreatSig, FromNode, State),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_WARNING("Failed to process threat signature: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({leave_fabric, NodeId}, State) ->
    ?LOG_INFO("Node ~s leaving fabric", [NodeId]),
    NewNodes = maps:remove(NodeId, State#gossip_state.nodes),
    {noreply, State#gossip_state{nodes = NewNodes}}.

handle_info({udp, _Socket, IP, Port, Data}, State) ->
    % Handle incoming gossip message
    case handle_gossip_packet(Data, {IP, Port}, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _Reason} ->
            {noreply, State}
    end;

handle_info(gossip_tick, State) ->
    % Perform gossip round
    NewState = perform_gossip_round(State),
    
    % Schedule next tick
    erlang:send_after(?GOSSIP_INTERVAL_MS, self(), gossip_tick),
    
    % Check node health
    UpdatedState = check_node_health(NewState),
    
    {noreply, UpdatedState};

handle_info(Info, State) ->
    ?LOG_WARNING("Unexpected message: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO("Aegis Gossip Protocol terminating: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

generate_node_id() ->
    Timestamp = erlang:system_time(nanosecond),
    Random = rand:uniform(1000000),
    NodeName = atom_to_binary(node(), utf8),
    Hash = crypto:hash(sha256, <<Timestamp:64, Random:32, NodeName/binary>>),
    base64:encode(Hash).

default_config() ->
    #{
        gossip_fanout => ?GOSSIP_FANOUT,
        max_hops => ?MAX_GOSSIP_HOPS,
        compression => lz4,
        encryption => aes256_gcm,
        signature_ttl_ms => ?SIGNATURE_TTL_MS,
        node_timeout_ms => ?NODE_TIMEOUT_MS
    }.

init_metrics() ->
    #{
        threats_broadcasted => 0,
        threats_received => 0,
        gossip_rounds => 0,
        avg_latency_ns => 0,
        max_latency_ns => 0,
        min_latency_ns => 999999999,
        bytes_sent => 0,
        bytes_received => 0
    }.

add_to_broadcast_queue(ThreatSig, _Options, State) ->
    % Add signature hash to seen list
    SignatureId = ThreatSig#threat_signature.signature_id,
    SeenSigs = maps:put(SignatureId, erlang:system_time(millisecond), 
                       State#gossip_state.seen_signatures),
    
    % Add to broadcast queue
    NewQueue = queue:in(ThreatSig, State#gossip_state.pending_broadcasts),
    
    State#gossip_state{
        seen_signatures = SeenSigs,
        pending_broadcasts = NewQueue
    }.

perform_gossip_round(State) ->
    case queue:out(State#gossip_state.pending_broadcasts) of
        {{value, ThreatSig}, RestQueue} ->
            % Select random nodes for gossip
            Targets = select_gossip_targets(State),
            
            % Broadcast to selected nodes
            lists:foreach(fun(NodeId) ->
                gossip_to_node(NodeId, ThreatSig, State)
            end, Targets),
            
            % Update metrics
            Metrics = maps:update_with(threats_broadcasted, fun(V) -> V + 1 end, 0,
                                     State#gossip_state.metrics),
            
            State#gossip_state{
                pending_broadcasts = RestQueue,
                metrics = Metrics
            };
        {empty, _} ->
            State
    end.

select_gossip_targets(State) ->
    ActiveNodes = [NodeId || {NodeId, Node} <- maps:to_list(State#gossip_state.nodes),
                            Node#aegis_node.status =:= active],
    
    % Randomly select up to GOSSIP_FANOUT nodes
    Fanout = maps:get(gossip_fanout, State#gossip_state.config, ?GOSSIP_FANOUT),
    ShuffledNodes = shuffle_list(ActiveNodes),
    lists:sublist(ShuffledNodes, Fanout).

gossip_to_node(NodeId, ThreatSig, State) ->
    case maps:find(NodeId, State#gossip_state.nodes) of
        {ok, Node} ->
            % Compress and encrypt threat signature
            CompressedData = compress_signature(ThreatSig),
            EncryptedData = encrypt_gossip(CompressedData, NodeId),
            
            % Send via UDP
            gen_udp:send(get_socket(), 
                        Node#aegis_node.ip_address,
                        Node#aegis_node.port,
                        EncryptedData),
            
            % Update sent bytes metric
            update_sent_bytes(byte_size(EncryptedData), State);
        error ->
            ok
    end.

compress_signature(ThreatSig) ->
    Binary = term_to_binary(ThreatSig),
    lz4:compress(Binary, [{compression_level, ?COMPRESSION_LEVEL}]).

decompress_signature(CompressedData) ->
    lz4:decompress(CompressedData).

encrypt_gossip(Data, _NodeId) ->
    % TODO: Implement proper AES256-GCM encryption
    % For now, just return data as-is
    Data.

decrypt_gossip(Data, _NodeId) ->
    % TODO: Implement proper AES256-GCM decryption
    % For now, just return data as-is
    {ok, Data}.

handle_gossip_packet(Data, {IP, Port}, State) ->
    try
        % Decrypt and decompress
        {ok, DecryptedData} = decrypt_gossip(Data, State#gossip_state.node_id),
        DecompressedData = decompress_signature(DecryptedData),
        ThreatSig = binary_to_term(DecompressedData),
        
        % Find source node
        SourceNode = find_node_by_ip(IP, Port, State),
        
        % Process the threat signature
        NewState = process_threat_signature(ThreatSig, SourceNode, State),
        
        {ok, NewState}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to handle gossip packet: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

process_threat_signature(ThreatSig, FromNode, State) ->
    SignatureId = ThreatSig#threat_signature.signature_id,
    
    % Check if we've seen this signature before
    case maps:is_key(SignatureId, State#gossip_state.seen_signatures) of
        true ->
            % Already seen, ignore
            State;
        false ->
            % New signature, process it
            ?LOG_INFO("New threat signature received: ~s from ~s", 
                     [SignatureId, FromNode]),
            
            % Add to seen signatures
            NewSeenSigs = maps:put(SignatureId, erlang:system_time(millisecond),
                                 State#gossip_state.seen_signatures),
            
            % Forward to BitActor for processing
            forward_to_bitactor(ThreatSig),
            
            % Check TTL and re-broadcast if needed
            case ThreatSig#threat_signature.ttl > 0 of
                true ->
                    % Decrement TTL and add to broadcast queue
                    UpdatedSig = ThreatSig#threat_signature{
                        ttl = ThreatSig#threat_signature.ttl - 1
                    },
                    NewQueue = queue:in(UpdatedSig, State#gossip_state.pending_broadcasts),
                    
                    State#gossip_state{
                        seen_signatures = NewSeenSigs,
                        pending_broadcasts = NewQueue
                    };
                false ->
                    State#gossip_state{seen_signatures = NewSeenSigs}
            end
    end.

forward_to_bitactor(ThreatSig) ->
    % Send to BitActor NIF for ultra-fast processing
    case bitactor_nif:process_threat(ThreatSig) of
        ok ->
            ?LOG_DEBUG("Threat forwarded to BitActor");
        {error, Reason} ->
            ?LOG_ERROR("Failed to forward threat to BitActor: ~p", [Reason])
    end.

check_node_health(State) ->
    CurrentTime = erlang:system_time(millisecond),
    TimeoutMs = maps:get(node_timeout_ms, State#gossip_state.config, ?NODE_TIMEOUT_MS),
    
    NewNodes = maps:map(fun(_NodeId, Node) ->
        TimeSinceLastSeen = CurrentTime - Node#aegis_node.last_seen,
        
        case {Node#aegis_node.status, TimeSinceLastSeen > TimeoutMs} of
            {active, true} ->
                ?LOG_WARNING("Node ~s suspected (no heartbeat for ~pms)", 
                           [Node#aegis_node.node_id, TimeSinceLastSeen]),
                Node#aegis_node{status = suspected};
            {suspected, true} when TimeSinceLastSeen > (TimeoutMs * 3) ->
                ?LOG_WARNING("Node ~s marked inactive", [Node#aegis_node.node_id]),
                Node#aegis_node{status = inactive};
            _ ->
                Node
        end
    end, State#gossip_state.nodes),
    
    State#gossip_state{nodes = NewNodes}.

update_broadcast_metrics(LatencyNs, State) ->
    Metrics = State#gossip_state.metrics,
    
    % Update latency metrics
    NewMetrics = Metrics#{
        avg_latency_ns => calculate_avg_latency(LatencyNs, Metrics),
        max_latency_ns => max(LatencyNs, maps:get(max_latency_ns, Metrics, 0)),
        min_latency_ns => min(LatencyNs, maps:get(min_latency_ns, Metrics, 999999999))
    },
    
    State#gossip_state{metrics = NewMetrics}.

calculate_avg_latency(NewLatency, Metrics) ->
    Count = maps:get(threats_broadcasted, Metrics, 0) + 1,
    OldAvg = maps:get(avg_latency_ns, Metrics, 0),
    
    ((OldAvg * (Count - 1)) + NewLatency) div Count.

count_active_nodes(State) ->
    length([N || {_, N} <- maps:to_list(State#gossip_state.nodes),
                N#aegis_node.status =:= active]).

shuffle_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

find_node_by_ip(IP, Port, State) ->
    case [NodeId || {NodeId, Node} <- maps:to_list(State#gossip_state.nodes),
                   Node#aegis_node.ip_address =:= IP,
                   Node#aegis_node.port =:= Port] of
        [NodeId | _] -> NodeId;
        [] -> <<"unknown">>
    end.

get_socket() ->
    % TODO: Properly manage socket
    {ok, Socket} = gen_udp:open(0),
    Socket.

send_welcome_packet(_NodeId, _NodeInfo, _State) ->
    % TODO: Implement welcome packet with current fabric state
    ok.

join_seed_nodes([], _State) ->
    ok;
join_seed_nodes([Seed | Rest], State) ->
    % TODO: Implement seed node joining
    ?LOG_INFO("Joining seed node: ~p", [Seed]),
    join_seed_nodes(Rest, State).

update_sent_bytes(Bytes, State) ->
    Metrics = maps:update_with(bytes_sent, fun(V) -> V + Bytes end, 0,
                             State#gossip_state.metrics),
    State#gossip_state{metrics = Metrics}.

decrypt_and_decompress(Data, State) ->
    try
        {ok, DecryptedData} = decrypt_gossip(Data, State#gossip_state.node_id),
        DecompressedData = decompress_signature(DecryptedData),
        ThreatSig = binary_to_term(DecompressedData),
        {ok, ThreatSig}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.