%%%-------------------------------------------------------------------
%%% @doc Aegis WebSocket Bridge
%%% Connects Nuxt dashboard to Erlang/BitActor backend
%%% Provides real-time threat updates and control interface
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(aegis_websocket_bridge).

-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% API
-export([start_http_server/1]).
-export([broadcast_to_clients/2]).
-export([send_to_client/2]).

-include_lib("kernel/include/logger.hrl").

-record(ws_state, {
    client_id :: binary(),
    subscriptions :: [atom()],
    authenticated :: boolean(),
    last_ping :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_http_server(map()) -> {ok, pid()} | {error, term()}.
start_http_server(Config) ->
    Port = maps:get(port, Config, 8081),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/aegis/ws", ?MODULE, []},
            {"/health", aegis_health_handler, []},
            {"/metrics", aegis_metrics_handler, []}
        ]}
    ]),
    
    TransOpts = [{port, Port}],
    ProtoOpts = #{
        env => #{dispatch => Dispatch},
        max_connections => 10000,
        stream_handlers => [cowboy_websocket, cowboy_stream_h]
    },
    
    case cowboy:start_clear(aegis_websocket, TransOpts, ProtoOpts) of
        {ok, _} ->
            ?LOG_INFO("Aegis WebSocket server started on port ~p", [Port]),
            {ok, self()};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start WebSocket server: ~p", [Reason]),
            {error, Reason}
    end.

-spec broadcast_to_clients(atom(), map()) -> ok.
broadcast_to_clients(MessageType, Payload) ->
    Message = encode_message(MessageType, Payload),
    
    % Get all connected WebSocket processes
    Clients = pg:get_members(aegis_ws_clients),
    
    lists:foreach(fun(Pid) ->
        Pid ! {broadcast, Message}
    end, Clients),
    
    ok.

-spec send_to_client(pid(), map()) -> ok.
send_to_client(ClientPid, Message) ->
    ClientPid ! {send_message, Message},
    ok.

%%%===================================================================
%%% Cowboy WebSocket Callbacks
%%%===================================================================

init(Req, State) ->
    ?LOG_DEBUG("WebSocket connection initiated"),
    
    % Upgrade to WebSocket
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000,  % 60 seconds
        max_frame_size => 1048576  % 1MB max frame
    }}.

websocket_init(_State) ->
    ClientId = generate_client_id(),
    
    ?LOG_INFO("WebSocket client connected: ~s", [ClientId]),
    
    % Join client group
    pg:join(aegis_ws_clients, self()),
    
    % Send welcome message
    WelcomeMsg = encode_message(connected, #{
        client_id => ClientId,
        server_time => erlang:system_time(millisecond),
        version => <<"1.0.0">>
    }),
    
    % Subscribe to threat updates by default
    aegis_event_manager:subscribe(threat_updates),
    
    State = #ws_state{
        client_id = ClientId,
        subscriptions = [threat_updates],
        authenticated = false,
        last_ping = erlang:system_time(second)
    },
    
    % Send initial state
    send_initial_state(),
    
    % Schedule ping
    erlang:send_after(30000, self(), ping),
    
    {[{text, WelcomeMsg}], State}.

websocket_handle({text, Data}, State) ->
    try
        Message = jiffy:decode(Data, [return_maps]),
        handle_client_message(Message, State)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to decode message: ~p:~p", [Error, Reason]),
            ErrorMsg = encode_message(error, #{
                error => <<"Invalid message format">>,
                details => list_to_binary(io_lib:format("~p", [Reason]))
            }),
            {[{text, ErrorMsg}], State}
    end;

websocket_handle({binary, _Data}, State) ->
    % We only handle text frames
    {ok, State};

websocket_handle(ping, State) ->
    % Respond to ping with pong
    {[pong], State};

websocket_handle(pong, State) ->
    % Client responded to our ping
    NewState = State#ws_state{last_ping = erlang:system_time(second)},
    {ok, NewState};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({broadcast, Message}, State) ->
    {[{text, Message}], State};

websocket_info({send_message, Message}, State) ->
    {[{text, jiffy:encode(Message)}], State};

websocket_info({threat_update, ThreatData}, State) ->
    Message = encode_message('ThreatUpdate', format_threat_data(ThreatData)),
    {[{text, Message}], State};

websocket_info({asset_update, AssetData}, State) ->
    Message = encode_message('AssetUpdate', format_asset_data(AssetData)),
    {[{text, Message}], State};

websocket_info({network_update, NetworkData}, State) ->
    Message = encode_message('NetworkUpdate', format_network_data(NetworkData)),
    {[{text, Message}], State};

websocket_info({fabric_sync, SyncData}, State) ->
    Message = encode_message('FabricSync', SyncData),
    {[{text, Message}], State};

websocket_info(ping, State) ->
    % Send ping to client
    erlang:send_after(30000, self(), ping),
    {[{text, encode_message(ping, #{})}], State};

websocket_info({timeout, _Ref, check_health}, State) ->
    % Check if client is still alive
    CurrentTime = erlang:system_time(second),
    TimeSinceLastPing = CurrentTime - State#ws_state.last_ping,
    
    if
        TimeSinceLastPing > 90 ->
            % Client hasn't responded in 90 seconds, close connection
            {stop, State};
        true ->
            % Schedule next health check
            erlang:send_after(30000, self(), {timeout, make_ref(), check_health}),
            {ok, State}
    end;

websocket_info(Info, State) ->
    ?LOG_WARNING("Unexpected WebSocket info: ~p", [Info]),
    {ok, State}.

terminate(Reason, _Req, State) ->
    ?LOG_INFO("WebSocket client ~s disconnected: ~p", 
              [State#ws_state.client_id, Reason]),
    
    % Leave client group
    pg:leave(aegis_ws_clients, self()),
    
    % Unsubscribe from all events
    lists:foreach(fun(Sub) ->
        aegis_event_manager:unsubscribe(Sub)
    end, State#ws_state.subscriptions),
    
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

generate_client_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

encode_message(Type, Payload) ->
    Message = #{
        type => Type,
        payload => Payload,
        timestamp => erlang:system_time(millisecond)
    },
    jiffy:encode(Message).

handle_client_message(#{<<"type">> := <<"subscribe">>} = Msg, State) ->
    Channels = maps:get(<<"channels">>, Msg, []),
    NewSubs = subscribe_to_channels(Channels, State#ws_state.subscriptions),
    
    ResponseMsg = encode_message(subscribed, #{
        channels => Channels,
        active_subscriptions => NewSubs
    }),
    
    {[{text, ResponseMsg}], State#ws_state{subscriptions = NewSubs}};

handle_client_message(#{<<"type">> := <<"unsubscribe">>} = Msg, State) ->
    Channels = maps:get(<<"channels">>, Msg, []),
    NewSubs = unsubscribe_from_channels(Channels, State#ws_state.subscriptions),
    
    ResponseMsg = encode_message(unsubscribed, #{
        channels => Channels,
        active_subscriptions => NewSubs
    }),
    
    {[{text, ResponseMsg}], State#ws_state{subscriptions = NewSubs}};

handle_client_message(#{<<"type">> := <<"neutralizeThreat">>} = Msg, State) ->
    ThreatId = maps:get(<<"threatId">>, Msg),
    
    % Forward to BitActor for neutralization
    case aegis_threat_manager:neutralize_threat(ThreatId) of
        ok ->
            % Broadcast neutralization to all clients
            broadcast_to_clients(threatNeutralized, #{
                threat_id => ThreatId,
                neutralized_by => State#ws_state.client_id,
                timestamp => erlang:system_time(millisecond)
            }),
            
            ResponseMsg = encode_message(neutralizationSuccess, #{
                threat_id => ThreatId
            }),
            {[{text, ResponseMsg}], State};
            
        {error, Reason} ->
            ErrorMsg = encode_message(neutralizationFailed, #{
                threat_id => ThreatId,
                reason => Reason
            }),
            {[{text, ErrorMsg}], State}
    end;

handle_client_message(#{<<"type">> := <<"getMetrics">>}, State) ->
    Metrics = collect_fabric_metrics(),
    ResponseMsg = encode_message(metricsUpdate, Metrics),
    {[{text, ResponseMsg}], State};

handle_client_message(#{<<"type">> := <<"getFabricStatus">>}, State) ->
    Status = aegis_gossip_protocol:get_fabric_status(),
    ResponseMsg = encode_message(fabricStatus, Status),
    {[{text, ResponseMsg}], State};

handle_client_message(#{<<"type">> := <<"pong">>}, State) ->
    % Client responded to ping
    NewState = State#ws_state{last_ping = erlang:system_time(second)},
    {ok, NewState};

handle_client_message(Msg, State) ->
    ?LOG_WARNING("Unknown message type: ~p", [Msg]),
    ErrorMsg = encode_message(error, #{
        error => <<"Unknown message type">>,
        received => maps:get(<<"type">>, Msg, <<"undefined">>)
    }),
    {[{text, ErrorMsg}], State}.

send_initial_state() ->
    % Send current state of all entity types
    
    % Get active threats
    Threats = aegis_threat_manager:get_active_threats(),
    self() ! {threat_update, Threats},
    
    % Get asset status
    Assets = aegis_asset_manager:get_all_assets(),
    self() ! {asset_update, Assets},
    
    % Get network topology
    Network = aegis_network_manager:get_topology(),
    self() ! {network_update, Network},
    
    % Get fabric sync state
    {ok, FabricStatus} = aegis_gossip_protocol:get_fabric_status(),
    self() ! {fabric_sync, FabricStatus},
    
    ok.

subscribe_to_channels([], Subs) ->
    Subs;
subscribe_to_channels([Channel | Rest], Subs) ->
    ChannelAtom = binary_to_atom(Channel, utf8),
    case lists:member(ChannelAtom, Subs) of
        true ->
            subscribe_to_channels(Rest, Subs);
        false ->
            aegis_event_manager:subscribe(ChannelAtom),
            subscribe_to_channels(Rest, [ChannelAtom | Subs])
    end.

unsubscribe_from_channels([], Subs) ->
    Subs;
unsubscribe_from_channels([Channel | Rest], Subs) ->
    ChannelAtom = binary_to_atom(Channel, utf8),
    aegis_event_manager:unsubscribe(ChannelAtom),
    unsubscribe_from_channels(Rest, lists:delete(ChannelAtom, Subs)).

format_threat_data(ThreatData) when is_list(ThreatData) ->
    [format_single_threat(T) || T <- ThreatData];
format_threat_data(ThreatData) ->
    format_single_threat(ThreatData).

format_single_threat(Threat) ->
    #{
        id => maps:get(id, Threat),
        type => maps:get(type, Threat),
        severity => maps:get(severity, Threat),
        source => maps:get(source, Threat),
        target => maps:get(target, Threat),
        status => maps:get(status, Threat, active),
        detected_at => maps:get(detected_at, Threat),
        signature => maps:get(signature, Threat)
    }.

format_asset_data(AssetData) when is_list(AssetData) ->
    [format_single_asset(A) || A <- AssetData];
format_asset_data(AssetData) ->
    format_single_asset(AssetData).

format_single_asset(Asset) ->
    #{
        id => maps:get(id, Asset),
        type => maps:get(type, Asset),
        name => maps:get(name, Asset),
        status => maps:get(status, Asset),
        health => maps:get(health, Asset),
        last_seen => maps:get(last_seen, Asset),
        metrics => maps:get(metrics, Asset, #{})
    }.

format_network_data(NetworkData) ->
    #{
        nodes => maps:get(nodes, NetworkData, []),
        edges => maps:get(edges, NetworkData, []),
        segments => maps:get(segments, NetworkData, []),
        active_flows => maps:get(active_flows, NetworkData, 0)
    }.

collect_fabric_metrics() ->
    % Collect metrics from various sources
    
    % BitActor metrics
    BitActorMetrics = bitactor_telemetry:get_metrics(),
    
    % Gossip protocol metrics
    {ok, GossipStatus} = aegis_gossip_protocol:get_fabric_status(),
    GossipMetrics = maps:get(metrics, GossipStatus, #{}),
    
    % System metrics
    SystemMetrics = #{
        cpu_usage => cpu_sup:util(),
        memory_usage => memsup:get_memory_data(),
        uptime_seconds => element(1, erlang:statistics(wall_clock)) div 1000
    },
    
    % Aggregate all metrics
    #{
        bitactor => BitActorMetrics,
        gossip => GossipMetrics,
        system => SystemMetrics,
        timestamp => erlang:system_time(millisecond)
    }.