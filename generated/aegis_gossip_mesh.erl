%%% CNS Aegis Fabric - Erlang/OTP Service Mesh
%%% Generated from TTL specifications
%%% Gossip: 3 fan-out, 100ms convergence

-module(aegis_gossip_mesh).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([propagate_threat/2, get_threats/0, join_cluster/1]).

-record(state, {
    threats = #{},
    peers = [],
    fan_out = 3,
    max_hops = 5,
    encryption = "AES256_GCM",
    compression = "LZ4"
}).

%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

propagate_threat(ThreatSig, Urgency) ->
    gen_server:cast(?MODULE, {propagate, ThreatSig, Urgency, 0}).

get_threats() ->
    gen_server:call(?MODULE, get_threats).

join_cluster(PeerNode) ->
    gen_server:call(?MODULE, {join, PeerNode}).

%%% Callbacks

init([]) ->
    process_flag(trap_exit, true),
    schedule_heartbeat(),
    {ok, #state{}}.

handle_call(get_threats, _From, State) ->
    {reply, maps:to_list(State#state.threats), State};

handle_call({join, PeerNode}, _From, State) ->
    NewPeers = lists:usort([PeerNode | State#state.peers]),
    {reply, ok, State#state{peers = NewPeers}}.

handle_cast({propagate, ThreatSig, Urgency, Hops}, State) when Hops < State#state.max_hops ->
    % Update local threat database
    ThreatId = erlang:phash2(ThreatSig),
    NewThreats = maps:put(ThreatId, {ThreatSig, erlang:system_time(nanosecond)}, State#state.threats),
    
    % Select peers for gossip
    SelectedPeers = select_peers(State#state.peers, State#state.fan_out),
    
    % Propagate to selected peers
    lists:foreach(fun(Peer) ->
        spawn(fun() -> 
            gen_server:cast({?MODULE, Peer}, {propagate, ThreatSig, Urgency, Hops + 1})
        end)
    end, SelectedPeers),
    
    {noreply, State#state{threats = NewThreats}};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(heartbeat, State) ->
    % Periodic convergence check
    ThreatCount = maps:size(State#state.threats),
    io:format("Gossip convergence: ~p threats in database~n", [ThreatCount]),
    schedule_heartbeat(),
    {noreply, State}.

%%% Internal functions

select_peers(Peers, FanOut) ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Peers])],
    lists:sublist(Shuffled, min(FanOut, length(Peers))).

schedule_heartbeat() ->
    erlang:send_after(100, self(), heartbeat).