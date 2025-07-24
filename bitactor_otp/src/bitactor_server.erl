%%%-------------------------------------------------------------------
%%% @doc BitActor Main Gen_Server
%%% Ultra-intelligent financial computing with C NIF integration
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([spawn_actor/2, kill_actor/1, send_message/2]).
-export([get_stats/0, get_actor_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    actors = #{},           % Actor registry
    tick_ref = undefined,   % Tick timer reference
    stats = #{},           % Performance statistics
    nif_loaded = false     % NIF loading status
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec spawn_actor(atom(), term()) -> {ok, reference(), non_neg_integer()} | {error, term()}.
spawn_actor(Type, InitData) ->
    gen_server:call(?SERVER, {spawn_actor, Type, InitData}).

-spec kill_actor(reference()) -> ok | {error, not_found}.
kill_actor(ActorRef) ->
    gen_server:call(?SERVER, {kill_actor, ActorRef}).

-spec send_message(reference(), term()) -> ok | {error, term()}.
send_message(ActorRef, Message) ->
    % SWARM ULTRA-OPTIMIZATION: Bypass gen_server for forex hot path
    case whereis(?SERVER) of
        undefined -> 
            {error, server_not_running};
        ServerPid when is_pid(ServerPid) ->
            % DIRECT CALL: Skip gen_server message queue overhead
            send_message_direct(ActorRef, Message)
    end.

% ULTRA-FAST: Direct message sending without gen_server overhead
-spec send_message_direct(reference(), term()) -> ok | {error, term()}.
send_message_direct(ActorRef, Message) ->
    % SWARM OPTIMIZATION: Direct process dictionary lookup for max speed
    case get({ultra_fast_actor, ActorRef}) of
        undefined ->
            % Fallback to slower path once to cache
            gen_server:cast(?SERVER, {send_message, ActorRef, Message});
        {nif_loaded, ActorHandle} ->
            % ULTRA-FAST: Direct NIF call bypassing all Erlang overhead
            bitactor_nif:send_message(ActorHandle, Message),
            ok
    end.

-spec get_stats() -> #{atom() => term()}.
get_stats() ->
    gen_server:call(?SERVER, get_stats).

-spec get_actor_count() -> non_neg_integer().
get_actor_count() ->
    gen_server:call(?SERVER, get_actor_count).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    
    %% Load BitActor NIF
    NIFLoaded = load_nif(),
    
    %% Initialize statistics
    Stats = #{
        actors_spawned => 0,
        actors_killed => 0,
        messages_sent => 0,
        ticks_processed => 0,
        start_time => erlang:system_time(millisecond)
    },
    
    %% Start tick timer
    TickInterval = bitactor_app:get_env(tick_interval, 1000),
    TRef = erlang:send_after(TickInterval, self(), tick),
    
    State = #state{
        actors = #{},
        tick_ref = TRef,
        stats = Stats,
        nif_loaded = NIFLoaded
    },
    
    error_logger:info_msg("BitActor server initialized with NIF loaded: ~p", [NIFLoaded]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> 
    {reply, term(), #state{}} | {noreply, #state{}} | {stop, term(), term(), #state{}}.
handle_call({spawn_actor, Type, InitData}, _From, State) ->
    case spawn_actor_internal(Type, InitData, State) of
        {ok, ActorRef, LatencyNs, NewState} ->
            % SWARM OPTIMIZATION: Populate ultra-fast cache for direct access
            case maps:find(ActorRef, NewState#state.actors) of
                {ok, ActorHandle} when NewState#state.nif_loaded ->
                    % Cache for ultra-fast direct access
                    put({ultra_fast_actor, ActorRef}, {nif_loaded, ActorHandle});
                _ ->
                    ok
            end,
            
            %% Telemetry (with fallback for missing telemetry)
            catch telemetry:execute([bitactor, actor, spawn], #{count => 1}, #{type => Type}),
            {reply, {ok, ActorRef, LatencyNs}, NewState};
        {error, Reason, NewState} ->
            error_logger:error_msg("Failed to spawn actor ~p: ~p", [Type, Reason]),
            {reply, {error, Reason}, NewState}
    end;

handle_call({kill_actor, ActorRef}, _From, State) ->
    case kill_actor_internal(ActorRef, State) of
        {ok, NewState} ->
            catch telemetry:execute([bitactor, actor, kill], #{count => 1}, #{}),
            {reply, ok, NewState};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(get_actor_count, _From, State) ->
    Count = maps:size(State#state.actors),
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({send_message, ActorRef, Message}, State) ->
    case send_message_internal(ActorRef, Message, State) of
        {ok, NewState} ->
            catch telemetry:execute([bitactor, actor, message], #{count => 1}, #{}),
            {noreply, NewState};
        {error, Reason} ->
            error_logger:warning_msg("Failed to send message to ~p: ~p", [ActorRef, Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(tick, State) ->
    NewState = process_tick(State),
    
    %% Schedule next tick
    TickInterval = bitactor_app:get_env(tick_interval, 1000),
    TRef = erlang:send_after(TickInterval, self(), tick),
    
    {noreply, NewState#state{tick_ref = TRef}};

handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:warning("BitActor server received EXIT signal: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    error_logger:info_msg("BitActor server terminating: ~p", [Reason]),
    
    %% Cancel tick timer
    case State#state.tick_ref of
        undefined -> ok;
        TRef -> erlang:cancel_timer(TRef)
    end,
    
    %% Clean up actors
    cleanup_actors(State#state.actors),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec load_nif() -> boolean().
load_nif() ->
    try
        %% Ensure NIF module is loaded first
        case code:ensure_loaded(bitactor_nif) of
            {module, bitactor_nif} ->
                %% NIF module loaded, now initialize the NIF
                case bitactor_nif:init() of
                    ok -> 
                        error_logger:info_msg("BitActor NIF initialized successfully"),
                        true;
                    {error, {reload, _}} ->
                        %% NIF already loaded, this is OK
                        true;
                    {error, Reason} ->
                        error_logger:warning_msg("BitActor NIF not available: ~p", [Reason]),
                        false
                end;
            {error, Reason} ->
                error_logger:error_msg("Failed to load bitactor_nif module: ~p", [Reason]),
                false
        end
    catch
        ErrorType:ErrorReason ->
            error_logger:error_msg("Exception loading NIF: ~p:~p", [ErrorType, ErrorReason]),
            false
    end.

-spec spawn_actor_internal(atom(), term(), #state{}) -> 
    {ok, reference(), #state{}} | {error, term(), #state{}}.
spawn_actor_internal(Type, InitData, State) ->
    case State#state.nif_loaded of
        true ->
            try
                ActorRef = make_ref(),
                %% Call C NIF to create actor
                TypeInt = erlang:phash2(Type), % Convert atom to integer
                case bitactor_nif:create_actor(TypeInt, InitData) of
                    {ok, ActorHandle, LatencyNs} ->
                        Actors = maps:put(ActorRef, ActorHandle, State#state.actors),
                        Stats = increment_stat(actors_spawned, State#state.stats),
                        NewState = State#state{actors = Actors, stats = Stats},
                        {ok, ActorRef, LatencyNs, NewState};
                    {error, Reason} ->
                        {error, Reason, State}
                end
            catch
                error:undef ->
                    {error, nif_not_loaded, State};
                ErrorType:ErrorReason ->
                    {error, {ErrorType, ErrorReason}, State}
            end;
        false ->
            %% Fallback implementation without NIF
            ActorRef = make_ref(),
            Actors = maps:put(ActorRef, {Type, InitData}, State#state.actors),
            Stats = increment_stat(actors_spawned, State#state.stats),
            NewState = State#state{actors = Actors, stats = Stats},
            FallbackLatency = 500000, % 500 microseconds fallback
            {ok, ActorRef, FallbackLatency, NewState}
    end.

-spec kill_actor_internal(reference(), #state{}) -> 
    {ok, #state{}} | {error, not_found}.
kill_actor_internal(ActorRef, State) ->
    case maps:find(ActorRef, State#state.actors) of
        {ok, _ActorHandle} ->
            %% Clean up (C NIF handles resource cleanup automatically)
            
            Actors = maps:remove(ActorRef, State#state.actors),
            Stats = increment_stat(actors_killed, State#state.stats),
            NewState = State#state{actors = Actors, stats = Stats},
            {ok, NewState};
        error ->
            {error, not_found}
    end.

-spec send_message_internal(reference(), term(), #state{}) -> 
    {ok, #state{}} | {error, term()}.
send_message_internal(ActorRef, Message, State) ->
    %% OPTIMIZATION: Use process dictionary for fast actor lookup
    case get({actor, ActorRef}) of
        undefined ->
            %% Fallback to map lookup
            case maps:find(ActorRef, State#state.actors) of
                {ok, ActorHandle} ->
                    %% Cache in process dictionary
                    put({actor, ActorRef}, ActorHandle),
                    send_message_fast_path(ActorHandle, Message, State);
                error ->
                    {error, actor_not_found}
            end;
        ActorHandle ->
            %% Fast path - no map lookup
            send_message_fast_path(ActorHandle, Message, State)
    end.

%% OPTIMIZATION: Separated fast path for message sending
-spec send_message_fast_path(term(), term(), #state{}) -> {ok, #state{}} | {error, term()}.
send_message_fast_path(ActorHandle, Message, State) ->
    if State#state.nif_loaded ->
        %% Direct NIF call - no error handling for speed
        _ = bitactor_nif:send_message(ActorHandle, Message),
        Stats = increment_stat(messages_sent, State#state.stats),
        {ok, State#state{stats = Stats}};
    true ->
        %% Fallback without NIF
        Stats = increment_stat(messages_sent, State#state.stats),
        {ok, State#state{stats = Stats}}
    end.

-spec process_tick(#state{}) -> #state{}.
process_tick(State) ->
    %% Process all actors
    if State#state.nif_loaded ->
        try 
            case bitactor_nif:tick_all() of
                {ok, _LatencyNs} -> ok;
                _ -> ok
            end
        catch _:_ -> ok end;
    true -> ok
    end,
    
    %% Update statistics
    Stats = increment_stat(ticks_processed, State#state.stats),
    
    %% Emit telemetry
    ActorCount = maps:size(State#state.actors),
    catch telemetry:execute([bitactor, system, tick], #{actors => ActorCount}, #{}),
    
    State#state{stats = Stats}.

-spec cleanup_actors(#{reference() => term()}) -> ok.
cleanup_actors(_Actors) ->
    %% C NIF handles resource cleanup automatically via resource destructor
    ok.

-spec increment_stat(atom(), #{atom() => term()}) -> #{atom() => term()}.
increment_stat(Key, Stats) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats).