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

-spec spawn_actor(atom(), term()) -> {ok, reference()} | {error, term()}.
spawn_actor(Type, InitData) ->
    gen_server:call(?SERVER, {spawn_actor, Type, InitData}).

-spec kill_actor(reference()) -> ok | {error, not_found}.
kill_actor(ActorRef) ->
    gen_server:call(?SERVER, {kill_actor, ActorRef}).

-spec send_message(reference(), term()) -> ok | {error, term()}.
send_message(ActorRef, Message) ->
    gen_server:cast(?SERVER, {send_message, ActorRef, Message}).

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
    
    error_logger:info("BitActor server initialized with NIF loaded: ~p", [NIFLoaded]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> 
    {reply, term(), #state{}} | {noreply, #state{}} | {stop, term(), term(), #state{}}.
handle_call({spawn_actor, Type, InitData}, _From, State) ->
    case spawn_actor_internal(Type, InitData, State) of
        {ok, ActorRef, NewState} ->
            %% Telemetry
            telemetry:execute([bitactor, actor, spawn], #{count => 1}, #{type => Type}),
            {reply, {ok, ActorRef}, NewState};
        {error, Reason, NewState} ->
            error_logger:error("Failed to spawn actor ~p: ~p", [Type, Reason]),
            {reply, {error, Reason}, NewState}
    end;

handle_call({kill_actor, ActorRef}, _From, State) ->
    case kill_actor_internal(ActorRef, State) of
        {ok, NewState} ->
            telemetry:execute([bitactor, actor, kill], #{count => 1}, #{}),
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
            telemetry:execute([bitactor, actor, message], #{count => 1}, #{}),
            {noreply, NewState};
        {error, Reason} ->
            error_logger:warning("Failed to send message to ~p: ~p", [ActorRef, Reason]),
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
    error_logger:info("BitActor server terminating: ~p", [Reason]),
    
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
        PrivDir = code:priv_dir(bitactor),
        NIFPath = filename:join(PrivDir, "bitactor_nif"),
        case erlang:load_nif(NIFPath, 0) of
            ok -> true;
            {error, Reason} ->
                error_logger:error("Failed to load BitActor NIF: ~p", [Reason]),
                false
        end
    catch
        _:_ -> false
    end.

-spec spawn_actor_internal(atom(), term(), #state{}) -> 
    {ok, reference(), #state{}} | {error, term(), #state{}}.
spawn_actor_internal(Type, InitData, State) ->
    case State#state.nif_loaded of
        true ->
            try
                ActorRef = make_ref(),
                %% Call C NIF to create actor
                case bitactor_nif:create_actor(Type, InitData) of
                    {ok, ActorHandle} ->
                        Actors = maps:put(ActorRef, ActorHandle, State#state.actors),
                        Stats = increment_stat(actors_spawned, State#state.stats),
                        NewState = State#state{actors = Actors, stats = Stats},
                        {ok, ActorRef, NewState};
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
            {ok, ActorRef, NewState}
    end.

-spec kill_actor_internal(reference(), #state{}) -> 
    {ok, #state{}} | {error, not_found}.
kill_actor_internal(ActorRef, State) ->
    case maps:find(ActorRef, State#state.actors) of
        {ok, ActorHandle} ->
            %% Clean up via NIF if available
            if State#state.nif_loaded ->
                try bitactor_nif:destroy_actor(ActorHandle)
                catch _:_ -> ok end;
            true -> ok
            end,
            
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
    case maps:find(ActorRef, State#state.actors) of
        {ok, ActorHandle} ->
            if State#state.nif_loaded ->
                try
                    case bitactor_nif:send_message(ActorHandle, Message) of
                        ok ->
                            Stats = increment_stat(messages_sent, State#state.stats),
                            {ok, State#state{stats = Stats}};
                        {error, Reason} ->
                            {error, Reason}
                    end
                catch
                    _:_ -> {error, nif_error}
                end;
            true ->
                %% Fallback without NIF
                Stats = increment_stat(messages_sent, State#state.stats),
                {ok, State#state{stats = Stats}}
            end;
        error ->
            {error, actor_not_found}
    end.

-spec process_tick(#state{}) -> #state{}.
process_tick(State) ->
    %% Process all actors
    if State#state.nif_loaded ->
        try bitactor_nif:tick_all()
        catch _:_ -> ok end;
    true -> ok
    end,
    
    %% Update statistics
    Stats = increment_stat(ticks_processed, State#state.stats),
    
    %% Emit telemetry
    ActorCount = maps:size(State#state.actors),
    telemetry:execute([bitactor, system, tick], #{actors => ActorCount}, #{}),
    
    State#state{stats = Stats}.

-spec cleanup_actors(#{reference() => term()}) -> ok.
cleanup_actors(Actors) ->
    maps:fold(fun(_, ActorHandle, _) ->
        try bitactor_nif:destroy_actor(ActorHandle)
        catch _:_ -> ok end
    end, ok, Actors).

-spec increment_stat(atom(), #{atom() => term()}) -> #{atom() => term()}.
increment_stat(Key, Stats) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats).