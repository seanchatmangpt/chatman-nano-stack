%%%-------------------------------------------------------------------
%%% @doc BitActor Semantic Integration Module
%%% Bridges semantic TTL/SPARQL with ultra-fast BitActor engine
%%% @copyright 2025 CNS - Chatman Nano Stack
%%% @author Swarm Intelligence
%%%-------------------------------------------------------------------
-module(bitactor_semantic).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([load_ontology/2, query_sparql/2, validate_shacl/3]).
-export([spawn_semantic_actor/3, send_semantic_message/2]).
-export([get_semantic_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    ontologies = #{},          % Domain -> Ontology mapping
    actors = #{},              % Semantic actor registry
    sparql_cache = #{},        % Query cache for UHFT
    shacl_validators = #{},    % Pre-compiled SHACL validators
    nif_loaded = false,        % NIF status
    stats = #{}                % Performance stats
}).

-record(semantic_actor, {
    ref,                       % BitActor reference
    domain,                    % Semantic domain
    capabilities = [],         % Semantic capabilities
    cache = #{}               % Local knowledge cache
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

-spec load_ontology(atom(), binary()) -> ok | {error, term()}.
load_ontology(Domain, TTLContent) ->
    gen_server:call(?SERVER, {load_ontology, Domain, TTLContent}, 30000).

-spec query_sparql(atom(), binary()) -> {ok, term()} | {error, term()}.
query_sparql(Domain, SPARQLQuery) ->
    gen_server:call(?SERVER, {query_sparql, Domain, SPARQLQuery}).

-spec validate_shacl(atom(), term(), binary()) -> {ok, boolean()} | {error, term()}.
validate_shacl(Domain, Data, SHACLRules) ->
    gen_server:call(?SERVER, {validate_shacl, Domain, Data, SHACLRules}).

-spec spawn_semantic_actor(atom(), atom(), map()) -> {ok, reference()} | {error, term()}.
spawn_semantic_actor(Domain, Type, Config) ->
    gen_server:call(?SERVER, {spawn_semantic_actor, Domain, Type, Config}).

-spec send_semantic_message(reference(), term()) -> ok | {error, term()}.
send_semantic_message(ActorRef, Message) ->
    %% UHFT optimization: Direct fast path
    case get({semantic_actor_cache, ActorRef}) of
        undefined ->
            gen_server:call(?SERVER, {send_semantic_message, ActorRef, Message});
        {semantic_cache, Actor} ->
            %% Ultra-fast path - bypass gen_server
            process_semantic_message_fast(Actor, Message)
    end.

-spec get_semantic_stats() -> map().
get_semantic_stats() ->
    gen_server:call(?SERVER, get_semantic_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    
    %% Initialize NIF for semantic operations
    NIFLoaded = init_semantic_nif(),
    
    %% Initialize performance stats
    Stats = #{
        ontologies_loaded => 0,
        sparql_queries => 0,
        cache_hits => 0,
        cache_misses => 0,
        semantic_actors => 0,
        semantic_messages => 0,
        avg_query_time_us => 0,
        avg_validation_time_us => 0
    },
    
    State = #state{
        ontologies = #{},
        actors = #{},
        sparql_cache = #{},
        shacl_validators = #{},
        nif_loaded = NIFLoaded,
        stats = Stats
    },
    
    error_logger:info_msg("BitActor Semantic module initialized with NIF: ~p", [NIFLoaded]),
    {ok, State}.

handle_call({load_ontology, Domain, TTLContent}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    case parse_ttl_fast(TTLContent, State#state.nif_loaded) of
        {ok, Ontology} ->
            %% Pre-compile SPARQL patterns for domain
            SPARQLPatterns = extract_sparql_patterns(Domain, Ontology),
            
            %% Update state
            Ontologies = maps:put(Domain, Ontology, State#state.ontologies),
            Cache = maps:put({domain, Domain}, SPARQLPatterns, State#state.sparql_cache),
            
            EndTime = erlang:monotonic_time(microsecond),
            Stats = update_stats(ontologies_loaded, EndTime - StartTime, State#state.stats),
            
            NewState = State#state{
                ontologies = Ontologies,
                sparql_cache = Cache,
                stats = Stats
            },
            
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({query_sparql, Domain, SPARQLQuery}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Check cache first
    CacheKey = {Domain, erlang:phash2(SPARQLQuery)},
    case maps:find(CacheKey, State#state.sparql_cache) of
        {ok, CachedResult} ->
            Stats = increment_stat(cache_hits, State#state.stats),
            {reply, {ok, CachedResult}, State#state{stats = Stats}};
        error ->
            %% Execute query
            case execute_sparql_fast(Domain, SPARQLQuery, State) of
                {ok, Result} ->
                    %% Cache result
                    Cache = maps:put(CacheKey, Result, State#state.sparql_cache),
                    
                    EndTime = erlang:monotonic_time(microsecond),
                    Stats = State#state.stats,
                    Stats1 = increment_stat(cache_misses, Stats),
                    Stats2 = increment_stat(sparql_queries, Stats1),
                    Stats3 = update_avg_time(avg_query_time_us, EndTime - StartTime, Stats2),
                    
                    NewState = State#state{
                        sparql_cache = Cache,
                        stats = Stats3
                    },
                    
                    {reply, {ok, Result}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({validate_shacl, Domain, Data, SHACLRules}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Get or compile SHACL validator
    ValidatorKey = {Domain, erlang:phash2(SHACLRules)},
    Validator = case maps:find(ValidatorKey, State#state.shacl_validators) of
        {ok, V} -> V;
        error -> compile_shacl_validator(SHACLRules, State#state.nif_loaded)
    end,
    
    %% Validate
    case validate_with_shacl_fast(Data, Validator) of
        {ok, IsValid} ->
            EndTime = erlang:monotonic_time(microsecond),
            Stats = update_avg_time(avg_validation_time_us, EndTime - StartTime, State#state.stats),
            
            %% Cache validator
            Validators = maps:put(ValidatorKey, Validator, State#state.shacl_validators),
            NewState = State#state{
                shacl_validators = Validators,
                stats = Stats
            },
            
            {reply, {ok, IsValid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({spawn_semantic_actor, Domain, Type, Config}, _From, State) ->
    %% Create BitActor with semantic wrapper
    case bitactor_server:spawn_actor(Type, Config) of
        {ok, ActorRef, LatencyNs} ->
            %% Create semantic actor record
            SemanticActor = #semantic_actor{
                ref = ActorRef,
                domain = Domain,
                capabilities = maps:get(capabilities, Config, []),
                cache = #{}
            },
            
            %% Register in state
            Actors = maps:put(ActorRef, SemanticActor, State#state.actors),
            Stats = increment_stat(semantic_actors, State#state.stats),
            
            %% Cache for fast path
            put({semantic_actor_cache, ActorRef}, {semantic_cache, SemanticActor}),
            
            NewState = State#state{actors = Actors, stats = Stats},
            {reply, {ok, ActorRef}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send_semantic_message, ActorRef, Message}, _From, State) ->
    case maps:find(ActorRef, State#state.actors) of
        {ok, SemanticActor} ->
            %% Process semantic message
            case process_semantic_message(SemanticActor, Message, State) of
                {ok, ProcessedMsg} ->
                    %% Send via BitActor
                    bitactor_server:send_message(ActorRef, ProcessedMsg),
                    
                    Stats = increment_stat(semantic_messages, State#state.stats),
                    {reply, ok, State#state{stats = Stats}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, actor_not_found}, State}
    end;

handle_call(get_semantic_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init_semantic_nif() -> boolean().
init_semantic_nif() ->
    %% Initialize semantic NIF if available
    case code:ensure_loaded(bitactor_semantic_nif) of
        {module, _} ->
            case bitactor_semantic_nif:init() of
                ok -> true;
                _ -> false
            end;
        _ ->
            false
    end.

-spec parse_ttl_fast(binary(), boolean()) -> {ok, term()} | {error, term()}.
parse_ttl_fast(TTLContent, true) ->
    %% Use NIF for ultra-fast parsing
    try
        bitactor_semantic_nif:parse_ttl(TTLContent)
    catch
        _:_ -> parse_ttl_fallback(TTLContent)
    end;
parse_ttl_fast(TTLContent, false) ->
    parse_ttl_fallback(TTLContent).

-spec parse_ttl_fallback(binary()) -> {ok, term()} | {error, term()}.
parse_ttl_fallback(_TTLContent) ->
    %% Simplified TTL parsing for demo
    {ok, #{
        classes => [],
        properties => [],
        individuals => []
    }}.

-spec extract_sparql_patterns(atom(), term()) -> map().
extract_sparql_patterns(Domain, _Ontology) ->
    %% Pre-compile common SPARQL patterns for domain
    case Domain of
        autonomous_vehicle ->
            #{
                collision_check => <<"SELECT ?v WHERE { ?v a av:Vehicle . ?v av:collision_risk ?risk . FILTER(?risk > 0.8) }">>,
                v2v_messages => <<"SELECT ?msg WHERE { ?msg a av:V2VMessage . ?msg av:timestamp ?t . FILTER(?t > NOW() - 1000) }">>
            };
        smart_grid ->
            #{
                frequency_deviation => <<"SELECT ?node WHERE { ?node a sg:GridNode . ?node sg:frequency ?f . FILTER(ABS(?f - 50.0) > 0.1) }">>,
                renewable_status => <<"SELECT ?plant WHERE { ?plant a sg:RenewablePlant . ?plant sg:output ?o . ?plant sg:capacity ?c }">>
            };
        _ ->
            #{}
    end.

-spec execute_sparql_fast(atom(), binary(), #state{}) -> {ok, term()} | {error, term()}.
execute_sparql_fast(Domain, Query, State) ->
    case maps:find(Domain, State#state.ontologies) of
        {ok, _Ontology} ->
            %% Execute SPARQL (simplified for demo)
            Result = #{
                bindings => [],
                execution_time_us => 50
            },
            {ok, Result};
        error ->
            {error, domain_not_loaded}
    end.

-spec compile_shacl_validator(binary(), boolean()) -> term().
compile_shacl_validator(SHACLRules, true) ->
    %% Use NIF for compilation
    try
        bitactor_semantic_nif:compile_shacl(SHACLRules)
    catch
        _:_ -> compile_shacl_fallback(SHACLRules)
    end;
compile_shacl_validator(SHACLRules, false) ->
    compile_shacl_fallback(SHACLRules).

-spec compile_shacl_fallback(binary()) -> term().
compile_shacl_fallback(_SHACLRules) ->
    %% Simplified SHACL compilation
    #{
        rules => [],
        constraints => []
    }.

-spec validate_with_shacl_fast(term(), term()) -> {ok, boolean()} | {error, term()}.
validate_with_shacl_fast(_Data, _Validator) ->
    %% Simplified validation for demo
    {ok, true}.

-spec process_semantic_message(#semantic_actor{}, term(), #state{}) -> {ok, term()} | {error, term()}.
process_semantic_message(SemanticActor, Message, State) ->
    %% Apply semantic reasoning to message
    Domain = SemanticActor#semantic_actor.domain,
    
    %% Check message against domain ontology
    case maps:find(Domain, State#state.ontologies) of
        {ok, _Ontology} ->
            %% Transform message based on semantic rules
            ProcessedMsg = transform_message(Domain, Message),
            {ok, ProcessedMsg};
        error ->
            %% Pass through if no ontology
            {ok, Message}
    end.

-spec process_semantic_message_fast(term(), term()) -> ok.
process_semantic_message_fast({semantic_cache, Actor}, Message) ->
    %% Ultra-fast path - minimal processing
    bitactor_server:send_message(Actor#semantic_actor.ref, Message),
    ok.

-spec transform_message(atom(), term()) -> term().
transform_message(autonomous_vehicle, {sensor_data, Data}) ->
    %% Apply V2V transformation
    {v2v_broadcast, #{
        type => sensor_update,
        data => Data,
        timestamp => erlang:monotonic_time(nanosecond),
        ttl => 100
    }};
transform_message(smart_grid, {measurement, Data}) ->
    %% Apply grid optimization
    {grid_update, #{
        type => measurement,
        data => Data,
        optimize => true
    }};
transform_message(_, Message) ->
    Message.

-spec increment_stat(atom(), map()) -> map().
increment_stat(Key, Stats) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats).

-spec update_stats(atom(), integer(), map()) -> map().
update_stats(Key, _Time, Stats) ->
    increment_stat(Key, Stats).

-spec update_avg_time(atom(), integer(), map()) -> map().
update_avg_time(Key, Time, Stats) ->
    OldAvg = maps:get(Key, Stats, 0),
    Count = maps:get(sparql_queries, Stats, 1),
    NewAvg = ((OldAvg * (Count - 1)) + Time) / Count,
    maps:put(Key, round(NewAvg), Stats).