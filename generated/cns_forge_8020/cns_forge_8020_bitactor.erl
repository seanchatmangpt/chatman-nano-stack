%%%-------------------------------------------------------------------
%%% @doc CNS Forge 8020 BitActor Implementation
%%% Integrates TTL-driven execution with existing BitActor infrastructure
%%% @copyright 2025 CNS - Technology Applications, Inc.
%%%-------------------------------------------------------------------
-module(cns_forge_8020_bitactor).
-behaviour(gen_server).

%% API
-export([start_link/0, execute_workflow/2, process_hop/3]).
-export([get_telemetry/0, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL, 8).

-record(state, {
    active_tokens = #{} :: map(),
    telemetry = #{} :: map()
}).

-record(token, {
    ttl :: non_neg_integer(),
    transaction_id :: binary(),
    payload :: term(),
    workflow_type :: atom(),
    created_at :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

execute_workflow(WorkflowType, Payload) ->
    gen_server:call(?SERVER, {execute_workflow, WorkflowType, Payload}).

process_hop(TokenId, HopType, HopData) ->
    gen_server:call(?SERVER, {process_hop, TokenId, HopType, HopData}).

get_telemetry() ->
    gen_server:call(?SERVER, get_telemetry).

get_status() ->
    gen_server:call(?SERVER, get_status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Telemetry = #{
        workflows_executed => 0,
        hops_processed => 0,
        ttl_expirations => 0,
        start_time => erlang:system_time(millisecond)
    },
    {ok, #state{telemetry = Telemetry}}.

handle_call({execute_workflow, WorkflowType, Payload}, _From, State) ->
    TokenId = generate_token_id(),
    Token = #token{
        ttl = ?DEFAULT_TTL,
        transaction_id = TokenId,
        payload = Payload,
        workflow_type = WorkflowType,
        created_at = erlang:system_time(nanosecond)
    },
    
    %% Emit pulse log
    emit_pulse_log(TokenId, workflow_started, Token),
    
    %% Start workflow execution
    case execute_workflow_step(Token, State) of
        {ok, NewState} ->
            Tokens = maps:put(TokenId, Token, State#state.active_tokens),
            UpdatedTelemetry = increment_counter(workflows_executed, State#state.telemetry),
            FinalState = NewState#state{
                active_tokens = Tokens,
                telemetry = UpdatedTelemetry
            },
            {reply, {ok, TokenId}, FinalState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({process_hop, TokenId, HopType, HopData}, _From, State) ->
    case maps:find(TokenId, State#state.active_tokens) of
        {ok, Token} ->
            case process_ttl_hop(Token, HopType, HopData, State) of
                {ok, UpdatedToken, NewState} ->
                    Tokens = maps:put(TokenId, UpdatedToken, NewState#state.active_tokens),
                    FinalState = NewState#state{active_tokens = Tokens},
                    {reply, ok, FinalState};
                {ttl_expired, NewState} ->
                    emit_pulse_log(TokenId, ttl_expired, Token),
                    Tokens = maps:remove(TokenId, NewState#state.active_tokens),
                    UpdatedTelemetry = increment_counter(ttl_expirations, NewState#state.telemetry),
                    FinalState = NewState#state{
                        active_tokens = Tokens,
                        telemetry = UpdatedTelemetry
                    },
                    {reply, {error, ttl_expired}, FinalState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, token_not_found}, State}
    end;

handle_call(get_telemetry, _From, State) ->
    {reply, State#state.telemetry, State};

handle_call(get_status, _From, State) ->
    Status = #{
        active_tokens => maps:size(State#state.active_tokens),
        telemetry => State#state.telemetry
    },
    {reply, Status, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

execute_workflow_step(Token, State) ->
    %% Delegate to existing BitActor infrastructure
    case bitactor_server:spawn_actor(Token#token.workflow_type, Token#token.payload) of
        {ok, _ActorRef, _LatencyNs} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

process_ttl_hop(Token, HopType, HopData, State) ->
    %% Check TTL before processing
    case Token#token.ttl of
        0 ->
            {ttl_expired, State};
        TTL when TTL > 0 ->
            %% Decrement TTL (hop-based execution)
            NewTTL = TTL - 1,
            UpdatedToken = Token#token{ttl = NewTTL},
            
            %% Process hop using existing infrastructure
            case bitactor_server:send_message(Token#token.transaction_id, {HopType, HopData}) of
                ok ->
                    emit_pulse_log(Token#token.transaction_id, hop_processed, UpdatedToken),
                    UpdatedTelemetry = increment_counter(hops_processed, State#state.telemetry),
                    NewState = State#state{telemetry = UpdatedTelemetry},
                    {ok, UpdatedToken, NewState};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

generate_token_id() ->
    <<(integer_to_binary(erlang:system_time(nanosecond)))/binary>>.

increment_counter(Key, Telemetry) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Telemetry).

emit_pulse_log(TokenId, Event, Token) ->
    %% CNS Forge Universal Observability
    PulseData = #{
        token_id => TokenId,
        event => Event,
        ttl_remaining => Token#token.ttl,
        timestamp => erlang:system_time(nanosecond),
        workflow_type => Token#token.workflow_type
    },
    
    %% Emit to existing telemetry infrastructure
    try
        telemetry:execute([cns_forge, pulse], PulseData, #{})
    catch
        _:_ -> ok
    end.
