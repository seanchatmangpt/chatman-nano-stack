%%%-------------------------------------------------------------------
%%% @doc CNS Forge Ash.Reactor Bridge
%%% 80/20 Implementation: Bridge existing BitActor to Ash.Reactor patterns
%%% Implements TTL-driven execution using existing infrastructure
%%% @copyright 2025 CNS - Technology Applications, Inc.
%%%-------------------------------------------------------------------
-module(cns_forge_ash_reactor_bridge).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([execute_workflow/2, spawn_bitactor_step/3]).
-export([get_workflow_status/1, get_telemetry/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL, 8).  % 8 hops as per spec
-define(MAX_TTL, 16).

-record(state, {
    active_workflows = #{} :: map(),
    bitactor_registry = #{} :: map(),
    workflow_counter = 0 :: non_neg_integer(),
    telemetry_data = #{} :: map()
}).

-record(workflow, {
    workflow_id :: binary(),
    token :: map(),
    steps :: list(),
    current_step :: non_neg_integer(),
    status :: pending | running | completed | failed,
    started_at :: integer(),
    telemetry :: list()
}).

-record(token, {
    ttl :: non_neg_integer(),
    transaction_id :: binary(),
    payload :: term(),
    metadata :: map()
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

-spec execute_workflow(binary(), term()) -> {ok, binary()} | {error, term()}.
execute_workflow(WorkflowType, InitialPayload) ->
    gen_server:call(?SERVER, {execute_workflow, WorkflowType, InitialPayload}).

-spec spawn_bitactor_step(binary(), atom(), term()) -> {ok, reference()} | {error, term()}.
spawn_bitactor_step(WorkflowId, StepType, StepData) ->
    gen_server:call(?SERVER, {spawn_bitactor_step, WorkflowId, StepType, StepData}).

-spec get_workflow_status(binary()) -> {ok, map()} | {error, not_found}.
get_workflow_status(WorkflowId) ->
    gen_server:call(?SERVER, {get_workflow_status, WorkflowId}).

-spec get_telemetry() -> map().
get_telemetry() ->
    gen_server:call(?SERVER, get_telemetry).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    %% Initialize telemetry
    TelemetryData = #{
        workflows_executed => 0,
        bitactors_spawned => 0,
        ttl_expirations => 0,
        average_latency_ns => 0,
        start_time => erlang:system_time(millisecond)
    },
    
    error_logger:info_msg("CNS Forge Ash.Reactor Bridge initialized"),
    {ok, #state{telemetry_data = TelemetryData}}.

handle_call({execute_workflow, WorkflowType, InitialPayload}, _From, State) ->
    case create_workflow(WorkflowType, InitialPayload, State) of
        {ok, WorkflowId, NewState} ->
            %% Start workflow execution asynchronously
            gen_server:cast(self(), {start_workflow, WorkflowId}),
            {reply, {ok, WorkflowId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({spawn_bitactor_step, WorkflowId, StepType, StepData}, _From, State) ->
    case execute_step(WorkflowId, StepType, StepData, State) of
        {ok, StepRef, NewState} ->
            {reply, {ok, StepRef}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_workflow_status, WorkflowId}, _From, State) ->
    case maps:find(WorkflowId, State#state.active_workflows) of
        {ok, Workflow} ->
            Status = #{
                workflow_id => Workflow#workflow.workflow_id,
                status => Workflow#workflow.status,
                current_step => Workflow#workflow.current_step,
                total_steps => length(Workflow#workflow.steps),
                ttl_remaining => (Workflow#workflow.token)#token.ttl,
                started_at => Workflow#workflow.started_at,
                telemetry => Workflow#workflow.telemetry
            },
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_telemetry, _From, State) ->
    {reply, State#state.telemetry_data, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({start_workflow, WorkflowId}, State) ->
    case maps:find(WorkflowId, State#state.active_workflows) of
        {ok, Workflow} ->
            NewState = execute_next_step(Workflow, State),
            {noreply, NewState};
        error ->
            error_logger:warning_msg("Workflow ~p not found for execution", [WorkflowId]),
            {noreply, State}
    end;

handle_cast({step_completed, WorkflowId, StepResult}, State) ->
    NewState = handle_step_completion(WorkflowId, StepResult, State),
    {noreply, NewState};

handle_cast({ttl_expired, WorkflowId}, State) ->
    NewState = handle_ttl_expiration(WorkflowId, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:warning_msg("BitActor process ~p exited: ~p", [Pid, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Workflow Management
%%%===================================================================

create_workflow(WorkflowType, InitialPayload, State) ->
    WorkflowId = generate_workflow_id(State#state.workflow_counter),
    TransactionId = generate_transaction_id(),
    
    %% Create initial token with TTL
    Token = #token{
        ttl = ?DEFAULT_TTL,
        transaction_id = TransactionId,
        payload = InitialPayload,
        metadata = #{
            workflow_type => WorkflowType,
            created_at => erlang:system_time(nanosecond)
        }
    },
    
    %% Define workflow steps based on type
    Steps = get_workflow_steps(WorkflowType),
    
    Workflow = #workflow{
        workflow_id = WorkflowId,
        token = Token,
        steps = Steps,
        current_step = 0,
        status = pending,
        started_at = erlang:system_time(millisecond),
        telemetry = []
    },
    
    Workflows = maps:put(WorkflowId, Workflow, State#state.active_workflows),
    TelemetryData = increment_telemetry(workflows_executed, State#state.telemetry_data),
    NewState = State#state{
        active_workflows = Workflows,
        workflow_counter = State#state.workflow_counter + 1,
        telemetry_data = TelemetryData
    },
    
    {ok, WorkflowId, NewState}.

execute_next_step(Workflow, State) ->
    CurrentStep = Workflow#workflow.current_step,
    Steps = Workflow#workflow.steps,
    Token = Workflow#workflow.token,
    
    %% Check TTL before proceeding
    case Token#token.ttl of
        0 ->
            %% TTL expired
            gen_server:cast(self(), {ttl_expired, Workflow#workflow.workflow_id}),
            State;
        TTL when TTL > 0 ->
            case CurrentStep < length(Steps) of
                true ->
                    StepDef = lists:nth(CurrentStep + 1, Steps),
                    execute_workflow_step(Workflow, StepDef, State);
                false ->
                    %% Workflow completed
                    complete_workflow(Workflow, State)
            end
    end.

execute_workflow_step(Workflow, {StepType, StepData}, State) ->
    StartTime = erlang:system_time(nanosecond),
    Token = Workflow#workflow.token,
    
    %% Decrement TTL (implementing hop-based TTL)
    NewTTL = Token#token.ttl - 1,
    NewToken = Token#token{ttl = NewTTL},
    
    %% Execute step using existing BitActor infrastructure
    case spawn_bitactor_for_step(StepType, StepData, NewToken) of
        {ok, BitActorRef} ->
            %% Update workflow state
            TelemetryEntry = #{
                step => Workflow#workflow.current_step + 1,
                step_type => StepType,
                ttl_remaining => NewTTL,
                started_at => StartTime,
                bitactor_ref => BitActorRef
            },
            
            UpdatedTelemetry = [TelemetryEntry | Workflow#workflow.telemetry],
            UpdatedWorkflow = Workflow#workflow{
                token = NewToken,
                current_step = Workflow#workflow.current_step + 1,
                status = running,
                telemetry = UpdatedTelemetry
            },
            
            Workflows = maps:put(Workflow#workflow.workflow_id, UpdatedWorkflow, State#state.active_workflows),
            TelemetryData = increment_telemetry(bitactors_spawned, State#state.telemetry_data),
            
            %% Register BitActor for tracking
            Registry = maps:put(BitActorRef, Workflow#workflow.workflow_id, State#state.bitactor_registry),
            
            %% Emit telemetry event (CNS Forge pulse log)
            emit_pulse_log(Workflow#workflow.workflow_id, TelemetryEntry),
            
            State#state{
                active_workflows = Workflows,
                bitactor_registry = Registry,
                telemetry_data = TelemetryData
            };
        {error, Reason} ->
            fail_workflow(Workflow, Reason, State)
    end.

spawn_bitactor_for_step(StepType, StepData, Token) ->
    %% Use existing BitActor infrastructure
    case bitactor_server:spawn_actor(StepType, {Token, StepData}) of
        {ok, ActorRef, _LatencyNs} ->
            {ok, ActorRef};
        {error, Reason} ->
            {error, Reason}
    end.

handle_step_completion(WorkflowId, StepResult, State) ->
    case maps:find(WorkflowId, State#state.active_workflows) of
        {ok, Workflow} ->
            %% Update telemetry
            [CurrentTelemetry | RestTelemetry] = Workflow#workflow.telemetry,
            CompletedTelemetry = CurrentTelemetry#{
                completed_at => erlang:system_time(nanosecond),
                result => StepResult
            },
            UpdatedTelemetry = [CompletedTelemetry | RestTelemetry],
            
            UpdatedWorkflow = Workflow#workflow{telemetry = UpdatedTelemetry},
            Workflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.active_workflows),
            
            %% Continue to next step
            execute_next_step(UpdatedWorkflow, State#state{active_workflows = Workflows});
        error ->
            State
    end.

handle_ttl_expiration(WorkflowId, State) ->
    case maps:find(WorkflowId, State#state.active_workflows) of
        {ok, Workflow} ->
            error_logger:warning_msg("Workflow ~p TTL expired", [WorkflowId]),
            
            %% Emit TTL expiration telemetry
            emit_pulse_log(WorkflowId, #{
                event => ttl_expired,
                timestamp => erlang:system_time(nanosecond),
                final_ttl => (Workflow#workflow.token)#token.ttl
            }),
            
            FailedWorkflow = Workflow#workflow{status = failed},
            Workflows = maps:put(WorkflowId, FailedWorkflow, State#state.active_workflows),
            TelemetryData = increment_telemetry(ttl_expirations, State#state.telemetry_data),
            
            State#state{
                active_workflows = Workflows,
                telemetry_data = TelemetryData
            };
        error ->
            State
    end.

complete_workflow(Workflow, State) ->
    CompletedWorkflow = Workflow#workflow{
        status = completed
    },
    Workflows = maps:put(Workflow#workflow.workflow_id, CompletedWorkflow, State#state.active_workflows),
    
    %% Emit completion telemetry
    emit_pulse_log(Workflow#workflow.workflow_id, #{
        event => workflow_completed,
        timestamp => erlang:system_time(nanosecond),
        total_steps => length(Workflow#workflow.steps)
    }),
    
    State#state{active_workflows = Workflows}.

fail_workflow(Workflow, Reason, State) ->
    FailedWorkflow = Workflow#workflow{status = failed},
    Workflows = maps:put(Workflow#workflow.workflow_id, FailedWorkflow, State#state.active_workflows),
    
    error_logger:error_msg("Workflow ~p failed: ~p", [Workflow#workflow.workflow_id, Reason]),
    
    State#state{active_workflows = Workflows}.

%%%===================================================================
%%% Internal functions - Support
%%%===================================================================

execute_step(_WorkflowId, StepType, StepData, State) ->
    %% Delegate to existing BitActor server
    case bitactor_server:spawn_actor(StepType, StepData) of
        {ok, ActorRef, _LatencyNs} ->
            TelemetryData = increment_telemetry(bitactors_spawned, State#state.telemetry_data),
            NewState = State#state{telemetry_data = TelemetryData},
            {ok, ActorRef, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

get_workflow_steps(WorkflowType) ->
    %% Define standard CNS Forge workflows
    case WorkflowType of
        <<"user_registration">> ->
            [
                {decode_params, #{}},
                {validate_user, #{}},
                {create_user_db, #{}},
                {create_profile, #{}},
                {send_welcome_email, #{}}
            ];
        <<"order_processing">> ->
            [
                {validate_order, #{}},
                {check_inventory, #{}},
                {process_payment, #{}},
                {create_shipment, #{}},
                {notify_customer, #{}}
            ];
        <<"system_health_check">> ->
            [
                {check_database, #{}},
                {check_services, #{}},
                {validate_metrics, #{}},
                {generate_report, #{}}
            ];
        _ ->
            %% Default workflow
            [
                {process_input, #{}},
                {execute_logic, #{}},
                {generate_output, #{}}
            ]
    end.

generate_workflow_id(Counter) ->
    <<(integer_to_binary(erlang:system_time(millisecond)))/binary, "_", 
      (integer_to_binary(Counter))/binary>>.

generate_transaction_id() ->
    <<(integer_to_binary(erlang:system_time(nanosecond)))/binary, "_",
      (integer_to_binary(erlang:phash2(make_ref())))/binary>>.

increment_telemetry(Key, TelemetryData) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, TelemetryData).

emit_pulse_log(WorkflowId, TelemetryData) ->
    %% Emit CNS Forge pulse log (universal observability)
    PulseLog = #{
        workflow_id => WorkflowId,
        timestamp => erlang:system_time(nanosecond),
        data => TelemetryData,
        source => cns_forge_ash_reactor_bridge
    },
    
    %% Send to telemetry system (existing infrastructure)
    try
        telemetry:execute([cns_forge, workflow, pulse], PulseLog, #{})
    catch
        _:_ -> ok %% Graceful degradation if telemetry not available
    end.