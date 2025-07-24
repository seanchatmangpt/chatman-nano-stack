%%%-------------------------------------------------------------------
%%% @doc BitActor Health Monitor
%%% System health monitoring with automatic recovery
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_health).
-behaviour(gen_server).

%% API
-export([start_link/0, get_health/0, force_health_check/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    health_status = healthy :: healthy | degraded | critical,
    last_check :: integer(),
    check_interval = 30000 :: pos_integer(),
    thresholds :: #{atom() => term()}
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_health() -> #{atom() => term()}.
get_health() ->
    gen_server:call(?MODULE, get_health).

-spec force_health_check() -> ok.
force_health_check() ->
    gen_server:cast(?MODULE, force_health_check).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Set up health check thresholds
    Thresholds = #{
        max_memory_mb => 1000,
        max_processes => 50000,
        max_run_queue => 100,
        min_actors => 0,
        max_actors => 10000,
        max_error_rate => 0.1
    },
    
    State = #state{
        health_status = healthy,
        last_check = erlang:system_time(millisecond),
        thresholds = Thresholds
    },
    
    %% Schedule first health check
    erlang:send_after(5000, self(), health_check),
    
    error_logger:info_msg("BitActor health monitor started"),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}} | {stop, term(), term(), #state{}}.
handle_call(get_health, _From, State) ->
    HealthReport = generate_health_report(State),
    {reply, HealthReport, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast(force_health_check, State) ->
    NewState = perform_health_check(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(health_check, State) ->
    NewState = perform_health_check(State),
    
    %% Schedule next health check
    erlang:send_after(State#state.check_interval, self(), health_check),
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, _State) ->
    error_logger:info_msg("BitActor health monitor terminating: ~p", [Reason]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec perform_health_check(#state{}) -> #state{}.
perform_health_check(State) ->
    Now = erlang:system_time(millisecond),
    
    %% Collect system metrics
    MemoryInfo = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    RunQueueLen = erlang:statistics(run_queue),
    ActorCount = case catch bitactor_server:get_actor_count() of
        Count when is_integer(Count) -> Count;
        _ -> 0
    end,
    
    %% Get telemetry metrics
    Metrics = case catch bitactor_telemetry:get_metrics() of
        MetricsMap when is_map(MetricsMap) -> MetricsMap;
        _ -> #{}
    end,
    ErrorRate = calculate_error_rate(Metrics),
    
    %% Assess health status
    HealthStatus = assess_health_status(
        MemoryInfo, ProcessCount, RunQueueLen, ActorCount, ErrorRate, State#state.thresholds
    ),
    
    %% Log health changes
    case {State#state.health_status, HealthStatus} of
        {Same, Same} -> ok;
        {Old, New} ->
            error_logger:warning("BitActor health status changed: ~p -> ~p", [Old, New])
    end,
    
    %% Take corrective action if needed
    maybe_take_corrective_action(HealthStatus, State),
    
    State#state{
        health_status = HealthStatus,
        last_check = Now
    }.

-spec assess_health_status(list(), pos_integer(), non_neg_integer(), 
                          non_neg_integer(), float(), map()) -> 
    healthy | degraded | critical.
assess_health_status(MemoryInfo, ProcessCount, RunQueueLen, ActorCount, ErrorRate, Thresholds) ->
    TotalMemoryMB = proplists:get_value(total, MemoryInfo, 0) div (1024*1024),
    
    CriticalConditions = [
        TotalMemoryMB > maps:get(max_memory_mb, Thresholds) * 0.95,
        ProcessCount > maps:get(max_processes, Thresholds) * 0.95,
        RunQueueLen > maps:get(max_run_queue, Thresholds),
        ErrorRate > maps:get(max_error_rate, Thresholds) * 2
    ],
    
    DegradedConditions = [
        TotalMemoryMB > maps:get(max_memory_mb, Thresholds) * 0.8,
        ProcessCount > maps:get(max_processes, Thresholds) * 0.8,
        RunQueueLen > maps:get(max_run_queue, Thresholds) * 0.5,
        ActorCount > maps:get(max_actors, Thresholds) * 0.8,
        ErrorRate > maps:get(max_error_rate, Thresholds)
    ],
    
    case lists:any(fun(X) -> X end, CriticalConditions) of
        true -> critical;
        false ->
            case lists:any(fun(X) -> X end, DegradedConditions) of
                true -> degraded;
                false -> healthy
            end
    end.

-spec calculate_error_rate(map()) -> float().
calculate_error_rate(Metrics) ->
    ErrorCount = maps:get(error_count, Metrics, 0),
    TotalOps = maps:get(actors_spawned, Metrics, 0) + 
               maps:get(messages_sent, Metrics, 0) + 
               maps:get(system_ticks, Metrics, 0),
    
    case TotalOps > 0 of
        true -> ErrorCount / TotalOps;
        false -> 0.0
    end.

-spec maybe_take_corrective_action(healthy | degraded | critical, #state{}) -> ok.
maybe_take_corrective_action(critical, _State) ->
    error_logger:critical("BitActor system in CRITICAL state - implementing emergency procedures"),
    %% Implement emergency procedures
    %% 1. Throttle new actor creation
    %% 2. Trigger garbage collection
    %% 3. Alert monitoring systems
    erlang:garbage_collect(),
    ok;

maybe_take_corrective_action(degraded, _State) ->
    error_logger:warning("BitActor system DEGRADED - implementing preventive measures"),
    %% Implement preventive measures
    %% 1. Gentle garbage collection
    %% 2. Log detailed diagnostics
    erlang:garbage_collect(),
    ok;

maybe_take_corrective_action(healthy, _State) ->
    ok.

-spec generate_health_report(#state{}) -> #{atom() => term()}.
generate_health_report(State) ->
    MemoryInfo = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    RunQueueLen = erlang:statistics(run_queue),
    
    ActorCount = case catch bitactor_server:get_actor_count() of
        Count when is_integer(Count) -> Count;
        _ -> 0
    end,
    
    Metrics = case catch bitactor_telemetry:get_metrics() of
        MetricsMap when is_map(MetricsMap) -> MetricsMap;
        _ -> #{}
    end,
    
    #{
        status => State#state.health_status,
        last_check => State#state.last_check,
        system => #{
            memory_mb => proplists:get_value(total, MemoryInfo, 0) div (1024*1024),
            processes => ProcessCount,
            run_queue_length => RunQueueLen,
            actors => ActorCount
        },
        metrics => Metrics,
        thresholds => State#state.thresholds,
        recommendations => generate_recommendations(State#state.health_status, Metrics)
    }.

-spec generate_recommendations(healthy | degraded | critical, map()) -> [binary()].
generate_recommendations(healthy, _Metrics) ->
    [<<"System operating normally">>];

generate_recommendations(degraded, Metrics) ->
    Recommendations = [<<"Monitor system closely">>],
    
    %% Add specific recommendations based on metrics
    ErrorRate = calculate_error_rate(Metrics),
    case ErrorRate > 0.05 of
        true -> [<<"Investigate error patterns">> | Recommendations];
        false -> Recommendations
    end;

generate_recommendations(critical, _Metrics) ->
    [
        <<"Immediate attention required">>,
        <<"Consider scaling down non-essential operations">>,
        <<"Review system resource allocation">>,
        <<"Check for memory leaks or runaway processes">>
    ].