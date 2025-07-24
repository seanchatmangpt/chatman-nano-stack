%%%-------------------------------------------------------------------
%%% @doc BitActor Telemetry Handler
%%% Production-grade telemetry and monitoring
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_telemetry).
-behaviour(gen_server).

%% API
-export([start_link/0, handle_event/4]).
-export([get_metrics/0, reset_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    metrics = #{} :: #{atom() => term()},
    start_time :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec handle_event([atom()], map(), map(), map()) -> ok.
handle_event([bitactor, actor, spawn], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {metric, actor_spawned, Measurements, Metadata});

handle_event([bitactor, actor, kill], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {metric, actor_killed, Measurements, Metadata});

handle_event([bitactor, actor, message], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {metric, message_sent, Measurements, Metadata});

handle_event([bitactor, system, tick], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {metric, system_tick, Measurements, Metadata});

handle_event([bitactor, system, memory], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {metric, memory_usage, Measurements, Metadata});

handle_event(_EventName, _Measurements, _Metadata, _Config) ->
    ok.

-spec get_metrics() -> #{atom() => term()}.
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize metrics collection
    Metrics = #{
        actors_spawned => 0,
        actors_killed => 0,
        messages_sent => 0,
        system_ticks => 0,
        memory_samples => [],
        error_count => 0,
        last_error => undefined
    },
    
    %% Start periodic metrics collection
    erlang:send_after(60000, self(), collect_system_metrics),
    
    State = #state{
        metrics = Metrics,
        start_time = erlang:system_time(millisecond)
    },
    
    error_logger:info_msg("BitActor telemetry handler started"),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}} | {stop, term(), term(), #state{}}.
handle_call(get_metrics, _From, State) ->
    Uptime = erlang:system_time(millisecond) - State#state.start_time,
    EnrichedMetrics = maps:merge(State#state.metrics, #{
        uptime_ms => Uptime,
        current_actors => bitactor_server:get_actor_count(),
        system_info => get_system_info()
    }),
    {reply, EnrichedMetrics, State};

handle_call(reset_metrics, _From, State) ->
    NewMetrics = #{
        actors_spawned => 0,
        actors_killed => 0,
        messages_sent => 0,
        system_ticks => 0,
        memory_samples => [],
        error_count => 0,
        last_error => undefined
    },
    NewState = State#state{
        metrics = NewMetrics,
        start_time = erlang:system_time(millisecond)
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({metric, Type, Measurements, Metadata}, State) ->
    NewState = update_metric(Type, Measurements, Metadata, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(collect_system_metrics, State) ->
    NewState = collect_system_metrics(State),
    erlang:send_after(60000, self(), collect_system_metrics),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, _State) ->
    error_logger:info_msg("BitActor telemetry handler terminating: ~p", [Reason]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec update_metric(atom(), map(), map(), #state{}) -> #state{}.
update_metric(actor_spawned, _Measurements, _Metadata, State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{actors_spawned => maps:get(actors_spawned, Metrics, 0) + 1},
    State#state{metrics = NewMetrics};

update_metric(actor_killed, _Measurements, _Metadata, State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{actors_killed => maps:get(actors_killed, Metrics, 0) + 1},
    State#state{metrics = NewMetrics};

update_metric(message_sent, _Measurements, _Metadata, State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{messages_sent => maps:get(messages_sent, Metrics, 0) + 1},
    State#state{metrics = NewMetrics};

update_metric(system_tick, Measurements, _Metadata, State) ->
    Metrics = State#state.metrics,
    ActorCount = maps:get(actors, Measurements, 0),
    NewMetrics = Metrics#{
        system_ticks => maps:get(system_ticks, Metrics, 0) + 1,
        last_tick_actors => ActorCount
    },
    State#state{metrics = NewMetrics};

update_metric(memory_usage, Measurements, _Metadata, State) ->
    Metrics = State#state.metrics,
    MemoryUsage = maps:get(memory, Measurements, 0),
    CurrentSamples = maps:get(memory_samples, Metrics, []),
    %% Keep only last 100 samples
    NewSamples = lists:sublist([MemoryUsage | CurrentSamples], 100),
    NewMetrics = Metrics#{memory_samples => NewSamples},
    State#state{metrics = NewMetrics};

update_metric(_Type, _Measurements, _Metadata, State) ->
    State.

-spec collect_system_metrics(#state{}) -> #state{}.
collect_system_metrics(State) ->
    %% Collect system-wide metrics
    MemoryInfo = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    RunQueueLen = erlang:statistics(run_queue),
    
    %% Emit telemetry events
    telemetry:execute([bitactor, system, memory], #{
        total => proplists:get_value(total, MemoryInfo, 0),
        processes => proplists:get_value(processes, MemoryInfo, 0),
        atom => proplists:get_value(atom, MemoryInfo, 0)
    }, #{}),
    
    %% Update internal metrics
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{
        system_memory => MemoryInfo,
        process_count => ProcessCount,
        run_queue_len => RunQueueLen,
        last_collection => erlang:system_time(millisecond)
    },
    
    State#state{metrics = NewMetrics}.

-spec get_system_info() -> #{atom() => term()}.
get_system_info() ->
    #{
        otp_release => erlang:system_info(otp_release),
        system_version => erlang:system_info(system_version),
        process_limit => erlang:system_info(process_limit),
        port_limit => erlang:system_info(port_limit),
        schedulers => erlang:system_info(schedulers),
        logical_processors => erlang:system_info(logical_processors_available)
    }.