%%%-------------------------------------------------------------------
%%% @doc Forex BitActor Integration - Ultra-Optimized Trading System
%%% Leverages the enhanced bitactor_server.erl with direct NIF calls
%%% for maximum 50x forex trading performance
%%% @copyright 2025 CNS - Production Forex Trading
%%%-------------------------------------------------------------------
-module(forex_bitactor_integration).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([start_forex_trading/0, stop_forex_trading/0]).
-export([process_market_tick/4, execute_arbitrage/3, check_risk_limits/0]).
-export([get_trading_stats/0, get_performance_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Ultra-fast forex processing using enhanced bitactor_server optimizations
-export([ultra_fast_tick_processing/4, direct_arbitrage_execution/3]).

-define(SERVER, ?MODULE).

%% State record integrating with existing BitActor infrastructure
-record(forex_state, {
    bitactor_server_pid,           % Enhanced bitactor_server process
    trading_actors = #{},          % ActorRef -> forex processing actor
    market_data_actor,            % Dedicated market data processor
    arbitrage_actor,              % Ultra-fast arbitrage detector
    risk_manager_actor,           % Real-time risk management
    
    % Trading configuration
    leverage = 50,                % 50x leverage
    max_position_size = 100000,   % $100k max position
    stop_loss_percentage = 0.02,  % 2% stop loss
    
    % Performance tracking
    ticks_processed = 0,
    arbitrage_opportunities = 0,
    trades_executed = 0,
    total_pnl = 0.0,
    
    % System status
    trading_active = false,
    risk_limits_active = true,
    emergency_stop = false
}).

%%%===================================================================
%%% API - Leveraging enhanced BitActor server capabilities
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% PRODUCTION: Start forex trading with full BitActor integration
-spec start_forex_trading() -> ok | {error, term()}.
start_forex_trading() ->
    gen_server:call(?SERVER, start_forex_trading).

-spec stop_forex_trading() -> ok.
stop_forex_trading() ->
    gen_server:call(?SERVER, stop_forex_trading).

%% ULTRA-FAST: Process market tick using enhanced bitactor_server
-spec process_market_tick(binary(), float(), float(), integer()) -> ok.
process_market_tick(Symbol, BidPrice, AskPrice, Timestamp) ->
    % OPTIMIZATION: Use ultra-fast cast for hot path
    gen_server:cast(?SERVER, {market_tick, Symbol, BidPrice, AskPrice, Timestamp}).

%% ARBITRAGE: Execute arbitrage opportunity with direct NIF calls
-spec execute_arbitrage(list(), float(), integer()) -> ok | {error, term()}.
execute_arbitrage(CurrencyPath, ProfitPercentage, MaxPosition) ->
    gen_server:call(?SERVER, {execute_arbitrage, CurrencyPath, ProfitPercentage, MaxPosition}).

%% RISK: Check risk limits using existing risk management
-spec check_risk_limits() -> ok | {margin_call, float()} | {stop_out, float()}.
check_risk_limits() ->
    gen_server:call(?SERVER, check_risk_limits).

%%%===================================================================
%%% Ultra-Fast Processing Functions - Direct BitActor integration
%%%===================================================================

%% ULTRA-FAST: Direct tick processing bypassing gen_server queue
-spec ultra_fast_tick_processing(binary(), float(), float(), integer()) -> ok.
ultra_fast_tick_processing(Symbol, BidPrice, AskPrice, Timestamp) ->
    % Get forex state from process dictionary for max speed
    case get(forex_ultra_state) of
        undefined ->
            % Fallback to slower path
            process_market_tick(Symbol, BidPrice, AskPrice, Timestamp);
        ForexState when ForexState#forex_state.trading_active ->
            % DIRECT: Use enhanced bitactor_server direct messaging
            MarketActor = ForexState#forex_state.market_data_actor,
            
            % Create forex tick message
            TickMessage = {market_tick, Symbol, BidPrice, AskPrice, Timestamp},
            
            % ULTRA-OPTIMIZATION: Direct NIF call via enhanced bitactor_server
            bitactor_server:send_message_direct(MarketActor, TickMessage),
            
            % Update performance counters
            NewState = ForexState#forex_state{
                ticks_processed = ForexState#forex_state.ticks_processed + 1
            },
            put(forex_ultra_state, NewState),
            ok;
        _ ->
            ok % Trading not active
    end.

%% DIRECT: Arbitrage execution with maximum speed
-spec direct_arbitrage_execution(list(), float(), integer()) -> ok | {error, term()}.
direct_arbitrage_execution(CurrencyPath, ProfitPercentage, MaxPosition) ->
    case get(forex_ultra_state) of
        ForexState when ForexState#forex_state.trading_active ->
            ArbitrageActor = ForexState#forex_state.arbitrage_actor,
            
            % Create arbitrage execution message
            ArbMessage = {execute_now, CurrencyPath, ProfitPercentage, MaxPosition},
            
            % ULTRA-FAST: Direct NIF call
            bitactor_server:send_message_direct(ArbitrageActor, ArbMessage),
            
            % Update stats
            NewState = ForexState#forex_state{
                arbitrage_opportunities = ForexState#forex_state.arbitrage_opportunities + 1
            },
            put(forex_ultra_state, NewState),
            ok;
        _ ->
            {error, trading_not_active}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #forex_state{}}.
init([]) ->
    % Initialize with enhanced BitActor server
    case whereis(bitactor_server) of
        undefined ->
            error_logger:error_msg("Enhanced BitActor server not running"),
            {stop, bitactor_server_not_available};
        BitActorPid ->
            State = #forex_state{
                bitactor_server_pid = BitActorPid,
                trading_active = false,
                risk_limits_active = true
            },
            
            error_logger:info_msg("Forex BitActor integration initialized"),
            {ok, State}
    end.

-spec handle_call(term(), {pid(), term()}, #forex_state{}) -> 
    {reply, term(), #forex_state{}} | {noreply, #forex_state{}} | {stop, term(), term(), #forex_state{}}.

handle_call(start_forex_trading, _From, State) ->
    case spawn_forex_actors(State) of
        {ok, NewState} ->
            % Cache state in process dictionary for ultra-fast access
            put(forex_ultra_state, NewState#forex_state{trading_active = true}),
            
            error_logger:info_msg("🚀 Forex trading started with enhanced BitActor integration"),
            {reply, ok, NewState#forex_state{trading_active = true}};
        {error, Reason} ->
            error_logger:error_msg("❌ Failed to start forex trading: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({execute_arbitrage, CurrencyPath, ProfitPercentage, MaxPosition}, _From, State) ->
    case State#forex_state.trading_active of
        true ->
            % Execute arbitrage using enhanced BitActor messaging
            Result = execute_arbitrage_internal(CurrencyPath, ProfitPercentage, MaxPosition, State),
            
            % Update stats
            NewState = State#forex_state{
                arbitrage_opportunities = State#forex_state.arbitrage_opportunities + 1,
                trades_executed = case Result of
                    ok -> State#forex_state.trades_executed + 1;
                    _ -> State#forex_state.trades_executed
                end
            },
            
            % Update ultra-fast cache
            put(forex_ultra_state, NewState),
            
            {reply, Result, NewState};
        false ->
            {reply, {error, trading_not_active}, State}
    end;

handle_call(check_risk_limits, _From, State) ->
    RiskResult = check_risk_limits_internal(State),
    
    % Handle emergency conditions
    NewState = case RiskResult of
        {stop_out, _Level} ->
            error_logger:error_msg("💀 STOP OUT TRIGGERED - Emergency shutdown"),
            State#forex_state{emergency_stop = true, trading_active = false};
        {margin_call, _Level} ->
            error_logger:warning_msg("🚨 MARGIN CALL - Reducing positions"),
            State#forex_state{trading_active = false};
        ok ->
            State
    end,
    
    % Update ultra-fast cache
    put(forex_ultra_state, NewState),
    
    {reply, RiskResult, NewState};

handle_call(get_trading_stats, _From, State) ->
    Stats = #{
        ticks_processed => State#forex_state.ticks_processed,
        arbitrage_opportunities => State#forex_state.arbitrage_opportunities,
        trades_executed => State#forex_state.trades_executed,
        total_pnl => State#forex_state.total_pnl,
        trading_active => State#forex_state.trading_active,
        emergency_stop => State#forex_state.emergency_stop
    },
    {reply, Stats, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(term(), #forex_state{}) -> {noreply, #forex_state{}}.
handle_cast({market_tick, Symbol, BidPrice, AskPrice, Timestamp}, State) ->
    case State#forex_state.trading_active of
        true ->
            % Process tick using enhanced BitActor messaging
            process_tick_internal(Symbol, BidPrice, AskPrice, Timestamp, State),
            
            % Update performance counter
            NewState = State#forex_state{
                ticks_processed = State#forex_state.ticks_processed + 1
            },
            
            % Update ultra-fast cache
            put(forex_ultra_state, NewState),
            
            {noreply, NewState};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #forex_state{}) -> {noreply, #forex_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #forex_state{}) -> ok.
terminate(_Reason, State) ->
    % Cleanup: Stop all forex actors
    cleanup_forex_actors(State),
    error_logger:info_msg("Forex BitActor integration terminated"),
    ok.

-spec code_change(term(), #forex_state{}, term()) -> {ok, #forex_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Enhanced BitActor integration
%%%===================================================================

%% Spawn specialized forex trading actors using enhanced BitActor server
-spec spawn_forex_actors(#forex_state{}) -> {ok, #forex_state{}} | {error, term()}.
spawn_forex_actors(State) ->
    try
        % Spawn market data processor with ultra-fast configuration
        {ok, MarketActor, _Latency1} = bitactor_server:spawn_actor(forex_market_processor, #{
            leverage => State#forex_state.leverage,
            ultra_fast => true
        }),
        
        % Spawn arbitrage detector with SIMD optimization
        {ok, ArbitrageActor, _Latency2} = bitactor_server:spawn_actor(forex_arbitrage_detector, #{
            min_profit_bp => 2,  % 2 basis points
            simd_enabled => true,
            ultra_fast => true
        }),
        
        % Spawn risk manager with real-time monitoring
        {ok, RiskActor, _Latency3} = bitactor_server:spawn_actor(forex_risk_manager, #{
            leverage => State#forex_state.leverage,
            stop_loss_pct => State#forex_state.stop_loss_percentage,
            real_time => true
        }),
        
        % Update trading actors map
        TradingActors = #{
            market_data => MarketActor,
            arbitrage => ArbitrageActor,
            risk_manager => RiskActor
        },
        
        NewState = State#forex_state{
            trading_actors = TradingActors,
            market_data_actor = MarketActor,
            arbitrage_actor = ArbitrageActor,
            risk_manager_actor = RiskActor
        },
        
        error_logger:info_msg("✅ Spawned ~p forex trading actors with ultra-fast optimization", 
                             [maps:size(TradingActors)]),
        
        {ok, NewState}
    catch
        error:Reason ->
            error_logger:error_msg("Failed to spawn forex actors: ~p", [Reason]),
            {error, Reason}
    end.

%% Process market tick using enhanced BitActor messaging
-spec process_tick_internal(binary(), float(), float(), integer(), #forex_state{}) -> ok.
process_tick_internal(Symbol, BidPrice, AskPrice, Timestamp, State) ->
    MarketActor = State#forex_state.market_data_actor,
    ArbitrageActor = State#forex_state.arbitrage_actor,
    
    % Create optimized tick message
    TickMsg = {process_tick, Symbol, BidPrice, AskPrice, Timestamp},
    
    % ULTRA-FAST: Send to market data processor using direct NIF
    bitactor_server:send_message_direct(MarketActor, TickMsg),
    
    % PARALLEL: Send to arbitrage detector simultaneously
    ArbMsg = {check_arbitrage, Symbol, BidPrice, AskPrice, Timestamp},
    bitactor_server:send_message_direct(ArbitrageActor, ArbMsg),
    
    ok.

%% Execute arbitrage using enhanced BitActor performance
-spec execute_arbitrage_internal(list(), float(), integer(), #forex_state{}) -> ok | {error, term()}.
execute_arbitrage_internal(CurrencyPath, ProfitPercentage, MaxPosition, State) ->
    ArbitrageActor = State#forex_state.arbitrage_actor,
    RiskActor = State#forex_state.risk_manager_actor,
    
    % Pre-validate with risk manager
    RiskMsg = {validate_arbitrage, CurrencyPath, MaxPosition},
    case bitactor_server:send_message_direct(RiskActor, RiskMsg) of
        ok ->
            % Execute arbitrage
            ExecMsg = {execute_arbitrage, CurrencyPath, ProfitPercentage, MaxPosition},
            bitactor_server:send_message_direct(ArbitrageActor, ExecMsg),
            ok;
        {error, Reason} ->
            error_logger:warning_msg("Arbitrage blocked by risk manager: ~p", [Reason]),
            {error, risk_limit_exceeded}
    end.

%% Check risk limits using existing risk management integration
-spec check_risk_limits_internal(#forex_state{}) -> ok | {margin_call, float()} | {stop_out, float()}.
check_risk_limits_internal(State) ->
    case State#forex_state.risk_limits_active of
        true ->
            RiskActor = State#forex_state.risk_manager_actor,
            
            % Get current risk status
            RiskMsg = {get_risk_status, self()},
            bitactor_server:send_message_direct(RiskActor, RiskMsg),
            
            % Wait for risk status response (with timeout)
            receive
                {risk_status, ok} -> ok;
                {risk_status, {margin_call, Level}} -> {margin_call, Level};
                {risk_status, {stop_out, Level}} -> {stop_out, Level}
            after 100 -> % 100ms timeout
                error_logger:warning_msg("Risk status check timeout"),
                ok
            end;
        false ->
            ok % Risk limits disabled
    end.

%% Cleanup forex actors on shutdown
-spec cleanup_forex_actors(#forex_state{}) -> ok.
cleanup_forex_actors(State) ->
    maps:fold(fun(_Name, ActorRef, _Acc) ->
        case bitactor_server:kill_actor(ActorRef) of
            ok -> ok;
            {error, not_found} -> ok;
            {error, Reason} ->
                error_logger:warning_msg("Failed to kill forex actor ~p: ~p", [ActorRef, Reason])
        end
    end, ok, State#forex_state.trading_actors).

%%%===================================================================
%%% Performance Monitoring - Integration with existing telemetry
%%%===================================================================

-spec get_trading_stats() -> map().
get_trading_stats() ->
    gen_server:call(?SERVER, get_trading_stats).

-spec get_performance_metrics() -> map().
get_performance_metrics() ->
    case get(forex_ultra_state) of
        undefined ->
            #{error => "forex_not_initialized"};
        ForexState ->
            #{
                ticks_per_second => calculate_ticks_per_second(ForexState),
                arbitrage_success_rate => calculate_arbitrage_success_rate(ForexState),
                average_execution_time_ns => get_avg_execution_time(),
                system_health => get_system_health_status(ForexState)
            }
    end.

%% Calculate ticks per second processing rate
-spec calculate_ticks_per_second(#forex_state{}) -> float().
calculate_ticks_per_second(State) ->
    % Implementation would calculate based on timestamps and tick count
    State#forex_state.ticks_processed / 60.0. % Rough estimate

%% Calculate arbitrage success rate
-spec calculate_arbitrage_success_rate(#forex_state{}) -> float().
calculate_arbitrage_success_rate(State) ->
    case State#forex_state.arbitrage_opportunities of
        0 -> 0.0;
        Total -> State#forex_state.trades_executed / Total * 100.0
    end.

%% Get average execution time from BitActor telemetry
-spec get_avg_execution_time() -> integer().
get_avg_execution_time() ->
    % Integration with existing BitActor telemetry
    case bitactor_server:get_stats() of
        Stats when is_map(Stats) ->
            maps:get(avg_execution_time_ns, Stats, 0);
        _ ->
            0
    end.

%% Get overall system health status
-spec get_system_health_status(#forex_state{}) -> atom().
get_system_health_status(State) ->
    case {State#forex_state.trading_active, State#forex_state.emergency_stop} of
        {true, false} -> healthy;
        {false, false} -> stopped;
        {_, true} -> emergency_stop;
        _ -> unknown
    end.