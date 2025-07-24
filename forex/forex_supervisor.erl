%%%-------------------------------------------------------------------
%%% @doc Forex Position Supervisor - Leveraging Existing OTP Infrastructure
%%% Reuses ALL existing BitActor supervision patterns for forex trading
%%% @copyright 2025 CNS - Forex Trading Division
%%%-------------------------------------------------------------------
-module(forex_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_position/3, stop_position/1, get_positions/0]).

%% supervisor callbacks
-export([init/1]).

%% LEVERAGE: Existing BitActor server patterns
-export([start_forex_engine/0, stop_forex_engine/0]).

-define(SERVER, ?MODULE).

%% REUSE: Existing state record patterns but for forex
-record(forex_state, {
    positions = #{},        % Position ID -> forex_position_worker pid
    account_balance = 10000.0,
    leverage = 50,          % 50x leverage
    margin_used = 0.0,
    margin_call = false,
    stop_out = false,
    risk_limits = #{},
    telemetry_ref          % Reuse existing telemetry
}).

%%%===================================================================
%%% API - LEVERAGE existing BitActor API patterns
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% INTEGRATE: With existing BitActor spawning but for forex positions
-spec start_position(atom(), term(), term()) -> {ok, pid()} | {error, term()}.
start_position(CurrencyPair, PositionSize, EntryPrice) ->
    %% Leverage existing BitActor spawn patterns
    ChildSpec = #{
        id => {forex_position, make_ref()},
        start => {forex_position_worker, start_link, [CurrencyPair, PositionSize, EntryPrice]},
        restart => temporary,    % Don't restart closed positions
        shutdown => 1000,
        type => worker,
        modules => [forex_position_worker]
    },
    
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok, Pid} ->
            %% REUSE: Existing telemetry patterns
            catch telemetry:execute([forex, position, start], 
                                  #{count => 1}, 
                                  #{pair => CurrencyPair, size => PositionSize}),
            {ok, Pid};
        Error ->
            Error
    end.

-spec stop_position(pid()) -> ok | {error, term()}.
stop_position(PositionPid) ->
    %% Use existing supervisor termination patterns
    supervisor:terminate_child(?SERVER, PositionPid).

-spec get_positions() -> [pid()].
get_positions() ->
    %% REUSE: Existing child enumeration
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)].

%%%===================================================================
%%% supervisor callbacks - REUSE existing patterns
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% LEVERAGE: Existing BitActor supervision strategy
    SupFlags = #{
        strategy => one_for_one,      % Don't kill other positions if one fails
        intensity => 10,              % Allow 10 restarts
        period => 60                  % In 60 seconds
    },
    
    %% INTEGRATE: Main forex engine using existing BitActor server pattern
    ForexEngine = #{
        id => forex_engine,
        start => {forex_engine_server, start_link, []},
        restart => permanent,         % Always restart the main engine
        shutdown => 5000,
        type => worker,
        modules => [forex_engine_server]
    },
    
    %% REUSE: Risk manager using existing patterns
    RiskManager = #{
        id => forex_risk_manager,
        start => {forex_risk_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [forex_risk_manager]
    },
    
    %% LEVERAGE: Telemetry using existing infrastructure
    TelemetryWorker = #{
        id => forex_telemetry,
        start => {forex_telemetry_worker, start_link, []},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [forex_telemetry_worker]
    },
    
    ChildSpecs = [ForexEngine, RiskManager, TelemetryWorker],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Forex Engine Server - LEVERAGE existing BitActor server patterns
%%%===================================================================

start_forex_engine() ->
    %% Start the forex supervisor tree
    case start_link() of
        {ok, Pid} ->
            error_logger:info_msg("🚀 Forex engine started with 50x leverage"),
            {ok, Pid};
        Error ->
            error_logger:error_msg("❌ Failed to start forex engine: ~p", [Error]),
            Error
    end.

stop_forex_engine() ->
    %% REUSE: Existing graceful shutdown patterns
    Positions = get_positions(),
    error_logger:info_msg("📈 Closing ~p active positions", [length(Positions)]),
    
    %% Close all positions gracefully
    lists:foreach(fun(Pid) ->
        catch forex_position_worker:close_position(Pid)
    end, Positions),
    
    %% Stop supervisor
    case whereis(?SERVER) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    
    error_logger:info_msg("✅ Forex engine stopped safely").

%%%===================================================================
%%% Forex Position Worker - NEW but using existing patterns
%%%===================================================================

%% This would be implemented in forex_position_worker.erl
%% Leveraging ALL existing BitActor worker patterns:

%% -module(forex_position_worker).
%% -behaviour(gen_server).
%% 
%% %% REUSE: Exact same API as bitactor_worker
%% -export([start_link/3, close_position/1, update_price/2, get_pnl/1]).
%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% 
%% -record(position_state, {
%%     currency_pair,
%%     position_size,
%%     entry_price,
%%     current_price,
%%     stop_loss,
%%     take_profit,
%%     unrealized_pnl = 0.0,
%%     margin_required,
%%     open_time,
%%     nif_handle          % REUSE: Existing C NIF integration
%% }).

%%%===================================================================
%%% Integration Examples - Show how existing components plug in
%%%===================================================================

%% EXAMPLE 1: Integrate with existing BitActor telemetry
handle_position_update(PositionPid, NewPrice) ->
    %% REUSE: Existing telemetry patterns
    case forex_position_worker:update_price(PositionPid, NewPrice) of
        {ok, PnL} ->
            catch telemetry:execute([forex, position, update], 
                                  #{pnl => PnL}, 
                                  #{position => PositionPid}),
            ok;
        {stop_loss_hit, PnL} ->
            catch telemetry:execute([forex, position, stop_loss], 
                                  #{pnl => PnL}, 
                                  #{position => PositionPid}),
            stop_position(PositionPid);
        Error ->
            Error
    end.

%% EXAMPLE 2: Leverage existing risk management
check_margin_requirements() ->
    Positions = get_positions(),
    TotalMargin = lists:foldl(fun(Pid, Acc) ->
        case forex_position_worker:get_margin(Pid) of
            {ok, Margin} -> Acc + Margin;
            _ -> Acc
        end
    end, 0.0, Positions),
    
    %% REUSE: Existing risk validation patterns
    case forex_risk_manager:validate_margin(TotalMargin) of
        {margin_call, Level} ->
            error_logger:warning_msg("🚨 MARGIN CALL at ~.1f%", [Level]),
            %% Trigger margin call procedures
            handle_margin_call(Positions);
        {stop_out, Level} ->
            error_logger:error_msg("💀 STOP OUT at ~.1f% - CLOSING ALL", [Level]),
            %% Emergency closure using existing patterns
            emergency_close_all_positions(Positions);
        ok ->
            ok
    end.

%% EXAMPLE 3: Integration with existing news pipeline
handle_economic_event(Event) ->
    %% LEVERAGE: Existing news validation but for forex
    case Event of
        {nfp, _Impact, TimeMs} ->
            %% Non-farm payrolls - high impact on USD pairs
            USDPositions = get_positions_by_currency(usd),
            reduce_position_sizes(USDPositions, 0.5),  % 50% reduction
            error_logger:info_msg("📰 NFP event - reduced USD exposure");
        {ecb_rate, Impact, _TimeMs} when Impact > 2 ->
            %% ECB rate decision - high impact on EUR pairs
            EURPositions = get_positions_by_currency(eur),
            reduce_position_sizes(EURPositions, 0.3),  % 30% reduction
            error_logger:info_msg("🏦 ECB rate decision - reduced EUR exposure");
        _ ->
            ok  % Other events
    end.

%%%===================================================================
%%% Helper Functions - All leverage existing infrastructure
%%%===================================================================

get_positions_by_currency(Currency) ->
    %% Filter positions by currency using existing patterns
    AllPositions = get_positions(),
    lists:filter(fun(Pid) ->
        case forex_position_worker:get_currency_pair(Pid) of
            {ok, Pair} -> currency_matches(Pair, Currency);
            _ -> false
        end
    end, AllPositions).

currency_matches(Pair, Currency) ->
    %% Simple currency matching - can be enhanced
    case {Pair, Currency} of
        {eur_usd, eur} -> true;
        {eur_usd, usd} -> true;
        {gbp_usd, gbp} -> true;
        {gbp_usd, usd} -> true;
        _ -> false
    end.

reduce_position_sizes(Positions, Factor) ->
    %% Reduce all position sizes by factor before news events
    lists:foreach(fun(Pid) ->
        forex_position_worker:reduce_size(Pid, Factor)
    end, Positions).

emergency_close_all_positions(Positions) ->
    %% CRITICAL: Emergency closure for stop-out situations
    error_logger:error_msg("🚨 EMERGENCY: Closing ~p positions due to stop-out", 
                          [length(Positions)]),
    
    %% Use existing supervisor patterns for graceful shutdown
    lists:foreach(fun(Pid) ->
        spawn(fun() ->  % Don't block on individual closures
            catch forex_position_worker:emergency_close(Pid)
        end)
    end, Positions).

handle_margin_call(Positions) ->
    %% Margin call procedure - close largest losing positions first
    PositionsWithPnL = lists:map(fun(Pid) ->
        {ok, PnL} = forex_position_worker:get_pnl(Pid),
        {Pid, PnL}
    end, Positions),
    
    %% Sort by P&L (most negative first)
    SortedPositions = lists:sort(fun({_,PnL1}, {_,PnL2}) -> PnL1 < PnL2 end, 
                                PositionsWithPnL),
    
    %% Close positions until margin level improves
    close_positions_until_safe(SortedPositions).