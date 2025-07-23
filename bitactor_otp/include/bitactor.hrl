%%%-------------------------------------------------------------------
%%% @doc BitActor Header Definitions
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------

%% Actor Types
-define(ACTOR_TYPE_MARKET_DATA, market_data).
-define(ACTOR_TYPE_ORDER_BOOK, order_book).
-define(ACTOR_TYPE_RISK_ENGINE, risk_engine).
-define(ACTOR_TYPE_EXECUTION, execution).
-define(ACTOR_TYPE_POSITION, position).

%% Message Types
-define(MSG_TICK, tick).
-define(MSG_ORDER, order).
-define(MSG_CANCEL, cancel).
-define(MSG_MODIFY, modify).
-define(MSG_FILL, fill).
-define(MSG_POSITION_UPDATE, position_update).

%% System Constants
-define(MAX_ACTORS, 10000).
-define(DEFAULT_TICK_INTERVAL, 1000).
-define(DEFAULT_SUPERVISION_INTENSITY, 10).
-define(DEFAULT_SUPERVISION_PERIOD, 60).

%% Error Codes
-define(ERROR_ACTOR_NOT_FOUND, actor_not_found).
-define(ERROR_INVALID_MESSAGE, invalid_message).
-define(ERROR_NIF_ERROR, nif_error).
-define(ERROR_SYSTEM_OVERLOAD, system_overload).

%% Telemetry Events
-define(TELEMETRY_ACTOR_SPAWN, [bitactor, actor, spawn]).
-define(TELEMETRY_ACTOR_KILL, [bitactor, actor, kill]).
-define(TELEMETRY_MESSAGE_SENT, [bitactor, actor, message]).
-define(TELEMETRY_SYSTEM_TICK, [bitactor, system, tick]).
-define(TELEMETRY_SYSTEM_MEMORY, [bitactor, system, memory]).

%% Health Status
-define(HEALTH_HEALTHY, healthy).
-define(HEALTH_DEGRADED, degraded).
-define(HEALTH_CRITICAL, critical).

%% Records
-record(market_tick, {
    symbol :: binary(),
    price :: float(),
    volume :: integer(),
    timestamp :: integer()
}).

-record(order, {
    id :: binary(),
    symbol :: binary(),
    side :: buy | sell,
    price :: float(),
    quantity :: integer(),
    type :: market | limit | stop,
    timestamp :: integer()
}).

-record(position, {
    symbol :: binary(),
    quantity :: integer(),
    avg_price :: float(),
    unrealized_pnl :: float(),
    realized_pnl :: float()
}).

%% Type definitions
-type actor_type() :: market_data | order_book | risk_engine | execution | position.
-type actor_handle() :: reference().
-type message_type() :: tick | order | cancel | modify | fill | position_update.
-type health_status() :: healthy | degraded | critical.