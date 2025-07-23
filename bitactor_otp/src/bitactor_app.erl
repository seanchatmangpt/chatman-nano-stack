%%%-------------------------------------------------------------------
%%% @doc BitActor OTP Application
%%% @copyright 2025 CNS - Chatman Nano Stack
%%% @author James I. Chatman <james@chatman.ai>
%%% @author Sean A. Chatman <sean@chatman.ai>
%%% Built for reliability. Designed to last.
%%%-------------------------------------------------------------------
-module(bitactor_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([get_env/1, get_env/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting BitActor application with ultra-intelligence patterns"),
    
    %% Initialize telemetry
    ok = init_telemetry(),
    
    %% Start the top supervisor
    case bitactor_sup:start_link() of
        {ok, Pid} ->
            error_logger:info_msg("BitActor application started successfully"),
            {ok, Pid};
        Error ->
            error_logger:error_msg("Failed to start BitActor application: ~p", [Error]),
            Error
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    error_logger:info_msg("Stopping BitActor application"),
    ok.

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_env(atom()) -> term().
get_env(Key) ->
    application:get_env(bitactor, Key, undefined).

-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    application:get_env(bitactor, Key, Default).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init_telemetry() -> ok.
init_telemetry() ->
    case get_env(enable_telemetry, true) of
        true ->
            %% Initialize telemetry collectors
            ok = telemetry:attach_many(
                bitactor_metrics,
                [
                    [bitactor, actor, spawn],
                    [bitactor, actor, tick],
                    [bitactor, actor, message],
                    [bitactor, system, memory]
                ],
                fun bitactor_telemetry:handle_event/4,
                #{}
            ),
            error_logger:info_msg("BitActor telemetry initialized");
        false ->
            error_logger:info_msg("BitActor telemetry disabled")
    end,
    ok.