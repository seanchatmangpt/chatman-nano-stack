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
    error_logger:info_msg("Starting BitActor application with UHFT optimizations"),
    
    %% Apply memory management optimizations
    apply_uhft_optimizations(),
    
    %% Initialize telemetry only if enabled
    case get_env(enable_telemetry, false) of
        true -> 
            ok = init_telemetry();
        false -> 
            error_logger:info_msg("Telemetry disabled for UHFT performance"),
            ok
    end,
    
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
    case get_env(enable_telemetry, false) of
        true ->
            %% Initialize telemetry collectors (requires telemetry dependency)
            case catch telemetry:attach_many(
                bitactor_metrics,
                [
                    [bitactor, actor, spawn],
                    [bitactor, actor, tick],
                    [bitactor, actor, message],
                    [bitactor, system, memory]
                ],
                fun bitactor_telemetry:handle_event/4,
                #{}
            ) of
                ok -> 
                    error_logger:info_msg("BitActor telemetry initialized");
                Error ->
                    error_logger:warning_msg("Failed to initialize telemetry: ~p", [Error])
            end;
        false ->
            error_logger:info_msg("BitActor telemetry disabled")
    end,
    ok.

-spec apply_uhft_optimizations() -> ok.
apply_uhft_optimizations() ->
    %% Apply process flag optimizations
    ProcessFlags = get_env(process_flags, []),
    
    %% Set default process flags for all new processes
    case proplists:get_value(priority, ProcessFlags) of
        high -> 
            try erlang:system_flag(priority, high) catch _:_ -> ok end;
        _ -> ok
    end,
    
    %% Configure garbage collection
    case proplists:get_value(fullsweep_after, ProcessFlags) of
        N when is_integer(N) -> 
            try erlang:system_flag(fullsweep_after, N) catch _:_ -> ok end;
        _ -> ok
    end,
    
    %% Pre-allocate memory if possible
    case get_env(memory_pool_size, 0) of
        Size when Size > 0 ->
            %% Force memory allocation
            _ = binary:copy(<<0>>, Size div 1000), % Allocate 1/1000th to avoid OOM
            erlang:garbage_collect();
        _ -> ok
    end,
    
    %% Optimize scheduler settings
    try
        erlang:system_flag(scheduler_bind_type, thread_spread),
        erlang:system_flag(dirty_cpu_schedulers_online, erlang:system_info(schedulers))
    catch
        _:_ -> ok
    end,
    
    error_logger:info_msg("UHFT optimizations applied"),
    ok.