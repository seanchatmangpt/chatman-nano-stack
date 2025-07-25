%%%-------------------------------------------------------------------
%%% @doc BitActor NIF Interface Module  
%%% Erlang interface to C BitActor implementation
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_nif).

%% NIF exports
-export([create_actor/2, destroy_actor/1, 
         send_message/2, tick_all/0]).
-export([measure_latency/0, get_stats/0]).

%% Fallback exports (when NIF not loaded)
-export([init/0]).

-on_load(init/0).

%%%===================================================================
%%% NIF Interface
%%%===================================================================

-spec create_actor(atom(), term()) -> {ok, reference()} | {error, term()}.
create_actor(_Type, _InitData) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec destroy_actor(reference()) -> ok | {error, term()}.
destroy_actor(_ActorHandle) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec send_message(reference(), term()) -> {ok, integer()} | {error, term()}.
send_message(_ActorHandle, _Message) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec tick_all() -> {ok, integer()} | {error, term()}.
tick_all() ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec measure_latency() -> {ok, integer(), integer(), integer()} | {error, term()}.
measure_latency() ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec get_stats() -> {ok, integer(), integer(), integer(), float()} | {error, term()}.
get_stats() ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%%%===================================================================
%%% NIF Loading
%%%===================================================================

-spec init() -> ok | {error, term()}.
init() ->
    PrivDir = case code:priv_dir(bitactor) of
        {error, bad_name} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    
    NIFPath = filename:join(PrivDir, "bitactor_nif"),
    
    case erlang:load_nif(NIFPath, 0) of
        ok ->
            error_logger:info_msg("BitActor NIF loaded successfully from ~s", [NIFPath]),
            ok;
        {error, {reload, _}} ->
            %% NIF already loaded
            ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to load BitActor NIF from ~s: ~p", [NIFPath, Reason]),
            error_logger:info_msg("BitActor will run in fallback mode without C acceleration"),
            {error, Reason}
    end.