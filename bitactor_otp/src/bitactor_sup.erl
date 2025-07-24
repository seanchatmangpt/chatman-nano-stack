%%%-------------------------------------------------------------------
%%% @doc BitActor Top-Level Supervisor
%%% Implements fault-tolerant supervision tree with multiple strategies
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => bitactor_app:get_env(supervision_intensity, 10),
        period => bitactor_app:get_env(supervision_period, 60)
    },
    
    %% Child specifications with hyper-intelligent fault tolerance
    ChildSpecs = [
        %% Main BitActor Server - Critical component
        #{
            id => bitactor_server,
            start => {bitactor_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [bitactor_server]
        },
        
        %% Actor Pool Supervisor - Manages dynamic actors
        #{
            id => bitactor_pool_sup,
            start => {bitactor_pool_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [bitactor_pool_sup]
        },
        
        %% Telemetry Handler - Non-critical but valuable
        #{
            id => bitactor_telemetry,
            start => {bitactor_telemetry, start_link, []},
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => [bitactor_telemetry]
        },
        
        %% Health Monitor - System health tracking
        #{
            id => bitactor_health,
            start => {bitactor_health, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [bitactor_health]
        }
    ],
    
    error_logger:info_msg("BitActor supervisor initialized with ~p children", [length(ChildSpecs)]),
    {ok, {SupFlags, ChildSpecs}}.