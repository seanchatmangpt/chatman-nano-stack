%%%-------------------------------------------------------------------
%%% @doc BitActor Pool Supervisor
%%% Dynamic supervisor for actor processes with intelligent scaling
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_actor/2, terminate_actor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_actor(atom(), term()) -> {ok, pid()} | {error, term()}.
start_actor(Type, Args) ->
    ChildSpec = #{
        id => make_ref(),
        start => {bitactor_worker, start_link, [Type, Args]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [bitactor_worker]
    },
    supervisor:start_child(?SERVER, ChildSpec).

-spec terminate_actor(pid()) -> ok | {error, not_found}.
terminate_actor(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 100,
        period => 60
    },
    
    %% Template for actor workers
    ChildSpec = #{
        id => bitactor_worker,
        start => {bitactor_worker, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [bitactor_worker]
    },
    
    {ok, {SupFlags, [ChildSpec]}}.