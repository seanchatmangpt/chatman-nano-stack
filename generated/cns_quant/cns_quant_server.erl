-module(cns_quant_server).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([emit_signal/1, get_stats/0]).

-record(state, {
    bitactor_ref :: reference(),
    tick_count = 0 :: non_neg_integer(),
    signal_count = 0 :: non_neg_integer()
}).

%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

emit_signal(Signal) ->
    gen_server:call(?MODULE, {emit_signal, Signal}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%% gen_server callbacks

init([]) ->
    case bitactor_nif:create_actor(cns_quant) of
        {ok, Ref} ->
            self() ! tick,
            {ok, #state{bitactor_ref = Ref}};
        Error ->
            {stop, Error}
    end.

handle_call({emit_signal, Signal}, _From, State = #state{bitactor_ref = Ref}) ->
    Result = bitactor_nif:emit_signal(Ref, Signal),
    {reply, Result, State#state{signal_count = State#state.signal_count + 1}};

handle_call(get_stats, _From, State) ->
    Stats = #{
        tick_count => State#state.tick_count,
        signal_count => State#state.signal_count
    },
    {reply, Stats, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{bitactor_ref = Ref}) ->
    case bitactor_nif:tick(Ref) of
        ok ->
            erlang:send_after(1, self(), tick),
            {noreply, State#state{tick_count = State#state.tick_count + 1}};
        Error ->
            {stop, Error, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{bitactor_ref = Ref}) ->
    bitactor_nif:destroy_actor(Ref),
    ok.
