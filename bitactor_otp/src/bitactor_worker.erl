%%%-------------------------------------------------------------------
%%% @doc BitActor Worker Process
%%% Individual actor implementation with C integration
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_worker).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1, send_message/2, get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    type :: atom(),
    c_handle :: term(),
    actor_state :: term(),
    message_count = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Type, Args) ->
    gen_server:start_link(?MODULE, [Type, Args], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec send_message(pid(), term()) -> ok.
send_message(Pid, Message) ->
    gen_server:cast(Pid, {message, Message}).

-spec get_state(pid()) -> {ok, term()} | {error, term()}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([atom() | term()]) -> {ok, #state{}} | {stop, term()}.
init([Type, Args]) ->
    process_flag(trap_exit, true),
    
    %% Initialize C handle if NIF is loaded
    CHandle = case catch bitactor_nif:create_actor(Type, Args) of
        {ok, Handle} -> Handle;
        _ -> undefined
    end,
    
    State = #state{
        type = Type,
        c_handle = CHandle,
        actor_state = Args,
        message_count = 0
    },
    
    error_logger:debug("BitActor worker ~p started with handle ~p", [Type, CHandle]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> 
    {reply, term(), #state{}} | {noreply, #state{}} | {stop, term(), term(), #state{}}.
handle_call(get_state, _From, State) ->
    Response = #{
        type => State#state.type,
        has_c_handle => State#state.c_handle =/= undefined,
        message_count => State#state.message_count,
        actor_state => State#state.actor_state
    },
    {reply, {ok, Response}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({message, Message}, State) ->
    NewState = process_message(Message, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:debug("BitActor worker received EXIT: ~p", [Reason]),
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    error_logger:debug("BitActor worker ~p terminating: ~p", [State#state.type, Reason]),
    
    %% Clean up C handle
    case State#state.c_handle of
        undefined -> ok;
        Handle ->
            try bitactor_nif:destroy_actor(Handle)
            catch _:_ -> ok
            end
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec process_message(term(), #state{}) -> #state{}.
process_message(Message, State) ->
    %% Process via C NIF if available
    case State#state.c_handle of
        undefined ->
            %% Fallback Erlang processing
            process_message_erlang(Message, State);
        Handle ->
            try
                case bitactor_nif:process_message(Handle, Message) of
                    {ok, NewCState} ->
                        State#state{
                            actor_state = NewCState,
                            message_count = State#state.message_count + 1
                        };
                    {error, _Reason} ->
                        %% Fallback to Erlang
                        process_message_erlang(Message, State)
                end
            catch
                _:_ ->
                    process_message_erlang(Message, State)
            end
    end.

-spec process_message_erlang(term(), #state{}) -> #state{}.
process_message_erlang(Message, State) ->
    %% Simple Erlang message processing based on actor type
    NewActorState = case State#state.type of
        market_data ->
            process_market_data(Message, State#state.actor_state);
        order_book ->
            process_order_book(Message, State#state.actor_state);
        risk_engine ->
            process_risk_engine(Message, State#state.actor_state);
        _ ->
            State#state.actor_state
    end,
    
    State#state{
        actor_state = NewActorState,
        message_count = State#state.message_count + 1
    }.

-spec process_market_data(term(), term()) -> term().
process_market_data({tick, Symbol, Price, Volume}, State) ->
    %% Update market data state
    maps:put(Symbol, {Price, Volume, erlang:system_time(microsecond)}, State);
process_market_data(_Message, State) ->
    State.

-spec process_order_book(term(), term()) -> term().
process_order_book({order, Side, Price, Quantity}, State) ->
    %% Simple order book processing
    OrderBook = maps:get(order_book, State, #{bids => [], asks => []}),
    NewOrderBook = case Side of
        buy -> 
            Bids = maps:get(bids, OrderBook, []),
            OrderBook#{bids => [{Price, Quantity} | Bids]};
        sell ->
            Asks = maps:get(asks, OrderBook, []),
            OrderBook#{asks => [{Price, Quantity} | Asks]}
    end,
    State#{order_book => NewOrderBook};
process_order_book(_Message, State) ->
    State.

-spec process_risk_engine(term(), term()) -> term().
process_risk_engine({position, Symbol, Quantity, Price}, State) ->
    %% Update position tracking
    Positions = maps:get(positions, State, #{}),
    CurrentPos = maps:get(Symbol, Positions, 0),
    NewPositions = Positions#{Symbol => CurrentPos + Quantity},
    State#{positions => NewPositions};
process_risk_engine(_Message, State) ->
    State.