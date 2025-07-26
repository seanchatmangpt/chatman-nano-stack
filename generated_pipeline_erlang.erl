%%% Generated Erlang GenServer
%%% 🔗 80/20: Minimal actor implementation

-module(ontology_actor).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call({create, Attrs}, _From, State) ->
    %% Delegate to Ash Resource
    Result = create_resource(Attrs),
    {reply, Result, State};
    
handle_call({read, Id}, _From, State) ->
    Result = read_resource(Id),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Helper functions
create_resource(Attrs) ->
    %% Call Ash Resource create action
    {ok, Attrs}.
    
read_resource(Id) ->
    %% Call Ash Resource read action
    {ok, [{id, Id}]}.
