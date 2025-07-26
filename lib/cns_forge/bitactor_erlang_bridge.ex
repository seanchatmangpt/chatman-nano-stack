defmodule CnsForge.BitActorErlangBridge do
  @moduledoc """
  🔗 SWARM 80/20: Bridge BitActor C modules to Erlang/Elixir
  
  Provides seamless integration between:
  - High-performance BitActor C NIFs
  - Fault-tolerant Erlang/OTP behaviors
  - Elixir Ash Resources
  """
  
  require Logger
  
  @doc """
  Initialize BitActor NIFs and start Erlang processes
  """
  def start_link(actors) when is_list(actors) do
    Logger.info("🔗 Starting BitActor-Erlang Bridge for #{length(actors)} actors")
    
    # Start supervisor for all actors
    children = Enum.map(actors, fn actor ->
      %{
        id: String.to_atom("#{actor.name}_supervisor"),
        start: {__MODULE__, :start_actor_supervisor, [actor]},
        type: :supervisor
      }
    end)
    
    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end
  
  @doc """
  Start supervisor for a single BitActor
  """
  def start_actor_supervisor(actor) do
    children = [
      # BitActor NIF wrapper
      %{
        id: String.to_atom("#{actor.name}_nif"),
        start: {__MODULE__, :start_nif_server, [actor]},
        type: :worker
      },
      # GenServer for actor logic
      %{
        id: String.to_atom("#{actor.name}_server"),
        start: {__MODULE__, :start_genserver, [actor]},
        type: :worker
      }
    ]
    
    Supervisor.start_link(children, 
      strategy: :one_for_all, 
      name: String.to_atom("#{actor.name}_supervisor")
    )
  end
  
  @doc """
  Start NIF server for BitActor C module
  """
  def start_nif_server(actor) do
    GenServer.start_link(__MODULE__.NIFServer, actor, 
      name: String.to_atom("#{actor.name}_nif")
    )
  end
  
  @doc """
  Start GenServer for actor business logic
  """
  def start_genserver(actor) do
    GenServer.start_link(__MODULE__.ActorServer, actor,
      name: String.to_atom("#{actor.name}_server")
    )
  end
  
  @doc """
  Call BitActor through Erlang bridge
  """
  def call_actor(actor_name, message) do
    server_name = String.to_atom("#{actor_name}_server")
    GenServer.call(server_name, {:process, message})
  end
  
  @doc """
  Cast async message to BitActor
  """
  def cast_actor(actor_name, message) do
    server_name = String.to_atom("#{actor_name}_server")
    GenServer.cast(server_name, {:process_async, message})
  end
  
  # NIF Server Module
  defmodule NIFServer do
    use GenServer
    
    def init(actor) do
      # Load BitActor NIF
      :ok = load_bitactor_nif(actor.name)
      {:ok, %{actor: actor}}
    end
    
    def handle_call({:execute, func, args}, _from, state) do
      # Call BitActor C function via NIF
      result = apply(:bitactor_nif, func, args)
      {:reply, result, state}
    end
    
    defp load_bitactor_nif(name) do
      # In production, would load actual NIF
      # For demo, simulate loading
      Logger.info("⚡ Loading BitActor NIF: #{name}")
      :ok
    end
  end
  
  # Actor GenServer Module  
  defmodule ActorServer do
    use GenServer
    
    def init(actor) do
      Logger.info("🟣 Starting Erlang GenServer for #{actor.name}")
      
      {:ok, %{
        actor: actor,
        stats: %{processed: 0, errors: 0},
        nif_ref: String.to_atom("#{actor.name}_nif")
      }}
    end
    
    def handle_call({:process, message}, _from, state) do
      # Process through BitActor NIF
      start_time = System.monotonic_time(:microsecond)
      
      result = case call_bitactor_nif(state.nif_ref, message) do
        {:ok, response} ->
          elapsed = System.monotonic_time(:microsecond) - start_time
          Logger.debug("⚡ BitActor processed in #{elapsed}μs")
          
          # Update stats
          new_stats = Map.update!(state.stats, :processed, &(&1 + 1))
          {:ok, response, %{state | stats: new_stats}}
          
        {:error, reason} ->
          Logger.error("❌ BitActor error: #{inspect(reason)}")
          new_stats = Map.update!(state.stats, :errors, &(&1 + 1))
          {:error, reason, %{state | stats: new_stats}}
      end
      
      case result do
        {:ok, response, new_state} -> {:reply, {:ok, response}, new_state}
        {:error, reason, new_state} -> {:reply, {:error, reason}, new_state}
      end
    end
    
    def handle_cast({:process_async, message}, state) do
      # Async processing
      Task.start(fn ->
        call_bitactor_nif(state.nif_ref, message)
      end)
      
      {:noreply, state}
    end
    
    def handle_info({:telemetry, metrics}, state) do
      # Report metrics
      Logger.info("📊 Actor #{state.actor.name} stats: #{inspect(state.stats)}")
      {:noreply, state}
    end
    
    defp call_bitactor_nif(nif_ref, message) do
      # Simulate BitActor NIF call
      # In production, would call actual NIF
      case GenServer.call(nif_ref, {:execute, :process, [message]}) do
        {:ok, result} -> {:ok, result}
        error -> {:error, error}
      end
    catch
      :exit, reason -> {:error, {:nif_crash, reason}}
    end
  end
  
  @doc """
  Create Ash Resource connector for BitActor
  """
  def create_ash_connector(actor_name, resource_module) do
    """
    defmodule #{resource_module}.BitActorConnector do
      @moduledoc \"\"\"
      Connects Ash Resource to BitActor via Erlang bridge
      \"\"\"
      
      alias CnsForge.BitActorErlangBridge
      
      def process(params) do
        BitActorErlangBridge.call_actor("#{actor_name}", params)
      end
      
      def process_async(params) do
        BitActorErlangBridge.cast_actor("#{actor_name}", params)
      end
    end
    """
  end
end