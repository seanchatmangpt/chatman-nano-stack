defmodule CNSForge.Application do
  @moduledoc """
  CNS Forge Application - Initializes the BitActor Mesh ecosystem
  
  Sets up the complete Ash/Reactor infrastructure for deterministic,
  observable, and fault-tolerant execution of ecosystem composition.
  """
  
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Registry for signal routing between BitActors
      {Registry, keys: :duplicate, name: CNSForge.SignalRegistry},
      
      # Mnesia setup for transactional state management
      {CNSForge.MnesiaSetup, []},
      
      # Telemetry supervisor for metrics collection
      {CNSForge.TelemetrySupervisor, []},
      
      # BitActor pool supervisor for managing actor lifecycles
      {CNSForge.BitActorSupervisor, []},
      
      # Phoenix endpoint for HTTP ingress (stimulus BitActors)
      {CNSForgeWeb.Endpoint, []}
    ]

    opts = [strategy: :one_for_one, name: CNSForge.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    CNSForgeWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

defmodule CNSForge.MnesiaSetup do
  @moduledoc """
  Sets up Mnesia for transactional state management as specified in the architecture
  """
  
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    # Initialize Mnesia for Ash.DataLayer.Mnesia
    case :mnesia.system_info(:db_nodes) do
      [_node] ->
        # Single node setup - create schema
        :mnesia.stop()
        :mnesia.delete_schema([node()])
        :mnesia.create_schema([node()])
        :mnesia.start()
        
        # Create tables for Ash resources
        create_ash_tables()
        
      _ ->
        # Multi-node setup - tables should already exist
        :mnesia.start()
        :mnesia.wait_for_tables([:bit_actor, :signal, :telemetry_frame], 5000)
    end
    
    {:ok, :ok}
  end

  defp create_ash_tables do
    # BitActor table
    :mnesia.create_table(:bit_actor, [
      attributes: [:id, :type, :transaction_id, :ttl, :token, :status, 
                   :created_at, :completed_at, :result, :error],
      type: :set,
      disc_copies: [node()]
    ])
    
    # Signal table
    :mnesia.create_table(:signal, [
      attributes: [:id, :type, :source_actor_id, :target_actor_type, 
                   :transaction_id, :payload, :ttl, :priority, 
                   :created_at, :routed_at, :consumed_at],
      type: :set,
      disc_copies: [node()]
    ])
    
    # TelemetryFrame table
    :mnesia.create_table(:telemetry_frame, [
      attributes: [:id, :transaction_id, :bit_actor_id, :hop_sequence,
                   :operation, :input_token, :output_token, :ttl_before,
                   :ttl_after, :execution_time_us, :status, :error_details,
                   :timestamp, :blake3_hash],
      type: :set,
      disc_copies: [node()],
      index: [:transaction_id, :hop_sequence]
    ])
  end
end

defmodule CNSForge.TelemetrySupervisor do
  @moduledoc """
  Supervises telemetry handlers for universal observability
  """
  
  use Supervisor

  def start_link(_) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {CNSForge.TelemetryHandler, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule CNSForge.TelemetryHandler do
  @moduledoc """
  Handles telemetry events for pulse logs and system monitoring
  """
  
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    # Attach telemetry handlers for CNS Forge events
    :telemetry.attach_many(
      "cns-forge-telemetry",
      [
        [:cns_forge, :bit_actor, :hop],
        [:cns_forge, :bit_actor, :ttl_expired],
        [:cns_forge, :signal, :routed],
        [:cns_forge, :signal, :dropped],
        [:cns_forge, :transaction, :completed],
        [:cns_forge, :bit_actor, :compensate],
        [:cns_forge, :bit_actor, :undo]
      ],
      &__MODULE__.handle_event/4,
      :ok
    )
    
    {:ok, %{}}
  end

  def handle_event([:cns_forge, :bit_actor, :hop], measurements, metadata, _config) do
    # Log pulse for every BitActor hop
    IO.puts("""
    [PULSE] #{metadata.transaction_id} | #{metadata.step_name} | TTL: #{measurements.ttl_remaining} | #{measurements.execution_time_us}μs
    """)
  end

  def handle_event([:cns_forge, :bit_actor, :ttl_expired], _measurements, metadata, _config) do
    IO.puts("""
    [TTL_EXPIRED] #{metadata.transaction_id} | #{metadata.step_name} - BitActor terminated due to TTL exhaustion
    """)
  end

  def handle_event([:cns_forge, :signal, :routed], measurements, metadata, _config) do
    IO.puts("""
    [SIGNAL] #{metadata.transaction_id} | #{metadata.signal_type} -> #{metadata.target_type} | TTL: #{measurements.ttl}
    """)
  end

  def handle_event([:cns_forge, :transaction, :completed], measurements, metadata, _config) do
    IO.puts("""
    [TRANSACTION_COMPLETE] #{metadata.transaction_id} | Hops: #{measurements.total_hops} | Status: #{metadata.status}
    """)
  end

  def handle_event(_event, _measurements, _metadata, _config) do
    :ok
  end
end

defmodule CNSForge.BitActorSupervisor do
  @moduledoc """
  Supervises BitActor processes for the execution mesh
  """
  
  use DynamicSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def spawn_bit_actor(type, transaction_id, token, ttl) do
    child_spec = {CNSForge.BitActorWorker, {type, transaction_id, token, ttl}}
    DynamicSupervisor.start_child(__MODULE__, child_spec)
  end
end

defmodule CNSForge.BitActorWorker do
  @moduledoc """
  GenServer worker for managing individual BitActor lifecycles
  """
  
  use GenServer

  def start_link({type, transaction_id, token, ttl}) do
    GenServer.start_link(__MODULE__, {type, transaction_id, token, ttl})
  end

  @impl true
  def init({type, transaction_id, token, ttl}) do
    # Register for signal consumption
    Registry.register(CNSForge.SignalRegistry, {:signal_consumer, type}, :ok)
    
    # Create BitActor resource
    bit_actor = CNSForge.BitActor.create!(%{
      type: type,
      transaction_id: transaction_id,
      token: token,
      ttl: ttl
    })
    
    {:ok, %{bit_actor: bit_actor}}
  end

  @impl true
  def handle_info({:signal, signal}, state) do
    # Process incoming signal
    case CNSForge.BitActor.execute_hop!(state.bit_actor, %{
      input_token: signal.payload,
      operation: :process_signal
    }) do
      updated_actor ->
        # Mark signal as consumed
        CNSForge.Signal.mark_consumed!(signal)
        {:noreply, %{state | bit_actor: updated_actor}}
        
    rescue
      error ->
        CNSForge.BitActor.fail!(state.bit_actor, %{
          error_message: "Signal processing failed: #{inspect(error)}"
        })
        {:stop, :normal, state}
    end
  end
end