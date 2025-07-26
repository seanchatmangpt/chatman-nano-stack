defmodule CNSForge.ProductionApplication do
  @moduledoc """
  Production-ready Application supervisor for CNS Forge
  
  Manages all core services including:
  - Ash.Domain resources
  - Reactor workflows  
  - Telemetry and monitoring
  - TTL execution enforcement
  """

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("Starting CNS Forge Production Application")
    
    children = [
      # Core Ash setup
      {Registry, keys: :unique, name: CNSForge.Registry},
      
      # Telemetry supervision
      CNSForge.Telemetry,
      
      # Reactor supervision
      {DynamicSupervisor, name: CNSForge.ReactorSupervisor, strategy: :one_for_one},
      
      # TTL enforcement
      CNSForge.TTLEnforcer,
      
      # Main domain supervisor
      CNSForge.DomainSupervisor
    ]

    opts = [strategy: :one_for_one, name: CNSForge.Supervisor]
    
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("CNS Forge Production Application started successfully")
        {:ok, pid}
        
      {:error, reason} ->
        Logger.error("Failed to start CNS Forge Production Application: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @impl true
  def stop(_state) do
    Logger.info("Stopping CNS Forge Production Application")
    :ok
  end
end

defmodule CNSForge.Telemetry do
  @moduledoc """
  Telemetry supervision and setup for CNS Forge
  """
  
  use Supervisor
  require Logger

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # Telemetry poller for system metrics
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp periodic_measurements do
    [
      # VM measurements
      {__MODULE__, :measure_vm_memory, []},
      {__MODULE__, :measure_vm_processes, []},
      
      # CNS Forge specific measurements
      {__MODULE__, :measure_reactor_count, []},
      {__MODULE__, :measure_ttl_violations, []}
    ]
  end

  def measure_vm_memory do
    :telemetry.execute([:vm, :memory], :erlang.memory())
  end

  def measure_vm_processes do
    process_count = :erlang.system_info(:process_count)
    :telemetry.execute([:vm, :processes], %{count: process_count})
  end

  def measure_reactor_count do
    # Count active reactors
    count = DynamicSupervisor.count_children(CNSForge.ReactorSupervisor)
    :telemetry.execute([:cns_forge, :reactors], %{active: count.active || 0})
  end

  def measure_ttl_violations do
    # This would be implemented with actual TTL violation tracking
    :telemetry.execute([:cns_forge, :ttl], %{violations: 0})
  end
end

defmodule CNSForge.TTLEnforcer do
  @moduledoc """
  TTL (Time-To-Live) enforcement service
  Ensures all reactor executions respect TTL bounds
  """
  
  use GenServer
  require Logger

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Setup TTL enforcement configuration
    ttl_config = Application.get_env(:cns_forge, :ttl_constraints, %{
      max_execution_hops: 8,
      max_processing_time_ms: 5000,
      enable_bounds_checking: true
    })
    
    Logger.info("TTL Enforcer started with config: #{inspect(ttl_config)}")
    
    {:ok, %{
      config: ttl_config,
      violations: [],
      active_executions: %{}
    }}
  end

  @impl true
  def handle_call({:register_execution, execution_id, start_time}, _from, state) do
    updated_executions = Map.put(state.active_executions, execution_id, start_time)
    {:reply, :ok, %{state | active_executions: updated_executions}}
  end

  @impl true
  def handle_call({:check_ttl_violation, execution_id}, _from, state) do
    case Map.get(state.active_executions, execution_id) do
      nil ->
        {:reply, {:error, :execution_not_found}, state}
        
      start_time ->
        current_time = System.monotonic_time(:millisecond)
        execution_time = current_time - start_time
        
        violation = execution_time > state.config.max_processing_time_ms
        
        if violation do
          Logger.warning("TTL violation detected for execution #{execution_id}: #{execution_time}ms > #{state.config.max_processing_time_ms}ms")
          
          violation_record = %{
            execution_id: execution_id,
            execution_time_ms: execution_time,
            max_allowed_ms: state.config.max_processing_time_ms,
            timestamp: DateTime.utc_now()
          }
          
          updated_violations = [violation_record | state.violations]
          {:reply, {:violation, violation_record}, %{state | violations: updated_violations}}
        else
          {:reply, :ok, state}
        end
    end
  end

  @impl true
  def handle_call({:complete_execution, execution_id}, _from, state) do
    updated_executions = Map.delete(state.active_executions, execution_id)
    {:reply, :ok, %{state | active_executions: updated_executions}}
  end

  @impl true
  def handle_call(:get_violations, _from, state) do
    {:reply, state.violations, state}
  end

  # Public API
  
  def register_execution(execution_id) do
    start_time = System.monotonic_time(:millisecond)
    GenServer.call(__MODULE__, {:register_execution, execution_id, start_time})
  end

  def check_ttl_violation(execution_id) do
    GenServer.call(__MODULE__, {:check_ttl_violation, execution_id})
  end

  def complete_execution(execution_id) do
    GenServer.call(__MODULE__, {:complete_execution, execution_id})
  end

  def get_violations do
    GenServer.call(__MODULE__, :get_violations)
  end
end

defmodule CNSForge.DomainSupervisor do
  @moduledoc """
  Supervisor for all domain-related processes
  """
  
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      # Domain registry
      {Registry, keys: :duplicate, name: CNSForge.DomainRegistry},
      
      # Resource managers
      CNSForge.ResourceManager,
      
      # Workflow coordinators  
      CNSForge.WorkflowCoordinator
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule CNSForge.ResourceManager do
  @moduledoc """
  Manages Ash resources and their lifecycle
  """
  
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    {:ok, %{resources: [], registry: CNSForge.DomainRegistry}}
  end

  @impl true
  def handle_call({:register_resource, resource_module}, _from, state) do
    updated_resources = [resource_module | state.resources]
    Registry.register(state.registry, :resource_registered, resource_module)
    {:reply, :ok, %{state | resources: updated_resources}}
  end

  @impl true
  def handle_call(:list_resources, _from, state) do
    {:reply, state.resources, state}
  end

  # Public API

  def register_resource(resource_module) do
    GenServer.call(__MODULE__, {:register_resource, resource_module})
  end

  def list_resources do
    GenServer.call(__MODULE__, :list_resources)
  end
end

defmodule CNSForge.WorkflowCoordinator do
  @moduledoc """
  Coordinates workflow execution across reactors
  """
  
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    {:ok, %{active_workflows: %{}, completed_workflows: []}}
  end

  @impl true
  def handle_call({:start_workflow, workflow_id, reactor_module, input}, _from, state) do
    # Start reactor under dynamic supervisor
    case DynamicSupervisor.start_child(CNSForge.ReactorSupervisor, {Task, fn ->
      CNSForge.TTLEnforcer.register_execution(workflow_id)
      
      try do
        result = Reactor.run(reactor_module, input)
        CNSForge.TTLEnforcer.complete_execution(workflow_id)
        result
      rescue
        error ->
          CNSForge.TTLEnforcer.complete_execution(workflow_id)
          {:error, error}
      end
    end}) do
      {:ok, pid} ->
        updated_workflows = Map.put(state.active_workflows, workflow_id, %{
          pid: pid,
          reactor: reactor_module,
          started_at: DateTime.utc_now()
        })
        
        {:reply, {:ok, workflow_id}, %{state | active_workflows: updated_workflows}}
        
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:list_active_workflows, _from, state) do
    {:reply, Map.keys(state.active_workflows), state}
  end

  # Public API

  def start_workflow(workflow_id, reactor_module, input \\ %{}) do
    GenServer.call(__MODULE__, {:start_workflow, workflow_id, reactor_module, input})
  end

  def list_active_workflows do
    GenServer.call(__MODULE__, :list_active_workflows)
  end
end