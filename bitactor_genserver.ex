defmodule BitActor.GenServer do
  @moduledoc """
  BitActor GenServer Implementation
  OTP-compliant actor with TTL constraints and signal processing
  Generated from BitActor DSL
  """

  use GenServer
  require Logger

  # =============================================================================
  # State Structure
  # =============================================================================

  defstruct [
    :id,
    :name,
    :status,
    :ttl_budget_ms,
    :ttl_constraint,
    :parent_pid,
    :child_pids,
    :signal_handlers,
    :telemetry_config,
    :swarm_config,
    :violation_handler,
    # Performance tracking
    :last_signal_id,
    :processing_time_ns,
    :signals_processed,
    :signals_failed,
    # Timestamps
    :created_at,
    :updated_at,
    :last_active_at,
    # TTL enforcement
    :ttl_enforcer_ref
  ]

  # =============================================================================
  # Client API
  # =============================================================================

  @doc """
  Starts a BitActor GenServer
  """
  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Process a signal with TTL enforcement
  """
  def process_signal(actor, signal) do
    GenServer.call(actor, {:process_signal, signal}, signal_timeout(signal))
  catch
    :exit, {:timeout, _} ->
      {:error, :ttl_exceeded}
  end

  @doc """
  Get actor status and metrics
  """
  def get_status(actor) do
    GenServer.call(actor, :get_status)
  end

  @doc """
  Emit telemetry frame
  """
  def emit_telemetry(actor, metric_name, value, unit \\ :count) do
    GenServer.cast(actor, {:emit_telemetry, metric_name, value, unit})
  end

  @doc """
  Update actor status
  """
  def update_status(actor, status) do
    GenServer.call(actor, {:update_status, status})
  end

  @doc """
  Add child actor
  """
  def add_child(actor, child_pid) do
    GenServer.call(actor, {:add_child, child_pid})
  end

  # =============================================================================
  # GenServer Callbacks
  # =============================================================================

  @impl true
  def init(opts) do
    # Initialize state from options
    state = %__MODULE__{
      id: Keyword.get(opts, :id, generate_id()),
      name: Keyword.fetch!(opts, :name),
      status: Keyword.get(opts, :status, :inactive),
      ttl_budget_ms: Keyword.get(opts, :ttl_budget_ms, 8),
      parent_pid: Keyword.get(opts, :parent_pid),
      child_pids: [],
      signal_handlers: Keyword.get(opts, :signal_handlers, %{}),
      telemetry_config: Keyword.get(opts, :telemetry_config, []),
      swarm_config: Keyword.get(opts, :swarm_config, default_swarm_config()),
      violation_handler: Keyword.get(opts, :violation_handler, &default_violation_handler/1),
      signals_processed: 0,
      signals_failed: 0,
      created_at: DateTime.utc_now(),
      last_active_at: DateTime.utc_now()
    }

    # Set up TTL constraint
    state = %{state | ttl_constraint: build_ttl_constraint(state.ttl_budget_ms)}

    # Start TTL enforcer
    state = start_ttl_enforcer(state)

    # Start telemetry emitters
    schedule_telemetry_emission(state)

    # Notify parent if exists
    if state.parent_pid do
      send(state.parent_pid, {:child_started, self(), state.id})
    end

    Logger.info("BitActor #{state.name} (#{state.id}) started with TTL budget #{state.ttl_budget_ms}ms")

    {:ok, state}
  end

  @impl true
  def handle_call({:process_signal, signal}, _from, state) do
    start_time = System.monotonic_time(:nanosecond)
    
    # Update status
    state = %{state | status: :processing, last_active_at: DateTime.utc_now()}
    
    # Find appropriate handler
    handler = Map.get(state.signal_handlers, signal.type, &default_signal_handler/2)
    
    # Check TTL budget
    ttl_budget_ns = get_signal_ttl_budget(signal, state)
    
    # Set up TTL timer
    timer_ref = Process.send_after(self(), {:ttl_exceeded, signal.id}, div(ttl_budget_ns, 1_000_000))
    
    # Process signal
    result = try do
      handler.(signal, build_context(state))
    catch
      kind, reason ->
        Logger.error("Signal processing error: #{inspect(kind)}: #{inspect(reason)}")
        {:error, {kind, reason}}
    after
      Process.cancel_timer(timer_ref)
    end
    
    # Calculate processing time
    end_time = System.monotonic_time(:nanosecond)
    processing_time_ns = end_time - start_time
    
    # Update state based on result
    state = case result do
      {:ok, _} ->
        %{state | 
          signals_processed: state.signals_processed + 1,
          processing_time_ns: processing_time_ns,
          last_signal_id: signal.id,
          status: :active
        }
        
      {:error, _} ->
        %{state | 
          signals_failed: state.signals_failed + 1,
          processing_time_ns: processing_time_ns,
          last_signal_id: signal.id,
          status: :error
        }
    end
    
    # Check for TTL violation
    if processing_time_ns > ttl_budget_ns do
      handle_ttl_violation(state, signal, processing_time_ns, ttl_budget_ns)
    end
    
    # Emit processing telemetry
    emit_processing_telemetry(state, signal, processing_time_ns)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call(:get_status, _from, state) do
    status = %{
      id: state.id,
      name: state.name,
      status: state.status,
      ttl_budget_ms: state.ttl_budget_ms,
      signals_processed: state.signals_processed,
      signals_failed: state.signals_failed,
      last_processing_time_ns: state.processing_time_ns,
      created_at: state.created_at,
      last_active_at: state.last_active_at,
      child_count: length(state.child_pids)
    }
    
    {:reply, {:ok, status}, state}
  end

  @impl true
  def handle_call({:update_status, new_status}, _from, state) do
    state = %{state | status: new_status, updated_at: DateTime.utc_now()}
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:add_child, child_pid}, _from, state) do
    Process.monitor(child_pid)
    state = %{state | child_pids: [child_pid | state.child_pids]}
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:emit_telemetry, metric_name, value, unit}, state) do
    telemetry_frame = %{
      id: generate_id(),
      bitactor_id: state.id,
      metric_name: metric_name,
      value: value,
      unit: unit,
      timestamp: DateTime.utc_now()
    }
    
    # Send to telemetry collector
    send_telemetry(telemetry_frame)
    
    {:noreply, state}
  end

  @impl true
  def handle_info({:emit_scheduled_telemetry}, state) do
    # Emit configured telemetry metrics
    Enum.each(state.telemetry_config, fn config ->
      value = calculate_metric_value(config.name, state)
      emit_telemetry(self(), config.name, value, config.unit)
    end)
    
    # Reschedule
    schedule_telemetry_emission(state)
    
    {:noreply, state}
  end

  @impl true
  def handle_info({:ttl_exceeded, signal_id}, state) do
    Logger.error("TTL exceeded for signal #{signal_id} in actor #{state.name}")
    
    # Force terminate signal processing
    state = %{state | status: :error, signals_failed: state.signals_failed + 1}
    
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    # Handle child process termination
    state = %{state | child_pids: List.delete(state.child_pids, pid)}
    
    Logger.warning("Child actor #{inspect(pid)} terminated: #{inspect(reason)}")
    
    {:noreply, state}
  end

  # =============================================================================
  # Private Functions
  # =============================================================================

  defp generate_id do
    UUID.uuid4()
  end

  defp signal_timeout(signal) do
    case signal[:ttl_constraint] do
      nil -> 5000  # Default 5 seconds
      constraint -> div(constraint.budget_ns, 1_000_000)
    end
  end

  defp build_ttl_constraint(budget_ms) do
    %{
      budget_ns: budget_ms * 1_000_000,
      precision: :nanosecond,
      max_budget_ms: budget_ms
    }
  end

  defp default_swarm_config do
    %{
      topology: :hierarchical,
      max_actors: 100,
      strategy: :balanced,
      auto_scale: true
    }
  end

  defp start_ttl_enforcer(state) do
    # Could implement more sophisticated TTL enforcement here
    state
  end

  defp schedule_telemetry_emission(state) do
    # Find minimum emission interval
    intervals = Enum.map(state.telemetry_config, & &1.interval_ms)
    min_interval = if Enum.empty?(intervals), do: 1000, else: Enum.min(intervals)
    
    Process.send_after(self(), {:emit_scheduled_telemetry}, min_interval)
  end

  defp build_context(state) do
    %{
      actor_id: state.id,
      actor_name: state.name,
      ttl_budget_ms: state.ttl_budget_ms,
      parent_pid: state.parent_pid,
      swarm_config: state.swarm_config
    }
  end

  defp get_signal_ttl_budget(signal, state) do
    case signal[:ttl_constraint] do
      nil -> state.ttl_constraint.budget_ns
      constraint -> min(constraint.budget_ns, state.ttl_constraint.budget_ns)
    end
  end

  defp default_signal_handler(signal, _context) do
    # Basic signal processing
    {:ok, %{processed: true, signal_id: signal.id}}
  end

  defp default_violation_handler(violation) do
    Logger.error("TTL violation: #{inspect(violation)}")
  end

  defp handle_ttl_violation(state, signal, actual_ns, expected_ns) do
    violation = %{
      actor_id: state.id,
      signal_id: signal.id,
      expected_ttl_ns: expected_ns,
      actual_time_ns: actual_ns,
      violation_amount_ns: actual_ns - expected_ns,
      timestamp: DateTime.utc_now()
    }
    
    state.violation_handler.(violation)
    
    # Emit violation telemetry
    emit_telemetry(self(), "ttl_violations", 1, :count)
  end

  defp emit_processing_telemetry(state, signal, processing_time_ns) do
    emit_telemetry(self(), "processing_time", processing_time_ns, :ns)
    emit_telemetry(self(), "signals_by_type_#{signal.type}", 1, :count)
    
    # Calculate TTL utilization
    ttl_budget_ns = get_signal_ttl_budget(signal, state)
    utilization = (processing_time_ns / ttl_budget_ns) * 100
    emit_telemetry(self(), "ttl_utilization", utilization, :percent)
  end

  defp calculate_metric_value(metric_name, state) do
    case metric_name do
      "signals_processed" -> state.signals_processed
      "signals_failed" -> state.signals_failed
      "processing_time" -> state.processing_time_ns || 0
      "ttl_utilization" ->
        if state.processing_time_ns && state.ttl_constraint.budget_ns > 0 do
          (state.processing_time_ns / state.ttl_constraint.budget_ns) * 100
        else
          0
        end
      _ -> 0
    end
  end

  defp send_telemetry(telemetry_frame) do
    # Send to telemetry collection system
    # In production, this would send to a telemetry service
    Logger.debug("Telemetry: #{inspect(telemetry_frame)}")
  end
end

# =============================================================================
# Supervisor Module
# =============================================================================

defmodule BitActor.Supervisor do
  @moduledoc """
  OTP Supervisor for BitActor processes
  """
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      # Dynamic supervisor for BitActors
      {DynamicSupervisor, name: BitActor.DynamicSupervisor, strategy: :one_for_one},
      
      # Registry for named BitActors
      {Registry, keys: :unique, name: BitActor.Registry},
      
      # Telemetry collector
      {BitActor.TelemetryCollector, []}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @doc """
  Start a new BitActor under supervision
  """
  def start_actor(opts) do
    spec = %{
      id: opts[:id] || UUID.uuid4(),
      start: {BitActor.GenServer, :start_link, [opts]},
      restart: :temporary
    }
    
    DynamicSupervisor.start_child(BitActor.DynamicSupervisor, spec)
  end
end

# =============================================================================
# Telemetry Collector
# =============================================================================

defmodule BitActor.TelemetryCollector do
  @moduledoc """
  Collects and aggregates telemetry from BitActors
  """
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %{
      metrics: %{},
      buffer: [],
      flush_interval: 1000
    }
    
    schedule_flush()
    {:ok, state}
  end

  defp schedule_flush do
    Process.send_after(self(), :flush, 1000)
  end
end