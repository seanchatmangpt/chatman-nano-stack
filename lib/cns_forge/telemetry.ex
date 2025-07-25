defmodule CNSForge.Telemetry do
  @moduledoc """
  OpenTelemetry instrumentation for CNS Forge
  Provides comprehensive observability for BitActor mesh and Reactor workflows
  """
  
  use Supervisor
  import Telemetry.Metrics
  require Logger
  
  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end
  
  @impl true
  def init(_arg) do
    children = [
      # OpenTelemetry setup
      {CNSForge.Telemetry.OtelSetup, []},
      # Telemetry poller for periodic measurements
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000},
      # Custom reporter for CNS Forge metrics
      {CNSForge.Telemetry.Reporter, metrics: metrics()}
    ]
    
    Supervisor.init(children, strategy: :one_for_one)
  end
  
  def metrics do
    [
      # BitActor metrics
      counter("cns_forge.bit_actor.hop",
        description: "Total number of BitActor hops executed",
        tags: [:bit_actor_id, :operation]
      ),
      
      distribution("cns_forge.bit_actor.ttl_remaining",
        description: "TTL remaining when hop executed",
        tags: [:bit_actor_id],
        buckets: [0, 1, 2, 3, 4, 5, 6, 7, 8]
      ),
      
      counter("cns_forge.bit_actor.created",
        description: "Number of BitActors created",
        tags: [:type]
      ),
      
      counter("cns_forge.bit_actor.completed",
        description: "Number of BitActors completed",
        tags: [:status]
      ),
      
      # Reactor workflow metrics
      counter("cns_forge.reactor.workflow.started",
        description: "Number of workflows started",
        tags: [:workflow_type]
      ),
      
      counter("cns_forge.reactor.workflow.completed",
        description: "Number of workflows completed",
        tags: [:workflow_type, :status]
      ),
      
      distribution("cns_forge.reactor.workflow.duration",
        description: "Workflow execution duration in milliseconds",
        tags: [:workflow_type],
        unit: {:native, :millisecond}
      ),
      
      counter("cns_forge.reactor.step.executed",
        description: "Number of reactor steps executed",
        tags: [:step_name, :step_type]
      ),
      
      # Semantic compilation metrics
      counter("cns_forge.semantic.compilation.started",
        description: "Number of semantic compilations started",
        tags: [:language]
      ),
      
      counter("cns_forge.semantic.compilation.completed",
        description: "Number of semantic compilations completed",
        tags: [:language, :status]
      ),
      
      distribution("cns_forge.semantic.compilation.duration",
        description: "Semantic compilation duration",
        tags: [:language],
        unit: {:native, :millisecond}
      ),
      
      # Mesh coordination metrics
      counter("cns_forge.mesh.signals.processed",
        description: "Number of signals processed by mesh",
        tags: [:signal_type, :priority]
      ),
      
      gauge("cns_forge.mesh.active_bitactors",
        description: "Number of active BitActors in mesh"
      ),
      
      gauge("cns_forge.mesh.active_workflows",
        description: "Number of active workflows"
      ),
      
      # System metrics
      last_value("cns_forge.system.memory_usage",
        description: "Memory usage in bytes",
        unit: :byte
      ),
      
      last_value("cns_forge.system.cpu_usage",
        description: "CPU usage percentage"
      ),
      
      # Performance metrics
      summary("cns_forge.performance.throughput",
        description: "Operations per second",
        tags: [:operation_type]
      ),
      
      distribution("cns_forge.performance.latency",
        description: "Operation latency",
        tags: [:operation_type],
        unit: {:native, :microsecond}
      )
    ]
  end
  
  defp periodic_measurements do
    [
      # Emit system metrics periodically
      {CNSForge.Telemetry, :emit_system_metrics, []},
      # Emit mesh statistics
      {CNSForge.Telemetry, :emit_mesh_stats, []},
      # Emit performance metrics
      {CNSForge.Telemetry, :emit_performance_metrics, []}
    ]
  end
  
  # Telemetry event handlers
  
  @doc """
  Emit telemetry event for BitActor hop
  """
  def emit_bitactor_hop(bit_actor_id, operation, ttl_remaining, metadata \\ %{}) do
    :telemetry.execute(
      [:cns_forge, :bit_actor, :hop],
      %{ttl_remaining: ttl_remaining},
      Map.merge(metadata, %{
        bit_actor_id: bit_actor_id,
        operation: operation,
        timestamp: System.system_time(:microsecond)
      })
    )
  end
  
  @doc """
  Emit workflow lifecycle events
  """
  def emit_workflow_event(event_type, workflow_id, workflow_type, metadata \\ %{}) do
    :telemetry.execute(
      [:cns_forge, :reactor, :workflow, event_type],
      %{count: 1},
      Map.merge(metadata, %{
        workflow_id: workflow_id,
        workflow_type: workflow_type,
        timestamp: System.system_time(:microsecond)
      })
    )
  end
  
  @doc """
  Emit compilation events
  """
  def emit_compilation_event(event_type, language, duration \\ nil, metadata \\ %{}) do
    measurements = if duration do
      %{duration: duration, count: 1}
    else
      %{count: 1}
    end
    
    :telemetry.execute(
      [:cns_forge, :semantic, :compilation, event_type],
      measurements,
      Map.merge(metadata, %{
        language: language,
        timestamp: System.system_time(:microsecond)
      })
    )
  end
  
  @doc """
  Start span for tracing
  """
  def start_span(name, attributes \\ %{}) do
    OpenTelemetry.Tracer.start_span(name, %{attributes: attributes})
  end
  
  @doc """
  End span with status
  """
  def end_span(status \\ :ok) do
    case status do
      :ok ->
        OpenTelemetry.Tracer.set_status(:ok)
      {:error, reason} ->
        OpenTelemetry.Tracer.set_status(:error, inspect(reason))
    end
    OpenTelemetry.Tracer.end_span()
  end
  
  # System metrics emission
  def emit_system_metrics do
    memory_info = :erlang.memory()
    schedulers = :erlang.system_info(:schedulers_online)
    
    :telemetry.execute(
      [:cns_forge, :system],
      %{
        memory_usage: memory_info[:total],
        process_count: :erlang.system_info(:process_count),
        port_count: :erlang.system_info(:port_count),
        schedulers: schedulers
      },
      %{node: node()}
    )
  end
  
  # Mesh statistics emission
  def emit_mesh_stats do
    # Get mesh statistics from registry
    active_bitactors = Registry.count(CNSForge.BitActorRegistry)
    active_workflows = Registry.count(CNSForge.WorkflowRegistry)
    
    :telemetry.execute(
      [:cns_forge, :mesh],
      %{
        active_bitactors: active_bitactors,
        active_workflows: active_workflows
      },
      %{mesh_id: get_mesh_id()}
    )
  end
  
  # Performance metrics emission
  def emit_performance_metrics do
    # Calculate throughput and latency metrics
    # This would aggregate from recent operations
    :telemetry.execute(
      [:cns_forge, :performance],
      %{
        throughput: calculate_throughput(),
        avg_latency: calculate_avg_latency()
      },
      %{}
    )
  end
  
  defp get_mesh_id do
    # Get current mesh ID from configuration or registry
    Application.get_env(:cns_forge, :mesh_id, "default")
  end
  
  defp calculate_throughput do
    # Calculate operations per second
    # Simplified implementation
    :rand.uniform(1000)
  end
  
  defp calculate_avg_latency do
    # Calculate average latency in microseconds
    # Simplified implementation
    :rand.uniform(100)
  end
end

defmodule CNSForge.Telemetry.OtelSetup do
  @moduledoc """
  OpenTelemetry configuration and setup
  """
  
  use GenServer
  require Logger
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
  
  def init(_) do
    # Configure OpenTelemetry
    Application.put_env(:opentelemetry, :resource, [
      service: [
        name: "cns-forge",
        version: Application.spec(:cns_forge, :vsn) |> to_string()
      ],
      deployment: [
        environment: Application.get_env(:cns_forge, :environment, "development")
      ]
    ])
    
    # Set up OTLP exporter
    endpoint = Application.get_env(:cns_forge, :otel_endpoint, "http://localhost:4317")
    
    Application.put_env(:opentelemetry_exporter, :otlp_endpoint, endpoint)
    Application.put_env(:opentelemetry_exporter, :otlp_headers, [
      {"x-service", "cns-forge"}
    ])
    
    Logger.info("OpenTelemetry configured with endpoint: #{endpoint}")
    
    {:ok, %{}}
  end
end

defmodule CNSForge.Telemetry.Reporter do
  @moduledoc """
  Custom telemetry reporter for CNS Forge specific metrics
  """
  
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(opts) do
    metrics = Keyword.get(opts, :metrics, [])
    
    # Attach telemetry handlers for each metric
    Enum.each(metrics, &attach_handler/1)
    
    {:ok, %{metrics: metrics}}
  end
  
  defp attach_handler(metric) do
    handler_id = "#{__MODULE__}_#{metric.name}_#{:erlang.unique_integer()}"
    
    :telemetry.attach(
      handler_id,
      metric.event_name,
      &handle_event/4,
      metric
    )
  end
  
  defp handle_event(_event_name, measurements, metadata, metric) do
    # Format and send metric to OpenTelemetry
    attributes = build_attributes(metadata, metric.tags)
    
    case metric do
      %Telemetry.Metrics.Counter{} ->
        :otel_metrics.add(metric.name, measurements[:count] || 1, attributes)
        
      %Telemetry.Metrics.Distribution{} ->
        value = measurements[metric.measurement] || measurements[:value]
        :otel_metrics.record(metric.name, value, attributes)
        
      %Telemetry.Metrics.Gauge{} ->
        value = measurements[metric.measurement] || measurements[:value]
        :otel_metrics.record(metric.name, value, attributes)
        
      _ ->
        Logger.debug("Unhandled metric type: #{inspect(metric)}")
    end
  end
  
  defp build_attributes(metadata, tags) do
    tags
    |> Enum.map(fn tag -> {tag, Map.get(metadata, tag)} end)
    |> Enum.filter(fn {_, value} -> value != nil end)
    |> Enum.into(%{})
  end
end