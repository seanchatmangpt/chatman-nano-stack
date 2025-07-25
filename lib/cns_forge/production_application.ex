defmodule CNSForge.ProductionApplication do
  @moduledoc """
  Production-ready CNS Forge Application with full observability
  """
  
  use Application
  require Logger
  
  @impl true
  def start(_type, _args) do
    # Configure OpenTelemetry before starting
    configure_opentelemetry()
    
    children = [
      # Telemetry and monitoring
      CNSForge.Telemetry,
      
      # Registries for BitActors and Workflows
      {Registry, keys: :unique, name: CNSForge.BitActorRegistry},
      {Registry, keys: :unique, name: CNSForge.WorkflowRegistry},
      
      # Phoenix PubSub for distributed coordination
      {Phoenix.PubSub, name: CNSForge.PubSub},
      
      # Task Supervisor for parallel execution
      {Task.Supervisor, name: CNSForge.TaskSupervisor},
      
      # DynamicSupervisor for BitActor processes
      {DynamicSupervisor, name: CNSForge.BitActorSupervisor, strategy: :one_for_one},
      
      # Web endpoint (if configured)
      maybe_web_endpoint(),
      
      # Health check server
      {CNSForge.HealthCheck, port: health_check_port()},
      
      # Metacompiler cache
      {CNSForge.MetacompilerCache, []},
      
      # Workflow scheduler
      {CNSForge.WorkflowScheduler, []},
      
      # Resource monitor
      {CNSForge.ResourceMonitor, [
        check_interval: 5_000,
        thresholds: %{
          memory: 0.9,
          cpu: 0.8,
          processes: 10_000
        }
      ]}
    ]
    |> Enum.filter(&(&1))
    
    opts = [strategy: :one_for_one, name: CNSForge.Supervisor]
    
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("CNS Forge started successfully in #{env()} mode")
        {:ok, pid}
        
      error ->
        Logger.error("Failed to start CNS Forge: #{inspect(error)}")
        error
    end
  end
  
  @impl true
  def stop(_state) do
    Logger.info("CNS Forge shutting down...")
    :ok
  end
  
  defp configure_opentelemetry do
    # Set up OpenTelemetry resource attributes
    resource_attributes = [
      service: [
        name: "cns-forge",
        version: Application.spec(:cns_forge, :vsn) |> to_string(),
        namespace: env()
      ],
      deployment: [
        environment: env()
      ],
      host: [
        name: node() |> to_string()
      ]
    ]
    
    Application.put_env(:opentelemetry, :resource, resource_attributes)
    
    # Configure OTLP exporter
    if otlp_endpoint = System.get_env("OTEL_EXPORTER_OTLP_ENDPOINT") do
      Application.put_env(:opentelemetry_exporter, :otlp_endpoint, otlp_endpoint)
      
      # Add authentication headers if provided
      if otlp_headers = System.get_env("OTEL_EXPORTER_OTLP_HEADERS") do
        headers = parse_otlp_headers(otlp_headers)
        Application.put_env(:opentelemetry_exporter, :otlp_headers, headers)
      end
    end
    
    # Set sampling rate
    sampling_rate = System.get_env("OTEL_TRACES_SAMPLER_ARG", "1.0") |> String.to_float()
    Application.put_env(:opentelemetry, :traces_sampler, {:parent_based, {:trace_id_ratio_based, sampling_rate}})
    
    Logger.info("OpenTelemetry configured for #{env()} environment")
  end
  
  defp maybe_web_endpoint do
    if Application.get_env(:cns_forge, :start_web_endpoint, true) do
      CNSForgeWeb.Endpoint
    else
      nil
    end
  end
  
  defp health_check_port do
    System.get_env("HEALTH_CHECK_PORT", "4001") |> String.to_integer()
  end
  
  defp env do
    Application.get_env(:cns_forge, :environment, "development")
  end
  
  defp parse_otlp_headers(headers_string) do
    headers_string
    |> String.split(",")
    |> Enum.map(&String.split(&1, "=", parts: 2))
    |> Enum.map(fn [k, v] -> {String.trim(k), String.trim(v)} end)
  end
end

defmodule CNSForge.HealthCheck do
  @moduledoc """
  Health check endpoint for Kubernetes probes
  """
  
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  get "/health" do
    health_status = check_health()
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(health_status.code, Jason.encode!(health_status))
  end
  
  get "/ready" do
    ready_status = check_readiness()
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(ready_status.code, Jason.encode!(ready_status))
  end
  
  match _ do
    send_resp(conn, 404, "Not Found")
  end
  
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end
  
  def start_link(opts) do
    port = Keyword.get(opts, :port, 4001)
    
    Plug.Cowboy.http(__MODULE__, [], port: port)
  end
  
  defp check_health do
    checks = %{
      database: check_database_connection(),
      registry: check_registry_health(),
      memory: check_memory_usage()
    }
    
    all_healthy = Enum.all?(checks, fn {_, status} -> status == :ok end)
    
    %{
      code: if(all_healthy, do: 200, else: 503),
      status: if(all_healthy, do: "healthy", else: "unhealthy"),
      checks: checks,
      timestamp: DateTime.utc_now()
    }
  end
  
  defp check_readiness do
    ready = CNSForge.WorkflowScheduler.ready?() and
            Registry.count(CNSForge.BitActorRegistry) >= 0
    
    %{
      code: if(ready, do: 200, else: 503),
      status: if(ready, do: "ready", else: "not_ready"),
      timestamp: DateTime.utc_now()
    }
  end
  
  defp check_database_connection do
    # Check Mnesia/database connectivity
    case :mnesia.system_info(:is_running) do
      :yes -> :ok
      _ -> :error
    end
  end
  
  defp check_registry_health do
    # Check if registries are responsive
    try do
      Registry.count(CNSForge.BitActorRegistry)
      :ok
    rescue
      _ -> :error
    end
  end
  
  defp check_memory_usage do
    # Check memory usage is below threshold
    memory_mb = :erlang.memory(:total) / 1_024 / 1_024
    limit_mb = System.get_env("MEMORY_LIMIT_MB", "2048") |> String.to_integer()
    
    if memory_mb < limit_mb * 0.9 do
      :ok
    else
      :error
    end
  end
end

defmodule CNSForge.ResourceMonitor do
  @moduledoc """
  Monitor system resources and emit alerts
  """
  
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(opts) do
    check_interval = Keyword.get(opts, :check_interval, 5_000)
    thresholds = Keyword.get(opts, :thresholds, %{})
    
    schedule_check(check_interval)
    
    {:ok, %{
      check_interval: check_interval,
      thresholds: thresholds,
      last_alert: %{}
    }}
  end
  
  def handle_info(:check_resources, state) do
    check_and_alert(state)
    schedule_check(state.check_interval)
    {:noreply, state}
  end
  
  defp check_and_alert(state) do
    # Memory check
    memory_usage = :erlang.memory(:total) / :erlang.memory(:system)
    if memory_usage > Map.get(state.thresholds, :memory, 0.9) do
      alert(:high_memory, memory_usage, state)
    end
    
    # Process count check
    process_count = :erlang.system_info(:process_count)
    if process_count > Map.get(state.thresholds, :processes, 10_000) do
      alert(:high_process_count, process_count, state)
    end
    
    # CPU check (simplified)
    scheduler_usage = :scheduler.utilization(1)
    avg_usage = Enum.sum(scheduler_usage) / length(scheduler_usage)
    if avg_usage > Map.get(state.thresholds, :cpu, 0.8) do
      alert(:high_cpu, avg_usage, state)
    end
  end
  
  defp alert(type, value, state) do
    now = System.monotonic_time(:second)
    last_alert_time = Map.get(state.last_alert, type, 0)
    
    # Alert at most once per minute
    if now - last_alert_time > 60 do
      Logger.warning("Resource alert: #{type} = #{value}")
      
      :telemetry.execute(
        [:cns_forge, :resource_monitor, :alert],
        %{value: value},
        %{type: type}
      )
    end
  end
  
  defp schedule_check(interval) do
    Process.send_after(self(), :check_resources, interval)
  end
end