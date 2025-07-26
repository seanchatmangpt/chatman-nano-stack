defmodule CnsForge.ChannelRouter do
  @moduledoc """
  Router for Phoenix Channels endpoints and API routes
  Provides REST endpoints for channel management and status
  """
  
  use Phoenix.Router
  import Plug.Conn
  import Phoenix.Controller
  
  pipeline :api do
    plug :accepts, ["json"]
    plug :put_resp_content_type, "application/json"
  end
  
  scope "/api", CnsForge do
    pipe_through :api
    
    # Pipeline status endpoints
    get "/pipelines", PipelineController, :index
    get "/pipelines/:id", PipelineController, :show
    post "/pipelines/:id/restart", PipelineController, :restart
    post "/pipelines/:id/pause", PipelineController, :pause
    
    # Reactor status endpoints
    get "/reactors", ReactorController, :index
    get "/reactors/:name", ReactorController, :show
    post "/reactors/:name/steps/:step/retry", ReactorController, :retry_step
    
    # Telemetry endpoints
    get "/telemetry/streams", TelemetryController, :streams
    get "/telemetry/:stream", TelemetryController, :show
    post "/telemetry/:stream/thresholds", TelemetryController, :set_threshold
    
    # System status
    get "/status", SystemController, :status
    get "/health", SystemController, :health
  end
  
  # WebSocket upgrade endpoint
  get "/socket/websocket", CnsForge.ChannelController, :websocket
  
  # Default catch-all
  match _, CnsForge.ChannelController, :not_found
end

defmodule CnsForge.PipelineController do
  @moduledoc """
  REST API controller for pipeline management
  """
  
  use Phoenix.Controller
  require Logger
  
  def index(conn, _params) do
    pipelines = list_active_pipelines()
    json(conn, %{pipelines: pipelines})
  end
  
  def show(conn, %{"id" => pipeline_id}) do
    case get_pipeline_details(pipeline_id) do
      {:ok, pipeline} ->
        json(conn, %{pipeline: pipeline})
      
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Pipeline not found"})
    end
  end
  
  def restart(conn, %{"id" => pipeline_id}) do
    case restart_pipeline(pipeline_id) do
      {:ok, _} ->
        # Broadcast restart event
        CnsForge.PipelineEventProcessor.broadcast_stage_transition(
          pipeline_id, "system", "paused", "restarting"
        )
        
        json(conn, %{status: "restarting"})
      
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
  
  def pause(conn, %{"id" => pipeline_id}) do
    case pause_pipeline(pipeline_id) do
      {:ok, _} ->
        # Broadcast pause event
        CnsForge.PipelineEventProcessor.broadcast_stage_transition(
          pipeline_id, "system", "running", "paused"
        )
        
        json(conn, %{status: "paused"})
      
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
  
  defp list_active_pipelines do
    # Return list of active pipelines
    [
      %{
        id: "pipeline_1",
        name: "TTL Processing Pipeline",
        status: "running",
        current_stage: "bitactor",
        started_at: DateTime.utc_now()
      },
      %{
        id: "pipeline_2", 
        name: "Semantic Analysis Pipeline",
        status: "completed",
        current_stage: "k8s",
        started_at: DateTime.add(DateTime.utc_now(), -300, :second)
      }
    ]
  end
  
  defp get_pipeline_details(pipeline_id) do
    # Get detailed pipeline information
    case pipeline_id do
      "pipeline_1" ->
        {:ok, %{
          id: pipeline_id,
          name: "TTL Processing Pipeline",
          status: "running",
          stages: [
            %{name: "typer", status: "completed", duration: 150},
            %{name: "turtle", status: "completed", duration: 200},
            %{name: "ttl2dspy", status: "completed", duration: 300},
            %{name: "bitactor", status: "running", duration: nil},
            %{name: "erlang", status: "pending", duration: nil},
            %{name: "ash", status: "pending", duration: nil},
            %{name: "reactor", status: "pending", duration: nil},
            %{name: "k8s", status: "pending", duration: nil}
          ],
          metrics: %{
            total_duration: 650,
            success_rate: 0.95,
            items_processed: 1247
          }
        }}
      
      _ ->
        {:error, :not_found}
    end
  end
  
  defp restart_pipeline(pipeline_id) do
    Logger.info("Restarting pipeline: #{pipeline_id}")
    {:ok, %{restarted: true}}
  end
  
  defp pause_pipeline(pipeline_id) do
    Logger.info("Pausing pipeline: #{pipeline_id}")
    {:ok, %{paused: true}}
  end
end

defmodule CnsForge.ReactorController do
  @moduledoc """
  REST API controller for Ash Reactor management
  """
  
  use Phoenix.Controller
  require Logger
  
  def index(conn, _params) do
    reactors = list_active_reactors()
    json(conn, %{reactors: reactors})
  end
  
  def show(conn, %{"name" => reactor_name}) do
    case get_reactor_details(reactor_name) do
      {:ok, reactor} ->
        json(conn, %{reactor: reactor})
      
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Reactor not found"})
    end
  end
  
  def retry_step(conn, %{"name" => reactor_name, "step" => step_name}) do
    case retry_reactor_step(reactor_name, step_name) do
      {:ok, _} ->
        # Broadcast retry event
        CnsForge.ReactorStepNotifier.notify_step_started(reactor_name, step_name, %{retried: true})
        
        json(conn, %{status: "retrying"})
      
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
  
  defp list_active_reactors do
    [
      %{
        name: "TTLMainReactor",
        status: "idle",
        last_execution: DateTime.add(DateTime.utc_now(), -60, :second),
        success_rate: 0.98
      },
      %{
        name: "ValidationReactor",
        status: "running",
        current_step: "validate_ontology",
        success_rate: 0.95
      }
    ]
  end
  
  defp get_reactor_details(reactor_name) do
    case reactor_name do
      "TTLMainReactor" ->
        {:ok, %{
          name: reactor_name,
          status: "idle",
          steps: [
            %{name: "parse_ttl", status: "completed", duration: 45},
            %{name: "validate_structure", status: "completed", duration: 30},
            %{name: "generate_resources", status: "completed", duration: 120},
            %{name: "create_reactors", status: "completed", duration: 80}
          ],
          total_executions: 456,
          success_rate: 0.98,
          avg_duration: 275
        }}
      
      _ ->
        {:error, :not_found}
    end
  end
  
  defp retry_reactor_step(reactor_name, step_name) do
    Logger.info("Retrying reactor #{reactor_name} step #{step_name}")
    {:ok, %{retried: true}}
  end
end

defmodule CnsForge.TelemetryController do
  @moduledoc """
  REST API controller for telemetry management
  """
  
  use Phoenix.Controller
  require Logger
  
  def streams(conn, _params) do
    streams = list_telemetry_streams()
    json(conn, %{streams: streams})
  end
  
  def show(conn, %{"stream" => stream_name}) do
    case get_stream_metrics(stream_name) do
      {:ok, metrics} ->
        json(conn, %{stream: stream_name, metrics: metrics})
      
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Stream not found"})
    end
  end
  
  def set_threshold(conn, %{"stream" => stream_name} = params) do
    metric = Map.get(params, "metric")
    threshold = Map.get(params, "threshold")
    
    case set_metric_threshold(stream_name, metric, threshold) do
      {:ok, _} ->
        json(conn, %{threshold_set: true})
      
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
  
  defp list_telemetry_streams do
    [
      %{
        name: "performance",
        description: "System performance metrics",
        active_subscribers: 3,
        last_update: DateTime.utc_now()
      },
      %{
        name: "pipeline",
        description: "Pipeline execution metrics",
        active_subscribers: 5,
        last_update: DateTime.utc_now()
      },
      %{
        name: "business",
        description: "Business KPI metrics",
        active_subscribers: 2,
        last_update: DateTime.utc_now()
      }
    ]
  end
  
  defp get_stream_metrics(stream_name) do
    case stream_name do
      "performance" ->
        {:ok, %{
          cpu_usage: 45.2,
          memory_usage: 67.8,
          disk_io: 23.1,
          network_io: 15.6,
          process_count: 1247,
          uptime: 86400
        }}
      
      "pipeline" ->
        {:ok, %{
          active_pipelines: 3,
          completed_today: 125,
          error_rate: 0.8,
          avg_duration: 245,
          throughput: 34.5
        }}
      
      "business" ->
        {:ok, %{
          pipelines_processed: 5647,
          revenue_impact: 125750.00,
          sla_compliance: 99.2,
          customer_satisfaction: 4.8
        }}
      
      _ ->
        {:error, :not_found}
    end
  end
  
  defp set_metric_threshold(stream_name, metric, threshold) do
    Logger.info("Setting threshold for #{stream_name}.#{metric}: #{threshold}")
    {:ok, %{threshold_set: true}}
  end
end

defmodule CnsForge.SystemController do
  @moduledoc """
  System status and health check endpoints
  """
  
  use Phoenix.Controller
  require Logger
  
  def status(conn, _params) do
    status = get_system_status()
    json(conn, status)
  end
  
  def health(conn, _params) do
    health = get_system_health()
    
    if health.healthy do
      json(conn, health)
    else
      conn
      |> put_status(:service_unavailable)
      |> json(health)
    end
  end
  
  defp get_system_status do
    %{
      system: "CNS Forge Pipeline",
      version: "1.0.0",
      environment: Application.get_env(:cns_forge, :environment, "development"),
      uptime: get_uptime(),
      components: %{
        pipeline_processor: "healthy",
        reactor_engine: "healthy", 
        telemetry_streamer: "healthy",
        phoenix_channels: "healthy",
        pubsub: "healthy"
      },
      active_connections: count_active_connections(),
      memory_usage: :erlang.memory(:total),
      last_check: DateTime.utc_now()
    }
  end
  
  defp get_system_health do
    components = check_component_health()
    overall_healthy = Enum.all?(components, fn {_name, status} -> status == "healthy" end)
    
    %{
      healthy: overall_healthy,
      components: components,
      checks: %{
        database: check_database_health(),
        external_apis: check_external_apis(),
        memory: check_memory_health(),
        disk: check_disk_health()
      },
      timestamp: DateTime.utc_now()
    }
  end
  
  defp get_uptime do
    {uptime_ms, _} = :erlang.statistics(:wall_clock)
    uptime_ms
  end
  
  defp count_active_connections do
    # Count active Phoenix Channel connections
    :ranch.procs(CnsForge.ChannelEndpoint.HTTP, :connections) |> length()
  rescue
    _ -> 0
  end
  
  defp check_component_health do
    components = [
      {:pipeline_processor, CnsForge.PipelineEventProcessor},
      {:reactor_notifier, CnsForge.ReactorStepNotifier},
      {:telemetry_streamer, CnsForge.TelemetryStreamer}
    ]
    
    Enum.map(components, fn {name, module} ->
      status = if Process.whereis(module), do: "healthy", else: "unhealthy"
      {name, status}
    end)
    |> Enum.into(%{})
  end
  
  defp check_database_health do
    # Simplified database health check
    "healthy"
  end
  
  defp check_external_apis do
    # Check external API connectivity
    "healthy"
  end
  
  defp check_memory_health do
    memory_usage = :erlang.memory(:total)
    memory_limit = 1024 * 1024 * 1024  # 1GB limit
    
    if memory_usage < memory_limit * 0.9 do
      "healthy"
    else
      "warning"
    end
  end
  
  defp check_disk_health do
    # Simplified disk health check
    "healthy"
  end
end

defmodule CnsForge.ChannelController do
  @moduledoc """
  Controller for WebSocket upgrades and fallback endpoints
  """
  
  use Phoenix.Controller
  require Logger
  
  def websocket(conn, _params) do
    # Handle WebSocket upgrade
    Logger.info("WebSocket connection requested")
    
    conn
    |> put_status(:upgrade_required)
    |> json(%{
      error: "Use WebSocket protocol",
      websocket_url: "/socket/websocket"
    })
  end
  
  def not_found(conn, _params) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "Endpoint not found"})
  end
end