defmodule CnsWeb.Channels.PipelineHandler do
  @moduledoc """
  Handles pipeline monitoring events with 80/20 optimization.
  Manages real-time updates for the pipeline stages:
  typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
  """
  
  use ChannelHandler.Handler
  require Logger
  
  alias Cns.Pipeline.{Orchestrator, StageMonitor, MetricsCollector}
  alias CnsWeb.Endpoint
  
  # Apply optimization plug only for non-critical operations
  plug CnsWeb.ChannelPlugs.OptimizationFilter when action in [:status, :list_stages]
  
  @doc """
  Execute pipeline with 80/20 optimization strategy
  """
  def execute(payload, _bindings, socket) do
    domain = payload["domain"] || "generic"
    strategy = payload["optimization_strategy"] || "skip_non_critical"
    stages = payload["stages"] || get_critical_stages(strategy)
    
    case Orchestrator.execute_pipeline(%{
      domain: domain,
      strategy: strategy,
      stages: stages,
      connector_id: socket.assigns[:connector_id],
      optimization_mode: socket.assigns[:optimization_mode] || "80_20"
    }) do
      {:ok, execution} ->
        # Start monitoring the execution
        monitor_execution(execution.id, socket)
        
        # Broadcast pipeline start
        broadcast_event(socket, "pipeline:started", %{
          execution_id: execution.id,
          stages: stages,
          strategy: strategy,
          estimated_duration: calculate_estimated_duration(stages, strategy)
        })
        
        {:reply, {:ok, %{execution_id: execution.id}}, socket}
        
      {:error, reason} ->
        Logger.error("Pipeline execution failed: #{inspect(reason)}")
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Apply 80/20 optimization to running pipeline
  """
  def optimize(payload, _bindings, socket) do
    execution_id = payload["execution_id"]
    optimization_type = payload["type"] || "skip_non_critical"
    
    case apply_optimization(execution_id, optimization_type, socket) do
      {:ok, result} ->
        # Broadcast optimization applied
        broadcast_event(socket, "pipeline:optimized", %{
          execution_id: execution_id,
          optimization: optimization_type,
          stages_affected: result.stages_affected,
          time_saved_ms: result.time_saved
        })
        
        {:reply, {:ok, result}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Get pipeline execution status
  """
  def status(payload, _bindings, socket) do
    execution_id = payload["execution_id"]
    
    case StageMonitor.get_status(execution_id) do
      {:ok, status} ->
        # Filter status based on 80/20 mode
        filtered_status = if socket.assigns[:optimization_mode] == "80_20" do
          filter_critical_status(status)
        else
          status
        end
        
        {:reply, {:ok, filtered_status}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Handle delegated pipeline events
  """
  def handle_in(event, payload, bindings, socket) do
    case parse_pipeline_event(event) do
      {:stage_event, stage, action} ->
        handle_stage_event(stage, action, payload, socket)
        
      {:monitor_event, action} ->
        handle_monitor_event(action, payload, socket)
        
      {:metrics_event, metric_type} ->
        handle_metrics_event(metric_type, payload, socket)
        
      _ ->
        Logger.debug("Unhandled pipeline event: #{event}")
        {:noreply, socket}
    end
  end
  
  # Private functions
  
  defp get_critical_stages("skip_non_critical") do
    ["typer", "turtle", "ash", "reactor", "k8s"]
  end
  
  defp get_critical_stages("minimal_path") do
    ["typer", "turtle", "k8s"]
  end
  
  defp get_critical_stages(_) do
    ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"]
  end
  
  defp monitor_execution(execution_id, socket) do
    # Start async monitoring task
    Task.start(fn ->
      StageMonitor.monitor(execution_id, fn stage_update ->
        # Broadcast stage updates
        broadcast_event(socket, "pipeline:stage_update", Map.merge(stage_update, %{
          execution_id: execution_id,
          timestamp: System.system_time(:millisecond)
        }))
        
        # Check for completion
        if stage_update.status == "completed" and stage_update.stage == "k8s" do
          handle_pipeline_completion(execution_id, socket)
        end
      end)
    end)
  end
  
  defp handle_pipeline_completion(execution_id, socket) do
    case Orchestrator.get_execution_result(execution_id) do
      {:ok, result} ->
        metrics = MetricsCollector.calculate_execution_metrics(result)
        
        broadcast_event(socket, "pipeline:completed", %{
          execution_id: execution_id,
          duration_ms: result.duration,
          stages_completed: result.stages_completed,
          optimizations_applied: result.optimizations,
          metrics: metrics
        })
        
      _ ->
        Logger.error("Failed to get execution result for #{execution_id}")
    end
  end
  
  defp apply_optimization(execution_id, optimization_type, socket) do
    with {:ok, execution} <- Orchestrator.get_execution(execution_id),
         {:ok, optimization_plan} <- create_optimization_plan(execution, optimization_type),
         {:ok, result} <- Orchestrator.apply_optimization(execution_id, optimization_plan) do
      
      # Track optimization metrics
      track_optimization_metrics(result, socket)
      
      {:ok, result}
    end
  end
  
  defp create_optimization_plan(execution, "skip_non_critical") do
    non_critical_stages = ["ttl2dspy", "erlang", "bitactor"]
    pending_stages = Enum.filter(execution.pending_stages, &(&1 in non_critical_stages))
    
    {:ok, %{
      skip_stages: pending_stages,
      parallel_stages: [],
      cache_stages: []
    }}
  end
  
  defp create_optimization_plan(execution, "parallel_execution") do
    # Identify stages that can run in parallel
    parallel_groups = [
      ["ash", "reactor"],
      ["ttl2dspy", "bitactor"]
    ]
    
    {:ok, %{
      skip_stages: [],
      parallel_stages: parallel_groups,
      cache_stages: []
    }}
  end
  
  defp create_optimization_plan(execution, "cached_bypass") do
    # Identify stages with cached results
    cacheable_stages = ["ttl2dspy", "erlang"]
    
    {:ok, %{
      skip_stages: [],
      parallel_stages: [],
      cache_stages: cacheable_stages
    }}
  end
  
  defp handle_stage_event(stage, action, payload, socket) do
    case action do
      "start" ->
        broadcast_event(socket, "stage:started", %{
          stage: stage,
          execution_id: payload["execution_id"],
          estimated_duration: get_stage_duration(stage)
        })
        
      "complete" ->
        broadcast_event(socket, "stage:completed", %{
          stage: stage,
          execution_id: payload["execution_id"],
          duration_ms: payload["duration"],
          outputs: payload["outputs"]
        })
        
      "error" ->
        broadcast_event(socket, "stage:error", %{
          stage: stage,
          execution_id: payload["execution_id"],
          error: payload["error"]
        })
        
      _ ->
        :ok
    end
    
    {:noreply, socket}
  end
  
  defp handle_monitor_event(action, payload, socket) do
    case action do
      "subscribe" ->
        execution_id = payload["execution_id"]
        
        # Subscribe to execution updates
        :ok = StageMonitor.subscribe(execution_id, self())
        
        {:reply, {:ok, %{subscribed: true}}, socket}
        
      "unsubscribe" ->
        execution_id = payload["execution_id"]
        
        :ok = StageMonitor.unsubscribe(execution_id, self())
        
        {:reply, {:ok, %{unsubscribed: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_metrics_event(metric_type, payload, socket) do
    execution_id = payload["execution_id"]
    
    metrics = case metric_type do
      "performance" ->
        MetricsCollector.get_performance_metrics(execution_id)
        
      "optimization" ->
        MetricsCollector.get_optimization_metrics(execution_id)
        
      "stage_timing" ->
        MetricsCollector.get_stage_timing_metrics(execution_id)
        
      _ ->
        %{}
    end
    
    {:reply, {:ok, metrics}, socket}
  end
  
  defp filter_critical_status(status) do
    # Filter to show only critical stage information
    critical_stages = ["typer", "turtle", "ash", "reactor", "k8s"]
    
    %{
      execution_id: status.execution_id,
      status: status.status,
      progress: status.progress,
      stages: Enum.filter(status.stages, fn stage ->
        stage.name in critical_stages
      end),
      estimated_completion: status.estimated_completion
    }
  end
  
  defp calculate_estimated_duration(stages, strategy) do
    base_durations = %{
      "typer" => 50,
      "turtle" => 30,
      "ttl2dspy" => 100,
      "bitactor" => 200,
      "erlang" => 100,
      "ash" => 150,
      "reactor" => 100,
      "k8s" => 70
    }
    
    optimization_factor = case strategy do
      "skip_non_critical" -> 0.6
      "parallel_execution" -> 0.7
      "cached_bypass" -> 0.5
      _ -> 1.0
    end
    
    stages
    |> Enum.map(&Map.get(base_durations, &1, 50))
    |> Enum.sum()
    |> Kernel.*(optimization_factor)
    |> round()
  end
  
  defp get_stage_duration(stage) do
    %{
      "typer" => 50,
      "turtle" => 30,
      "ttl2dspy" => 100,
      "bitactor" => 200,
      "erlang" => 100,
      "ash" => 150,
      "reactor" => 100,
      "k8s" => 70
    }[stage] || 50
  end
  
  defp parse_pipeline_event(event) do
    case String.split(event, ":") do
      ["stage", stage, action] ->
        {:stage_event, stage, action}
        
      ["monitor", action] ->
        {:monitor_event, action}
        
      ["metrics", metric_type] ->
        {:metrics_event, metric_type}
        
      _ ->
        {:unknown, event}
    end
  end
  
  defp broadcast_event(socket, event, payload) do
    topic = socket.topic || "swarm:*"
    
    Endpoint.broadcast!(topic, event, payload)
  end
  
  defp track_optimization_metrics(result, socket) do
    metrics = %{
      optimization_applied: result.optimization_type,
      stages_optimized: length(result.stages_affected),
      time_saved_ms: result.time_saved,
      efficiency_gain: result.efficiency_gain
    }
    
    # Send to telemetry system
    :telemetry.execute(
      [:cns, :pipeline, :optimization],
      metrics,
      %{
        connector_id: socket.assigns[:connector_id],
        execution_id: result.execution_id
      }
    )
  end
end