defmodule CnsWeb.Channels.PipelineHandler do
  @moduledoc """
  Handles pipeline execution and optimization across all stages.
  Implements 80/20 principle for efficient swarm execution.
  """
  
  use ChannelHandler.Handler
  
  alias Cns.Swarm.{Pipeline, Optimizer, Metrics}
  alias CnsWeb.Channels.StageCoordinator
  
  # Apply optimization plug to all actions
  plug &ensure_optimization_mode/4
  plug &track_execution_metrics/4
  
  @pipeline_stages ~w(k8s reactor ash erlang bitactor ttl2dspy turtle typer)
  @critical_stages ~w(k8s reactor ash turtle typer)
  
  @doc """
  Execute pipeline with 80/20 optimization
  """
  def execute(payload, _bindings, socket) do
    strategy = Map.get(payload, "strategy", "80_20")
    stages = determine_stages(strategy, payload)
    
    case Pipeline.execute(stages, payload, strategy: strategy) do
      {:ok, result} ->
        # Broadcast execution status
        broadcast_execution_status(socket, result)
        
        # Track metrics
        track_execution(socket, result)
        
        {:reply, {:ok, format_result(result, strategy)}, socket}
        
      {:error, stage, reason} ->
        broadcast_execution_error(socket, stage, reason)
        {:reply, {:error, %{stage: stage, reason: reason}}, socket}
    end
  end
  
  @doc """
  Optimize pipeline execution based on current metrics
  """
  def optimize(payload, _bindings, socket) do
    optimization_type = Map.get(payload, "type", "auto")
    current_metrics = Metrics.get_current(socket.assigns.swarm_id)
    
    case Optimizer.analyze_and_optimize(current_metrics, optimization_type) do
      {:ok, optimization} ->
        # Apply optimization
        apply_optimization_to_socket(socket, optimization)
        
        # Broadcast optimization
        broadcast!(socket, "optimization:applied", %{
          type: optimization_type,
          stages: optimization.stages,
          expected_improvement: optimization.expected_improvement
        })
        
        {:reply, {:ok, optimization}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Get current pipeline status with 80/20 focus
  """
  def status(_payload, _bindings, socket) do
    status = Pipeline.get_status(socket.assigns.swarm_id)
    
    # Format status based on optimization mode
    formatted_status = if socket.assigns.optimization_mode == "80_20" do
      %{
        critical_stages: filter_critical_stages(status),
        overall_health: calculate_80_20_health(status),
        bottlenecks: identify_bottlenecks(status),
        optimization_opportunities: find_optimization_opportunities(status)
      }
    else
      status
    end
    
    {:reply, {:ok, formatted_status}, socket}
  end
  
  @doc """
  Handle delegated pipeline events
  """
  def handle_in(event, payload, bindings, socket) do
    case parse_pipeline_event(event) do
      {:stage_event, stage, action} ->
        handle_stage_event(stage, action, payload, socket)
        
      {:optimization_event, action} ->
        handle_optimization_event(action, payload, socket)
        
      {:metrics_event, metric_type} ->
        handle_metrics_event(metric_type, payload, socket)
        
      :unknown ->
        {:reply, {:error, %{reason: "Unknown pipeline event: #{event}"}}, socket}
    end
  end
  
  # Private functions
  
  defp ensure_optimization_mode(socket, payload, bindings, _opts) do
    socket = if Map.has_key?(payload, "force_mode") do
      assign(socket, :optimization_mode, payload["force_mode"])
    else
      socket
    end
    
    {:cont, socket, payload, bindings}
  end
  
  defp track_execution_metrics(socket, payload, bindings, _opts) do
    # Start tracking metrics for this execution
    Task.start(fn ->
      Metrics.track_channel_event(socket.assigns.swarm_id, "pipeline", bindings)
    end)
    
    {:cont, socket, payload, bindings}
  end
  
  defp determine_stages(strategy, payload) do
    case strategy do
      "80_20" ->
        # Use only critical stages for 80/20 optimization
        custom_stages = Map.get(payload, "stages", [])
        if Enum.empty?(custom_stages) do
          @critical_stages
        else
          Enum.filter(custom_stages, &(&1 in @critical_stages))
        end
        
      "full" ->
        # Use all stages
        Map.get(payload, "stages", @pipeline_stages)
        
      "custom" ->
        # Use specified stages
        Map.get(payload, "stages", @pipeline_stages)
        
      _ ->
        @critical_stages
    end
  end
  
  defp broadcast_execution_status(socket, result) do
    # Only broadcast critical updates in 80/20 mode
    if should_broadcast?(socket, result) do
      broadcast!(socket, "execution:progress", %{
        stage: result.current_stage,
        progress: result.progress,
        metrics: extract_critical_metrics(result)
      })
    end
  end
  
  defp broadcast_execution_error(socket, stage, reason) do
    broadcast!(socket, "execution:error", %{
      stage: stage,
      reason: reason,
      timestamp: DateTime.utc_now()
    })
  end
  
  defp track_execution(socket, result) do
    Metrics.record_execution(socket.assigns.swarm_id, %{
      duration: result.duration,
      stages_executed: result.stages_executed,
      optimization_mode: socket.assigns.optimization_mode,
      success: true
    })
  end
  
  defp format_result(result, "80_20") do
    %{
      executed_stages: result.stages_executed,
      skipped_stages: result.skipped_stages,
      duration: result.duration,
      optimization_savings: calculate_savings(result),
      critical_metrics: extract_critical_metrics(result),
      bottlenecks: result.bottlenecks
    }
  end
  
  defp format_result(result, _) do
    result
  end
  
  defp apply_optimization_to_socket(socket, optimization) do
    socket
    |> assign(:active_stages, optimization.stages)
    |> assign(:optimization_thresholds, optimization.thresholds)
    |> assign(:skip_stages, optimization.skip_stages)
  end
  
  defp filter_critical_stages(status) do
    Enum.filter(status.stages, fn {stage, _info} ->
      stage in @critical_stages
    end)
    |> Enum.into(%{})
  end
  
  defp calculate_80_20_health(status) do
    critical_health = status.stages
    |> Enum.filter(fn {stage, _} -> stage in @critical_stages end)
    |> Enum.map(fn {_, info} -> info.health end)
    |> Enum.sum()
    
    critical_health / length(@critical_stages)
  end
  
  defp identify_bottlenecks(status) do
    status.stages
    |> Enum.filter(fn {_, info} -> info.latency > 200 or info.error_rate > 5 end)
    |> Enum.map(fn {stage, info} ->
      %{
        stage: stage,
        latency: info.latency,
        error_rate: info.error_rate,
        impact: calculate_bottleneck_impact(info)
      }
    end)
    |> Enum.sort_by(& &1.impact, :desc)
    |> Enum.take(3)
  end
  
  defp find_optimization_opportunities(status) do
    non_critical_stages = status.stages
    |> Enum.reject(fn {stage, _} -> stage in @critical_stages end)
    |> Enum.filter(fn {_, info} -> info.utilization < 20 end)
    |> Enum.map(fn {stage, info} ->
      %{
        stage: stage,
        utilization: info.utilization,
        recommendation: "Consider removing from pipeline",
        potential_saving: "#{100 - info.utilization}%"
      }
    end)
    
    non_critical_stages
  end
  
  defp calculate_bottleneck_impact(info) do
    # Impact score based on latency and error rate
    latency_impact = min(info.latency / 1000, 1.0) * 0.6
    error_impact = min(info.error_rate / 100, 1.0) * 0.4
    (latency_impact + error_impact) * 100
  end
  
  defp should_broadcast?(socket, result) do
    socket.assigns.optimization_mode != "80_20" or
    result.current_stage in @critical_stages or
    result.progress in [0, 100] or
    Map.get(result, :is_critical, false)
  end
  
  defp extract_critical_metrics(result) do
    %{
      cpu: result.metrics.cpu,
      memory: result.metrics.memory,
      latency: result.metrics.latency,
      throughput: result.metrics.throughput
    }
  end
  
  defp calculate_savings(result) do
    total_stages = length(@pipeline_stages)
    executed_stages = length(result.stages_executed)
    skipped_stages = total_stages - executed_stages
    
    %{
      stages_skipped: skipped_stages,
      time_saved: result.estimated_full_duration - result.duration,
      efficiency_gain: (skipped_stages / total_stages) * 100
    }
  end
  
  defp parse_pipeline_event(event) do
    case String.split(event, ":", parts: 2) do
      [stage, action] when stage in @pipeline_stages ->
        {:stage_event, stage, action}
        
      ["optimize", action] ->
        {:optimization_event, action}
        
      ["metrics", type] ->
        {:metrics_event, type}
        
      _ ->
        :unknown
    end
  end
  
  defp handle_stage_event(stage, action, payload, socket) do
    case StageCoordinator.handle_stage_action(stage, action, payload) do
      {:ok, result} ->
        {:reply, {:ok, result}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{stage: stage, action: action, reason: reason}}, socket}
    end
  end
  
  defp handle_optimization_event(action, payload, socket) do
    case action do
      "suggest" ->
        suggestions = Optimizer.suggest_optimizations(
          socket.assigns.swarm_id,
          Map.get(payload, "context", %{})
        )
        {:reply, {:ok, suggestions}, socket}
        
      "apply_suggestion" ->
        suggestion_id = Map.fetch!(payload, "suggestion_id")
        case Optimizer.apply_suggestion(socket.assigns.swarm_id, suggestion_id) do
          {:ok, result} -> {:reply, {:ok, result}, socket}
          error -> {:reply, error, socket}
        end
        
      _ ->
        {:reply, {:error, %{reason: "Unknown optimization action"}}, socket}
    end
  end
  
  defp handle_metrics_event(metric_type, payload, socket) do
    metrics = Metrics.get_by_type(
      socket.assigns.swarm_id,
      metric_type,
      Map.get(payload, "options", %{})
    )
    
    {:reply, {:ok, metrics}, socket}
  end
end