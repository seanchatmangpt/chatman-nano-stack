defmodule CnsWeb.Channels.OptimizationHandler do
  @moduledoc """
  Handles 80/20 optimization strategies and controls.
  Manages optimization modes, thresholds, and strategy application.
  """
  
  use ChannelHandler.Handler
  
  alias Cns.Optimization.{StrategyEngine, ThresholdManager, ReportGenerator}
  
  # Require admin access for optimization controls
  plug CnsWeb.ChannelPlugs.RequireAdmin
  plug &track_optimization_events/4
  
  @optimization_modes ~w(80_20 full custom adaptive)
  @optimization_strategies ~w(skip_stages parallel_execution batch_processing smart_routing)
  
  @doc """
  Set optimization mode for the swarm
  """
  def set_mode(payload, _bindings, socket) do
    mode = Map.fetch!(payload, "mode")
    
    unless mode in @optimization_modes do
      {:reply, {:error, %{reason: "Invalid optimization mode"}}, socket}
    else
      case StrategyEngine.set_mode(socket.assigns.swarm_id, mode) do
        {:ok, config} ->
          # Update socket state
          socket = assign(socket, :optimization_mode, mode)
          
          # Broadcast mode change
          broadcast!(socket, "optimization:mode:changed", %{
            mode: mode,
            config: config,
            applied_at: DateTime.utc_now()
          })
          
          {:reply, {:ok, config}, socket}
          
        {:error, reason} ->
          {:reply, {:error, reason}, socket}
      end
    end
  end
  
  @doc """
  Adjust optimization thresholds
  """
  def adjust_threshold(payload, _bindings, socket) do
    threshold_type = Map.fetch!(payload, "type")
    value = Map.fetch!(payload, "value")
    
    case ThresholdManager.update_threshold(
      socket.assigns.swarm_id,
      threshold_type,
      value
    ) do
      {:ok, updated_thresholds} ->
        # Apply new thresholds immediately
        StrategyEngine.apply_thresholds(socket.assigns.swarm_id, updated_thresholds)
        
        # Broadcast threshold change
        broadcast!(socket, "optimization:threshold:updated", %{
          type: threshold_type,
          value: value,
          all_thresholds: updated_thresholds
        })
        
        {:reply, {:ok, updated_thresholds}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Apply specific optimization strategy
  """
  def apply_strategy(payload, _bindings, socket) do
    strategy = Map.fetch!(payload, "strategy")
    options = Map.get(payload, "options", %{})
    
    unless strategy in @optimization_strategies do
      {:reply, {:error, %{reason: "Invalid optimization strategy"}}, socket}
    else
      case StrategyEngine.apply_strategy(
        socket.assigns.swarm_id,
        strategy,
        options
      ) do
        {:ok, result} ->
          # Track strategy application
          track_strategy_application(socket, strategy, result)
          
          # Broadcast strategy application
          broadcast!(socket, "optimization:strategy:applied", %{
            strategy: strategy,
            result: result,
            swarm_id: socket.assigns.swarm_id
          })
          
          {:reply, {:ok, result}, socket}
          
        {:error, reason} ->
          {:reply, {:error, reason}, socket}
      end
    end
  end
  
  @doc """
  Generate optimization report
  """
  def generate_report(payload, _bindings, socket) do
    report_type = Map.get(payload, "type", "comprehensive")
    time_range = Map.get(payload, "range", "24h")
    
    case ReportGenerator.generate(
      socket.assigns.swarm_id,
      report_type,
      time_range
    ) do
      {:ok, report} ->
        # Format report based on type
        formatted_report = format_optimization_report(report, report_type)
        
        {:reply, {:ok, formatted_report}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Handle delegated optimization events
  """
  def handle_in(event, payload, bindings, socket) do
    case parse_optimization_event(event) do
      {:analysis_event, action} ->
        handle_analysis_action(action, payload, socket)
        
      {:strategy_event, action} ->
        handle_strategy_action(action, payload, socket)
        
      {:threshold_event, action} ->
        handle_threshold_action(action, payload, socket)
        
      {:report_event, action} ->
        handle_report_action(action, payload, socket)
        
      :unknown ->
        {:reply, {:error, %{reason: "Unknown optimization event: #{event}"}}, socket}
    end
  end
  
  # Private functions
  
  defp track_optimization_events(socket, payload, bindings, _opts) do
    # Track all optimization events for analysis
    Task.start(fn ->
      Cns.Metrics.track_optimization_event(
        socket.assigns.swarm_id,
        extract_event_type(bindings),
        payload
      )
    end)
    
    {:cont, socket, payload, bindings}
  end
  
  defp extract_event_type(bindings) do
    Map.get(bindings, "event", "unknown")
  end
  
  defp track_strategy_application(socket, strategy, result) do
    Task.start(fn ->
      Cns.Metrics.track_strategy_application(
        socket.assigns.swarm_id,
        strategy,
        result.duration,
        result.improvement_percentage,
        result.resources_saved
      )
    end)
  end
  
  defp format_optimization_report(report, "comprehensive") do
    %{
      summary: %{
        total_optimizations: report.total_optimizations,
        avg_improvement: "#{report.avg_improvement}%",
        time_saved: "#{report.time_saved}ms",
        resources_saved: "#{report.resources_saved}%"
      },
      pareto_analysis: %{
        critical_stages: report.pareto_analysis.critical_stages,
        impact_distribution: report.pareto_analysis.impact_distribution,
        optimization_opportunities: report.pareto_analysis.opportunities
      },
      recommendations: format_recommendations(report.recommendations),
      trends: %{
        performance_trend: report.trends.performance,
        efficiency_trend: report.trends.efficiency,
        cost_trend: report.trends.cost
      },
      metrics: report.detailed_metrics
    }
  end
  
  defp format_optimization_report(report, "summary") do
    %{
      optimization_score: report.optimization_score,
      key_improvements: Enum.take(report.improvements, 5),
      critical_issues: Enum.take(report.issues, 3),
      next_actions: Enum.take(report.recommendations, 3)
    }
  end
  
  defp format_optimization_report(report, _), do: report
  
  defp format_recommendations(recommendations) do
    recommendations
    |> Enum.map(fn rec ->
      %{
        title: rec.title,
        description: rec.description,
        impact: "#{rec.impact}%",
        effort: rec.effort_level,
        priority: rec.priority,
        implementation: rec.implementation_guide
      }
    end)
    |> Enum.sort_by(& &1.priority, :desc)
  end
  
  defp parse_optimization_event(event) do
    case String.split(event, ":", parts: 2) do
      ["analyze", action] ->
        {:analysis_event, action}
        
      ["strategy", action] ->
        {:strategy_event, action}
        
      ["threshold", action] ->
        {:threshold_event, action}
        
      ["report", action] ->
        {:report_event, action}
        
      _ ->
        :unknown
    end
  end
  
  defp handle_analysis_action("run", payload, socket) do
    analysis_type = Map.get(payload, "type", "performance")
    
    case StrategyEngine.run_analysis(socket.assigns.swarm_id, analysis_type) do
      {:ok, analysis} ->
        # Broadcast analysis results
        broadcast!(socket, "optimization:analysis:completed", %{
          type: analysis_type,
          results: analysis,
          recommendations: analysis.recommendations
        })
        
        {:reply, {:ok, analysis}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_analysis_action("schedule", payload, socket) do
    schedule = Map.fetch!(payload, "schedule")
    analysis_type = Map.get(payload, "type", "performance")
    
    case StrategyEngine.schedule_analysis(
      socket.assigns.swarm_id,
      analysis_type,
      schedule
    ) do
      {:ok, job_id} ->
        {:reply, {:ok, %{job_id: job_id, schedule: schedule}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_strategy_action("list", _payload, socket) do
    strategies = StrategyEngine.list_available_strategies()
    active_strategies = StrategyEngine.get_active_strategies(socket.assigns.swarm_id)
    
    {:reply, {:ok, %{
      available: strategies,
      active: active_strategies
    }}, socket}
  end
  
  defp handle_strategy_action("remove", payload, socket) do
    strategy = Map.fetch!(payload, "strategy")
    
    case StrategyEngine.remove_strategy(socket.assigns.swarm_id, strategy) do
      {:ok, result} ->
        broadcast!(socket, "optimization:strategy:removed", %{
          strategy: strategy,
          result: result
        })
        
        {:reply, {:ok, result}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_threshold_action("list", _payload, socket) do
    thresholds = ThresholdManager.get_all_thresholds(socket.assigns.swarm_id)
    
    {:reply, {:ok, thresholds}, socket}
  end
  
  defp handle_threshold_action("reset", payload, socket) do
    threshold_type = Map.get(payload, "type", "all")
    
    case ThresholdManager.reset_thresholds(socket.assigns.swarm_id, threshold_type) do
      {:ok, reset_thresholds} ->
        broadcast!(socket, "optimization:thresholds:reset", %{
          type: threshold_type,
          thresholds: reset_thresholds
        })
        
        {:reply, {:ok, reset_thresholds}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_report_action("list", _payload, socket) do
    reports = ReportGenerator.list_reports(socket.assigns.swarm_id)
    
    {:reply, {:ok, reports}, socket}
  end
  
  defp handle_report_action("export", payload, socket) do
    report_id = Map.fetch!(payload, "report_id")
    format = Map.get(payload, "format", "json")
    
    case ReportGenerator.export_report(report_id, format) do
      {:ok, export_data} ->
        {:reply, {:ok, export_data}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_report_action(action, _payload, socket) do
    {:reply, {:error, %{reason: "Unknown report action: #{action}"}}, socket}
  end
end