defmodule CnsWeb.Channels.SwarmIntelligenceHandler do
  @moduledoc """
  Handles swarm intelligence coordination with 80/20 optimization.
  Manages adaptive learning, pattern recognition, and optimization recommendations.
  """
  
  use ChannelHandler.Handler
  require Logger
  
  alias Cns.SwarmIntelligence.{
    OptimizationEngine,
    PatternRecognizer,
    AdaptiveLearner,
    RecommendationEngine,
    MetricsCollector
  }
  alias CnsWeb.Endpoint
  
  # Rate limiting for resource-intensive operations
  plug CnsWeb.ChannelPlugs.RateLimit, [max: 20, window: 60_000] when action in [:optimize, :learn_pattern]
  
  @doc """
  Apply swarm optimization to pipeline or workflow
  """
  def optimize(payload, _bindings, socket) do
    optimization_request = %{
      target: payload["target"], # "pipeline" | "workflow" | "stage"
      target_id: payload["target_id"],
      strategy: payload["strategy"] || "80_20",
      constraints: payload["constraints"] || %{},
      priority: payload["priority"] || "medium",
      context: extract_context(socket)
    }
    
    case OptimizationEngine.optimize(optimization_request) do
      {:ok, optimization} ->
        # Broadcast optimization results
        broadcast_swarm_event(socket, "swarm:optimization_complete", %{
          target: optimization_request.target,
          target_id: optimization_request.target_id,
          improvements: optimization.improvements,
          estimated_gains: optimization.estimated_gains,
          applied_strategies: optimization.strategies_used
        })
        
        # Learn from successful optimization
        learn_from_optimization(optimization, socket)
        
        {:reply, {:ok, optimization}, socket}
        
      {:error, reason} ->
        Logger.warning("Swarm optimization failed: #{inspect(reason)}")
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Learn patterns from execution data
  """
  def learn_pattern(payload, _bindings, socket) do
    pattern_data = %{
      execution_id: payload["execution_id"],
      domain: payload["domain"],
      metrics: payload["metrics"],
      outcomes: payload["outcomes"],
      context: payload["context"] || %{}
    }
    
    case PatternRecognizer.analyze_and_learn(pattern_data) do
      {:ok, learning_result} ->
        # Update global pattern database
        AdaptiveLearner.update_patterns(learning_result.patterns)
        
        # Broadcast new insights
        if learning_result.new_insights do
          broadcast_swarm_event(socket, "swarm:new_insights", %{
            insights: learning_result.insights,
            confidence: learning_result.confidence,
            applicable_domains: learning_result.domains
          })
        end
        
        {:reply, {:ok, %{
          patterns_learned: length(learning_result.patterns),
          insights_discovered: length(learning_result.insights),
          confidence_score: learning_result.confidence
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Get optimization recommendations based on current context
  """
  def get_recommendations(payload, _bindings, socket) do
    context = %{
      current_pipeline: payload["pipeline_state"],
      execution_history: payload["execution_history"] || [],
      performance_metrics: payload["metrics"] || %{},
      user_preferences: socket.assigns[:user_preferences] || %{},
      optimization_mode: socket.assigns[:optimization_mode] || "80_20"
    }
    
    recommendations = RecommendationEngine.generate_recommendations(context)
    
    # Filter recommendations based on 80/20 principle
    filtered_recommendations = if context.optimization_mode == "80_20" do
      filter_high_impact_recommendations(recommendations)
    else
      recommendations
    end
    
    {:reply, {:ok, %{
      recommendations: filtered_recommendations,
      context_score: calculate_context_relevance(context),
      confidence: recommendations.confidence || 0.8
    }}, socket}
  end
  
  @doc """
  Report metrics to swarm intelligence system
  """
  def report_metrics(payload, _bindings, socket) do
    metrics = %{
      execution_id: payload["execution_id"],
      pipeline_metrics: payload["pipeline_metrics"],
      optimization_metrics: payload["optimization_metrics"],
      user_satisfaction: payload["user_satisfaction"],
      timestamp: System.system_time(:millisecond),
      context: extract_context(socket)
    }
    
    # Store metrics for analysis
    MetricsCollector.record_metrics(metrics)
    
    # Trigger adaptive learning if significant patterns detected
    if should_trigger_learning?(metrics) do
      Task.start(fn ->
        learn_from_metrics(metrics, socket)
      end)
    end
    
    # Update swarm health metrics
    update_swarm_health(metrics, socket)
    
    {:reply, :ok, socket}
  end
  
  @doc """
  Handle delegated swarm intelligence events
  """
  def handle_in(event, payload, _bindings, socket) do
    case parse_swarm_event(event) do
      {:pattern_event, action} ->
        handle_pattern_event(action, payload, socket)
        
      {:learning_event, action} ->
        handle_learning_event(action, payload, socket)
        
      {:recommendation_event, action} ->
        handle_recommendation_event(action, payload, socket)
        
      {:health_event, action} ->
        handle_health_event(action, payload, socket)
        
      _ ->
        Logger.debug("Unhandled swarm intelligence event: #{event}")
        {:noreply, socket}
    end
  end
  
  # Info handlers for background processing
  def handle_info(:background_optimization_check, socket) do
    # Perform background optimization analysis
    try do
      context = extract_context(socket)
      
      # Check for optimization opportunities
      opportunities = OptimizationEngine.scan_for_opportunities(context)
      
      if opportunities != [] do
        # Send recommendations to client
        push(socket, "swarm:optimization_opportunities", %{
          opportunities: opportunities,
          auto_apply_threshold: 0.9
        })
      end
      
      # Schedule next check
      Process.send_after(self(), :background_optimization_check, 30_000)
      
    rescue
      error ->
        Logger.error("Background optimization check failed: #{inspect(error)}")
        Process.send_after(self(), :background_optimization_check, 60_000)
    end
    
    {:noreply, socket}
  end
  
  def handle_info(:pattern_analysis_complete, {patterns, insights}, socket) do
    # Background pattern analysis completed
    push(socket, "swarm:patterns_updated", %{
      new_patterns: length(patterns),
      insights: insights,
      confidence: calculate_pattern_confidence(patterns)
    })
    
    {:noreply, socket}
  end
  
  # Private functions
  
  defp extract_context(socket) do
    %{
      swarm_id: socket.assigns[:swarm_id],
      optimization_mode: socket.assigns[:optimization_mode] || "80_20",
      user_id: socket.assigns[:current_user]&.id,
      active_pipelines: socket.assigns[:active_pipelines] || [],
      performance_targets: socket.assigns[:performance_targets] || %{},
      resource_constraints: socket.assigns[:resource_constraints] || %{}
    }
  end
  
  defp learn_from_optimization(optimization, socket) do
    learning_data = %{
      optimization_type: optimization.type,
      strategies_used: optimization.strategies_used,
      performance_before: optimization.baseline_metrics,
      performance_after: optimization.result_metrics,
      context: extract_context(socket)
    }
    
    Task.start(fn ->
      case AdaptiveLearner.learn_from_optimization(learning_data) do
        {:ok, insights} ->
          send(self(), {:learning_complete, insights})
          
        {:error, reason} ->
          Logger.error("Learning from optimization failed: #{inspect(reason)}")
      end
    end)
  end
  
  defp filter_high_impact_recommendations(recommendations) do
    # Apply 80/20 filter: show recommendations with high impact and low effort
    recommendations
    |> Enum.filter(fn rec ->
      impact_score = rec.impact_score || 0.5
      effort_score = rec.effort_score || 0.5
      
      # High impact (> 0.7) OR low effort (< 0.3)
      impact_score > 0.7 or effort_score < 0.3
    end)
    |> Enum.sort_by(fn rec ->
      # Sort by impact/effort ratio
      impact = rec.impact_score || 0.5
      effort = max(rec.effort_score || 0.5, 0.1) # Avoid division by zero
      -(impact / effort)
    end)
    |> Enum.take(5) # Limit to top 5 recommendations
  end
  
  defp calculate_context_relevance(context) do
    # Calculate how relevant the context is for optimization
    score = 0.0
    
    score = score + if context.current_pipeline, do: 0.3, else: 0.0
    score = score + if context.execution_history != [], do: 0.2, else: 0.0
    score = score + if context.performance_metrics != %{}, do: 0.3, else: 0.0
    score = score + if context.user_preferences != %{}, do: 0.2, else: 0.0
    
    score
  end
  
  defp should_trigger_learning?(metrics) do
    # Determine if metrics indicate patterns worth learning
    cond do
      # Significant performance change
      metrics.pipeline_metrics["duration_change_percent"] &&
      abs(metrics.pipeline_metrics["duration_change_percent"]) > 20 ->
        true
        
      # High user satisfaction with optimization
      metrics.user_satisfaction && metrics.user_satisfaction > 0.8 ->
        true
        
      # Error patterns detected
      metrics.pipeline_metrics["error_rate"] &&
      metrics.pipeline_metrics["error_rate"] > 0.05 ->
        true
        
      true ->
        false
    end
  end
  
  defp learn_from_metrics(metrics, socket) do
    # Extract patterns from metrics data
    pattern_data = %{
      metrics: metrics,
      context: extract_context(socket),
      timestamp: System.system_time(:millisecond)
    }
    
    case PatternRecognizer.extract_patterns(pattern_data) do
      {:ok, patterns} ->
        insights = AdaptiveLearner.analyze_patterns(patterns)
        send(self(), {:pattern_analysis_complete, {patterns, insights}})
        
      {:error, reason} ->
        Logger.error("Pattern extraction failed: #{inspect(reason)}")
    end
  end
  
  defp update_swarm_health(metrics, socket) do
    health_update = %{
      swarm_id: socket.assigns[:swarm_id],
      performance_metrics: metrics.pipeline_metrics,
      optimization_effectiveness: calculate_optimization_effectiveness(metrics),
      resource_utilization: metrics.optimization_metrics["resource_utilization"] || %{},
      timestamp: System.system_time(:millisecond)
    }
    
    # Broadcast health update
    broadcast_swarm_event(socket, "swarm:health_update", health_update)
  end
  
  defp calculate_optimization_effectiveness(metrics) do
    # Calculate how effective current optimizations are
    baseline = metrics.pipeline_metrics["baseline_duration"] || 1000
    current = metrics.pipeline_metrics["current_duration"] || 1000
    
    improvement = max(0, (baseline - current) / baseline)
    
    %{
      duration_improvement: improvement,
      error_rate: metrics.pipeline_metrics["error_rate"] || 0,
      resource_efficiency: metrics.optimization_metrics["efficiency_score"] || 0.5,
      overall_score: (improvement + 
                     max(0, 1 - (metrics.pipeline_metrics["error_rate"] || 0)) +
                     (metrics.optimization_metrics["efficiency_score"] || 0.5)) / 3
    }
  end
  
  defp calculate_pattern_confidence(patterns) do
    if patterns == [] do
      0.0
    else
      patterns
      |> Enum.map(& &1.confidence)
      |> Enum.sum()
      |> Kernel./(length(patterns))
    end
  end
  
  defp broadcast_swarm_event(socket, event, payload) do
    topic = socket.topic || "swarm:*"
    Endpoint.broadcast!(topic, event, payload)
  end
  
  defp handle_pattern_event(action, payload, socket) do
    case action do
      "search" ->
        query = payload["query"]
        patterns = PatternRecognizer.search_patterns(query)
        {:reply, {:ok, %{patterns: patterns}}, socket}
        
      "export" ->
        patterns = PatternRecognizer.export_patterns(payload["filters"] || %{})
        {:reply, {:ok, %{patterns: patterns}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_learning_event(action, payload, socket) do
    case action do
      "status" ->
        status = AdaptiveLearner.get_learning_status()
        {:reply, {:ok, status}, socket}
        
      "reset" ->
        AdaptiveLearner.reset_learning_data()
        {:reply, {:ok, %{reset: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_recommendation_event(action, payload, socket) do
    case action do
      "apply" ->
        recommendation_id = payload["recommendation_id"]
        result = RecommendationEngine.apply_recommendation(recommendation_id)
        {:reply, result, socket}
        
      "dismiss" ->
        recommendation_id = payload["recommendation_id"]
        RecommendationEngine.dismiss_recommendation(recommendation_id)
        {:reply, {:ok, %{dismissed: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_health_event(action, payload, socket) do
    case action do
      "check" ->
        health = get_swarm_health_status(socket)
        {:reply, {:ok, health}, socket}
        
      "report" ->
        health_data = payload["health_data"]
        store_health_report(health_data, socket)
        {:reply, :ok, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp get_swarm_health_status(socket) do
    %{
      swarm_id: socket.assigns[:swarm_id],
      active_optimizations: OptimizationEngine.get_active_count(),
      learning_progress: AdaptiveLearner.get_progress(),
      pattern_count: PatternRecognizer.get_pattern_count(),
      recommendation_queue: RecommendationEngine.get_queue_size(),
      performance_score: calculate_overall_performance_score(socket)
    }
  end
  
  defp calculate_overall_performance_score(socket) do
    # Calculate overall swarm performance based on various metrics
    metrics = MetricsCollector.get_recent_metrics(socket.assigns[:swarm_id])
    
    if metrics == [] do
      0.5
    else
      avg_optimization_score = Enum.map(metrics, & &1.optimization_effectiveness.overall_score)
                              |> Enum.sum()
                              |> Kernel./(length(metrics))
      
      min(max(avg_optimization_score, 0.0), 1.0)
    end
  end
  
  defp store_health_report(health_data, socket) do
    report = %{
      swarm_id: socket.assigns[:swarm_id],
      data: health_data,
      timestamp: System.system_time(:millisecond),
      reporter: socket.assigns[:current_user]&.id
    }
    
    MetricsCollector.store_health_report(report)
  end
  
  defp parse_swarm_event(event) do
    case String.split(event, ":") do
      ["pattern", action] ->
        {:pattern_event, action}
        
      ["learning", action] ->
        {:learning_event, action}
        
      ["recommendation", action] ->
        {:recommendation_event, action}
        
      ["health", action] ->
        {:health_event, action}
        
      _ ->
        {:unknown, event}
    end
  end
end