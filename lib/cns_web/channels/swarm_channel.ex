defmodule CnsWeb.SwarmChannel do
  @moduledoc """
  Main Swarm Channel with 80/20 optimization routing.
  Handles all swarm communications across the pipeline:
  typer < turtle < ttl2dspy < BitActor < Erlang < Ash < Reactor < k8s
  """
  
  use CnsWeb, :channel
  use ChannelHandler.Router
  
  alias CnsWeb.Channels.{
    PipelineHandler,
    ReactorHandler,
    NotificationHandler,
    TelemetryHandler,
    OptimizationHandler
  }
  
  @doc """
  Join the swarm channel with 80/20 optimization
  """
  join fn topic, payload, socket ->
    case String.split(topic, ":") do
      ["swarm", swarm_id] ->
        socket = 
          socket
          |> assign(:swarm_id, swarm_id)
          |> assign(:optimization_mode, Map.get(payload, "optimization", "80_20"))
          |> assign(:critical_stages, ["k8s", "reactor", "ash", "turtle", "typer"])
          |> track_presence()
        
        send(self(), :after_join)
        {:ok, %{status: "connected", swarm_id: swarm_id}, socket}
        
      _ ->
        {:error, %{reason: "Invalid topic"}}
    end
  end
  
  # Global channel plugs
  plug CnsWeb.ChannelPlugs.EnsureAuthenticated
  plug CnsWeb.ChannelPlugs.TrackMetrics
  plug &apply_80_20_filter/4
  
  # Pipeline event routing with 80/20 optimization
  scope "pipeline:" do
    plug &validate_pipeline_stage/4
    
    # Critical path events (20% that handle 80% of load)
    event "execute", PipelineHandler, :execute
    event "optimize", PipelineHandler, :optimize
    event "status", PipelineHandler, :status
    
    # Delegate all other pipeline events
    delegate PipelineHandler
  end
  
  # Reactor-specific events
  scope "reactor:" do
    plug CnsWeb.ChannelPlugs.CheckPermission, :manage_reactor
    
    event "step:execute", ReactorHandler, :execute_step
    event "step:status", ReactorHandler, :step_status
    event "workflow:create", ReactorHandler, :create_workflow
    event "workflow:execute", ReactorHandler, :execute_workflow
    
    # Critical optimization events
    event "optimize:apply", ReactorHandler, :apply_optimization
    event "optimize:analyze", ReactorHandler, :analyze_optimization
  end
  
  # Notification routing with smart filtering
  scope "notifications:" do
    plug &filter_notifications_80_20/4
    
    event "subscribe", NotificationHandler, :subscribe
    event "unsubscribe", NotificationHandler, :unsubscribe
    event "filter", NotificationHandler, :set_filter
    
    # Critical notifications only
    event "critical:*", NotificationHandler, :handle_critical
    
    # Batch operations for efficiency
    event "batch:process", NotificationHandler, :batch_process
    
    delegate NotificationHandler
  end
  
  # Telemetry streaming with aggregation
  scope "telemetry:" do
    plug &throttle_telemetry/4
    
    event "subscribe", TelemetryHandler, :subscribe
    event "metrics:get", TelemetryHandler, :get_metrics
    event "pattern:detect", TelemetryHandler, :detect_patterns
    
    # 80/20 aggregated streams
    event "stream:critical", TelemetryHandler, :stream_critical
    event "stream:aggregated", TelemetryHandler, :stream_aggregated
  end
  
  # Direct stage handlers with optimization
  scope "stage:" do
    plug &validate_stage_access/4
    
    # Map stages to their handlers using delegation
    delegate "k8s:", ReactorHandler.K8sStageHandler
    delegate "reactor:", ReactorHandler.ReactorStageHandler
    delegate "ash:", ReactorHandler.AshStageHandler
    delegate "erlang:", ReactorHandler.ErlangStageHandler
    delegate "bitactor:", ReactorHandler.BitActorStageHandler
    delegate "ttl2dspy:", ReactorHandler.Ttl2dspyStageHandler
    delegate "turtle:", ReactorHandler.TurtleStageHandler
    delegate "typer:", ReactorHandler.TyperStageHandler
  end
  
  # Optimization control events
  scope "optimize:" do
    plug CnsWeb.ChannelPlugs.RequireAdmin
    
    event "mode:set", OptimizationHandler, :set_mode
    event "threshold:adjust", OptimizationHandler, :adjust_threshold
    event "strategy:apply", OptimizationHandler, :apply_strategy
    event "report:generate", OptimizationHandler, :generate_report
  end
  
  # Handle direct swarm control messages
  handle "swarm:control", fn payload, _bindings, socket ->
    case payload["action"] do
      "pause" ->
        broadcast!(socket, "swarm:paused", %{swarm_id: socket.assigns.swarm_id})
        {:reply, :ok, assign(socket, :paused, true)}
        
      "resume" ->
        broadcast!(socket, "swarm:resumed", %{swarm_id: socket.assigns.swarm_id})
        {:reply, :ok, assign(socket, :paused, false)}
        
      "reset" ->
        {:reply, :ok, reset_swarm_state(socket)}
        
      _ ->
        {:reply, {:error, %{reason: "Unknown action"}}, socket}
    end
  end
  
  # Broadcast optimized updates
  handle "broadcast:optimized", fn payload, _bindings, socket ->
    if should_broadcast_80_20?(payload, socket) do
      broadcast!(socket, "update:critical", payload)
      {:reply, :ok, socket}
    else
      {:noreply, socket}
    end
  end
  
  # Info handlers
  handle_info :after_join, fn socket ->
    push(socket, "presence_state", Presence.list(socket))
    
    # Send initial optimization config
    push(socket, "optimization:config", %{
      mode: socket.assigns.optimization_mode,
      critical_stages: socket.assigns.critical_stages,
      thresholds: get_optimization_thresholds()
    })
    
    {:noreply, socket}
  end
  
  # Channel plug implementations
  defp apply_80_20_filter(socket, payload, bindings, _opts) do
    if socket.assigns.optimization_mode == "80_20" do
      # Filter non-critical events
      if is_critical_event?(payload, bindings) do
        {:cont, socket, payload, bindings}
      else
        # Skip non-critical events silently
        {:noreply, socket}
      end
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  defp validate_pipeline_stage(socket, payload, bindings, _opts) do
    stage = Map.get(payload, "stage")
    
    if stage in socket.assigns.critical_stages or 
       socket.assigns.optimization_mode != "80_20" do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Non-critical stage in 80/20 mode"}}, socket}
    end
  end
  
  defp filter_notifications_80_20(socket, payload, bindings, _opts) do
    if socket.assigns.optimization_mode == "80_20" do
      # Only allow critical notifications through
      level = Map.get(payload, "level", "info")
      if level in ["critical", "error"] do
        {:cont, socket, payload, bindings}
      else
        {:noreply, socket}
      end
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  defp throttle_telemetry(socket, payload, bindings, _opts) do
    # Implement telemetry throttling for efficiency
    key = "telemetry_throttle:#{socket.assigns.swarm_id}"
    
    case Hammer.check_rate(key, 60_000, 100) do
      {:allow, _count} ->
        {:cont, socket, payload, bindings}
      {:deny, _limit} ->
        {:reply, {:error, %{reason: "Rate limit exceeded"}}, socket}
    end
  end
  
  defp validate_stage_access(socket, payload, bindings, _opts) do
    # Extract stage from bindings
    stage = extract_stage_from_bindings(bindings)
    
    if has_stage_access?(socket, stage) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Unauthorized stage access"}}, socket}
    end
  end
  
  # Helper functions
  defp track_presence(socket) do
    {:ok, _} = Presence.track(socket, socket.assigns.swarm_id, %{
      online_at: inspect(System.system_time(:second)),
      optimization_mode: socket.assigns.optimization_mode
    })
    socket
  end
  
  defp reset_swarm_state(socket) do
    socket
    |> assign(:metrics, %{})
    |> assign(:notifications, [])
    |> assign(:active_stages, socket.assigns.critical_stages)
  end
  
  defp should_broadcast_80_20?(payload, socket) do
    socket.assigns.optimization_mode == "80_20" and
    Map.get(payload, "priority", "low") in ["high", "critical"]
  end
  
  defp is_critical_event?(payload, _bindings) do
    Map.get(payload, "critical", false) or
    Map.get(payload, "priority", "normal") in ["high", "critical"]
  end
  
  defp get_optimization_thresholds do
    %{
      cpu_critical: 80,
      memory_critical: 85,
      latency_critical: 200,
      error_rate_critical: 5
    }
  end
  
  defp extract_stage_from_bindings(bindings) do
    # Extract stage name from event bindings
    case bindings do
      %{"stage" => stage} -> stage
      [stage | _] when is_binary(stage) -> stage
      _ -> nil
    end
  end
  
  defp has_stage_access?(socket, stage) do
    # Check if user has access to the stage
    stage in socket.assigns.critical_stages or
    CnsWeb.Authorization.can?(socket.assigns.current_user, :access_stage, stage)
  end
end