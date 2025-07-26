defmodule CnsForgeWeb.Channels.TelemetryHandler do
  @moduledoc """
  📊 TELEMETRY HANDLER - Real-time Metrics and Monitoring
  
  Handles telemetry events, metrics collection, and performance monitoring
  across the entire pipeline stack.
  
  80/20 Focus: Critical metrics that impact performance decisions
  """
  
  use ChannelHandler.Handler
  
  require Logger
  
  # State for subscriber management
  @registry CnsForge.TelemetryRegistry
  
  # Critical telemetry events (80/20 principle)
  @critical_events [
    [:swarm, :pipeline, :stage, :stop],
    [:swarm, :channel, :fatal_error],
    [:bitactor, :performance, :degraded],
    [:k8s, :deployment, :failed]
  ]
  
  plug :ensure_telemetry_authorized
  
  def handle_in("subscribe", payload, _bindings, socket) do
    Logger.info("📊 TELEMETRY: New subscription request")
    
    events = Map.get(payload, "events", ["all"])
    filters = Map.get(payload, "filters", %{})
    
    # Register subscriber
    subscription_id = register_subscriber(socket, events, filters)
    
    # Start sending telemetry
    socket = assign(socket, :telemetry_subscription, subscription_id)
    
    {:reply, {:ok, %{subscription_id: subscription_id}}, socket}
  end
  
  def handle_in("unsubscribe", _payload, _bindings, socket) do
    Logger.info("📊 TELEMETRY: Unsubscribe request")
    
    case socket.assigns[:telemetry_subscription] do
      nil ->
        {:reply, {:error, "No active subscription"}, socket}
        
      subscription_id ->
        unregister_subscriber(subscription_id)
        socket = assign(socket, :telemetry_subscription, nil)
        {:reply, {:ok, %{unsubscribed: true}}, socket}
    end
  end
  
  def handle_in("query", payload, _bindings, socket) do
    Logger.info("📊 TELEMETRY: Metrics query")
    
    query_params = %{
      metric: Map.get(payload, "metric", "all"),
      timeframe: Map.get(payload, "timeframe", "1h"),
      aggregation: Map.get(payload, "aggregation", "avg")
    }
    
    metrics = query_metrics(query_params)
    
    {:reply, {:ok, metrics}, socket}
  end
  
  def handle_critical(payload, _bindings, socket) do
    Logger.error("🚨 TELEMETRY: Critical event received")
    
    # Process critical telemetry with highest priority
    handle_critical_telemetry(payload, socket)
    
    # Broadcast to all critical subscribers
    broadcast_critical_event(payload)
    
    {:reply, {:ok, %{processed: true, priority: :critical}}, socket}
  end
  
  def handle_event(event, payload, _bindings, socket) do
    Logger.debug("📊 TELEMETRY: Generic event #{event}")
    
    # Process and forward telemetry event
    process_telemetry_event(event, payload, socket)
    
    {:noreply, socket}
  end
  
  # Delegated catch-all
  def handle_in(event, _payload, _bindings, socket) do
    Logger.warn("📊 TELEMETRY: Unknown event #{event}")
    {:reply, {:error, "Unknown telemetry event: #{event}"}, socket}
  end
  
  # Private functions
  
  defp ensure_telemetry_authorized(socket, _payload, _bindings, _opts) do
    if can_access_telemetry?(socket) do
      {:cont, socket}
    else
      {:reply, {:error, "Telemetry access denied"}, socket}
    end
  end
  
  defp can_access_telemetry?(socket) do
    socket.assigns[:access_level] in [:authenticated, :admin]
  end
  
  defp register_subscriber(socket, events, filters) do
    subscription_id = generate_subscription_id()
    
    subscription = %{
      id: subscription_id,
      socket_id: socket.id,
      events: normalize_events(events),
      filters: filters,
      created_at: DateTime.utc_now()
    }
    
    # Store in registry
    Registry.register(@registry, subscription_id, subscription)
    
    # Attach telemetry handlers for this subscription
    attach_telemetry_handlers(subscription)
    
    subscription_id
  end
  
  defp unregister_subscriber(subscription_id) do
    # Detach telemetry handlers
    detach_telemetry_handlers(subscription_id)
    
    # Remove from registry
    Registry.unregister(@registry, subscription_id)
  end
  
  defp query_metrics(params) do
    # Simulate metrics query
    %{
      metric: params.metric,
      timeframe: params.timeframe,
      data_points: generate_mock_metrics(params),
      summary: %{
        avg: 45.2,
        min: 10.5,
        max: 125.8,
        p95: 89.3,
        p99: 115.2
      }
    }
  end
  
  defp handle_critical_telemetry(payload, socket) do
    # Log critical event
    Logger.error("Critical telemetry: #{inspect(payload)}")
    
    # Store in time-series database (simulated)
    store_critical_metric(payload)
    
    # Trigger alerts if needed
    maybe_trigger_alerts(payload, socket)
  end
  
  defp broadcast_critical_event(payload) do
    # Broadcast to all subscribers interested in critical events
    Registry.dispatch(@registry, :critical_subscribers, fn entries ->
      for {_pid, subscription} <- entries do
        send_telemetry_to_subscriber(subscription, payload)
      end
    end)
  end
  
  defp process_telemetry_event(event, payload, socket) do
    # Process based on event type
    telemetry_data = %{
      event: event,
      payload: payload,
      timestamp: DateTime.utc_now(),
      socket_id: socket.id
    }
    
    # Store metric
    store_telemetry_metric(telemetry_data)
    
    # Forward to interested subscribers
    forward_to_subscribers(event, telemetry_data)
  end
  
  defp attach_telemetry_handlers(subscription) do
    Enum.each(subscription.events, fn event_pattern ->
      handler_id = "telemetry-#{subscription.id}-#{:erlang.phash2(event_pattern)}"
      
      :telemetry.attach(
        handler_id,
        event_pattern,
        &handle_telemetry_event/4,
        %{subscription: subscription}
      )
    end)
  end
  
  defp detach_telemetry_handlers(subscription_id) do
    # Find and detach all handlers for this subscription
    :telemetry.list_handlers()
    |> Enum.filter(fn %{id: id} -> String.starts_with?(id, "telemetry-#{subscription_id}") end)
    |> Enum.each(fn %{id: id} -> :telemetry.detach(id) end)
  end
  
  defp handle_telemetry_event(event_name, measurements, metadata, %{subscription: subscription}) do
    # Filter based on subscription filters
    if passes_filters?(subscription.filters, metadata) do
      telemetry_event = %{
        event: event_name,
        measurements: measurements,
        metadata: metadata,
        timestamp: DateTime.utc_now()
      }
      
      send_telemetry_to_subscriber(subscription, telemetry_event)
    end
  end
  
  defp send_telemetry_to_subscriber(subscription, event) do
    # Send via Phoenix PubSub to the subscriber's socket
    CnsForgeWeb.Endpoint.broadcast(
      "telemetry:#{subscription.socket_id}",
      "telemetry_event",
      event
    )
  end
  
  defp normalize_events(["all"]), do: [[:swarm, :*, :*, :*]]
  defp normalize_events(events) when is_list(events) do
    Enum.map(events, &parse_event_pattern/1)
  end
  
  defp parse_event_pattern(pattern) when is_binary(pattern) do
    pattern
    |> String.split(":")
    |> Enum.map(fn
      "*" -> :*
      part -> String.to_atom(part)
    end)
  end
  defp parse_event_pattern(pattern), do: pattern
  
  defp passes_filters?(filters, metadata) when map_size(filters) == 0, do: true
  defp passes_filters?(filters, metadata) do
    Enum.all?(filters, fn {key, value} ->
      Map.get(metadata, String.to_atom(key)) == value
    end)
  end
  
  defp generate_mock_metrics(params) do
    # Generate mock time-series data
    points = 24  # 24 hours of data
    
    Enum.map(1..points, fn i ->
      %{
        timestamp: DateTime.add(DateTime.utc_now(), -i * 3600, :second),
        value: 30 + :rand.uniform(50) + :math.sin(i / 4) * 20
      }
    end)
  end
  
  defp store_critical_metric(payload) do
    # Simulate storing in time-series DB
    Logger.info("Stored critical metric: #{inspect(payload, limit: :infinity, printable_limit: :infinity)}")
  end
  
  defp store_telemetry_metric(telemetry_data) do
    # Simulate storing telemetry
    Logger.debug("Stored telemetry: #{telemetry_data.event}")
  end
  
  defp maybe_trigger_alerts(payload, socket) do
    # Check if alert conditions are met
    if should_alert?(payload) do
      trigger_alert(payload, socket)
    end
  end
  
  defp should_alert?(payload) do
    # Simple alert logic - customize based on needs
    Map.get(payload, "severity", "info") in ["critical", "fatal"]
  end
  
  defp trigger_alert(payload, socket) do
    alert = %{
      type: "telemetry_alert",
      severity: Map.get(payload, "severity", "critical"),
      message: Map.get(payload, "message", "Critical event detected"),
      source: socket.id,
      timestamp: DateTime.utc_now()
    }
    
    # Broadcast alert
    CnsForgeWeb.Endpoint.broadcast("alerts:critical", "new_alert", alert)
  end
  
  defp forward_to_subscribers(event, telemetry_data) do
    # Forward to subscribers interested in this event
    Registry.dispatch(@registry, :all_subscribers, fn entries ->
      for {_pid, subscription} <- entries do
        if event_matches_subscription?(event, subscription) do
          send_telemetry_to_subscriber(subscription, telemetry_data)
        end
      end
    end)
  end
  
  defp event_matches_subscription?(event, subscription) do
    # Check if event matches any of the subscription patterns
    Enum.any?(subscription.events, fn pattern ->
      match_event_pattern?(event, pattern)
    end)
  end
  
  defp match_event_pattern?(event, pattern) when is_binary(event) and is_list(pattern) do
    event_parts = String.split(event, ":")
    pattern_parts = Enum.map(pattern, &to_string/1)
    
    length(event_parts) == length(pattern_parts) and
      Enum.zip(event_parts, pattern_parts)
      |> Enum.all?(fn {event_part, pattern_part} ->
        pattern_part == "*" or event_part == pattern_part
      end)
  end
  defp match_event_pattern?(_, _), do: false
  
  defp generate_subscription_id do
    "sub_#{System.unique_integer([:positive])}_#{:rand.uniform(10000)}"
  end
end