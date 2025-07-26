defmodule CnsWeb.Channels.NotificationHandler do
  @moduledoc """
  Handles notification routing and filtering with 80/20 optimization.
  Manages subscriptions, filters, and smart notification batching.
  """
  
  use ChannelHandler.Handler
  
  alias Cns.Notifications.{Manager, Filter, Batcher}
  alias CnsWeb.Presence
  
  # Apply notification optimization
  plug &apply_notification_filtering/4
  plug &enforce_rate_limits/4 when action in [:handle_critical, :batch_process]
  
  @critical_levels ~w(critical error)
  @batch_interval 1000  # 1 second batching for non-critical
  
  @doc """
  Subscribe to notification types with 80/20 filtering
  """
  def subscribe(payload, _bindings, socket) do
    notification_types = Map.get(payload, "types", ["all"])
    filter_level = Map.get(payload, "level", "critical")
    
    # Apply 80/20 principle - default to critical only
    subscription = if socket.assigns.optimization_mode == "80_20" do
      %{
        types: filter_critical_types(notification_types),
        level: "critical",
        batch_enabled: true,
        batch_interval: @batch_interval
      }
    else
      %{
        types: notification_types,
        level: filter_level,
        batch_enabled: Map.get(payload, "batch_enabled", false),
        batch_interval: Map.get(payload, "batch_interval", @batch_interval)
      }
    end
    
    # Store subscription
    socket = assign(socket, :notification_subscription, subscription)
    
    # Register with notification manager
    Manager.subscribe(socket.assigns.swarm_id, self(), subscription)
    
    {:reply, {:ok, subscription}, socket}
  end
  
  @doc """
  Unsubscribe from notifications
  """
  def unsubscribe(payload, _bindings, socket) do
    types = Map.get(payload, "types", ["all"])
    
    Manager.unsubscribe(socket.assigns.swarm_id, self(), types)
    
    # Update socket subscription
    socket = update_in(socket.assigns.notification_subscription.types, fn current ->
      current -- types
    end)
    
    {:reply, :ok, socket}
  end
  
  @doc """
  Set notification filters with 80/20 optimization
  """
  def set_filter(payload, _bindings, socket) do
    filter_config = Map.fetch!(payload, "filter")
    
    # Enhance filter for 80/20 mode
    optimized_filter = if socket.assigns.optimization_mode == "80_20" do
      filter_config
      |> Map.put("min_level", "error")
      |> Map.put("max_notifications_per_minute", 20)
      |> Map.put("aggregate_similar", true)
      |> Map.put("smart_batching", true)
    else
      filter_config
    end
    
    # Apply filter
    case Filter.create_and_apply(socket.assigns.swarm_id, optimized_filter) do
      {:ok, filter} ->
        socket = assign(socket, :notification_filter, filter)
        {:reply, {:ok, filter}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Handle critical notifications - always delivered immediately
  """
  def handle_critical(payload, bindings, socket) do
    notification = build_notification(payload, bindings, "critical")
    
    # Critical notifications bypass all filtering
    push(socket, "notification:critical", notification)
    
    # Track critical notification
    track_critical_notification(socket, notification)
    
    # Broadcast to other subscribers if needed
    if Map.get(payload, "broadcast", false) do
      broadcast!(socket, "notification:critical:broadcast", notification)
    end
    
    {:noreply, socket}
  end
  
  @doc """
  Process notifications in batches for efficiency
  """
  def batch_process(payload, _bindings, socket) do
    notifications = Map.fetch!(payload, "notifications")
    
    # Apply 80/20 filtering
    processed = if socket.assigns.optimization_mode == "80_20" do
      notifications
      |> filter_by_importance()
      |> group_similar_notifications()
      |> limit_non_critical(20)  # Max 20 non-critical notifications
    else
      notifications
    end
    
    # Process in batches
    batch_results = Batcher.process_batch(processed, socket.assigns.swarm_id)
    
    # Send batch update
    push(socket, "notification:batch", %{
      count: length(processed),
      critical_count: count_critical(processed),
      results: batch_results
    })
    
    {:reply, {:ok, batch_results}, socket}
  end
  
  @doc """
  Handle delegated notification events
  """
  def handle_in(event, payload, bindings, socket) do
    case parse_notification_event(event) do
      {:level_event, level} ->
        handle_level_notification(level, payload, bindings, socket)
        
      {:stage_event, stage} ->
        handle_stage_notification(stage, payload, bindings, socket)
        
      {:filter_event, action} ->
        handle_filter_action(action, payload, socket)
        
      :unknown ->
        {:reply, {:error, %{reason: "Unknown notification event: #{event}"}}, socket}
    end
  end
  
  # Private functions
  
  defp apply_notification_filtering(socket, payload, bindings, _opts) do
    # Check if notification should be filtered
    if should_filter_notification?(socket, payload) do
      {:noreply, socket}  # Silently drop filtered notifications
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  defp enforce_rate_limits(socket, payload, bindings, _opts) do
    key = "notification_rate:#{socket.assigns.swarm_id}"
    
    # Different limits for 80/20 mode
    limit = if socket.assigns.optimization_mode == "80_20" do
      {100, 60_000}  # 100 per minute for critical
    else
      {500, 60_000}  # 500 per minute for all
    end
    
    case Hammer.check_rate(key, elem(limit, 1), elem(limit, 0)) do
      {:allow, _count} ->
        {:cont, socket, payload, bindings}
        
      {:deny, _limit} ->
        {:reply, {:error, %{reason: "Rate limit exceeded"}}, socket}
    end
  end
  
  defp filter_critical_types(types) do
    # In 80/20 mode, only keep notification types that matter
    critical_types = ~w(error critical security performance deployment)
    
    if "all" in types do
      critical_types
    else
      Enum.filter(types, &(&1 in critical_types))
    end
  end
  
  defp should_filter_notification?(socket, payload) do
    return false if Map.get(payload, "force", false)
    
    subscription = socket.assigns[:notification_subscription] || %{}
    filter = socket.assigns[:notification_filter] || %{}
    
    # Apply 80/20 filtering
    if socket.assigns.optimization_mode == "80_20" do
      level = Map.get(payload, "level", "info")
      level not in @critical_levels and
      not Map.get(payload, "important", false)
    else
      # Standard filtering
      not matches_subscription?(payload, subscription) or
      not passes_filter?(payload, filter)
    end
  end
  
  defp matches_subscription?(payload, subscription) do
    type = Map.get(payload, "type", "general")
    level = Map.get(payload, "level", "info")
    
    (type in subscription.types or "all" in subscription.types) and
    notification_level_allowed?(level, subscription.level)
  end
  
  defp passes_filter?(payload, filter) do
    # Apply custom filter rules
    Filter.matches?(payload, filter)
  end
  
  defp notification_level_allowed?(level, min_level) do
    level_priority = %{
      "debug" => 0,
      "info" => 1,
      "warning" => 2,
      "error" => 3,
      "critical" => 4
    }
    
    level_priority[level] >= level_priority[min_level]
  end
  
  defp build_notification(payload, bindings, level) do
    %{
      id: Ecto.UUID.generate(),
      level: level,
      type: Map.get(payload, "type", "general"),
      title: Map.fetch!(payload, "title"),
      message: Map.fetch!(payload, "message"),
      data: Map.get(payload, "data", %{}),
      source: Map.get(bindings, "source", "system"),
      timestamp: DateTime.utc_now(),
      metadata: extract_metadata(payload, bindings)
    }
  end
  
  defp extract_metadata(payload, bindings) do
    %{
      stage: Map.get(bindings, "stage"),
      step: Map.get(bindings, "step"),
      correlation_id: Map.get(payload, "correlation_id"),
      swarm_id: Map.get(bindings, "swarm_id")
    }
  end
  
  defp track_critical_notification(socket, notification) do
    Task.start(fn ->
      Cns.Metrics.track_notification(
        socket.assigns.swarm_id,
        notification.level,
        notification.type
      )
    end)
  end
  
  defp filter_by_importance(notifications) do
    # Sort by importance score
    notifications
    |> Enum.map(&add_importance_score/1)
    |> Enum.sort_by(& &1.importance_score, :desc)
  end
  
  defp add_importance_score(notification) do
    score = calculate_importance_score(notification)
    Map.put(notification, :importance_score, score)
  end
  
  defp calculate_importance_score(notification) do
    base_score = case notification.level do
      "critical" -> 100
      "error" -> 80
      "warning" -> 40
      "info" -> 20
      _ -> 10
    end
    
    # Adjust based on other factors
    adjustments = [
      if(notification[:affects_production], do: 50, else: 0),
      if(notification[:user_facing], do: 30, else: 0),
      if(notification[:security_related], do: 40, else: 0),
      if(notification[:performance_impact], do: 20, else: 0)
    ]
    
    base_score + Enum.sum(adjustments)
  end
  
  defp group_similar_notifications(notifications) do
    notifications
    |> Enum.group_by(&notification_group_key/1)
    |> Enum.map(fn {_key, group} ->
      if length(group) > 1 do
        merge_notification_group(group)
      else
        hd(group)
      end
    end)
  end
  
  defp notification_group_key(notification) do
    {notification.type, notification.level, notification[:stage]}
  end
  
  defp merge_notification_group(notifications) do
    first = hd(notifications)
    count = length(notifications)
    
    first
    |> Map.put(:message, "#{first.message} (#{count} similar notifications)")
    |> Map.put(:grouped_count, count)
    |> Map.put(:importance_score, Enum.max_by(notifications, & &1[:importance_score]))
  end
  
  defp limit_non_critical(notifications, limit) do
    {critical, non_critical} = Enum.split_with(notifications, fn n ->
      n.level in @critical_levels
    end)
    
    critical ++ Enum.take(non_critical, limit)
  end
  
  defp count_critical(notifications) do
    Enum.count(notifications, &(&1.level in @critical_levels))
  end
  
  defp parse_notification_event(event) do
    case String.split(event, ":", parts: 2) do
      ["level", level] ->
        {:level_event, level}
        
      ["stage", stage] ->
        {:stage_event, stage}
        
      ["filter", action] ->
        {:filter_event, action}
        
      _ ->
        :unknown
    end
  end
  
  defp handle_level_notification(level, payload, bindings, socket) do
    if notification_level_allowed?(level, socket.assigns.notification_subscription.level) do
      notification = build_notification(payload, bindings, level)
      
      # Send based on level
      if level in @critical_levels do
        push(socket, "notification:#{level}", notification)
      else
        # Batch non-critical
        Batcher.add(socket.assigns.swarm_id, notification)
      end
      
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end
  
  defp handle_stage_notification(stage, payload, bindings, socket) do
    # Check if stage notifications are enabled
    if stage in socket.assigns.critical_stages or
       socket.assigns.optimization_mode != "80_20" do
      notification = build_notification(payload, Map.put(bindings, "stage", stage), "info")
      push(socket, "notification:stage:#{stage}", notification)
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end
  
  defp handle_filter_action("test", payload, socket) do
    # Test filter with sample notification
    test_notification = Map.get(payload, "notification", %{
      "level" => "info",
      "type" => "test",
      "message" => "Test notification"
    })
    
    result = passes_filter?(test_notification, socket.assigns.notification_filter)
    {:reply, {:ok, %{passed: result}}, socket}
  end
  
  defp handle_filter_action(action, _payload, socket) do
    {:reply, {:error, %{reason: "Unknown filter action: #{action}"}}, socket}
  end
end