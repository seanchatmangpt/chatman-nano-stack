defmodule CnsWeb.Channels.NotificationHandler do
  @moduledoc """
  Handles notification broadcasting with 80/20 optimization filtering.
  Implements smart filtering to reduce noise and focus on critical events.
  """
  
  use ChannelHandler.Handler
  require Logger
  
  alias CnsWeb.{Endpoint, NotificationFilter}
  alias Cns.Notifications.{Store, Aggregator, PriorityQueue}
  
  # 80/20 optimization: batch low-priority notifications
  plug CnsWeb.ChannelPlugs.BatchNotifications when action in [:broadcast]
  
  # Channel configuration
  @critical_channels ~w(error_alerts ash_resources reactor_workflows)
  @high_priority_channels ~w(swarm_intelligence pipeline_events)
  @batch_interval_ms 500
  @max_batch_size 50
  
  @doc """
  Subscribe to notification channels with smart filtering
  """
  def subscribe(payload, _bindings, socket) do
    channels = payload["channels"] || []
    filters = payload["filters"] || %{}
    priority_threshold = payload["priority_threshold"] || "medium"
    
    # Apply 80/20 rule: only subscribe to critical channels by default
    filtered_channels = if socket.assigns[:optimization_mode] == "80_20" do
      filter_critical_channels(channels)
    else
      channels
    end
    
    # Store subscription preferences
    socket = socket
    |> assign(:subscribed_channels, filtered_channels)
    |> assign(:notification_filters, filters)
    |> assign(:priority_threshold, priority_threshold)
    |> assign(:notification_queue, PriorityQueue.new())
    
    # Start batch processor for non-critical notifications
    if socket.assigns[:optimization_mode] == "80_20" do
      start_batch_processor(socket)
    end
    
    {:reply, {:ok, %{
      subscribed: filtered_channels,
      mode: socket.assigns[:optimization_mode],
      batch_enabled: socket.assigns[:optimization_mode] == "80_20"
    }}, socket}
  end
  
  @doc """
  Unsubscribe from notification channels
  """
  def unsubscribe(payload, _bindings, socket) do
    channels = payload["channels"] || socket.assigns[:subscribed_channels] || []
    
    remaining_channels = (socket.assigns[:subscribed_channels] || []) -- channels
    
    socket = assign(socket, :subscribed_channels, remaining_channels)
    
    # Stop batch processor if no channels remain
    if remaining_channels == [] and socket.assigns[:batch_processor] do
      Process.cancel_timer(socket.assigns[:batch_processor])
    end
    
    {:reply, {:ok, %{unsubscribed: channels}}, socket}
  end
  
  @doc """
  Update notification filters for smart filtering
  """
  def set_filter(payload, _bindings, socket) do
    filter_type = payload["type"]
    filter_config = payload["config"]
    
    updated_filters = Map.put(
      socket.assigns[:notification_filters] || %{},
      filter_type,
      filter_config
    )
    
    socket = assign(socket, :notification_filters, updated_filters)
    
    {:reply, {:ok, %{filters: updated_filters}}, socket}
  end
  
  @doc """
  Handle critical notifications - bypass all filtering
  """
  def handle_critical(event, payload, _bindings, socket) do
    notification = build_notification(event, payload, "critical")
    
    # Critical notifications always go through immediately
    push(socket, "notification:critical", notification)
    
    # Also broadcast to all subscribers
    broadcast_critical(notification, socket)
    
    # Track critical notification
    track_notification(notification, socket)
    
    {:noreply, socket}
  end
  
  @doc """
  Broadcast notification with 80/20 filtering
  """
  def broadcast(payload, _bindings, socket) do
    notification = build_notification(
      payload["type"],
      payload,
      payload["level"] || "info"
    )
    
    cond do
      # Always send critical notifications immediately
      notification.level in ["critical", "error"] ->
        send_immediate_notification(notification, socket)
        
      # Apply 80/20 filtering for non-critical
      socket.assigns[:optimization_mode] == "80_20" ->
        queue_for_batch_processing(notification, socket)
        
      # Normal mode - send all notifications
      true ->
        send_immediate_notification(notification, socket)
    end
    
    {:reply, :ok, socket}
  end
  
  @doc """
  Process notifications in batches for efficiency
  """
  def batch_process(payload, _bindings, socket) do
    notifications = payload["notifications"] || []
    
    # Aggregate similar notifications
    aggregated = Aggregator.aggregate_notifications(notifications, %{
      group_by: [:type, :source],
      time_window: @batch_interval_ms,
      max_size: @max_batch_size
    })
    
    # Send aggregated notifications
    Enum.each(aggregated, fn {group, notifications} ->
      if should_send_group?(group, socket) do
        push(socket, "notification:batch", %{
          group: group,
          count: length(notifications),
          summary: create_summary(notifications),
          notifications: compress_notifications(notifications, socket)
        })
      end
    end)
    
    {:reply, {:ok, %{processed: length(notifications)}}, socket}
  end
  
  @doc """
  Handle delegated notification events
  """
  def handle_in(event, payload, _bindings, socket) do
    case parse_notification_event(event) do
      {:channel_event, channel, action} ->
        handle_channel_event(channel, action, payload, socket)
        
      {:filter_event, action} ->
        handle_filter_event(action, payload, socket)
        
      _ ->
        {:noreply, socket}
    end
  end
  
  # Info handlers for batch processing
  def handle_info(:process_batch, socket) do
    queue = socket.assigns[:notification_queue] || PriorityQueue.new()
    
    if PriorityQueue.size(queue) > 0 do
      {notifications, new_queue} = PriorityQueue.take_batch(queue, @max_batch_size)
      
      # Process the batch
      process_notification_batch(notifications, socket)
      
      socket = assign(socket, :notification_queue, new_queue)
    end
    
    # Schedule next batch
    schedule_next_batch(socket)
    
    {:noreply, socket}
  end
  
  # Private functions
  
  defp filter_critical_channels(channels) do
    Enum.filter(channels, fn channel ->
      channel in @critical_channels or
      channel in @high_priority_channels
    end)
  end
  
  defp build_notification(type, payload, level) do
    %{
      id: generate_notification_id(),
      type: type,
      level: level,
      message: payload["message"],
      source: payload["source"],
      channel: payload["channel"] || infer_channel(type),
      metadata: payload["metadata"] || %{},
      timestamp: System.system_time(:millisecond),
      priority: calculate_priority(type, level, payload)
    }
  end
  
  defp generate_notification_id do
    "notif_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
  end
  
  defp infer_channel(type) do
    cond do
      String.contains?(type, "ash") -> "ash_resources"
      String.contains?(type, "reactor") -> "reactor_workflows"
      String.contains?(type, "error") -> "error_alerts"
      String.contains?(type, "pipeline") -> "pipeline_events"
      String.contains?(type, "swarm") -> "swarm_intelligence"
      true -> "general"
    end
  end
  
  defp calculate_priority(type, level, payload) do
    base_priority = case level do
      "critical" -> 100
      "error" -> 80
      "warning" -> 60
      "info" -> 40
      "debug" -> 20
      _ -> 30
    end
    
    # Boost priority for certain types
    type_boost = cond do
      type in ["error_alerts", "pipeline_failure"] -> 20
      type in ["ash_resources", "reactor_workflows"] -> 15
      type in ["swarm_intelligence"] -> 10
      true -> 0
    end
    
    # Custom priority from payload
    custom_priority = payload["priority"] || 0
    
    min(base_priority + type_boost + custom_priority, 100)
  end
  
  defp send_immediate_notification(notification, socket) do
    if should_send_notification?(notification, socket) do
      push(socket, "notification", notification)
      
      # Store for later retrieval
      Store.save_notification(notification)
      
      # Track metrics
      track_notification(notification, socket)
    end
  end
  
  defp queue_for_batch_processing(notification, socket) do
    queue = socket.assigns[:notification_queue] || PriorityQueue.new()
    
    new_queue = PriorityQueue.insert(queue, notification, notification.priority)
    
    assign(socket, :notification_queue, new_queue)
  end
  
  defp should_send_notification?(notification, socket) do
    filters = socket.assigns[:notification_filters] || %{}
    threshold = socket.assigns[:priority_threshold] || "medium"
    
    # Check channel subscription
    channel_subscribed = notification.channel in (socket.assigns[:subscribed_channels] || [])
    
    # Check priority threshold
    priority_met = meets_priority_threshold?(notification.priority, threshold)
    
    # Apply custom filters
    passes_filters = NotificationFilter.apply_filters(notification, filters)
    
    channel_subscribed and priority_met and passes_filters
  end
  
  defp meets_priority_threshold?(priority, threshold) do
    threshold_values = %{
      "low" => 20,
      "medium" => 40,
      "high" => 60,
      "critical" => 80
    }
    
    priority >= Map.get(threshold_values, threshold, 40)
  end
  
  defp start_batch_processor(socket) do
    timer_ref = Process.send_after(self(), :process_batch, @batch_interval_ms)
    assign(socket, :batch_processor, timer_ref)
  end
  
  defp schedule_next_batch(socket) do
    if socket.assigns[:batch_processor] do
      Process.cancel_timer(socket.assigns[:batch_processor])
    end
    
    timer_ref = Process.send_after(self(), :process_batch, @batch_interval_ms)
    assign(socket, :batch_processor, timer_ref)
  end
  
  defp process_notification_batch(notifications, socket) do
    # Group by channel and type for efficient delivery
    grouped = Enum.group_by(notifications, &{&1.channel, &1.type})
    
    Enum.each(grouped, fn {{channel, type}, group_notifications} ->
      if length(group_notifications) == 1 do
        # Send single notification normally
        send_immediate_notification(hd(group_notifications), socket)
      else
        # Send as aggregated batch
        push(socket, "notification:batch", %{
          channel: channel,
          type: type,
          count: length(group_notifications),
          summary: create_summary(group_notifications),
          notifications: compress_notifications(group_notifications, socket)
        })
      end
    end)
  end
  
  defp create_summary(notifications) do
    %{
      total: length(notifications),
      by_level: Enum.frequencies_by(notifications, & &1.level),
      by_source: Enum.frequencies_by(notifications, & &1.source),
      time_range: %{
        start: notifications |> Enum.map(& &1.timestamp) |> Enum.min(),
        end: notifications |> Enum.map(& &1.timestamp) |> Enum.max()
      }
    }
  end
  
  defp compress_notifications(notifications, socket) do
    # In 80/20 mode, only include essential fields
    if socket.assigns[:optimization_mode] == "80_20" do
      Enum.map(notifications, fn n ->
        %{
          id: n.id,
          type: n.type,
          level: n.level,
          message: truncate_message(n.message),
          timestamp: n.timestamp
        }
      end)
    else
      notifications
    end
  end
  
  defp truncate_message(message) when byte_size(message) > 100 do
    String.slice(message, 0, 97) <> "..."
  end
  defp truncate_message(message), do: message
  
  defp should_send_group?({channel, _type}, socket) do
    channel in (socket.assigns[:subscribed_channels] || [])
  end
  
  defp broadcast_critical(notification, socket) do
    # Broadcast to all connected clients on critical channel
    Endpoint.broadcast!("notifications:critical", "alert", notification)
    
    # Also send to monitoring systems
    send_to_monitoring(notification)
  end
  
  defp send_to_monitoring(notification) do
    :telemetry.execute(
      [:cns, :notification, :critical],
      %{count: 1},
      %{
        type: notification.type,
        source: notification.source,
        level: notification.level
      }
    )
  end
  
  defp track_notification(notification, socket) do
    # Update metrics
    socket = update_in(
      socket.assigns[:metrics][:notifications_sent],
      &(&1 + 1)
    )
    
    # Track by type
    :telemetry.execute(
      [:cns, :notification, :sent],
      %{count: 1},
      %{
        type: notification.type,
        channel: notification.channel,
        level: notification.level,
        connector_id: socket.assigns[:connector_id]
      }
    )
    
    socket
  end
  
  defp handle_channel_event(channel, action, payload, socket) do
    case action do
      "mute" ->
        filters = Map.put(socket.assigns[:notification_filters] || %{}, 
                         "muted_channels", 
                         [channel | Map.get(socket.assigns[:notification_filters] || %{}, "muted_channels", [])])
        socket = assign(socket, :notification_filters, filters)
        {:reply, {:ok, %{muted: channel}}, socket}
        
      "unmute" ->
        muted = Map.get(socket.assigns[:notification_filters] || %{}, "muted_channels", [])
        filters = Map.put(socket.assigns[:notification_filters] || %{}, 
                         "muted_channels", 
                         muted -- [channel])
        socket = assign(socket, :notification_filters, filters)
        {:reply, {:ok, %{unmuted: channel}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_filter_event(action, payload, socket) do
    case action do
      "reset" ->
        socket = assign(socket, :notification_filters, %{})
        {:reply, {:ok, %{filters_reset: true}}, socket}
        
      "list" ->
        {:reply, {:ok, socket.assigns[:notification_filters] || %{}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp parse_notification_event(event) do
    case String.split(event, ":") do
      ["channel", channel, action] ->
        {:channel_event, channel, action}
        
      ["filter", action] ->
        {:filter_event, action}
        
      _ ->
        {:unknown, event}
    end
  end
end