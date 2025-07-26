defmodule CnsForgeWeb.Channels.NotificationHandler do
  @moduledoc """
  📢 NOTIFICATION HANDLER - Real-time Notification System
  
  Handles notification subscriptions, broadcasts, and targeted messaging
  across the swarm pipeline. Implements 80/20 principle for notification
  prioritization and delivery.
  """
  
  use ChannelHandler.Handler
  
  require Logger
  
  # Notification priorities (80/20 - focus on critical notifications)
  @priority_levels [:critical, :high, :normal, :low]
  @critical_types ["pipeline:failed", "security:breach", "system:down"]
  
  plug :check_notification_permission when action in [:broadcast]
  
  def handle_in("subscribe", payload, _bindings, socket) do
    Logger.info("📢 NOTIFICATION: Subscribe request")
    
    subscription = %{
      types: Map.get(payload, "types", ["all"]),
      priority: Map.get(payload, "min_priority", :normal),
      filters: Map.get(payload, "filters", %{})
    }
    
    socket = assign(socket, :notification_subscription, subscription)
    
    # Send recent notifications
    recent = get_recent_notifications(subscription)
    
    {:reply, {:ok, %{subscribed: true, recent_notifications: recent}}, socket}
  end
  
  def handle_in("unsubscribe", _payload, _bindings, socket) do
    Logger.info("📢 NOTIFICATION: Unsubscribe request")
    
    socket = assign(socket, :notification_subscription, nil)
    
    {:reply, {:ok, %{unsubscribed: true}}, socket}
  end
  
  def handle_in("broadcast", payload, _bindings, socket) do
    Logger.info("📢 NOTIFICATION: Broadcast request")
    
    notification = build_notification(payload, socket)
    
    # Apply 80/20 rule - critical notifications get priority
    if critical_notification?(notification) do
      broadcast_critical_notification(notification)
    else
      broadcast_standard_notification(notification)
    end
    
    {:reply, {:ok, %{broadcast: true, id: notification.id}}, socket}
  end
  
  def handle_in("send", payload, _bindings, socket) do
    Logger.info("📢 NOTIFICATION: Targeted send")
    
    target = Map.get(payload, "target")
    notification = build_notification(payload, socket)
    
    case send_targeted_notification(target, notification) do
      :ok ->
        {:reply, {:ok, %{sent: true, id: notification.id}}, socket}
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  def handle_in("acknowledge", payload, _bindings, socket) do
    notification_id = Map.get(payload, "notification_id")
    
    mark_as_acknowledged(notification_id, socket)
    
    {:reply, {:ok, %{acknowledged: true}}, socket}
  end
  
  def handle_in("get_history", payload, _bindings, socket) do
    filters = Map.get(payload, "filters", %{})
    limit = Map.get(payload, "limit", 50)
    
    history = get_notification_history(socket, filters, limit)
    
    {:reply, {:ok, history}, socket}
  end
  
  # Delegated catch-all
  def handle_in(event, _payload, _bindings, socket) do
    Logger.warn("📢 NOTIFICATION: Unknown event #{event}")
    {:reply, {:error, "Unknown notification event: #{event}"}, socket}
  end
  
  # Private functions
  
  defp check_notification_permission(socket, _payload, _bindings, _opts) do
    if can_broadcast_notifications?(socket) do
      {:cont, socket}
    else
      {:reply, {:error, "Broadcast permission denied"}, socket}
    end
  end
  
  defp can_broadcast_notifications?(socket) do
    socket.assigns[:access_level] in [:authenticated, :admin]
  end
  
  defp build_notification(payload, socket) do
    %{
      id: generate_notification_id(),
      type: Map.get(payload, "type", "generic"),
      priority: String.to_atom(Map.get(payload, "priority", "normal")),
      title: Map.get(payload, "title", "Notification"),
      message: Map.get(payload, "message", ""),
      data: Map.get(payload, "data", %{}),
      sender_id: socket.id,
      created_at: DateTime.utc_now(),
      acknowledged: false
    }
  end
  
  defp critical_notification?(notification) do
    notification.priority == :critical or
    notification.type in @critical_types
  end
  
  defp broadcast_critical_notification(notification) do
    Logger.warn("🚨 Broadcasting critical notification: #{notification.type}")
    
    # Store notification
    store_notification(notification)
    
    # Broadcast to all connections immediately
    CnsForgeWeb.Endpoint.broadcast!(
      "notifications:critical",
      "critical_notification",
      notification
    )
    
    # Also send to specific subscribers
    broadcast_to_subscribers(notification, :critical)
    
    # Trigger additional alerts
    trigger_critical_alerts(notification)
  end
  
  defp broadcast_standard_notification(notification) do
    Logger.info("📢 Broadcasting standard notification: #{notification.type}")
    
    # Store notification
    store_notification(notification)
    
    # Broadcast to subscribers based on their filters
    broadcast_to_subscribers(notification, :standard)
  end
  
  defp broadcast_to_subscribers(notification, mode) do
    # Get all active channel connections
    subscribers = get_active_subscribers()
    
    Enum.each(subscribers, fn {socket_id, subscription} ->
      if should_receive_notification?(subscription, notification, mode) do
        CnsForgeWeb.Endpoint.broadcast(
          "notifications:#{socket_id}",
          "new_notification",
          notification
        )
      end
    end)
  end
  
  defp should_receive_notification?(subscription, notification, mode) do
    # 80/20 rule - always deliver critical notifications
    if mode == :critical do
      true
    else
      # Check subscription filters
      type_matches?(subscription.types, notification.type) and
      priority_matches?(subscription.priority, notification.priority) and
      filters_match?(subscription.filters, notification)
    end
  end
  
  defp type_matches?(["all"], _type), do: true
  defp type_matches?(types, type), do: type in types
  
  defp priority_matches?(min_priority, notification_priority) do
    priority_level(notification_priority) >= priority_level(min_priority)
  end
  
  defp priority_level(:critical), do: 4
  defp priority_level(:high), do: 3
  defp priority_level(:normal), do: 2
  defp priority_level(:low), do: 1
  
  defp filters_match?(filters, notification) when map_size(filters) == 0, do: true
  defp filters_match?(filters, notification) do
    Enum.all?(filters, fn {key, value} ->
      get_in(notification.data, [String.to_atom(key)]) == value
    end)
  end
  
  defp send_targeted_notification(target, notification) do
    case find_target_socket(target) do
      {:ok, socket_id} ->
        CnsForgeWeb.Endpoint.broadcast(
          "notifications:#{socket_id}",
          "targeted_notification",
          notification
        )
        :ok
        
      :error ->
        {:error, "Target not found"}
    end
  end
  
  defp get_recent_notifications(subscription) do
    # Get last 10 notifications matching subscription
    stored_notifications()
    |> Enum.filter(fn notif ->
      should_receive_notification?(subscription, notif, :standard)
    end)
    |> Enum.take(10)
  end
  
  defp get_notification_history(socket, filters, limit) do
    stored_notifications()
    |> apply_history_filters(filters)
    |> Enum.take(limit)
  end
  
  defp apply_history_filters(notifications, filters) when map_size(filters) == 0 do
    notifications
  end
  defp apply_history_filters(notifications, filters) do
    Enum.filter(notifications, fn notif ->
      Enum.all?(filters, fn {key, value} ->
        Map.get(notif, String.to_atom(key)) == value
      end)
    end)
  end
  
  defp mark_as_acknowledged(notification_id, socket) do
    # Update notification as acknowledged
    Logger.info("📢 Notification #{notification_id} acknowledged by #{socket.id}")
    
    # In production, update persistent storage
    :ok
  end
  
  defp trigger_critical_alerts(notification) do
    # Additional alerting for critical notifications
    alert = %{
      source: "notification_system",
      type: notification.type,
      severity: "critical",
      notification_id: notification.id,
      timestamp: DateTime.utc_now()
    }
    
    # Send to monitoring system
    :telemetry.execute(
      [:notifications, :critical_alert],
      %{count: 1},
      alert
    )
  end
  
  defp store_notification(notification) do
    # In production, store in persistent storage
    # For demo, store in ETS or similar
    :ets.insert(:notifications, {notification.id, notification})
  end
  
  defp stored_notifications do
    # In production, query from persistent storage
    # For demo, return mock data
    [
      %{
        id: "notif_1",
        type: "pipeline:complete",
        priority: :normal,
        title: "Pipeline Completed",
        message: "80/20 pipeline executed successfully",
        created_at: DateTime.utc_now()
      },
      %{
        id: "notif_2",
        type: "telemetry:alert",
        priority: :high,
        title: "Performance Alert",
        message: "Latency exceeded threshold",
        created_at: DateTime.add(DateTime.utc_now(), -300, :second)
      }
    ]
  end
  
  defp get_active_subscribers do
    # In production, query from registry or state
    # For demo, return mock subscribers
    %{
      "socket_123" => %{
        types: ["all"],
        priority: :normal,
        filters: %{}
      }
    }
  end
  
  defp find_target_socket(target) do
    # In production, look up socket by user ID or other identifier
    case target do
      "admin" -> {:ok, "socket_admin"}
      "user_" <> id -> {:ok, "socket_#{id}"}
      _ -> :error
    end
  end
  
  defp generate_notification_id do
    "notif_#{System.unique_integer([:positive])}_#{:rand.uniform(10000)}"
  end
end