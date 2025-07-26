defmodule CnsForgeWeb.NotificationBroadcaster do
  @moduledoc """
  Centralized notification broadcasting system for the ULTRATHINK 80/20 pipeline.
  Manages real-time notifications across all channels and stages.
  """
  
  alias Phoenix.PubSub
  require Logger
  
  @pubsub CnsForge.PubSub
  
  # Notification channels and their configurations
  @notification_channels %{
    websocket: %{
      topic_prefix: "ws:",
      broadcast_fn: &broadcast_websocket/2,
      enabled: true
    },
    pubsub: %{
      topic_prefix: "pubsub:",
      broadcast_fn: &broadcast_pubsub/2,
      enabled: true
    },
    k8s_events: %{
      topic_prefix: "k8s:",
      broadcast_fn: &broadcast_k8s_event/2,
      enabled: true
    },
    webhooks: %{
      topic_prefix: "webhooks:",
      broadcast_fn: &broadcast_webhook/2,
      enabled: true
    },
    bitactor: %{
      topic_prefix: "bitactor:",
      broadcast_fn: &broadcast_bitactor/2,
      enabled: true
    },
    genserver: %{
      topic_prefix: "genserver:",
      broadcast_fn: &broadcast_genserver/2,
      enabled: true
    },
    nuxt_realtime: %{
      topic_prefix: "nuxt:",
      broadcast_fn: &broadcast_nuxt_realtime/2,
      enabled: true
    }
  }
  
  @doc """
  Broadcast a stage event across all enabled notification channels
  """
  def broadcast_stage_event(socket, event_type, payload) do
    notification = build_stage_notification(socket, event_type, payload)
    
    # Broadcast to all enabled channels
    @notification_channels
    |> Enum.filter(fn {_name, config} -> config.enabled end)
    |> Enum.each(fn {channel, config} ->
      spawn(fn ->
        try do
          config.broadcast_fn.(notification, channel)
        rescue
          error ->
            Logger.error("Failed to broadcast to #{channel}: #{inspect(error)}")
        end
      end)
    end)
    
    # Track the broadcast
    track_broadcast_metrics(notification, event_type)
    
    notification.id
  end
  
  @doc """
  Broadcast to specific notification channels
  """
  def broadcast_to_channels(socket, event_type, payload, channels) when is_list(channels) do
    notification = build_stage_notification(socket, event_type, payload)
    
    Enum.each(channels, fn channel ->
      if config = @notification_channels[channel] do
        spawn(fn ->
          try do
            config.broadcast_fn.(notification, channel)
          rescue
            error ->
              Logger.error("Failed to broadcast to #{channel}: #{inspect(error)}")
          end
        end)
      else
        Logger.warn("Unknown notification channel: #{channel}")
      end
    end)
    
    notification.id
  end
  
  @doc """
  Broadcast pipeline lifecycle events
  """
  def broadcast_pipeline_event(pipeline_id, event_type, payload) do
    notification = %{
      id: generate_notification_id(),
      type: :pipeline_event,
      event: event_type,
      pipeline_id: pipeline_id,
      payload: payload,
      timestamp: System.system_time(:millisecond),
      metadata: %{
        source: "pipeline_orchestrator",
        priority: get_event_priority(event_type)
      }
    }
    
    # Broadcast to pipeline-specific topics
    topic = "pipeline:#{pipeline_id}"
    
    Phoenix.Channel.broadcast(@pubsub, topic, event_type, notification)
    
    # Also broadcast to global pipeline events
    Phoenix.Channel.broadcast(@pubsub, "pipeline:events", event_type, notification)
    
    notification.id
  end
  
  @doc """
  Broadcast performance metrics and alerts
  """
  def broadcast_metrics(metrics, alert_level \\ :info) do
    notification = %{
      id: generate_notification_id(),
      type: :metrics,
      alert_level: alert_level,
      metrics: metrics,
      timestamp: System.system_time(:millisecond),
      metadata: %{
        source: "metrics_collector",
        priority: alert_priority(alert_level)
      }
    }
    
    # Route based on alert level
    case alert_level do
      :critical ->
        broadcast_critical_alert(notification)
        
      :warning ->
        broadcast_warning_alert(notification)
        
      _ ->
        broadcast_info_metrics(notification)
    end
    
    notification.id
  end
  
  @doc """
  Broadcast BitActor coordination events
  """
  def broadcast_actor_event(actor_id, event_type, payload) do
    notification = %{
      id: generate_notification_id(),
      type: :actor_event,
      event: event_type,
      actor_id: actor_id,
      payload: payload,
      timestamp: System.system_time(:millisecond),
      metadata: %{
        source: "bitactor_system",
        priority: :normal
      }
    }
    
    # Broadcast to actor-specific and general actor topics
    Phoenix.Channel.broadcast(@pubsub, "actor:#{actor_id}", event_type, notification)
    Phoenix.Channel.broadcast(@pubsub, "actors:events", event_type, notification)
    
    # Send to BitActor mesh if available
    spawn(fn ->
      CnsForge.BitActor.MeshNetwork.broadcast_event(notification)
    end)
    
    notification.id
  end
  
  # Private implementation functions
  
  defp build_stage_notification(socket, event_type, payload) do
    %{
      id: generate_notification_id(),
      type: :stage_event,
      event: event_type,
      stage: socket.assigns[:current_stage],
      pipeline_id: socket.assigns[:pipeline_id],
      user_id: socket.assigns[:current_user][:id],
      payload: payload,
      timestamp: System.system_time(:millisecond),
      metadata: %{
        topic: socket.topic,
        channel_pid: socket.channel_pid,
        priority: get_event_priority(event_type)
      }
    }
  end
  
  # Channel-specific broadcast implementations
  
  defp broadcast_websocket(notification, _channel) do
    topic = "ws:pipeline:#{notification.pipeline_id}"
    
    Phoenix.Channel.broadcast(@pubsub, topic, "notification", %{
      id: notification.id,
      type: notification.event,
      data: notification.payload,
      timestamp: notification.timestamp,
      stage: notification.stage
    })
    
    # Also send to user-specific WebSocket if available
    if user_id = notification[:user_id] do
      Phoenix.Channel.broadcast(@pubsub, "user:#{user_id}", "notification", notification)
    end
  end
  
  defp broadcast_pubsub(notification, _channel) do
    topics = [
      "pubsub:pipeline:#{notification.pipeline_id}",
      "pubsub:stage:#{notification.stage}",
      "pubsub:events:all"
    ]
    
    Enum.each(topics, fn topic ->
      PubSub.broadcast(@pubsub, topic, {:stage_notification, notification})
    end)
  end
  
  defp broadcast_k8s_event(notification, _channel) do
    # Send to Kubernetes event API
    k8s_event = %{
      apiVersion: "v1",
      kind: "Event",
      metadata: %{
        name: "cns-forge-#{notification.id}",
        namespace: "cns-forge",
        labels: %{
          "cns-forge/pipeline-id" => notification.pipeline_id,
          "cns-forge/stage" => notification.stage,
          "cns-forge/event-type" => notification.event
        }
      },
      involvedObject: %{
        apiVersion: "apps/v1",
        kind: "Deployment",
        name: "cns-forge-pipeline",
        namespace: "cns-forge"
      },
      reason: format_event_reason(notification.event),
      message: format_k8s_message(notification),
      type: get_k8s_event_type(notification.event),
      eventTime: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    
    # Send to K8s API (this would be actual K8s client call in production)
    spawn(fn ->
      CnsForge.K8s.EventClient.create_event(k8s_event)
    end)
  end
  
  defp broadcast_webhook(notification, _channel) do
    webhook_payload = %{
      event: notification.event,
      pipeline_id: notification.pipeline_id,
      stage: notification.stage,
      data: notification.payload,
      timestamp: notification.timestamp,
      signature: generate_webhook_signature(notification)
    }
    
    # Send to all configured webhooks
    spawn(fn ->
      CnsForge.Webhooks.Dispatcher.send_webhook(webhook_payload)
    end)
  end
  
  defp broadcast_bitactor(notification, _channel) do
    # Send to BitActor mesh network
    bitactor_message = %{
      type: :notification,
      source: :pipeline,
      target: :all,
      payload: notification
    }
    
    spawn(fn ->
      CnsForge.BitActor.MeshNetwork.broadcast(bitactor_message)
    end)
  end
  
  defp broadcast_genserver(notification, _channel) do
    # Send to GenServer processes that are subscribed
    message = {:pipeline_notification, notification}
    
    # Broadcast to registered processes
    spawn(fn ->
      CnsForge.ProcessRegistry.broadcast(message)
    end)
  end
  
  defp broadcast_nuxt_realtime(notification, _channel) do
    # Format for Nuxt.js real-time updates (NO TypeScript)
    nuxt_payload = %{
      id: notification.id,
      event: notification.event,
      pipeline: notification.pipeline_id,
      stage: notification.stage,
      data: notification.payload,
      timestamp: notification.timestamp,
      ui_update: %{
        type: "stage_progress",
        component: "pipeline-monitor",
        action: "update"
      }
    }
    
    # Send to Nuxt.js SSE endpoint
    Phoenix.Channel.broadcast(@pubsub, "nuxt:realtime", "update", nuxt_payload)
    
    # Also send to specific UI channels
    Phoenix.Channel.broadcast(@pubsub, "ui:pipeline:#{notification.pipeline_id}", "update", nuxt_payload)
  end
  
  # Alert-specific broadcasting
  
  defp broadcast_critical_alert(notification) do
    # Critical alerts go to all channels immediately
    channels = [:websocket, :pubsub, :k8s_events, :webhooks, :genserver]
    
    Enum.each(channels, fn channel ->
      config = @notification_channels[channel]
      spawn(fn ->
        config.broadcast_fn.(notification, channel)
      end)
    end)
    
    # Also trigger emergency notifications
    spawn(fn ->
      CnsForge.Emergency.Notifications.trigger_alert(notification)
    end)
  end
  
  defp broadcast_warning_alert(notification) do
    # Warning alerts to main channels
    channels = [:websocket, :pubsub, :webhooks]
    
    Enum.each(channels, fn channel ->
      config = @notification_channels[channel]
      spawn(fn ->
        config.broadcast_fn.(notification, channel)
      end)
    end)
  end
  
  defp broadcast_info_metrics(notification) do
    # Info metrics to monitoring channels
    Phoenix.Channel.broadcast(@pubsub, "metrics:info", "update", notification)
  end
  
  # Utility functions
  
  defp generate_notification_id do
    "notif_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
  end
  
  defp get_event_priority(event_type) do
    case event_type do
      event when event in ["error", "failed", "timeout"] -> :high
      event when event in ["warning", "retry"] -> :medium
      _ -> :normal
    end
  end
  
  defp alert_priority(:critical), do: :high
  defp alert_priority(:warning), do: :medium
  defp alert_priority(_), do: :normal
  
  defp format_event_reason(event) do
    event
    |> to_string()
    |> String.split(":")
    |> List.last()
    |> String.replace("_", " ")
    |> String.capitalize()
  end
  
  defp format_k8s_message(notification) do
    "Pipeline #{notification.pipeline_id} stage '#{notification.stage}': #{notification.event}"
  end
  
  defp get_k8s_event_type(event) do
    case event do
      event when event in ["error", "failed"] -> "Warning"
      _ -> "Normal"
    end
  end
  
  defp generate_webhook_signature(notification) do
    secret = Application.get_env(:cns_forge, :webhook_secret, "default_secret")
    payload = Jason.encode!(notification)
    
    :crypto.mac(:hmac, :sha256, secret, payload)
    |> Base.encode16(case: :lower)
  end
  
  defp track_broadcast_metrics(notification, event_type) do
    :telemetry.execute(
      [:cns_forge, :notifications, :broadcast],
      %{count: 1},
      %{
        event_type: event_type,
        stage: notification.stage,
        pipeline_id: notification.pipeline_id,
        channels: length(Map.keys(@notification_channels))
      }
    )
  end
end