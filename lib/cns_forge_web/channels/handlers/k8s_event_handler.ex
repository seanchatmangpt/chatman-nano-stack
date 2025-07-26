defmodule CnsForgeWeb.Channels.K8sEventHandler do
  @moduledoc """
  ☸️ K8s Event Handler for UltraThink Swarm Channels
  Processes Kubernetes cluster events in the reverse flow pipeline
  """
  
  use ChannelHandler.Handler
  require Logger
  
  # Apply rate limiting only for high-frequency events
  plug CnsForgeWeb.ChannelPlugs.RateLimiter, 
    max_requests: 50, 
    window_ms: 10_000 
    when event in ["pod:created", "pod:updated", "pod:deleted"]
  
  @doc """
  Handle generic K8s events with wildcard pattern
  """
  def handle_event(event_type, payload, _bindings, socket) do
    Logger.debug("☸️ K8s event received: #{event_type}")
    
    # Extract event details
    %{
      "namespace" => namespace,
      "resource" => resource,
      "action" => action,
      "metadata" => metadata
    } = payload
    
    # Process event based on type
    result = case event_type do
      "events:pod_" <> _ -> process_pod_event(resource, action, metadata, socket)
      "events:deployment_" <> _ -> process_deployment_event(resource, action, metadata, socket)
      "events:service_" <> _ -> process_service_event(resource, action, metadata, socket)
      "events:node_" <> _ -> process_node_event(resource, action, metadata, socket)
      _ -> {:error, "Unknown event type: #{event_type}"}
    end
    
    case result do
      {:ok, processed_event} ->
        # Trigger reverse flow
        trigger_reverse_flow(processed_event, socket)
        
        # Broadcast to subscribers
        CnsForgeWeb.Endpoint.broadcast!(
          "k8s:#{namespace}",
          "event_processed",
          processed_event
        )
        
        {:reply, {:ok, processed_event}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp process_pod_event(resource, action, metadata, socket) do
    event = %{
      type: "pod_event",
      resource: resource,
      action: action,
      pod_name: metadata["name"],
      pod_namespace: metadata["namespace"],
      pod_status: metadata["status"],
      timestamp: DateTime.utc_now()
    }
    
    # Update metrics
    update_k8s_metrics(event, socket)
    
    {:ok, event}
  end
  
  defp process_deployment_event(resource, action, metadata, socket) do
    event = %{
      type: "deployment_event",
      resource: resource,
      action: action,
      deployment_name: metadata["name"],
      replicas: metadata["replicas"],
      available_replicas: metadata["availableReplicas"],
      timestamp: DateTime.utc_now()
    }
    
    # Check for scaling events
    if action == "scaled" do
      notify_scaling_event(event, socket)
    end
    
    {:ok, event}
  end
  
  defp process_service_event(resource, action, metadata, socket) do
    event = %{
      type: "service_event",
      resource: resource,
      action: action,
      service_name: metadata["name"],
      service_type: metadata["type"],
      cluster_ip: metadata["clusterIP"],
      timestamp: DateTime.utc_now()
    }
    
    {:ok, event}
  end
  
  defp process_node_event(resource, action, metadata, socket) do
    event = %{
      type: "node_event",
      resource: resource,
      action: action,
      node_name: metadata["name"],
      node_status: metadata["status"],
      capacity: metadata["capacity"],
      timestamp: DateTime.utc_now()
    }
    
    # Check for node health issues
    if metadata["status"] != "Ready" do
      trigger_node_alert(event, socket)
    end
    
    {:ok, event}
  end
  
  defp trigger_reverse_flow(event, socket) do
    # Start the reverse flow pipeline
    Task.Supervisor.start_child(CnsForge.TaskSupervisor, fn ->
      CnsForge.UltraThinkSwarmReverseFlowNuxtOrchestrator.process_k8s_event(event)
    end)
  end
  
  defp update_k8s_metrics(event, socket) do
    # Update real-time metrics
    Phoenix.PubSub.broadcast(
      CnsForge.PubSub,
      "metrics:k8s",
      {:k8s_metrics_update, event}
    )
  end
  
  defp notify_scaling_event(event, socket) do
    # Send scaling notification
    push(socket, "k8s:scaling", event)
  end
  
  defp trigger_node_alert(event, socket) do
    # Send critical alert for node issues
    CnsForgeWeb.Endpoint.broadcast!(
      "alerts:critical",
      "node_issue",
      event
    )
  end
end