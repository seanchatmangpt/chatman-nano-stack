defmodule CnsWeb.UserSocket do
  @moduledoc """
  Phoenix Socket for Swarm channels with 80/20 optimization.
  Handles WebSocket connections and channel routing.
  """
  
  use Phoenix.Socket
  
  # Import presence for tracking swarm participants
  alias CnsWeb.Presence
  
  ## Channels
  
  # Main swarm channel with 80/20 optimization
  channel "swarm:*", CnsWeb.SwarmChannel
  
  # Ultrathink 80/20 Pipeline Channels
  channel "pipeline:*", CnsWeb.Channels.PipelineHandler  
  channel "notifications:*", CnsWeb.Channels.NotificationHandler
  channel "reactor:*", CnsWeb.Channels.ReactorHandler
  channel "swarm_intelligence:*", CnsWeb.Channels.SwarmIntelligenceHandler
  
  # 80/20 Optimized Stage Channels
  channel "stage:ash:*", CnsWeb.Channels.ReactorHandler.AshStageHandler
  channel "stage:reactor:*", CnsWeb.Channels.ReactorHandler.ReactorStageHandler  
  channel "stage:k8s:*", CnsWeb.Channels.ReactorHandler.K8sStageHandler
  
  # Legacy channels for backward compatibility
  channel "room:*", CnsWeb.RoomChannel
  channel "user:*", CnsWeb.UserChannel
  
  # Testing channels (only in dev/test)
  if Mix.env() in [:dev, :test] do
    channel "test:*", CnsWeb.TestChannel
  end
  
  # Socket params are passed from the client and can
  # be used to verify and authenticate a user. After
  # verification, you can put default assigns into
  # the socket that will be set for all channels, ie
  #
  #     {:ok, assign(socket, :user_id, verified_user_id)}
  #
  # To deny connection, return `:error`.
  #
  # See `Phoenix.Token` documentation for examples in
  # performing token verification on connect.
  
  @impl true
  def connect(params, socket, _connect_info) do
    case authenticate_user(params) do
      {:ok, user} ->
        socket = socket
        |> assign(:current_user, user)
        |> assign(:optimization_mode, Map.get(params, "optimization", "80_20"))
        |> assign(:remote_ip, get_remote_ip(socket))
        |> assign(:user_agent, get_user_agent(params))
        |> assign(:connected_at, DateTime.utc_now())
        |> assign(:swarm_config, get_swarm_config(params, user))
        |> assign(:notification_preferences, get_notification_preferences(user))
        |> assign(:rate_limits, get_user_rate_limits(user))
        
        {:ok, socket}
        
      {:error, _reason} ->
        :error
    end
  end
  
  # Socket id's are topics that allow you to identify all sockets for a given user:
  #
  #     def id(socket), do: "user_socket:#{socket.assigns.user_id}"
  #
  # Would allow you to broadcast a "disconnect" event and terminate
  # all active sockets and channels for a given user:
  #
  #     Elixir.CnsWeb.Endpoint.broadcast("user_socket:#{user.id}", "disconnect", %{})
  #
  # Returning `nil` makes this socket anonymous.
  @impl true
  def id(socket), do: "user_socket:#{socket.assigns.current_user.id}"
  
  # Authentication functions
  
  defp authenticate_user(%{"token" => token}) do
    case Phoenix.Token.verify(CnsWeb.Endpoint, "user socket", token, max_age: 86400) do
      {:ok, user_id} ->
        case Cns.Accounts.get_user(user_id) do
          {:ok, user} when user.active ->
            {:ok, user}
          {:ok, _inactive_user} ->
            {:error, :inactive_user}
          {:error, _} ->
            {:error, :invalid_user}
        end
        
      {:error, _} ->
        {:error, :invalid_token}
    end
  end
  
  defp authenticate_user(%{"api_key" => api_key}) do
    case Cns.Accounts.authenticate_by_api_key(api_key) do
      {:ok, user} -> {:ok, user}
      {:error, _} -> {:error, :invalid_api_key}
    end
  end
  
  defp authenticate_user(_params) do
    {:error, :missing_credentials}
  end
  
  defp get_remote_ip(socket) do
    case get_connect_info(socket, :peer_data) do
      %{address: address} -> :inet_parse.ntoa(address) |> to_string()
      _ -> "unknown"
    end
  end
  
  defp get_user_agent(params) do
    Map.get(params, "user_agent", "unknown")
  end
  
  # Swarm-specific configuration helpers
  
  defp get_swarm_config(params, user) do
    %{
      critical_stages: get_critical_stages(params["domain"]),
      optimization_thresholds: get_optimization_thresholds(user),
      pipeline_preferences: get_pipeline_preferences(user),
      auto_optimization: Map.get(params, "auto_optimization", false)
    }
  end
  
  defp get_critical_stages(domain) do
    case domain do
      "cybersecurity" -> ["typer", "turtle", "ash", "reactor", "k8s"]
      "finance" -> ["typer", "turtle", "bitactor", "ash", "k8s"]
      "healthcare" -> ["typer", "turtle", "ash", "k8s"]
      _ -> ["typer", "turtle", "ash", "reactor", "k8s"]
    end
  end
  
  defp get_optimization_thresholds(user) do
    case user.role do
      "admin" -> %{cpu: 95, memory: 90, latency: 500}
      "operator" -> %{cpu: 85, memory: 80, latency: 200}
      _ -> %{cpu: 80, memory: 75, latency: 150}
    end
  end
  
  defp get_pipeline_preferences(user) do
    # Load user's pipeline execution preferences
    %{
      preferred_optimization: "80_20",
      enable_parallel_execution: true,
      cache_intermediate_results: true,
      notification_level: "critical_only"
    }
  end
  
  defp get_notification_preferences(user) do
    %{
      channels: get_subscribed_channels(user),
      batch_size: get_batch_size(user),
      filters: get_notification_filters(user)
    }
  end
  
  defp get_subscribed_channels(user) do
    case user.role do
      "admin" -> ["error_alerts", "ash_resources", "reactor_workflows", "swarm_intelligence", "performance_metrics"]
      "operator" -> ["error_alerts", "ash_resources", "reactor_workflows", "pipeline_events"]
      _ -> ["error_alerts", "pipeline_events"]
    end
  end
  
  defp get_batch_size(user) do
    case user.role do
      "admin" -> 100
      "operator" -> 50
      _ -> 20
    end
  end
  
  defp get_notification_filters(user) do
    %{
      priority_threshold: if(user.role == "admin", do: "info", else: "warning"),
      muted_channels: [],
      rate_limit: get_notification_rate_limit(user)
    }
  end
  
  defp get_notification_rate_limit(user) do
    case user.role do
      "admin" -> 1000  # per minute
      "operator" -> 500
      _ -> 100
    end
  end
  
  defp get_user_rate_limits(user) do
    case user.role do
      "admin" -> %{
        channel_operations: 10000,
        pipeline_executions: 500,
        swarm_optimizations: 200
      }
      "operator" -> %{
        channel_operations: 5000,
        pipeline_executions: 100,
        swarm_optimizations: 50
      }
      _ -> %{
        channel_operations: 1000,
        pipeline_executions: 20,
        swarm_optimizations: 5
      }
    end
  end
end