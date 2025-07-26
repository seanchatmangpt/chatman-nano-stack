defmodule CnsWeb.ChannelPlugs.EnsureAuthenticated do
  @moduledoc """
  Channel plug to ensure user is authenticated for protected operations.
  Implements 80/20 optimization - only authenticate for critical operations.
  """
  
  def call(socket, payload, bindings, _opts) do
    if authenticated?(socket) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Authentication required"}}, socket}
    end
  end
  
  defp authenticated?(socket) do
    case socket.assigns[:current_user] do
      nil -> false
      %{id: _} -> true
      _ -> false
    end
  end
end

defmodule CnsWeb.ChannelPlugs.OptimizationFilter do
  @moduledoc """
  Applies 80/20 optimization filtering to channel events.
  Filters non-critical events when in optimization mode.
  """
  
  def call(socket, payload, bindings, _opts) do
    if should_process_event?(socket, payload) do
      {:cont, socket, payload, bindings}
    else
      # Silently drop non-critical events in 80/20 mode
      {:noreply, socket}
    end
  end
  
  defp should_process_event?(socket, payload) do
    case socket.assigns[:optimization_mode] do
      "80_20" ->
        is_critical_event?(payload)
      _ ->
        true
    end
  end
  
  defp is_critical_event?(payload) do
    level = payload["level"] || "info"
    priority = payload["priority"] || "normal"
    
    level in ["critical", "error"] or
    priority in ["high", "critical"] or
    payload["force_process"] == true
  end
end

defmodule CnsWeb.ChannelPlugs.RateLimit do
  @moduledoc """
  Rate limiting for channel operations using Hammer.
  Implements adaptive limits based on operation criticality.
  """
  
  def call(socket, payload, bindings, opts) do
    max_requests = Keyword.get(opts, :max, 100)
    window_ms = Keyword.get(opts, :window, 60_000)
    
    # Create rate limit key
    user_id = socket.assigns[:current_user]&.id || "anonymous"
    key = "channel_rate_limit:#{user_id}"
    
    case Hammer.check_rate(key, window_ms, max_requests) do
      {:allow, _count} ->
        {:cont, socket, payload, bindings}
        
      {:deny, _limit} ->
        {:reply, {:error, %{
          reason: "Rate limit exceeded",
          retry_after: div(window_ms, 1000)
        }}, socket}
    end
  end
end

defmodule CnsWeb.ChannelPlugs.TrackMetrics do
  @moduledoc """
  Tracks channel operation metrics for swarm intelligence optimization.
  """
  
  def call(socket, payload, bindings, _opts) do
    start_time = System.monotonic_time(:millisecond)
    
    # Store start time for duration calculation
    socket = Phoenix.Socket.assign(socket, :operation_start_time, start_time)
    
    # Continue processing
    {:cont, socket, payload, bindings}
  end
  
  def after_call(socket, _payload, _bindings, result) do
    # Calculate operation duration
    start_time = socket.assigns[:operation_start_time]
    duration = System.monotonic_time(:millisecond) - (start_time || 0)
    
    # Track metrics
    :telemetry.execute([:cns, :channel, :operation], %{
      duration: duration
    }, %{
      user_id: socket.assigns[:current_user]&.id,
      optimization_mode: socket.assigns[:optimization_mode],
      result: extract_result_type(result)
    })
    
    result
  end
  
  defp extract_result_type({:reply, {:ok, _}, _}), do: "success"
  defp extract_result_type({:reply, {:error, _}, _}), do: "error"
  defp extract_result_type({:noreply, _}), do: "noreply"
  defp extract_result_type(_), do: "unknown"
end

defmodule CnsWeb.ChannelPlugs.CheckPermission do
  @moduledoc """
  Checks user permissions for specific operations.
  """
  
  def call(socket, payload, bindings, permission) do
    user = socket.assigns[:current_user]
    
    if has_permission?(user, permission) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Insufficient permissions"}}, socket}
    end
  end
  
  defp has_permission?(nil, _), do: false
  
  defp has_permission?(user, permission) do
    # Mock permission check - replace with actual authorization logic
    case permission do
      :manage_reactor -> user.role in ["admin", "reactor_manager"]
      :access_stage -> user.role in ["admin", "developer", "operator"]
      :do_secret_stuff -> user.role == "admin"
      _ -> false
    end
  end
end

defmodule CnsWeb.ChannelPlugs.CheckSwarmPermission do
  @moduledoc """
  Specialized permission check for swarm operations.
  """
  
  def call(socket, payload, bindings, _opts) do
    user = socket.assigns[:current_user]
    
    if can_access_swarm?(user, socket.assigns[:swarm_id]) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Swarm access denied"}}, socket}
    end
  end
  
  defp can_access_swarm?(nil, _), do: false
  
  defp can_access_swarm?(user, swarm_id) do
    # Check if user has access to this specific swarm
    user.role in ["admin", "swarm_operator"] or
    user_in_swarm?(user.id, swarm_id)
  end
  
  defp user_in_swarm?(user_id, swarm_id) do
    # Mock check - replace with actual swarm membership logic
    true
  end
end

defmodule CnsWeb.ChannelPlugs.RequireAdmin do
  @moduledoc """
  Requires admin role for sensitive operations.
  """
  
  def call(socket, payload, bindings, _opts) do
    user = socket.assigns[:current_user]
    
    if admin?(user) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, %{reason: "Admin access required"}}, socket}
    end
  end
  
  defp admin?(nil), do: false
  defp admin?(user), do: user.role == "admin"
end

defmodule CnsWeb.ChannelPlugs.BatchNotifications do
  @moduledoc """
  Batches notifications for efficiency in 80/20 mode.
  """
  
  def call(socket, payload, bindings, _opts) do
    if should_batch?(socket, payload) do
      batch_notification(socket, payload)
      {:noreply, socket}
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  defp should_batch?(socket, payload) do
    socket.assigns[:optimization_mode] == "80_20" and
    payload["level"] not in ["critical", "error"]
  end
  
  defp batch_notification(socket, payload) do
    # Add to batch queue
    batch = socket.assigns[:notification_batch] || []
    updated_batch = [payload | batch]
    
    socket = Phoenix.Socket.assign(socket, :notification_batch, updated_batch)
    
    # Process batch if it's full or after timeout
    if length(updated_batch) >= 10 do
      process_notification_batch(socket)
    end
  end
  
  defp process_notification_batch(socket) do
    batch = socket.assigns[:notification_batch] || []
    
    if batch != [] do
      # Send batched notifications
      Phoenix.Socket.push(socket, "notification:batch", %{
        notifications: batch,
        count: length(batch)
      })
      
      # Clear batch
      Phoenix.Socket.assign(socket, :notification_batch, [])
    end
  end
end