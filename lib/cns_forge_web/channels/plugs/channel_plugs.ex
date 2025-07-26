defmodule CnsForgeWeb.ChannelPlugs do
  @moduledoc """
  🔌 Channel Plugs for UltraThink Swarm
  Common plugs for authentication, rate limiting, and telemetry
  """
  
  defmodule EnsureAuthenticated do
    @moduledoc "Ensures channel connection is authenticated"
    
    def init(opts), do: opts
    
    def call(socket, _payload, _bindings, _opts) do
      if socket.assigns[:authenticated] do
        {:cont, socket}
      else
        {:reply, {:error, %{reason: "Unauthorized"}}, socket}
      end
    end
  end
  
  defmodule RateLimiter do
    @moduledoc "Rate limits channel events"
    
    def init(opts) do
      %{
        max_requests: opts[:max_requests] || 100,
        window_ms: opts[:window_ms] || 60_000
      }
    end
    
    def call(socket, _payload, _bindings, opts) do
      key = "rate_limit:#{socket.id}:#{socket.topic}"
      current_time = System.system_time(:millisecond)
      
      case check_rate_limit(key, current_time, opts) do
        :ok ->
          {:cont, socket}
          
        {:error, :rate_limited} ->
          {:reply, {:error, %{
            reason: "Rate limit exceeded",
            retry_after: opts.window_ms
          }}, socket}
      end
    end
    
    defp check_rate_limit(key, current_time, opts) do
      # Simple in-memory rate limiting
      # In production, use Redis or similar
      case :ets.lookup(:rate_limits, key) do
        [{^key, requests, window_start}] ->
          if current_time - window_start > opts.window_ms do
            # New window
            :ets.insert(:rate_limits, {key, 1, current_time})
            :ok
          else
            if requests >= opts.max_requests do
              {:error, :rate_limited}
            else
              :ets.update_counter(:rate_limits, key, {2, 1})
              :ok
            end
          end
          
        [] ->
          # First request
          :ets.insert(:rate_limits, {key, 1, current_time})
          :ok
      end
    end
  end
  
  defmodule TelemetryTracker do
    @moduledoc "Tracks channel telemetry"
    
    def init(opts), do: opts
    
    def call(socket, payload, bindings, _opts) do
      start_time = System.monotonic_time(:microsecond)
      
      # Add telemetry metadata to socket
      socket = assign(socket, :telemetry_start, start_time)
      
      # Execute telemetry event
      :telemetry.execute(
        [:cns_forge, :channel, :event],
        %{count: 1},
        %{
          topic: socket.topic,
          event: bindings[:event] || "unknown",
          transport: socket.transport
        }
      )
      
      {:cont, socket}
    end
  end
  
  defmodule PerformanceTracker do
    @moduledoc "Tracks channel performance metrics"
    
    def init(opts), do: opts
    
    def call(socket, _payload, bindings, _opts) do
      # Track performance metrics
      socket = socket
      |> assign(:performance_start, System.monotonic_time(:microsecond))
      |> assign(:event_name, bindings[:event])
      
      {:cont, socket}
    end
  end
  
  defmodule NotificationRateLimiter do
    @moduledoc "Special rate limiter for notification channels"
    
    def init(opts) do
      %{
        max_notifications_per_second: opts[:max_per_second] || 10,
        burst_limit: opts[:burst_limit] || 50
      }
    end
    
    def call(socket, _payload, _bindings, opts) do
      key = "notif_limit:#{socket.assigns.user_id}"
      
      case check_notification_limit(key, opts) do
        :ok ->
          {:cont, socket}
          
        {:error, :limit_exceeded} ->
          {:reply, {:error, %{
            reason: "Notification rate limit exceeded",
            max_per_second: opts.max_notifications_per_second
          }}, socket}
      end
    end
    
    defp check_notification_limit(key, opts) do
      # Token bucket algorithm for notifications
      current_time = System.system_time(:second)
      
      case :ets.lookup(:notification_buckets, key) do
        [{^key, tokens, last_refill}] ->
          # Calculate tokens to add
          seconds_passed = current_time - last_refill
          new_tokens = min(
            tokens + (seconds_passed * opts.max_notifications_per_second),
            opts.burst_limit
          )
          
          if new_tokens >= 1 do
            :ets.insert(:notification_buckets, {key, new_tokens - 1, current_time})
            :ok
          else
            {:error, :limit_exceeded}
          end
          
        [] ->
          # Initialize bucket
          :ets.insert(:notification_buckets, {key, opts.burst_limit - 1, current_time})
          :ok
      end
    end
  end
  
  defmodule CheckPermission do
    @moduledoc "Checks specific permissions for channel actions"
    
    def init(permission), do: permission
    
    def call(socket, _payload, _bindings, permission) do
      if has_permission?(socket, permission) do
        {:cont, socket}
      else
        {:reply, {:error, %{
          reason: "Permission denied",
          required_permission: permission
        }}, socket}
      end
    end
    
    defp has_permission?(socket, permission) do
      user = socket.assigns[:current_user]
      user && permission in (user.permissions || [])
    end
  end
  
  @doc """
  Initialize ETS tables for rate limiting
  Call this in your application start
  """
  def init_tables do
    :ets.new(:rate_limits, [:set, :public, :named_table, write_concurrency: true])
    :ets.new(:notification_buckets, [:set, :public, :named_table, write_concurrency: true])
  end
end