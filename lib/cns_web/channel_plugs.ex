defmodule CnsWeb.ChannelPlugs do
  @moduledoc """
  Channel plugs for authentication, authorization, and optimization.
  """
  
  alias Cns.{Accounts, Authorization, Metrics}
  
  @doc """
  Ensures user is authenticated before accessing swarm channels
  """
  def ensure_authenticated(socket, _payload, _bindings, _opts) do
    case socket.assigns[:current_user] do
      nil ->
        {:reply, {:error, %{reason: "Authentication required"}}, socket}
        
      user ->
        # Refresh user data and verify active status
        case Accounts.get_user(user.id) do
          {:ok, fresh_user} when fresh_user.active ->
            {:cont, assign(socket, :current_user, fresh_user), _payload, _bindings}
            
          {:ok, _inactive_user} ->
            {:reply, {:error, %{reason: "Account inactive"}}, socket}
            
          {:error, _} ->
            {:reply, {:error, %{reason: "Invalid user"}}, socket}
        end
    end
  end
  
  @doc """
  Requires admin privileges for sensitive operations
  """
  def require_admin(socket, _payload, _bindings, _opts) do
    case socket.assigns[:current_user] do
      %{role: role} when role in ["admin", "super_admin"] ->
        {:cont, socket, _payload, _bindings}
        
      _ ->
        {:reply, {:error, %{reason: "Admin access required"}}, socket}
    end
  end
  
  @doc """
  Checks specific permissions for channel operations
  """
  def check_permission(socket, _payload, _bindings, permission) do
    user = socket.assigns.current_user
    
    if Authorization.can?(user, permission) do
      {:cont, socket, _payload, _bindings}
    else
      {:reply, {:error, %{reason: "Insufficient permissions"}}, socket}
    end
  end
  
  @doc """
  Tracks metrics for channel events
  """
  def track_metrics(socket, payload, bindings, _opts) do
    # Track channel event metrics asynchronously
    Task.start(fn ->
      Metrics.track_channel_event(
        socket.assigns[:swarm_id],
        extract_event_name(bindings),
        %{
          user_id: socket.assigns[:current_user]&.id,
          payload_size: byte_size(:erlang.term_to_binary(payload)),
          timestamp: DateTime.utc_now()
        }
      )
    end)
    
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Validates reactor-specific payload structure
  """
  def validate_reactor_payload(socket, payload, bindings, _opts) do
    case validate_reactor_data(payload) do
      :ok ->
        {:cont, socket, payload, bindings}
        
      {:error, errors} ->
        {:reply, {:error, %{validation_errors: errors}}, socket}
    end
  end
  
  @doc """
  Applies rate limiting based on user and action type
  """
  def rate_limit(socket, _payload, bindings, opts) do
    action = Keyword.get(opts, :action, "default")
    user_id = socket.assigns.current_user.id
    
    {limit, window} = get_rate_limit(action, socket.assigns.current_user.role)
    key = "rate_limit:#{user_id}:#{action}"
    
    case Hammer.check_rate(key, window, limit) do
      {:allow, _count} ->
        {:cont, socket, _payload, bindings}
        
      {:deny, limit} ->
        {:reply, {:error, %{
          reason: "Rate limit exceeded",
          limit: limit,
          action: action
        }}, socket}
    end
  end
  
  @doc """
  Validates swarm membership and permissions
  """
  def validate_swarm_access(socket, _payload, _bindings, _opts) do
    user = socket.assigns.current_user
    swarm_id = socket.assigns.swarm_id
    
    case Authorization.can_access_swarm?(user, swarm_id) do
      true ->
        {:cont, socket, _payload, _bindings}
        
      false ->
        {:reply, {:error, %{reason: "Swarm access denied"}}, socket}
    end
  end
  
  @doc """
  Logs channel events for audit purposes
  """
  def audit_log(socket, payload, bindings, opts) do
    action = Keyword.get(opts, :action, extract_event_name(bindings))
    
    # Log audit event asynchronously
    Task.start(fn ->
      Cns.Audit.log_channel_event(%{
        user_id: socket.assigns.current_user.id,
        swarm_id: socket.assigns.swarm_id,
        action: action,
        payload: sanitize_payload_for_audit(payload),
        ip_address: socket.assigns[:remote_ip],
        user_agent: socket.assigns[:user_agent],
        timestamp: DateTime.utc_now()
      })
    end)
    
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Enforces 80/20 optimization rules globally
  """
  def enforce_80_20_optimization(socket, payload, bindings, _opts) do
    if socket.assigns.optimization_mode == "80_20" do
      # Apply 80/20 filtering to payload
      optimized_payload = apply_80_20_filtering(payload)
      
      # Check if event should be processed in 80/20 mode
      if should_process_in_80_20_mode?(payload, bindings) do
        {:cont, socket, optimized_payload, bindings}
      else
        # Silently drop non-critical events
        {:noreply, socket}
      end
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  @doc """
  Validates and sanitizes input payload
  """
  def sanitize_payload(socket, payload, bindings, _opts) do
    sanitized_payload = payload
    |> remove_dangerous_keys()
    |> limit_payload_size()
    |> validate_data_types()
    
    case sanitized_payload do
      {:error, reason} ->
        {:reply, {:error, %{reason: "Payload validation failed: #{reason}"}}, socket}
        
      clean_payload ->
        {:cont, socket, clean_payload, bindings}
    end
  end
  
  # Private helper functions
  
  defp extract_event_name(bindings) do
    case bindings do
      %{"event" => event} -> event
      [event | _] when is_binary(event) -> event
      _ -> "unknown"
    end
  end
  
  defp validate_reactor_data(payload) do
    errors = []
    
    # Validate required fields for reactor operations
    errors = if Map.has_key?(payload, "step") do
      case validate_step_name(payload["step"]) do
        :ok -> errors
        {:error, error} -> [error | errors]
      end
    else
      errors
    end
    
    # Validate workflow data if present
    errors = if Map.has_key?(payload, "workflow") do
      case validate_workflow_structure(payload["workflow"]) do
        :ok -> errors
        {:error, error} -> [error | errors]
      end
    else
      errors
    end
    
    case errors do
      [] -> :ok
      _ -> {:error, errors}
    end
  end
  
  defp validate_step_name(step_name) when is_binary(step_name) do
    if String.match?(step_name, ~r/^[a-zA-Z0-9_-]+$/) and String.length(step_name) <= 50 do
      :ok
    else
      {:error, "Invalid step name format"}
    end
  end
  
  defp validate_step_name(_), do: {:error, "Step name must be a string"}
  
  defp validate_workflow_structure(workflow) when is_map(workflow) do
    required_fields = ["name", "steps"]
    
    missing_fields = Enum.filter(required_fields, fn field ->
      not Map.has_key?(workflow, field)
    end)
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, "Missing required workflow fields: #{Enum.join(missing_fields, ", ")}"}
    end
  end
  
  defp validate_workflow_structure(_), do: {:error, "Workflow must be a map"}
  
  defp get_rate_limit(action, role) do
    base_limits = %{
      "execute" => {10, 60_000},  # 10 per minute
      "optimize" => {5, 60_000},   # 5 per minute
      "telemetry" => {100, 60_000}, # 100 per minute
      "notification" => {50, 60_000}, # 50 per minute
      "default" => {20, 60_000}    # 20 per minute
    }
    
    {limit, window} = Map.get(base_limits, action, base_limits["default"])
    
    # Increase limits for admin users
    adjusted_limit = case role do
      "admin" -> limit * 2
      "super_admin" -> limit * 5
      _ -> limit
    end
    
    {adjusted_limit, window}
  end
  
  defp sanitize_payload_for_audit(payload) do
    # Remove sensitive data from payload for audit logging
    payload
    |> Map.drop(["password", "token", "secret", "private_key"])
    |> Map.update("data", %{}, fn data ->
      if is_map(data) do
        Map.drop(data, ["password", "token", "secret"])
      else
        data
      end
    end)
  end
  
  defp apply_80_20_filtering(payload) do
    payload
    |> Map.update("batch_size", 20, fn size -> min(size, 20) end)
    |> Map.put("optimization", "80_20")
    |> Map.update("priority", "normal", fn 
      p when p in ["low", "normal"] -> "normal"
      p -> p
    end)
  end
  
  defp should_process_in_80_20_mode?(payload, _bindings) do
    # Process if marked as critical or high priority
    Map.get(payload, "critical", false) or
    Map.get(payload, "priority", "normal") in ["high", "critical"] or
    Map.get(payload, "force_process", false)
  end
  
  defp remove_dangerous_keys(payload) when is_map(payload) do
    dangerous_keys = ["__proto__", "constructor", "prototype", "eval", "function"]
    Map.drop(payload, dangerous_keys)
  end
  
  defp remove_dangerous_keys(payload), do: payload
  
  defp limit_payload_size(payload) do
    # Limit payload size to 1MB
    max_size = 1024 * 1024
    payload_size = byte_size(:erlang.term_to_binary(payload))
    
    if payload_size > max_size do
      {:error, "Payload too large"}
    else
      payload
    end
  end
  
  defp validate_data_types(payload) when is_map(payload) do
    # Ensure no functions or atoms (except allowed ones) in payload
    if contains_dangerous_types?(payload) do
      {:error, "Invalid data types in payload"}
    else
      payload
    end
  end
  
  defp validate_data_types({:error, _} = error), do: error
  defp validate_data_types(payload), do: payload
  
  defp contains_dangerous_types?(value) when is_map(value) do
    Enum.any?(value, fn {_k, v} -> contains_dangerous_types?(v) end)
  end
  
  defp contains_dangerous_types?(value) when is_list(value) do
    Enum.any?(value, &contains_dangerous_types?/1)
  end
  
  defp contains_dangerous_types?(value) when is_function(value), do: true
  defp contains_dangerous_types?(value) when is_atom(value) do
    # Allow common atoms but reject potentially dangerous ones
    allowed_atoms = [:ok, :error, :true, :false, :nil]
    value not in allowed_atoms
  end
  
  defp contains_dangerous_types?(_), do: false
end