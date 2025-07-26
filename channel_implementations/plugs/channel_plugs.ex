# BitActor Channel Plugs
# Reusable plugs for channel authentication, authorization, and monitoring
# Implements 80/20 pattern for the most critical channel operations

defmodule BitActorWeb.ChannelPlugs do
  @moduledoc """
  Channel plugs for BitActor pipeline using ChannelHandler patterns.
  
  Provides reusable authentication, authorization, monitoring, and validation
  functionality across all channel handlers in the pipeline.
  """
  
  import Phoenix.Socket
  
  # TTL monitoring thresholds
  @ttl_warning_threshold_ns 800_000_000  # 800ms warning
  @ttl_critical_threshold_ns 1_000_000_000  # 1s critical
  
  @doc """
  Ensures user is authenticated before proceeding with channel operations
  """
  def ensure_authenticated(socket, payload, bindings, _opts) do
    if authenticated?(socket) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, "Authentication required"}, socket}
    end
  end
  
  @doc """
  Monitors channel operation performance and TTL compliance
  """
  def monitor_performance(socket, payload, bindings, _opts) do
    start_time = System.monotonic_time(:nanosecond)
    socket = assign(socket, :operation_start_time, start_time)
    
    # Continue processing and track completion time in after_reply hook
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Enforces TTL constraints for channel operations
  """
  def enforce_ttl_constraints(socket, payload, bindings, _opts) do
    case get_operation_ttl_budget(socket, payload) do
      {:ok, budget_ns} ->
        socket = socket
        |> assign(:ttl_budget_ns, budget_ns)
        |> assign(:ttl_start_time, System.monotonic_time(:nanosecond))
        
        {:cont, socket, payload, bindings}
        
      {:error, reason} ->
        {:reply, {:error, "TTL configuration error: #{reason}"}, socket}
    end
  end
  
  @doc """
  Validates Ash resource operations before execution
  """
  def validate_ash_resource(socket, payload, bindings, _opts) do
    case validate_resource_payload(payload) do
      :ok ->
        {:cont, socket, payload, bindings}
        
      {:error, validation_errors} ->
        {:reply, {:error, %{validation_errors: validation_errors}}, socket}
    end
  end
  
  @doc """
  Authorizes Ash actions based on user permissions
  """
  def authorize_ash_action(socket, payload, bindings, _opts) do
    resource = payload["resource"]
    action = payload["action"]
    user_id = socket.assigns.user_id
    
    if authorized_for_ash_action?(user_id, resource, action) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, "Unauthorized for action #{action} on #{resource}"}, socket}
    end
  end
  
  @doc """
  Tracks Ash resource changes for auditing and real-time updates
  """
  def track_resource_changes(socket, payload, bindings, _opts) do
    if should_track_changes?(payload) do
      tracking_id = generate_tracking_id()
      socket = assign(socket, :change_tracking_id, tracking_id)
      
      record_change_start(tracking_id, payload, socket)
      {:cont, socket, payload, bindings}
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  @doc """
  Validates type checking payloads for Typer stage
  """
  def validate_type_payload(socket, payload, bindings, _opts) do
    case validate_typing_data(payload) do
      :ok ->
        {:cont, socket, payload, bindings}
        
      {:error, type_errors} ->
        {:reply, {:error, %{type_validation_errors: type_errors}}, socket}
    end
  end
  
  @doc """
  Enforces Typer-specific TTL constraints
  """
  def enforce_typer_ttl(socket, payload, bindings, _opts) do
    action = socket.assigns[:action] || extract_action_from_payload(payload)
    
    typer_budget = case action do
      :validate -> 100_000_000   # 100ms
      :infer -> 200_000_000      # 200ms
      :analyze -> 300_000_000    # 300ms
      _ -> 150_000_000           # 150ms default
    end
    
    socket = assign(socket, :typer_ttl_budget_ns, typer_budget)
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Requires TTL monitoring capability for TTL2DSpy operations
  """
  def require_ttl_monitoring(socket, payload, bindings, _opts) do
    if has_ttl_monitoring_capability?(socket) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, "TTL monitoring capability required"}, socket}
    end
  end
  
  @doc """
  Enables high precision timing for TTL operations
  """
  def high_precision_timing(socket, payload, bindings, _opts) do
    precision_start = System.monotonic_time(:nanosecond)
    
    socket = socket
    |> assign(:high_precision_start, precision_start)
    |> assign(:precision_enabled, true)
    
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Validates workflow definitions for Reactor operations
  """
  def validate_workflow_definition(socket, payload, bindings, _opts) do
    case validate_reactor_workflow(payload) do
      :ok ->
        {:cont, socket, payload, bindings}
        
      {:error, workflow_errors} ->
        {:reply, {:error, %{workflow_validation_errors: workflow_errors}}, socket}
    end
  end
  
  @doc """
  Tracks workflow execution for monitoring and debugging
  """
  def track_workflow_execution(socket, payload, bindings, _opts) do
    execution_id = generate_execution_id()
    
    socket = socket
    |> assign(:workflow_execution_id, execution_id)
    |> assign(:workflow_tracking_enabled, true)
    
    start_workflow_tracking(execution_id, payload, socket)
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Monitors Reactor step performance in real-time
  """
  def monitor_step_performance(socket, payload, bindings, _opts) do
    if should_monitor_steps?(payload) do
      monitoring_id = start_step_monitoring(payload, socket)
      socket = assign(socket, :step_monitoring_id, monitoring_id)
      {:cont, socket, payload, bindings}
    else
      {:cont, socket, payload, bindings}
    end
  end
  
  @doc """
  Validates swarm agent configurations
  """
  def validate_swarm_agent(socket, payload, bindings, _opts) do
    case validate_agent_config(payload) do
      :ok ->
        {:cont, socket, payload, bindings}
        
      {:error, agent_errors} ->
        {:reply, {:error, %{agent_validation_errors: agent_errors}}, socket}
    end
  end
  
  @doc """
  Authorizes swarm operations based on user role
  """
  def authorize_swarm_operation(socket, payload, bindings, _opts) do
    user_role = get_user_role(socket)
    operation = extract_swarm_operation(payload)
    
    if authorized_for_swarm_operation?(user_role, operation) do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, "Insufficient permissions for swarm operation"}, socket}
    end
  end
  
  @doc """
  Monitors swarm health and performance
  """
  def monitor_swarm_health(socket, payload, bindings, _opts) do
    health_check_start = System.monotonic_time(:nanosecond)
    
    socket = socket
    |> assign(:swarm_health_check_start, health_check_start)
    |> assign(:swarm_monitoring_enabled, true)
    
    {:cont, socket, payload, bindings}
  end
  
  @doc """
  Requires monitor role for performance monitoring operations
  """
  def require_monitor_role(socket, payload, bindings, _opts) do
    user_role = get_user_role(socket)
    
    if user_role in [:admin, :monitor, :operator] do
      {:cont, socket, payload, bindings}
    else
      {:reply, {:error, "Monitor role required for this operation"}, socket}
    end
  end
  
  # Helper functions
  
  defp authenticated?(socket) do
    socket.assigns[:user_id] != nil
  end
  
  defp get_operation_ttl_budget(socket, payload) do
    stage = socket.assigns[:pipeline_stage]
    operation = extract_operation_type(payload)
    
    budget = case {stage, operation} do
      {:typer, :validate} -> 100_000_000
      {:typer, :infer} -> 200_000_000
      {:typer, :analyze} -> 300_000_000
      {:turtle, _} -> 150_000_000
      {:ttl2dspy, _} -> 50_000_000
      {:bitactor, _} -> 400_000_000
      {:erlang, _} -> 300_000_000
      {:ash, :query} -> 300_000_000
      {:ash, :create} -> 500_000_000
      {:ash, :update} -> 400_000_000
      {:ash, :destroy} -> 200_000_000
      {:reactor, _} -> 800_000_000
      {:k8s, _} -> 1_000_000_000
      _ -> 500_000_000  # Default budget
    end
    
    {:ok, budget}
  end
  
  defp extract_operation_type(payload) do
    cond do
      payload["action"] -> String.to_atom(payload["action"])
      payload["operation"] -> String.to_atom(payload["operation"])
      payload["type"] -> String.to_atom(payload["type"])
      true -> :unknown
    end
  end
  
  defp validate_resource_payload(payload) do
    required_fields = ["resource"]
    
    case check_required_fields(payload, required_fields) do
      :ok -> validate_resource_structure(payload)
      error -> error
    end
  end
  
  defp validate_resource_structure(payload) do
    resource = payload["resource"]
    
    cond do
      not is_binary(resource) ->
        {:error, ["Resource must be a string"]}
        
      String.length(resource) == 0 ->
        {:error, ["Resource name cannot be empty"]}
        
      not valid_resource_name?(resource) ->
        {:error, ["Invalid resource name format"]}
        
      true -> :ok
    end
  end
  
  defp valid_resource_name?(name) do
    Regex.match?(~r/^[a-zA-Z][a-zA-Z0-9_]*$/, name)
  end
  
  defp authorized_for_ash_action?(user_id, resource, action) do
    # Simplified authorization logic
    # In production, this would check against a proper authorization system
    user_permissions = get_user_permissions(user_id)
    
    permission_key = "#{resource}:#{action}"
    permission_key in user_permissions
  end
  
  defp get_user_permissions(user_id) do
    # Placeholder - would fetch from actual permission system
    [
      "post:create", "post:read", "post:update",
      "comment:create", "comment:read",
      "user:read", "user:update"
    ]
  end
  
  defp should_track_changes?(payload) do
    action = payload["action"]
    action in ["create", "update", "destroy"]
  end
  
  defp generate_tracking_id do
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end
  
  defp record_change_start(tracking_id, payload, socket) do
    # Record the start of a tracked change
    BitActorWeb.ChangeTracker.start_tracking(tracking_id, %{
      user_id: socket.assigns.user_id,
      payload: payload,
      started_at: System.monotonic_time(:nanosecond)
    })
  end
  
  defp validate_typing_data(payload) do
    case payload do
      %{"data" => data} when is_map(data) or is_list(data) ->
        :ok
        
      %{"value" => _value} ->
        :ok
        
      _ ->
        {:error, ["Missing or invalid typing data"]}
    end
  end
  
  defp extract_action_from_payload(payload) do
    case payload do
      %{"action" => action} -> String.to_atom(action)
      _ -> :unknown
    end
  end
  
  defp has_ttl_monitoring_capability?(socket) do
    capabilities = socket.assigns[:capabilities] || []
    :ttl_monitoring in capabilities
  end
  
  defp validate_reactor_workflow(payload) do
    case payload do
      %{"workflow" => workflow} when is_map(workflow) ->
        validate_workflow_structure(workflow)
        
      %{"steps" => steps} when is_list(steps) ->
        validate_steps_structure(steps)
        
      _ ->
        {:error, ["Missing or invalid workflow definition"]}
    end
  end
  
  defp validate_workflow_structure(workflow) do
    required_fields = ["steps"]
    
    case check_required_fields(workflow, required_fields) do
      :ok ->
        validate_steps_structure(workflow["steps"])
      error -> error
    end
  end
  
  defp validate_steps_structure(steps) when is_list(steps) do
    case Enum.find(steps, &(not valid_step?(&1))) do
      nil -> :ok
      invalid_step -> {:error, ["Invalid step structure: #{inspect(invalid_step)}"]}
    end
  end
  defp validate_steps_structure(_), do: {:error, ["Steps must be a list"]}
  
  defp valid_step?(step) when is_map(step) do
    Map.has_key?(step, "name") and Map.has_key?(step, "action")
  end
  defp valid_step?(_), do: false
  
  defp generate_execution_id do
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end
  
  defp start_workflow_tracking(execution_id, payload, socket) do
    BitActorWeb.WorkflowTracker.start_tracking(execution_id, %{
      user_id: socket.assigns.user_id,
      payload: payload,
      started_at: System.monotonic_time(:nanosecond)
    })
  end
  
  defp should_monitor_steps?(payload) do
    Map.get(payload, "monitor_steps", true)
  end
  
  defp start_step_monitoring(payload, socket) do
    monitoring_id = generate_monitoring_id()
    
    BitActorWeb.StepMonitor.start_monitoring(monitoring_id, %{
      user_id: socket.assigns.user_id,
      payload: payload,
      started_at: System.monotonic_time(:nanosecond)
    })
    
    monitoring_id
  end
  
  defp generate_monitoring_id do
    :crypto.strong_rand_bytes(12) |> Base.encode64()
  end
  
  defp validate_agent_config(payload) do
    case payload do
      %{"type" => type, "capabilities" => capabilities} 
      when is_binary(type) and is_list(capabilities) ->
        validate_agent_type_and_capabilities(type, capabilities)
        
      _ ->
        {:error, ["Missing or invalid agent configuration"]}
    end
  end
  
  defp validate_agent_type_and_capabilities(type, capabilities) do
    valid_types = ["worker", "coordinator", "monitor", "specialist"]
    valid_capabilities = ["processing", "coordination", "monitoring", "analysis"]
    
    cond do
      type not in valid_types ->
        {:error, ["Invalid agent type: #{type}"]}
        
      not Enum.all?(capabilities, &(&1 in valid_capabilities)) ->
        {:error, ["Invalid capabilities specified"]}
        
      true -> :ok
    end
  end
  
  defp get_user_role(socket) do
    socket.assigns[:user_role] || :user
  end
  
  defp extract_swarm_operation(payload) do
    case payload do
      %{"operation" => operation} -> operation
      %{"action" => action} -> action
      _ -> "unknown"
    end
  end
  
  defp authorized_for_swarm_operation?(user_role, operation) do
    swarm_permissions = %{
      admin: ["spawn", "terminate", "coordinate", "configure"],
      operator: ["spawn", "coordinate", "status"],
      monitor: ["status", "health_check"],
      user: ["status"]
    }
    
    allowed_operations = Map.get(swarm_permissions, user_role, [])
    operation in allowed_operations
  end
  
  defp check_required_fields(payload, required_fields) do
    missing_fields = Enum.filter(required_fields, fn field ->
      not Map.has_key?(payload, field)
    end)
    
    if Enum.empty?(missing_fields) do
      :ok
    else
      {:error, ["Missing required fields: #{Enum.join(missing_fields, ", ")}"]}
    end
  end
end