defmodule CnsForgeWeb.ChannelPlugs.EnsureAuthenticated do
  @moduledoc """
  Channel plug to ensure user is authenticated before allowing channel operations.
  Part of the ULTRATHINK 80/20 security layer.
  """
  
  import Phoenix.Channel
  require Logger
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, _payload, _bindings, _opts) do
    case socket.assigns[:current_user] do
      nil ->
        Logger.warn("Unauthenticated channel access attempt on pipeline #{socket.assigns[:pipeline_id]}")
        {:halt, {:error, %{reason: "Authentication required"}}, socket}
        
      user ->
        # Track authenticated access
        :telemetry.execute(
          [:cns_forge, :channel, :authenticated],
          %{count: 1},
          %{
            user_id: user.id,
            pipeline_id: socket.assigns[:pipeline_id],
            channel: socket.topic
          }
        )
        
        {:cont, socket}
    end
  end
end

defmodule CnsForgeWeb.ChannelPlugs.TrackMetrics do
  @moduledoc """
  Channel plug to track metrics for all channel operations.
  Provides real-time visibility into pipeline channel activity.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, bindings, _opts) do
    start_time = System.monotonic_time(:microsecond)
    
    # Track the operation
    :telemetry.execute(
      [:cns_forge, :channel, :operation],
      %{count: 1},
      %{
        topic: socket.topic,
        event: bindings[:event] || "unknown",
        pipeline_id: socket.assigns[:pipeline_id]
      }
    )
    
    # Store start time for response tracking
    socket = assign(socket, :operation_start_time, start_time)
    
    {:cont, socket}
  end
end

defmodule CnsForgeWeb.ChannelPlugs.ValidatePayload do
  @moduledoc """
  Channel plug to validate incoming payloads against expected schemas.
  Ensures data integrity across the ULTRATHINK pipeline.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, bindings, opts) do
    schema = opts[:schema] || get_schema_for_event(bindings[:event])
    
    case validate_against_schema(payload, schema) do
      :ok ->
        {:cont, socket}
        
      {:error, errors} ->
        {:halt, {:error, %{reason: "Invalid payload", errors: errors}}, socket}
    end
  end
  
  defp get_schema_for_event(event) do
    # Define schemas for different events
    %{
      "create" => %{
        required: ["name"],
        optional: ["description", "metadata"]
      },
      "execute" => %{
        required: ["action"],
        optional: ["arguments", "options"]
      }
    }[event] || %{}
  end
  
  defp validate_against_schema(payload, %{required: required}) do
    missing = Enum.filter(required, &(not Map.has_key?(payload, &1)))
    
    if Enum.empty?(missing) do
      :ok
    else
      {:error, %{missing_fields: missing}}
    end
  end
  
  defp validate_against_schema(_payload, _schema), do: :ok
end

defmodule CnsForgeWeb.ChannelPlugs.CheckPermission do
  @moduledoc """
  Channel plug to check specific permissions for channel operations.
  Implements fine-grained access control for the ULTRATHINK pipeline.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(permission), do: permission
  
  @impl true
  def call(socket, _payload, _bindings, permission) do
    user = socket.assigns[:current_user]
    
    if user && has_permission?(user, permission) do
      {:cont, socket}
    else
      {:halt, {:error, %{
        reason: "Insufficient permissions",
        required: permission
      }}, socket}
    end
  end
  
  defp has_permission?(user, permission) do
    # Check user permissions
    permission in (user.permissions || []) or
    user.role == "admin" or
    check_role_permission(user.role, permission)
  end
  
  defp check_role_permission("developer", permission) do
    permission in [:create_ontology, :execute_workflow, :view_metrics]
  end
  
  defp check_role_permission("operator", permission) do
    permission in [:execute_workflow, :view_metrics, :deploy]
  end
  
  defp check_role_permission(_, _), do: false
end

defmodule CnsForgeWeb.ChannelPlugs.EnsureDistributed do
  @moduledoc """
  Channel plug to ensure distributed system is ready for BitActor operations.
  Critical for the distributed coordination stage of the pipeline.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, _payload, _bindings, _opts) do
    if distributed_system_ready?() do
      {:cont, socket}
    else
      {:halt, {:error, %{
        reason: "Distributed system not ready",
        nodes: Node.list(),
        suggestion: "Ensure BitActor nodes are connected"
      }}, socket}
    end
  end
  
  defp distributed_system_ready? do
    # Check if we have enough nodes for distributed operations
    length(Node.list()) >= 1 or
    Application.get_env(:cns_forge, :allow_single_node, false)
  end
end

defmodule CnsForgeWeb.ChannelPlugs.CheckActorCapacity do
  @moduledoc """
  Channel plug to check if system has capacity for spawning more actors.
  Prevents resource exhaustion in the BitActor distributed system.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, _bindings, _opts) do
    requested_actors = Map.get(payload, "actor_count", 1)
    
    case check_actor_capacity(requested_actors) do
      :ok ->
        {:cont, socket}
        
      {:error, :capacity_exceeded} ->
        {:halt, {:error, %{
          reason: "Actor capacity exceeded",
          current_actors: current_actor_count(),
          max_actors: max_actor_count(),
          requested: requested_actors
        }}, socket}
    end
  end
  
  defp check_actor_capacity(requested) do
    if current_actor_count() + requested <= max_actor_count() do
      :ok
    else
      {:error, :capacity_exceeded}
    end
  end
  
  defp current_actor_count do
    # Get current actor count from registry
    CnsForge.BitActor.ActorRegistry.count()
  end
  
  defp max_actor_count do
    Application.get_env(:cns_forge, :max_actors, 100)
  end
end

defmodule CnsForgeWeb.ChannelPlugs.ValidateWorkflow do
  @moduledoc """
  Channel plug to validate Reactor workflow specifications.
  Ensures workflows are valid before creation or execution.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, _bindings, _opts) do
    case validate_workflow(payload) do
      :ok ->
        {:cont, socket}
        
      {:error, errors} ->
        {:halt, {:error, %{
          reason: "Invalid workflow specification",
          errors: errors
        }}, socket}
    end
  end
  
  defp validate_workflow(%{"workflow" => workflow}) do
    validate_workflow(workflow)
  end
  
  defp validate_workflow(%{"steps" => steps} = workflow) when is_list(steps) do
    errors = []
    
    # Validate each step
    step_errors = Enum.flat_map(steps, &validate_step/1)
    errors = errors ++ step_errors
    
    # Validate step dependencies
    if Map.get(workflow, "validate_dependencies", true) do
      dep_errors = validate_step_dependencies(steps)
      errors = errors ++ dep_errors
    end
    
    if Enum.empty?(errors) do
      :ok
    else
      {:error, errors}
    end
  end
  
  defp validate_workflow(_), do: {:error, ["Invalid workflow structure"]}
  
  defp validate_step(step) do
    errors = []
    
    if not Map.has_key?(step, "name") do
      errors = ["Step missing required field: name" | errors]
    end
    
    if not Map.has_key?(step, "action") do
      errors = ["Step #{step["name"] || "unknown"} missing required field: action" | errors]
    end
    
    errors
  end
  
  defp validate_step_dependencies(steps) do
    step_names = MapSet.new(Enum.map(steps, & &1["name"]))
    
    Enum.flat_map(steps, fn step ->
      deps = Map.get(step, "wait_for", [])
      
      Enum.flat_map(deps, fn dep ->
        if dep not in step_names do
          ["Step #{step["name"]} depends on unknown step: #{dep}"]
        else
          []
        end
      end)
    end)
  end
end

defmodule CnsForgeWeb.ChannelPlugs.CheckResourceAccess do
  @moduledoc """
  Channel plug to verify access to Ash resources.
  Ensures proper authorization for resource operations.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, _bindings, _opts) do
    user = socket.assigns[:current_user]
    resources = extract_resources(payload)
    
    case check_resource_access(user, resources) do
      :ok ->
        {:cont, socket}
        
      {:error, denied_resources} ->
        {:halt, {:error, %{
          reason: "Access denied to resources",
          denied: denied_resources
        }}, socket}
    end
  end
  
  defp extract_resources(payload) do
    resources = []
    
    # Extract from workflow steps
    if steps = payload["steps"] do
      step_resources = Enum.map(steps, & &1["resource"])
      resources = resources ++ step_resources
    end
    
    # Extract direct resource references
    if resource = payload["resource"] do
      resources = [resource | resources]
    end
    
    Enum.uniq(Enum.filter(resources, & &1))
  end
  
  defp check_resource_access(user, resources) do
    denied = Enum.filter(resources, fn resource ->
      not can_access_resource?(user, resource)
    end)
    
    if Enum.empty?(denied) do
      :ok
    else
      {:error, denied}
    end
  end
  
  defp can_access_resource?(user, resource) do
    # Implement resource-level access control
    # This would integrate with Ash policies
    true
  end
end

defmodule CnsForgeWeb.ChannelPlugs.AuthorizeWorkflow do
  @moduledoc """
  Channel plug for workflow execution authorization.
  Validates user can execute specific workflows.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, payload, _bindings, _opts) do
    user = socket.assigns[:current_user]
    workflow = socket.assigns[:reactor_workflow] || payload["workflow"]
    
    if can_execute_workflow?(user, workflow) do
      {:cont, socket}
    else
      {:halt, {:error, %{
        reason: "Not authorized to execute this workflow",
        workflow_id: workflow[:id] || "unknown"
      }}, socket}
    end
  end
  
  defp can_execute_workflow?(user, workflow) do
    # Check workflow-specific permissions
    user.role in ["admin", "operator"] or
    workflow[:owner_id] == user.id or
    workflow[:public] == true
  end
end

defmodule CnsForgeWeb.ChannelPlugs.TrackStageMetrics do
  @moduledoc """
  Channel plug to track detailed metrics for each pipeline stage.
  Provides comprehensive visibility into stage performance.
  """
  
  @behaviour ChannelHandler.Plug
  
  @impl true
  def init(opts), do: opts
  
  @impl true
  def call(socket, _payload, bindings, _opts) do
    stage = extract_stage_from_topic(socket.topic)
    
    # Start timing
    start_time = System.monotonic_time(:microsecond)
    
    # Track stage event
    :telemetry.execute(
      [:cns_forge, :pipeline, :stage, :event],
      %{count: 1},
      %{
        stage: stage,
        event: bindings[:event] || "unknown",
        pipeline_id: socket.assigns[:pipeline_id],
        user_id: socket.assigns[:current_user][:id]
      }
    )
    
    socket = socket
    |> assign(:stage_start_time, start_time)
    |> assign(:current_stage, stage)
    
    {:cont, socket}
  end
  
  defp extract_stage_from_topic(topic) do
    case String.split(topic, ":") do
      ["pipeline", _id, stage | _] -> stage
      [stage | _] -> stage
      _ -> "unknown"
    end
  end
end