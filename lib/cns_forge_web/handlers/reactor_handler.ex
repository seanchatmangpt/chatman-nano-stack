defmodule CnsForgeWeb.Handlers.ReactorHandler do
  @moduledoc """
  Handler for Reactor Workflow stage in the ULTRATHINK pipeline.
  Manages workflow creation, execution, and step-by-step notifications.
  """
  
  use ChannelHandler.Handler
  
  alias CnsForge.Reactor.{WorkflowEngine, StepExecutor, WorkflowMonitor}
  alias CnsForgeWeb.NotificationBroadcaster
  
  require Logger
  
  # Plugs for workflow validation
  plug CnsForgeWeb.ChannelPlugs.ValidateWorkflow
  plug CnsForgeWeb.ChannelPlugs.CheckResourceAccess
  
  # Specific authorization for workflow execution
  plug CnsForgeWeb.ChannelPlugs.AuthorizeWorkflow when action in [:execute_workflow]
  
  @doc """
  Create a new Reactor workflow
  """
  def create_workflow(payload, _bindings, socket) do
    Logger.info("Creating Reactor workflow for pipeline #{socket.assigns.pipeline_id}")
    
    with {:ok, workflow_spec} <- validate_workflow_spec(payload),
         {:ok, resources} <- get_ash_resources(socket),
         {:ok, workflow} <- build_reactor_workflow(workflow_spec, resources) do
      
      socket = assign(socket, :reactor_workflow, workflow)
      
      # Broadcast workflow creation
      NotificationBroadcaster.broadcast_stage_event(socket, "reactor:workflow_created", %{
        workflow_id: workflow.id,
        steps: length(workflow.steps),
        resources: length(workflow.resources),
        triggers: workflow.triggers
      })
      
      {:reply, {:ok, %{
        message: "Reactor workflow created successfully",
        workflow_id: workflow.id,
        workflow_summary: summarize_workflow(workflow)
      }}, socket}
    else
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Execute a Reactor workflow with real-time step notifications
  """
  def execute_workflow(payload, _bindings, socket) do
    Logger.info("Executing Reactor workflow for pipeline #{socket.assigns.pipeline_id}")
    
    workflow = socket.assigns[:reactor_workflow] || payload["workflow"]
    execution_options = Map.get(payload, "options", %{})
    
    # Start workflow execution with real-time monitoring
    case WorkflowEngine.execute(workflow, execution_options) do
      {:ok, execution_pid} ->
        # Monitor the execution
        monitor_ref = WorkflowMonitor.monitor(execution_pid, self())
        
        socket = socket
        |> assign(:workflow_execution_pid, execution_pid)
        |> assign(:workflow_monitor_ref, monitor_ref)
        |> assign(:workflow_start_time, System.monotonic_time(:millisecond))
        
        # Start receiving step notifications
        WorkflowMonitor.subscribe_to_steps(execution_pid, socket.channel_pid)
        
        NotificationBroadcaster.broadcast_stage_event(socket, "reactor:workflow_started", %{
          workflow_id: workflow.id,
          execution_id: execution_pid,
          total_steps: length(workflow.steps)
        })
        
        {:reply, {:ok, %{
          message: "Workflow execution started",
          execution_id: execution_pid,
          monitoring: true
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Handle step-related events during workflow execution
  """
  def handle_steps(payload, %{splat: [step_event]}, socket) do
    case step_event do
      "started" ->
        handle_step_started(payload, socket)
        
      "completed" ->
        handle_step_completed(payload, socket)
        
      "failed" ->
        handle_step_failed(payload, socket)
        
      "retry" ->
        handle_step_retry(payload, socket)
        
      "skipped" ->
        handle_step_skipped(payload, socket)
        
      _ ->
        {:reply, {:error, %{reason: "Unknown step event: #{step_event}"}}, socket}
    end
  end
  
  @doc """
  Generic handler for workflow events
  """
  def handle_in(event, payload, _bindings, socket) do
    Logger.debug("Handling Reactor event: #{event}")
    
    case String.split(event, ":") do
      ["execution", action] ->
        handle_execution_action(action, payload, socket)
        
      ["step", step_name, action] ->
        handle_step_action(step_name, action, payload, socket)
        
      ["compensation", action] ->
        handle_compensation_action(action, payload, socket)
        
      _ ->
        {:reply, {:error, %{reason: "Unknown Reactor event: #{event}"}}, socket}
    end
  end
  
  # Step event handlers
  
  defp handle_step_started(payload, socket) do
    step_name = payload["step_name"]
    
    NotificationBroadcaster.broadcast_stage_event(socket, "reactor:step_started", %{
      workflow_id: socket.assigns.reactor_workflow.id,
      step_name: step_name,
      step_type: payload["step_type"],
      timestamp: System.system_time(:millisecond)
    })
    
    # Update UI with step progress
    push(socket, "step_progress", %{
      step: step_name,
      status: "running",
      progress: 0
    })
    
    {:noreply, socket}
  end
  
  defp handle_step_completed(payload, socket) do
    step_name = payload["step_name"]
    duration = payload["duration_ms"]
    
    NotificationBroadcaster.broadcast_stage_event(socket, "reactor:step_completed", %{
      workflow_id: socket.assigns.reactor_workflow.id,
      step_name: step_name,
      duration_ms: duration,
      result: payload["result"],
      timestamp: System.system_time(:millisecond)
    })
    
    # Update UI with completion
    push(socket, "step_progress", %{
      step: step_name,
      status: "completed",
      progress: 100,
      duration: duration
    })
    
    # Check if all steps completed
    socket = update_workflow_progress(socket, step_name)
    
    {:noreply, socket}
  end
  
  defp handle_step_failed(payload, socket) do
    step_name = payload["step_name"]
    error = payload["error"]
    
    NotificationBroadcaster.broadcast_stage_event(socket, "reactor:step_failed", %{
      workflow_id: socket.assigns.reactor_workflow.id,
      step_name: step_name,
      error: error,
      timestamp: System.system_time(:millisecond)
    })
    
    # Update UI with failure
    push(socket, "step_progress", %{
      step: step_name,
      status: "failed",
      error: error
    })
    
    # Check for compensation
    if socket.assigns.reactor_workflow.compensate_on_error do
      initiate_compensation(socket, step_name, error)
    end
    
    {:noreply, socket}
  end
  
  defp handle_step_retry(payload, socket) do
    step_name = payload["step_name"]
    attempt = payload["attempt"]
    max_attempts = payload["max_attempts"]
    
    NotificationBroadcaster.broadcast_stage_event(socket, "reactor:step_retry", %{
      workflow_id: socket.assigns.reactor_workflow.id,
      step_name: step_name,
      attempt: attempt,
      max_attempts: max_attempts
    })
    
    push(socket, "step_progress", %{
      step: step_name,
      status: "retrying",
      attempt: attempt,
      max_attempts: max_attempts
    })
    
    {:noreply, socket}
  end
  
  defp handle_step_skipped(payload, socket) do
    step_name = payload["step_name"]
    reason = payload["reason"]
    
    push(socket, "step_progress", %{
      step: step_name,
      status: "skipped",
      reason: reason
    })
    
    {:noreply, socket}
  end
  
  # Private helper functions
  
  defp validate_workflow_spec(payload) do
    required = ~w(name steps)
    
    case Enum.filter(required, &(not Map.has_key?(payload, &1))) do
      [] ->
        validate_steps(payload["steps"])
        {:ok, payload}
        
      missing ->
        {:error, "Missing required fields: #{Enum.join(missing, ", ")}"}
    end
  end
  
  defp validate_steps(steps) when is_list(steps) do
    Enum.all?(steps, fn step ->
      Map.has_key?(step, "name") and Map.has_key?(step, "action")
    end)
  end
  
  defp get_ash_resources(socket) do
    # Get Ash resources from previous stage
    case socket.assigns[:ash_resources] do
      nil ->
        {:error, "Ash resources not found. Ensure Ash stage is completed."}
        
      resources ->
        {:ok, resources}
    end
  end
  
  defp build_reactor_workflow(spec, resources) do
    workflow = %{
      id: generate_workflow_id(),
      name: spec["name"],
      description: spec["description"],
      resources: resources,
      steps: build_workflow_steps(spec["steps"], resources),
      triggers: spec["triggers"] || [],
      compensate_on_error: Map.get(spec, "compensate_on_error", true),
      timeout: Map.get(spec, "timeout", 300_000), # 5 minutes default
      metadata: %{
        created_at: DateTime.utc_now(),
        version: "1.0.0"
      }
    }
    
    {:ok, workflow}
  end
  
  defp build_workflow_steps(step_specs, resources) do
    Enum.map(step_specs, fn spec ->
      %{
        name: spec["name"],
        action: parse_action(spec["action"]),
        resource: find_resource(spec["resource"], resources),
        arguments: spec["arguments"] || %{},
        wait_for: spec["wait_for"] || [],
        max_retries: spec["max_retries"] || 3,
        timeout: spec["timeout"] || 30_000,
        compensate: spec["compensate"],
        async: Map.get(spec, "async", false)
      }
    end)
  end
  
  defp parse_action(action) when is_binary(action) do
    case String.split(action, ":") do
      [resource, action_name] ->
        {String.to_atom(resource), String.to_atom(action_name)}
        
      [action_name] ->
        String.to_atom(action_name)
    end
  end
  
  defp find_resource(resource_name, resources) when is_binary(resource_name) do
    Enum.find(resources, fn r -> 
      r.name == resource_name or to_string(r.module) =~ resource_name
    end)
  end
  
  defp summarize_workflow(workflow) do
    %{
      name: workflow.name,
      total_steps: length(workflow.steps),
      async_steps: Enum.count(workflow.steps, & &1.async),
      resources_used: workflow.resources |> Enum.map(& &1.name) |> Enum.uniq() |> length(),
      has_compensation: workflow.compensate_on_error,
      estimated_duration: estimate_workflow_duration(workflow)
    }
  end
  
  defp estimate_workflow_duration(workflow) do
    # Simple estimation based on step timeouts
    sync_duration = workflow.steps
    |> Enum.reject(& &1.async)
    |> Enum.map(& &1.timeout)
    |> Enum.sum()
    
    async_duration = workflow.steps
    |> Enum.filter(& &1.async)
    |> Enum.map(& &1.timeout)
    |> Enum.max(fn -> 0 end)
    
    sync_duration + async_duration
  end
  
  defp update_workflow_progress(socket, completed_step) do
    completed_steps = [completed_step | socket.assigns[:completed_steps] || []]
    total_steps = length(socket.assigns.reactor_workflow.steps)
    
    socket = assign(socket, :completed_steps, completed_steps)
    
    if length(completed_steps) == total_steps do
      # Workflow completed
      duration = System.monotonic_time(:millisecond) - socket.assigns.workflow_start_time
      
      NotificationBroadcaster.broadcast_stage_event(socket, "reactor:workflow_completed", %{
        workflow_id: socket.assigns.reactor_workflow.id,
        total_duration_ms: duration,
        steps_completed: total_steps
      })
      
      # Mark Reactor stage as complete
      update_in(socket.assigns.stages_completed, &([:reactor | &1]))
    else
      socket
    end
  end
  
  defp initiate_compensation(socket, failed_step, error) do
    Logger.info("Initiating compensation for failed step: #{failed_step}")
    
    case WorkflowEngine.compensate(
      socket.assigns.workflow_execution_pid,
      failed_step,
      error
    ) do
      {:ok, compensation_id} ->
        NotificationBroadcaster.broadcast_stage_event(socket, "reactor:compensation_started", %{
          workflow_id: socket.assigns.reactor_workflow.id,
          failed_step: failed_step,
          compensation_id: compensation_id
        })
        
      {:error, reason} ->
        Logger.error("Failed to initiate compensation: #{reason}")
    end
  end
  
  defp handle_execution_action("pause", _payload, socket) do
    case WorkflowEngine.pause(socket.assigns.workflow_execution_pid) do
      :ok ->
        push(socket, "workflow_status", %{status: "paused"})
        {:reply, {:ok, %{message: "Workflow paused"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_execution_action("resume", _payload, socket) do
    case WorkflowEngine.resume(socket.assigns.workflow_execution_pid) do
      :ok ->
        push(socket, "workflow_status", %{status: "running"})
        {:reply, {:ok, %{message: "Workflow resumed"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_execution_action("cancel", _payload, socket) do
    case WorkflowEngine.cancel(socket.assigns.workflow_execution_pid) do
      :ok ->
        push(socket, "workflow_status", %{status: "cancelled"})
        {:reply, {:ok, %{message: "Workflow cancelled"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp generate_workflow_id do
    "wf_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
  end
end