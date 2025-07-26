# Reactor Stage Channel Handler
# Real-time Reactor workflow orchestration via channels
# Coordinates complex workflows in the BitActor pipeline

defmodule BitActorWeb.ReactorHandler do
  @moduledoc """
  Channel handler for Reactor workflow stage events using ChannelHandler.Handler.
  
  Manages Reactor workflow execution, step coordination, and error recovery
  in real-time as part of the BitActor pipeline.
  """
  
  use ChannelHandler.Handler
  
  # Reactor-specific plugs
  plug BitActorWeb.ChannelPlugs.ValidateWorkflowDefinition when action in [:create, :execute]
  plug BitActorWeb.ChannelPlugs.TrackWorkflowExecution
  plug BitActorWeb.ChannelPlugs.MonitorStepPerformance
  
  # Workflow operation TTL budgets (nanoseconds)
  @reactor_ttl_budgets %{
    workflow_creation_ns: 200_000_000,     # 200ms to create workflow
    step_execution_ns: 500_000_000,        # 500ms per step
    workflow_execution_ns: 5_000_000_000,  # 5s for full workflow
    compensation_ns: 1_000_000_000,        # 1s for compensations
    monitoring_interval_ns: 50_000_000     # 50ms monitoring interval
  }
  
  @doc """
  Handles all delegated reactor:* events
  """
  def handle_in("reactor:" <> event_type, payload, bindings, socket) do
    workflow_start = System.monotonic_time(:nanosecond)
    
    result = case String.split(event_type, ":", parts: 2) do
      ["workflow", action] -> handle_workflow_action(action, payload, socket)
      ["step", action] -> handle_step_action(action, payload, socket)
      ["execution", control] -> handle_execution_control(control, payload, socket)
      ["monitor", target] -> handle_monitoring_request(target, payload, socket)
      ["compensation", action] -> handle_compensation_action(action, payload, socket)
      _ -> {:error, "Unknown Reactor event: #{event_type}"}
    end
    
    workflow_duration = System.monotonic_time(:nanosecond) - workflow_start
    track_reactor_operation_metrics(socket, event_type, workflow_duration)
    
    format_reactor_response(result, socket)
  end
  
  # Workflow management actions
  
  defp handle_workflow_action("create", payload, socket) do
    with {:ok, workflow_def} <- parse_workflow_definition(payload),
         {:ok, validated_def} <- validate_workflow_definition(workflow_def),
         {:ok, workflow} <- execute_within_ttl(
           fn -> create_workflow(validated_def, socket) end,
           @reactor_ttl_budgets.workflow_creation_ns
         ) do
      
      broadcast_workflow_created(socket, workflow)
      {:ok, %{
        workflow_id: workflow.id,
        status: :created,
        steps_count: length(workflow.steps),
        created_at_ns: System.monotonic_time(:nanosecond)
      }}
    end
  end
  
  defp handle_workflow_action("execute", payload, socket) do
    with {:ok, workflow_id} <- get_workflow_id(payload),
         {:ok, workflow} <- get_workflow(workflow_id),
         {:ok, context} <- prepare_execution_context(payload, socket),
         {:ok, execution} <- start_workflow_execution(workflow, context, socket) do
      
      # Monitor execution asynchronously
      monitor_workflow_execution(execution, socket)
      
      {:ok, %{
        execution_id: execution.id,
        workflow_id: workflow_id,
        status: :executing,
        started_at_ns: System.monotonic_time(:nanosecond)
      }}
    end
  end
  
  defp handle_workflow_action("cancel", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, execution} <- get_execution(execution_id),
         {:ok, cancelled} <- cancel_workflow_execution(execution, socket) do
      
      broadcast_workflow_cancelled(socket, cancelled)
      {:ok, %{
        execution_id: execution_id,
        status: :cancelled,
        cancelled_at_ns: System.monotonic_time(:nanosecond),
        cleanup_performed: true
      }}
    end
  end
  
  defp handle_workflow_action("status", payload, socket) do
    with {:ok, workflow_id} <- get_workflow_id(payload),
         {:ok, status} <- get_workflow_status(workflow_id) do
      
      {:ok, %{
        workflow_id: workflow_id,
        status: status.state,
        completed_steps: status.completed_steps,
        total_steps: status.total_steps,
        current_step: status.current_step,
        duration_ns: status.duration_ns
      }}
    end
  end
  
  # Step execution actions
  
  defp handle_step_action("execute", payload, socket) do
    with {:ok, step_id} <- get_step_id(payload),
         {:ok, step} <- get_step_definition(step_id),
         {:ok, inputs} <- prepare_step_inputs(payload, socket),
         {:ok, result} <- execute_within_ttl(
           fn -> execute_step(step, inputs, socket) end,
           @reactor_ttl_budgets.step_execution_ns
         ) do
      
      broadcast_step_completed(socket, step_id, result)
      {:ok, %{
        step_id: step_id,
        status: :completed,
        result: result,
        duration_ns: result.duration_ns
      }}
    end
  end
  
  defp handle_step_action("retry", payload, socket) do
    with {:ok, step_id} <- get_step_id(payload),
         {:ok, execution} <- get_step_execution(step_id),
         :failed <- execution.status,
         {:ok, retry_result} <- retry_step_execution(execution, socket) do
      
      broadcast_step_retried(socket, step_id, retry_result)
      {:ok, %{
        step_id: step_id,
        retry_attempt: execution.retry_count + 1,
        status: retry_result.status,
        result: retry_result
      }}
    else
      {:error, reason} -> {:error, reason}
      status when status != :failed -> {:error, "Step not in failed state"}
    end
  end
  
  defp handle_step_action("skip", payload, socket) do
    with {:ok, step_id} <- get_step_id(payload),
         {:ok, reason} <- get_skip_reason(payload),
         {:ok, skipped} <- skip_step_execution(step_id, reason, socket) do
      
      broadcast_step_skipped(socket, step_id, reason)
      {:ok, %{
        step_id: step_id,
        status: :skipped,
        reason: reason,
        skipped_at_ns: System.monotonic_time(:nanosecond)
      }}
    end
  end
  
  # Execution control actions
  
  defp handle_execution_control("pause", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, execution} <- pause_execution(execution_id, socket) do
      
      broadcast_execution_paused(socket, execution)
      {:ok, %{
        execution_id: execution_id,
        status: :paused,
        paused_at_ns: System.monotonic_time(:nanosecond),
        current_step: execution.current_step
      }}
    end
  end
  
  defp handle_execution_control("resume", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, execution} <- resume_execution(execution_id, socket) do
      
      broadcast_execution_resumed(socket, execution)
      {:ok, %{
        execution_id: execution_id,
        status: :running,
        resumed_at_ns: System.monotonic_time(:nanosecond),
        resuming_from_step: execution.current_step
      }}
    end
  end
  
  defp handle_execution_control("rollback", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, target_step} <- get_rollback_target(payload),
         {:ok, rollback} <- initiate_rollback(execution_id, target_step, socket) do
      
      broadcast_execution_rollback(socket, rollback)
      {:ok, %{
        execution_id: execution_id,
        status: :rolling_back,
        rollback_to_step: target_step,
        compensation_steps: length(rollback.compensation_steps)
      }}
    end
  end
  
  # Monitoring requests
  
  defp handle_monitoring_request("execution", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, metrics} <- collect_execution_metrics(execution_id) do
      
      {:ok, %{
        execution_id: execution_id,
        metrics: %{
          total_duration_ns: metrics.duration_ns,
          steps_completed: metrics.completed_steps,
          steps_failed: metrics.failed_steps,
          average_step_duration_ns: metrics.avg_step_duration,
          ttl_violations: metrics.ttl_violations,
          resource_usage: metrics.resource_usage
        }
      }}
    end
  end
  
  defp handle_monitoring_request("performance", payload, socket) do
    time_range = payload["time_range"] || "last_hour"
    
    performance_data = %{
      workflow_executions: get_execution_count(time_range),
      average_duration_ns: get_average_execution_duration(time_range),
      success_rate: calculate_success_rate(time_range),
      failure_reasons: get_top_failure_reasons(time_range),
      bottleneck_steps: identify_bottleneck_steps(time_range),
      ttl_compliance_rate: calculate_ttl_compliance_rate(time_range)
    }
    
    {:ok, performance_data}
  end
  
  # Compensation actions
  
  defp handle_compensation_action("execute", payload, socket) do
    with {:ok, execution_id} <- get_execution_id(payload),
         {:ok, failed_step} <- get_failed_step(execution_id),
         {:ok, compensation} <- execute_within_ttl(
           fn -> execute_compensation(failed_step, socket) end,
           @reactor_ttl_budgets.compensation_ns
         ) do
      
      broadcast_compensation_executed(socket, compensation)
      {:ok, %{
        execution_id: execution_id,
        compensated_step: failed_step.id,
        compensation_status: :completed,
        duration_ns: compensation.duration_ns
      }}
    end
  end
  
  # Workflow execution monitoring
  
  defp monitor_workflow_execution(execution, socket) do
    Task.start(fn ->
      monitor_loop(execution, socket, System.monotonic_time(:nanosecond))
    end)
  end
  
  defp monitor_loop(execution, socket, start_time) do
    current_time = System.monotonic_time(:nanosecond)
    elapsed = current_time - start_time
    
    case get_execution_state(execution.id) do
      {:ok, %{status: :completed} = state} ->
        broadcast_workflow_completed(socket, execution.id, state, elapsed)
        
      {:ok, %{status: :failed} = state} ->
        broadcast_workflow_failed(socket, execution.id, state, elapsed)
        
      {:ok, %{status: :running} = state} ->
        broadcast_workflow_progress(socket, execution.id, state, elapsed)
        
        if elapsed < @reactor_ttl_budgets.workflow_execution_ns do
          Process.sleep(@reactor_ttl_budgets.monitoring_interval_ns / 1_000_000)
          monitor_loop(execution, socket, start_time)
        else
          handle_workflow_timeout(execution, socket, elapsed)
        end
        
      _ ->
        # Execution no longer found or in unexpected state
        :ok
    end
  end
  
  defp handle_workflow_timeout(execution, socket, elapsed_ns) do
    timeout_result = %{
      execution_id: execution.id,
      status: :timeout,
      elapsed_ns: elapsed_ns,
      ttl_budget_ns: @reactor_ttl_budgets.workflow_execution_ns
    }
    
    cancel_workflow_execution(execution, socket)
    broadcast_workflow_timeout(socket, timeout_result)
  end
  
  # Helper functions
  
  defp execute_within_ttl(func, ttl_budget_ns) do
    task = Task.async(func)
    
    case Task.yield(task, ttl_budget_ns / 1_000_000) do
      {:ok, result} -> result
      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, "Operation exceeded TTL budget of #{ttl_budget_ns}ns"}
    end
  end
  
  defp track_reactor_operation_metrics(socket, operation, duration_ns) do
    push(socket, "reactor:metrics", %{
      operation: operation,
      duration_ns: duration_ns,
      ttl_compliant: duration_ns <= get_ttl_budget_for_reactor_operation(operation),
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp get_ttl_budget_for_reactor_operation("workflow:create"), do: @reactor_ttl_budgets.workflow_creation_ns
  defp get_ttl_budget_for_reactor_operation("workflow:execute"), do: @reactor_ttl_budgets.workflow_execution_ns
  defp get_ttl_budget_for_reactor_operation("step:" <> _), do: @reactor_ttl_budgets.step_execution_ns
  defp get_ttl_budget_for_reactor_operation("compensation:" <> _), do: @reactor_ttl_budgets.compensation_ns
  defp get_ttl_budget_for_reactor_operation(_), do: @reactor_ttl_budgets.step_execution_ns
  
  defp format_reactor_response({:ok, data}, socket) do
    {:reply, {:ok, data}, socket}
  end
  defp format_reactor_response({:error, %Reactor.Error{} = error}, socket) do
    {:reply, {:error, format_reactor_error(error)}, socket}
  end
  defp format_reactor_response({:error, reason}, socket) do
    {:reply, {:error, reason}, socket}
  end
  
  # Broadcast functions
  
  defp broadcast_workflow_created(socket, workflow) do
    broadcast!(socket, "reactor:workflow:created", %{
      workflow_id: workflow.id,
      steps_count: length(workflow.steps),
      created_by: socket.assigns.user_id,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_workflow_completed(socket, execution_id, state, duration_ns) do
    broadcast!(socket, "reactor:workflow:completed", %{
      execution_id: execution_id,
      completed_steps: state.completed_steps,
      total_duration_ns: duration_ns,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_workflow_failed(socket, execution_id, state, duration_ns) do
    broadcast!(socket, "reactor:workflow:failed", %{
      execution_id: execution_id,
      failed_step: state.failed_step,
      error: state.error,
      duration_ns: duration_ns,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_workflow_progress(socket, execution_id, state, elapsed_ns) do
    broadcast!(socket, "reactor:workflow:progress", %{
      execution_id: execution_id,
      current_step: state.current_step,
      completed_steps: state.completed_steps,
      total_steps: state.total_steps,
      elapsed_ns: elapsed_ns,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_workflow_timeout(socket, timeout_result) do
    broadcast!(socket, "reactor:workflow:timeout", timeout_result)
  end
  
  defp broadcast_workflow_cancelled(socket, cancelled) do
    broadcast!(socket, "reactor:workflow:cancelled", %{
      execution_id: cancelled.id,
      cancelled_by: socket.assigns.user_id,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_step_completed(socket, step_id, result) do
    broadcast!(socket, "reactor:step:completed", %{
      step_id: step_id,
      result: result,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_step_retried(socket, step_id, retry_result) do
    broadcast!(socket, "reactor:step:retried", %{
      step_id: step_id,
      retry_result: retry_result,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_step_skipped(socket, step_id, reason) do
    broadcast!(socket, "reactor:step:skipped", %{
      step_id: step_id,
      reason: reason,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_execution_paused(socket, execution) do
    broadcast!(socket, "reactor:execution:paused", %{
      execution_id: execution.id,
      paused_by: socket.assigns.user_id,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_execution_resumed(socket, execution) do
    broadcast!(socket, "reactor:execution:resumed", %{
      execution_id: execution.id,
      resumed_by: socket.assigns.user_id,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_execution_rollback(socket, rollback) do
    broadcast!(socket, "reactor:execution:rollback", %{
      execution_id: rollback.execution_id,
      rollback_initiated_by: socket.assigns.user_id,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  defp broadcast_compensation_executed(socket, compensation) do
    broadcast!(socket, "reactor:compensation:executed", %{
      compensation: compensation,
      timestamp_ns: System.monotonic_time(:nanosecond)
    })
  end
  
  # Placeholder implementations for Reactor operations
  defp parse_workflow_definition(%{"steps" => steps}), do: {:ok, %{steps: steps}}
  defp parse_workflow_definition(_), do: {:error, "Invalid workflow definition"}
  defp validate_workflow_definition(def), do: {:ok, def}
  defp create_workflow(def, _socket), do: {:ok, %{id: generate_id(), steps: def.steps}}
  defp get_workflow_id(%{"workflow_id" => id}), do: {:ok, id}
  defp get_workflow_id(_), do: {:error, "Workflow ID required"}
  defp get_workflow(id), do: {:ok, %{id: id, steps: []}}
  defp prepare_execution_context(_payload, _socket), do: {:ok, %{}}
  defp start_workflow_execution(workflow, _context, _socket), do: {:ok, %{id: generate_id(), workflow_id: workflow.id}}
  defp get_execution_id(%{"execution_id" => id}), do: {:ok, id}
  defp get_execution_id(_), do: {:error, "Execution ID required"}
  defp get_execution(id), do: {:ok, %{id: id, status: :running}}
  defp cancel_workflow_execution(execution, _socket), do: {:ok, execution}
  defp get_workflow_status(id), do: {:ok, %{state: :running, completed_steps: 3, total_steps: 5, current_step: "step_4", duration_ns: 1_500_000_000}}
  defp get_step_id(%{"step_id" => id}), do: {:ok, id}
  defp get_step_id(_), do: {:error, "Step ID required"}
  defp get_step_definition(id), do: {:ok, %{id: id, type: :action}}
  defp prepare_step_inputs(_payload, _socket), do: {:ok, %{}}
  defp execute_step(_step, _inputs, _socket), do: {:ok, %{success: true, duration_ns: 250_000_000}}
  defp get_step_execution(id), do: {:ok, %{id: id, status: :failed, retry_count: 0}}
  defp retry_step_execution(execution, _socket), do: {:ok, %{status: :completed}}
  defp get_skip_reason(%{"reason" => reason}), do: {:ok, reason}
  defp get_skip_reason(_), do: {:error, "Skip reason required"}
  defp skip_step_execution(_id, _reason, _socket), do: {:ok, %{}}
  defp pause_execution(id, _socket), do: {:ok, %{id: id, current_step: "step_3"}}
  defp resume_execution(id, _socket), do: {:ok, %{id: id, current_step: "step_3"}}
  defp get_rollback_target(%{"target_step" => target}), do: {:ok, target}
  defp get_rollback_target(_), do: {:error, "Rollback target required"}
  defp initiate_rollback(id, _target, _socket), do: {:ok, %{execution_id: id, compensation_steps: ["comp_1", "comp_2"]}}
  defp collect_execution_metrics(id), do: {:ok, %{duration_ns: 2_000_000_000, completed_steps: 4, failed_steps: 1, avg_step_duration: 400_000_000, ttl_violations: 0, resource_usage: %{cpu: 45, memory: 60}}}
  defp get_execution_state(id), do: {:ok, %{status: :running, completed_steps: 3, total_steps: 5, current_step: "step_4"}}
  defp get_execution_count(_time_range), do: 150
  defp get_average_execution_duration(_time_range), do: 2_500_000_000
  defp calculate_success_rate(_time_range), do: 0.945
  defp get_top_failure_reasons(_time_range), do: ["timeout", "resource_exhausted", "validation_error"]
  defp identify_bottleneck_steps(_time_range), do: ["heavy_computation_step", "external_api_call"]
  defp calculate_ttl_compliance_rate(_time_range), do: 0.982
  defp get_failed_step(id), do: {:ok, %{id: "step_failed_123", execution_id: id}}
  defp execute_compensation(step, _socket), do: {:ok, %{step_id: step.id, duration_ns: 500_000_000}}
  defp format_reactor_error(_error), do: "Reactor workflow error"
  defp generate_id, do: System.unique_integer([:positive]) |> Integer.to_string()
end