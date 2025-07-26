defmodule BitActor.Reactor.Workflow.Saga do
  @moduledoc """
  Saga Pattern BitActor Reactor Workflow Variant - Distributed Transactions
  
  This variant explores distributed transaction patterns with:
  - Saga orchestration for long-running processes
  - Compensating actions for rollback
  - Distributed TTL constraint coordination
  - Cross-service transaction boundaries
  - Failure recovery and partial rollback
  - Event-driven saga coordination
  """
  
  # =============================================================================
  # Saga Coordinator Workflow
  # =============================================================================
  
  defmodule BitActor.Reactor.SagaCoordinatorWorkflow do
    @moduledoc "Orchestrates saga transactions across multiple BitActors"
    
    use Reactor
    
    input :saga_id
    input :transaction_steps, default: []
    input :compensation_strategy, default: :reverse_order
    input :ttl_budget_ms, default: 30000  # 30 second saga timeout
    input :participants, default: []
    
    # Initialize saga transaction
    step :initialize_saga do
      argument :saga_id, input(:saga_id)
      argument :ttl_budget_ms, input(:ttl_budget_ms)
      argument :participants, input(:participants)
      
      run fn args, context ->
        saga_state = %{
          saga_id: args.saga_id,
          status: :started,
          started_at: DateTime.utc_now(),
          ttl_budget_ns: args.ttl_budget_ms * 1_000_000,
          ttl_used_ns: 0,
          participants: args.participants,
          completed_steps: [],
          failed_steps: [],
          compensation_actions: [],
          context: context
        }
        
        Logger.info("Starting saga transaction #{args.saga_id}")
        {:ok, saga_state}
      end
    end
    
    # Execute transaction steps with compensation tracking
    step :execute_transaction_steps do
      argument :saga_state, result(:initialize_saga)
      argument :transaction_steps, input(:transaction_steps)
      
      run fn args, context ->
        execute_saga_steps(args.saga_state, args.transaction_steps, context)
      end
      
      compensate fn _args, _context ->
        Logger.warning("Compensating transaction steps due to failure")
        {:ok, :compensated}
      end
    end
    
    # Handle saga completion or failure
    step :finalize_saga do
      argument :saga_state, result(:initialize_saga)
      argument :execution_result, result(:execute_transaction_steps)
      argument :compensation_strategy, input(:compensation_strategy)
      
      run fn args, _context ->
        case args.execution_result do
          {:ok, completed_saga} ->
            finalize_successful_saga(completed_saga)
          
          {:error, {failed_saga, error}} ->
            execute_compensation_actions(failed_saga, error, args.compensation_strategy)
        end
      end
    end
    
    return :finalize_saga
    
    # Private saga execution logic
    defp execute_saga_steps(initial_saga_state, steps, context) do
      start_time = System.monotonic_time(:nanosecond)
      
      result = Enum.reduce_while(steps, {:ok, initial_saga_state}, fn step, {:ok, saga_state} ->
        case execute_saga_step(step, saga_state, context) do
          {:ok, updated_state} ->
            # Check TTL budget
            current_time = System.monotonic_time(:nanosecond)
            ttl_used = current_time - start_time
            
            if ttl_used >= saga_state.ttl_budget_ns do
              Logger.error("Saga #{saga_state.saga_id} exceeded TTL budget")
              {:halt, {:error, {updated_state, :ttl_exceeded}}}
            else
              updated_state = %{updated_state | ttl_used_ns: ttl_used}
              {:cont, {:ok, updated_state}}
            end
          
          {:error, reason} ->
            Logger.error("Saga step failed: #{inspect(reason)}")
            updated_state = %{saga_state | 
              status: :failed,
              failed_steps: [step | saga_state.failed_steps]
            }
            {:halt, {:error, {updated_state, reason}}}
        end
      end)
      
      case result do
        {:ok, final_state} ->
          final_state = %{final_state | status: :completed}
          {:ok, final_state}
        
        {:error, _} = error ->
          error
      end
    end
    
    defp execute_saga_step(step, saga_state, context) do
      step_start_time = System.monotonic_time(:nanosecond)
      
      Logger.info("Executing saga step: #{step.name}")
      
      # Execute the step with TTL enforcement
      remaining_ttl_ns = saga_state.ttl_budget_ns - saga_state.ttl_used_ns
      step_timeout_ms = div(remaining_ttl_ns, 1_000_000)
      
      case execute_step_with_timeout(step, saga_state, context, step_timeout_ms) do
        {:ok, step_result} ->
          step_end_time = System.monotonic_time(:nanosecond)
          step_duration = step_end_time - step_start_time
          
          # Record successful step and its compensation action
          compensation_action = create_compensation_action(step, step_result)
          
          updated_state = %{saga_state |
            completed_steps: [%{step: step, result: step_result, duration_ns: step_duration} | saga_state.completed_steps],
            compensation_actions: [compensation_action | saga_state.compensation_actions],
            ttl_used_ns: saga_state.ttl_used_ns + step_duration
          }
          
          {:ok, updated_state}
        
        {:error, reason} ->
          {:error, reason}
      end
    end
    
    defp execute_step_with_timeout(step, saga_state, context, timeout_ms) do
      # Create step execution task
      task = Task.async(fn ->
        case step.type do
          :bitactor_signal ->
            execute_bitactor_signal_step(step, saga_state, context)
          
          :external_service ->
            execute_external_service_step(step, saga_state, context)
          
          :database_operation ->
            execute_database_operation_step(step, saga_state, context)
          
          :custom_workflow ->
            execute_custom_workflow_step(step, saga_state, context)
          
          _ ->
            {:error, :unknown_step_type}
        end
      end)
      
      # Wait for completion with timeout
      case Task.yield(task, timeout_ms) || Task.shutdown(task) do
        {:ok, result} -> result
        nil -> {:error, :step_timeout}
      end
    end
    
    defp execute_bitactor_signal_step(step, saga_state, _context) do
      # Execute BitActor signal processing as part of saga
      signal = %{
        id: UUID.uuid4(),
        type: step.signal_type,
        payload: step.payload,
        saga_id: saga_state.saga_id,
        ttl_constraint: %{
          budget_ns: step.ttl_budget_ns || 5_000_000  # 5ms default
        }
      }
      
      case BitActor.GenServer.process_signal(step.actor_pid, signal) do
        {:ok, result} ->
          {:ok, %{type: :bitactor_signal, signal_id: signal.id, result: result}}
        
        {:error, reason} ->
          {:error, reason}
      end
    end
    
    defp execute_external_service_step(step, saga_state, _context) do
      # Simulate external service call
      Logger.info("Calling external service: #{step.service_name}")
      
      # In real implementation, would make actual HTTP call
      case step.service_name do
        "payment_service" ->
          # Simulate payment processing
          if step.payload.amount > 0 do
            {:ok, %{
              type: :external_service,
              service: step.service_name,
              transaction_id: UUID.uuid4(),
              amount: step.payload.amount,
              status: :charged
            }}
          else
            {:error, :invalid_amount}
          end
        
        "inventory_service" ->
          # Simulate inventory reservation
          {:ok, %{
            type: :external_service,
            service: step.service_name,
            reservation_id: UUID.uuid4(),
            items: step.payload.items,
            status: :reserved
          }}
        
        _ ->
          {:error, :unknown_service}
      end
    end
    
    defp execute_database_operation_step(step, saga_state, _context) do
      # Execute database operation with saga tracking
      Logger.info("Executing database operation: #{step.operation}")
      
      case step.operation do
        :create_order ->
          # Simulate order creation
          {:ok, %{
            type: :database_operation,
            operation: :create_order,
            order_id: UUID.uuid4(),
            saga_id: saga_state.saga_id,
            data: step.data
          }}
        
        :update_user_balance ->
          # Simulate balance update
          {:ok, %{
            type: :database_operation,
            operation: :update_user_balance,
            user_id: step.data.user_id,
            old_balance: step.data.old_balance,
            new_balance: step.data.new_balance
          }}
        
        _ ->
          {:error, :unknown_operation}
      end
    end
    
    defp execute_custom_workflow_step(step, saga_state, context) do
      # Execute nested Reactor workflow
      case step.workflow_module.run(step.inputs, context) do
        {:ok, result} ->
          {:ok, %{
            type: :custom_workflow,
            workflow: step.workflow_module,
            result: result
          }}
        
        {:error, reason} ->
          {:error, reason}
      end
    end
    
    defp create_compensation_action(step, step_result) do
      %{
        step_name: step.name,
        compensation_type: determine_compensation_type(step, step_result),
        compensation_data: extract_compensation_data(step, step_result),
        created_at: DateTime.utc_now()
      }
    end
    
    defp determine_compensation_type(step, step_result) do
      case {step.type, step_result.type} do
        {:bitactor_signal, :bitactor_signal} -> :reverse_signal
        {:external_service, :external_service} -> :compensating_api_call
        {:database_operation, :database_operation} -> :reverse_database_operation
        {:custom_workflow, :custom_workflow} -> :compensating_workflow
        _ -> :generic_compensation
      end
    end
    
    defp extract_compensation_data(step, step_result) do
      case step.type do
        :external_service ->
          case step_result do
            %{service: "payment_service", transaction_id: tx_id, amount: amount} ->
              %{action: :refund, transaction_id: tx_id, amount: amount}
            
            %{service: "inventory_service", reservation_id: res_id} ->
              %{action: :release_reservation, reservation_id: res_id}
            
            _ ->
              %{action: :generic_rollback, step_result: step_result}
          end
        
        :database_operation ->
          case step_result do
            %{operation: :create_order, order_id: order_id} ->
              %{action: :delete_order, order_id: order_id}
            
            %{operation: :update_user_balance, user_id: user_id, old_balance: old_balance} ->
              %{action: :restore_balance, user_id: user_id, balance: old_balance}
            
            _ ->
              %{action: :generic_database_rollback, step_result: step_result}
          end
        
        _ ->
          %{action: :generic_compensation, step_data: step, step_result: step_result}
      end
    end
    
    defp finalize_successful_saga(saga_state) do
      Logger.info("Saga #{saga_state.saga_id} completed successfully")
      
      final_state = %{saga_state |
        status: :completed,
        completed_at: DateTime.utc_now(),
        total_duration_ms: div(saga_state.ttl_used_ns, 1_000_000)
      }
      
      # Notify participants of successful completion
      notify_saga_participants(final_state, :completed)
      
      {:ok, final_state}
    end
    
    defp execute_compensation_actions(failed_saga_state, error, compensation_strategy) do
      Logger.warning("Executing compensation actions for failed saga #{failed_saga_state.saga_id}")
      
      compensation_actions = case compensation_strategy do
        :reverse_order -> Enum.reverse(failed_saga_state.compensation_actions)
        :priority_order -> Enum.sort_by(failed_saga_state.compensation_actions, &get_compensation_priority/1, :desc)
        :parallel -> failed_saga_state.compensation_actions
      end
      
      compensation_results = case compensation_strategy do
        :parallel ->
          execute_parallel_compensations(compensation_actions)
        _ ->
          execute_sequential_compensations(compensation_actions)
      end
      
      compensation_success = Enum.all?(compensation_results, fn
        {:ok, _} -> true
        _ -> false
      end)
      
      final_status = if compensation_success, do: :compensated, else: :compensation_failed
      
      final_state = %{failed_saga_state |
        status: final_status,
        error: error,
        compensation_results: compensation_results,
        completed_at: DateTime.utc_now()
      }
      
      # Notify participants of compensation completion
      notify_saga_participants(final_state, final_status)
      
      if compensation_success do
        {:ok, final_state}
      else
        {:error, final_state}
      end
    end
    
    defp execute_sequential_compensations(compensation_actions) do
      Enum.map(compensation_actions, fn action ->
        execute_single_compensation(action)
      end)
    end
    
    defp execute_parallel_compensations(compensation_actions) do
      compensation_actions
      |> Enum.map(fn action ->
        Task.async(fn -> execute_single_compensation(action) end)
      end)
      |> Enum.map(fn task -> Task.await(task, 10_000) end)  # 10 second timeout per compensation
    end
    
    defp execute_single_compensation(action) do
      Logger.info("Executing compensation: #{action.step_name}")
      
      case action.compensation_type do
        :reverse_signal ->
          execute_reverse_signal_compensation(action)
        
        :compensating_api_call ->
          execute_api_compensation(action)
        
        :reverse_database_operation ->
          execute_database_compensation(action)
        
        :compensating_workflow ->
          execute_workflow_compensation(action)
        
        _ ->
          Logger.warning("Unknown compensation type: #{action.compensation_type}")
          {:ok, :skipped}
      end
    end
    
    defp execute_reverse_signal_compensation(action) do
      # Send reverse/undo signal to BitActor
      reverse_signal = %{
        id: UUID.uuid4(),
        type: :compensation,
        payload: %{
          original_action: action.step_name,
          compensation_data: action.compensation_data
        }
      }
      
      # In real implementation, would send to appropriate BitActor
      Logger.info("Sending reverse signal for #{action.step_name}")
      {:ok, :reverse_signal_sent}
    end
    
    defp execute_api_compensation(action) do
      case action.compensation_data.action do
        :refund ->
          Logger.info("Processing refund for transaction #{action.compensation_data.transaction_id}")
          # Make refund API call
          {:ok, :refund_processed}
        
        :release_reservation ->
          Logger.info("Releasing inventory reservation #{action.compensation_data.reservation_id}")
          # Release inventory API call
          {:ok, :reservation_released}
        
        _ ->
          Logger.warning("Unknown API compensation: #{action.compensation_data.action}")
          {:ok, :api_compensation_completed}
      end
    end
    
    defp execute_database_compensation(action) do
      case action.compensation_data.action do
        :delete_order ->
          Logger.info("Deleting order #{action.compensation_data.order_id}")
          # Delete order from database
          {:ok, :order_deleted}
        
        :restore_balance ->
          Logger.info("Restoring balance for user #{action.compensation_data.user_id}")
          # Restore user balance
          {:ok, :balance_restored}
        
        _ ->
          Logger.warning("Unknown database compensation: #{action.compensation_data.action}")
          {:ok, :database_compensation_completed}
      end
    end
    
    defp execute_workflow_compensation(action) do
      Logger.info("Executing compensating workflow for #{action.step_name}")
      # Execute compensating workflow
      {:ok, :workflow_compensation_completed}
    end
    
    defp get_compensation_priority(action) do
      # Higher priority compensations should be executed first
      case action.compensation_type do
        :reverse_database_operation -> 10
        :compensating_api_call -> 8
        :reverse_signal -> 6
        :compensating_workflow -> 4
        _ -> 1
      end
    end
    
    defp notify_saga_participants(saga_state, status) do
      notification = %{
        saga_id: saga_state.saga_id,
        status: status,
        completed_at: saga_state.completed_at,
        duration_ms: saga_state.total_duration_ms || 0
      }
      
      Enum.each(saga_state.participants, fn participant ->
        spawn(fn ->
          try do
            GenServer.cast(participant, {:saga_notification, notification})
          catch
            _, _ -> Logger.warning("Failed to notify participant #{inspect(participant)}")
          end
        end)
      end)
    end
  end
  
  # =============================================================================
  # Distributed TTL Saga Workflow
  # =============================================================================
  
  defmodule BitActor.Reactor.DistributedTTLSagaWorkflow do
    @moduledoc "Manages TTL constraints across distributed saga participants"
    
    use Reactor
    
    input :saga_id
    input :global_ttl_budget_ms
    input :participant_actors
    input :ttl_distribution_strategy, default: :proportional
    
    # Distribute TTL budget across participants
    step :distribute_ttl_budget do
      argument :global_ttl_budget_ms, input(:global_ttl_budget_ms)
      argument :participant_actors, input(:participant_actors)
      argument :distribution_strategy, input(:ttl_distribution_strategy)
      
      run fn args, _context ->
        distribute_ttl_across_participants(
          args.global_ttl_budget_ms,
          args.participant_actors,
          args.distribution_strategy
        )
      end
    end
    
    # Monitor TTL usage across participants
    step :monitor_distributed_ttl do
      argument :saga_id, input(:saga_id)
      argument :ttl_distribution, result(:distribute_ttl_budget)
      
      run fn args, _context ->
        monitor_ttl_usage_across_saga(args.saga_id, args.ttl_distribution)
      end
    end
    
    # Coordinate TTL violations and compensation
    step :handle_ttl_violations do
      argument :saga_id, input(:saga_id)
      argument :monitoring_result, result(:monitor_distributed_ttl)
      
      run fn args, _context ->
        case args.monitoring_result do
          {:ok, %{violations: []}} ->
            {:ok, :no_violations}
          
          {:ok, %{violations: violations}} ->
            handle_distributed_ttl_violations(args.saga_id, violations)
          
          {:error, reason} ->
            {:error, reason}
        end
      end
    end
    
    return :handle_ttl_violations
    
    defp distribute_ttl_across_participants(global_budget_ms, participants, strategy) do
      case strategy do
        :equal ->
          per_participant_ms = div(global_budget_ms, length(participants))
          distribution = Enum.map(participants, fn participant ->
            {participant, per_participant_ms}
          end)
          {:ok, %{strategy: :equal, distribution: distribution}}
        
        :proportional ->
          # Distribute based on participant complexity/weight
          total_weight = Enum.sum(Enum.map(participants, fn p -> p.weight || 1 end))
          distribution = Enum.map(participants, fn participant ->
            weight = participant.weight || 1
            allocated_ms = div(global_budget_ms * weight, total_weight)
            {participant, allocated_ms}
          end)
          {:ok, %{strategy: :proportional, distribution: distribution}}
        
        :priority_based ->
          # Allocate more TTL to higher priority participants
          sorted_participants = Enum.sort_by(participants, fn p -> p.priority || 5 end, :desc)
          distribution = allocate_by_priority(sorted_participants, global_budget_ms)
          {:ok, %{strategy: :priority_based, distribution: distribution}}
        
        _ ->
          {:error, :unknown_distribution_strategy}
      end
    end
    
    defp allocate_by_priority(sorted_participants, total_budget_ms) do
      # High priority gets more budget
      {_, distribution} = Enum.reduce(sorted_participants, {total_budget_ms, []}, fn participant, {remaining_budget, acc} ->
        priority = participant.priority || 5
        multiplier = case priority do
          1 -> 0.4  # Critical: 40% of remaining
          2 -> 0.3  # High: 30% of remaining
          3 -> 0.2  # Medium: 20% of remaining
          4 -> 0.15 # Low: 15% of remaining  
          5 -> 0.1  # Very low: 10% of remaining
        end
        
        allocated = trunc(remaining_budget * multiplier)
        new_remaining = remaining_budget - allocated
        
        {new_remaining, [{participant, allocated} | acc]}
      end)
      
      Enum.reverse(distribution)
    end
    
    defp monitor_ttl_usage_across_saga(saga_id, ttl_distribution) do
      monitoring_start = System.monotonic_time(:nanosecond)
      
      # Monitor each participant's TTL usage
      participant_results = Enum.map(ttl_distribution.distribution, fn {participant, allocated_ms} ->
        monitor_participant_ttl(participant, allocated_ms, saga_id)
      end)
      
      # Aggregate results
      violations = Enum.filter(participant_results, fn
        {:violation, _} -> true
        _ -> false
      end)
      
      successful = Enum.filter(participant_results, fn
        {:ok, _} -> true
        _ -> false
      end)
      
      monitoring_end = System.monotonic_time(:nanosecond)
      monitoring_duration_ms = div(monitoring_end - monitoring_start, 1_000_000)
      
      {:ok, %{
        saga_id: saga_id,
        participants_monitored: length(participant_results),
        successful_participants: length(successful),
        violations: violations,
        monitoring_duration_ms: monitoring_duration_ms
      }}
    end
    
    defp monitor_participant_ttl(participant, allocated_ms, saga_id) do
      case GenServer.call(participant.pid, {:get_ttl_status, saga_id}, 1000) do
        {:ok, status} ->
          used_ms = div(status.ttl_used_ns || 0, 1_000_000)
          
          if used_ms > allocated_ms do
            {:violation, %{
              participant: participant,
              allocated_ms: allocated_ms,
              used_ms: used_ms,
              violation_ms: used_ms - allocated_ms
            }}
          else
            {:ok, %{
              participant: participant,
              allocated_ms: allocated_ms,
              used_ms: used_ms,
              remaining_ms: allocated_ms - used_ms
            }}
          end
        
        {:error, reason} ->
          {:error, %{participant: participant, reason: reason}}
      end
    end
    
    defp handle_distributed_ttl_violations(saga_id, violations) do
      Logger.warning("Handling TTL violations for saga #{saga_id}: #{length(violations)} violations")
      
      violation_responses = Enum.map(violations, fn {:violation, violation_data} ->
        handle_single_ttl_violation(saga_id, violation_data)
      end)
      
      total_violation_ms = Enum.sum(Enum.map(violations, fn {:violation, v} -> v.violation_ms end))
      
      {:ok, %{
        saga_id: saga_id,
        violations_handled: length(violations),
        total_violation_ms: total_violation_ms,
        violation_responses: violation_responses
      }}
    end
    
    defp handle_single_ttl_violation(saga_id, violation_data) do
      participant = violation_data.participant
      violation_ms = violation_data.violation_ms
      
      Logger.error("TTL violation: Participant #{inspect(participant.pid)} exceeded budget by #{violation_ms}ms")
      
      # Strategy: Try to compensate by borrowing TTL from other participants
      case attempt_ttl_rebalancing(saga_id, participant, violation_ms) do
        {:ok, rebalanced} ->
          Logger.info("Successfully rebalanced TTL for participant #{inspect(participant.pid)}")
          {:rebalanced, rebalanced}
        
        {:error, :insufficient_global_ttl} ->
          Logger.error("Insufficient global TTL budget for rebalancing")
          # Trigger saga compensation
          {:compensate, %{reason: :ttl_exceeded, participant: participant}}
        
        {:error, reason} ->
          Logger.error("TTL rebalancing failed: #{inspect(reason)}")
          {:compensate, %{reason: reason, participant: participant}}
      end
    end
    
    defp attempt_ttl_rebalancing(saga_id, violating_participant, needed_ms) do
      # Find participants with unused TTL budget
      # In real implementation, would query all saga participants
      available_participants = []  # Placeholder
      
      total_available = Enum.sum(Enum.map(available_participants, fn p -> p.unused_ms end))
      
      if total_available >= needed_ms do
        # Redistribute TTL
        rebalancing_plan = create_rebalancing_plan(available_participants, needed_ms)
        execute_ttl_rebalancing(saga_id, violating_participant, rebalancing_plan)
      else
        {:error, :insufficient_global_ttl}
      end
    end
    
    defp create_rebalancing_plan(available_participants, needed_ms) do
      # Create plan to redistribute TTL from available participants
      Enum.reduce(available_participants, {needed_ms, []}, fn participant, {remaining_needed, plan} ->
        if remaining_needed <= 0 do
          {remaining_needed, plan}
        else
          can_donate = min(participant.unused_ms, remaining_needed)
          new_remaining = remaining_needed - can_donate
          
          donation = %{
            from_participant: participant,
            amount_ms: can_donate
          }
          
          {new_remaining, [donation | plan]}
        end
      end)
      |> elem(1)
      |> Enum.reverse()
    end
    
    defp execute_ttl_rebalancing(saga_id, recipient, rebalancing_plan) do
      # Execute the TTL rebalancing across participants
      results = Enum.map(rebalancing_plan, fn donation ->
        donor = donation.from_participant
        amount_ms = donation.amount_ms
        
        # Transfer TTL budget
        case GenServer.call(donor.pid, {:transfer_ttl, saga_id, recipient.pid, amount_ms}, 1000) do
          :ok ->
            {:ok, donation}
          error ->
            {:error, {donation, error}}
        end
      end)
      
      successful_transfers = Enum.filter(results, fn
        {:ok, _} -> true
        _ -> false
      end)
      
      total_transferred = Enum.sum(Enum.map(successful_transfers, fn {:ok, donation} -> 
        donation.amount_ms 
      end))
      
      {:ok, %{
        transfers: length(successful_transfers),
        total_transferred_ms: total_transferred,
        rebalancing_plan: rebalancing_plan
      }}
    end
  end
  
  # =============================================================================
  # Saga Event Workflow
  # =============================================================================
  
  defmodule BitActor.Reactor.SagaEventWorkflow do
    @moduledoc "Event-driven saga coordination workflow"
    
    use Reactor
    
    input :saga_event
    input :saga_registry
    
    # Process saga events
    step :process_saga_event do
      argument :saga_event, input(:saga_event)
      argument :saga_registry, input(:saga_registry)
      
      run fn args, _context ->
        process_event_for_saga(args.saga_event, args.saga_registry)
      end
    end
    
    return :process_saga_event
    
    defp process_event_for_saga(event, saga_registry) do
      case event.type do
        :saga_step_completed ->
          handle_step_completed_event(event, saga_registry)
        
        :saga_step_failed ->
          handle_step_failed_event(event, saga_registry)
        
        :saga_ttl_warning ->
          handle_ttl_warning_event(event, saga_registry)
        
        :saga_participant_ready ->
          handle_participant_ready_event(event, saga_registry)
        
        _ ->
          {:error, :unknown_saga_event_type}
      end
    end
    
    defp handle_step_completed_event(event, saga_registry) do
      saga_id = event.saga_id
      
      case Map.get(saga_registry, saga_id) do
        nil ->
          {:error, :saga_not_found}
        
        saga_state ->
          # Update saga state with completed step
          updated_state = update_saga_with_completed_step(saga_state, event)
          
          # Check if saga is complete
          if saga_complete?(updated_state) do
            finalize_saga(updated_state)
          else
            continue_saga_execution(updated_state)
          end
      end
    end
    
    defp handle_step_failed_event(event, saga_registry) do
      saga_id = event.saga_id
      
      case Map.get(saga_registry, saga_id) do
        nil ->
          {:error, :saga_not_found}
        
        saga_state ->
          Logger.error("Saga step failed: #{inspect(event)}")
          initiate_saga_compensation(saga_state, event.failure_reason)
      end
    end
    
    defp handle_ttl_warning_event(event, saga_registry) do
      saga_id = event.saga_id
      
      case Map.get(saga_registry, saga_id) do
        nil ->
          {:error, :saga_not_found}
        
        saga_state ->
          Logger.warning("TTL warning for saga #{saga_id}: #{event.warning_message}")
          
          # Attempt TTL optimization
          case optimize_saga_ttl_usage(saga_state) do
            {:ok, optimized_state} ->
              {:ok, optimized_state}
            
            {:error, :cannot_optimize} ->
              # Consider early saga termination with partial compensation
              initiate_controlled_saga_termination(saga_state)
          end
      end
    end
    
    defp handle_participant_ready_event(event, saga_registry) do
      saga_id = event.saga_id
      participant_id = event.participant_id
      
      case Map.get(saga_registry, saga_id) do
        nil ->
          {:error, :saga_not_found}
        
        saga_state ->
          # Mark participant as ready
          updated_state = mark_participant_ready(saga_state, participant_id)
          
          # Check if all participants are ready
          if all_participants_ready?(updated_state) do
            begin_saga_execution(updated_state)
          else
            {:ok, updated_state}
          end
      end
    end
    
    # Helper functions for saga event processing
    defp update_saga_with_completed_step(saga_state, event) do
      # Implementation would update saga step tracking
      %{saga_state | completed_steps: [event.step_id | saga_state.completed_steps]}
    end
    
    defp saga_complete?(saga_state) do
      required_steps = saga_state.total_steps
      completed_steps = length(saga_state.completed_steps)
      completed_steps >= required_steps
    end
    
    defp finalize_saga(saga_state) do
      Logger.info("Saga #{saga_state.saga_id} completed successfully")
      {:ok, %{saga_state | status: :completed}}
    end
    
    defp continue_saga_execution(saga_state) do
      Logger.info("Continuing saga #{saga_state.saga_id} execution")
      {:ok, saga_state}
    end
    
    defp initiate_saga_compensation(saga_state, failure_reason) do
      Logger.warning("Initiating compensation for saga #{saga_state.saga_id}")
      {:ok, %{saga_state | status: :compensating, failure_reason: failure_reason}}
    end
    
    defp optimize_saga_ttl_usage(saga_state) do
      # Attempt to optimize TTL usage across remaining steps
      Logger.info("Optimizing TTL usage for saga #{saga_state.saga_id}")
      {:ok, saga_state}  # Placeholder implementation
    end
    
    defp initiate_controlled_saga_termination(saga_state) do
      Logger.warning("Initiating controlled termination for saga #{saga_state.saga_id}")
      {:ok, %{saga_state | status: :terminated_controlled}}
    end
    
    defp mark_participant_ready(saga_state, participant_id) do
      ready_participants = [participant_id | saga_state.ready_participants || []]
      %{saga_state | ready_participants: ready_participants}
    end
    
    defp all_participants_ready?(saga_state) do
      ready_count = length(saga_state.ready_participants || [])
      total_count = length(saga_state.participants)
      ready_count >= total_count
    end
    
    defp begin_saga_execution(saga_state) do
      Logger.info("Beginning execution for saga #{saga_state.saga_id}")
      {:ok, %{saga_state | status: :executing}}
    end
  end
end