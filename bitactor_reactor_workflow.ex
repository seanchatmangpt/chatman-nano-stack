defmodule BitActor.Reactor.SignalProcessingWorkflow do
  @moduledoc """
  Reactor workflow for BitActor signal processing with TTL enforcement
  """
  use Reactor

  input :signal_id
  input :actor_id
  input :ttl_constraints, default: %{max_execution_ms: 10}

  # Step 1: Load the signal
  step :load_signal do
    argument :signal_id, input(:signal_id)
    
    run fn %{signal_id: signal_id}, _context ->
      case BitActor.Ash.Resources.Signal.get(signal_id) do
        {:ok, signal} -> {:ok, signal}
        {:error, _} -> {:error, "Signal not found: #{signal_id}"}
      end
    end
  end

  # Step 2: Load the target actor
  step :load_actor do
    argument :actor_id, input(:actor_id)
    
    run fn %{actor_id: actor_id}, _context ->
      case BitActor.Ash.Resources.Actor.get(actor_id) do
        {:ok, actor} -> {:ok, actor}
        {:error, _} -> {:error, "Actor not found: #{actor_id}"}
      end
    end
  end

  # Step 3: Validate TTL constraints
  step :validate_ttl do
    argument :signal, result(:load_signal)
    argument :actor, result(:load_actor)
    argument :ttl_constraints, input(:ttl_constraints)
    
    run fn args, _context ->
      actor_ttl_ms = args.actor.ttl_budget_ms
      max_ttl_ms = args.ttl_constraints.max_execution_ms
      
      if actor_ttl_ms <= max_ttl_ms do
        {:ok, %{ttl_budget_ns: actor_ttl_ms * 1_000_000, valid: true}}
      else
        {:error, "Actor TTL budget exceeds maximum: #{actor_ttl_ms}ms > #{max_ttl_ms}ms"}
      end
    end
  end

  # Step 4: Process the signal with TTL monitoring
  step :process_signal do
    argument :signal, result(:load_signal)
    argument :actor, result(:load_actor)
    argument :ttl_budget, result(:validate_ttl, [:ttl_budget_ns])
    
    max_retries 1
    
    run fn args, _context ->
      start_time = System.monotonic_time(:nanosecond)
      
      # Process via GenServer
      result = BitActor.GenServer.process_signal(
        {:via, Registry, {BitActor.Registry, args.actor.id}},
        args.signal
      )
      
      end_time = System.monotonic_time(:nanosecond)
      processing_time_ns = end_time - start_time
      
      case result do
        {:ok, response} ->
          {:ok, %{
            response: response,
            processing_time_ns: processing_time_ns,
            ttl_compliant: processing_time_ns <= args.ttl_budget
          }}
          
        {:error, :ttl_exceeded} ->
          {:error, "TTL exceeded during signal processing"}
          
        {:error, reason} ->
          {:error, "Signal processing failed: #{inspect(reason)}"}
      end
    end
    
    compensate fn error, args, _context ->
      # Record the failure
      BitActor.Ash.Resources.Actor.update!(args.actor.id, %{
        signals_failed: args.actor.signals_failed + 1,
        status: :error
      })
      
      :ok
    end
  end

  # Step 5: Update actor metrics
  step :update_metrics do
    argument :actor, result(:load_actor)
    argument :signal, result(:load_signal)
    argument :processing_result, result(:process_signal)
    
    run fn args, _context ->
      updates = %{
        signals_processed: args.actor.signals_processed + 1,
        last_processing_time_ns: args.processing_result.processing_time_ns,
        last_signal_id: args.signal.id,
        last_active_at: DateTime.utc_now()
      }
      
      {:ok, _} = BitActor.Ash.Resources.Actor.update(args.actor.id, updates)
      
      {:ok, %{metrics_updated: true}}
    end
  end

  # Step 6: Record telemetry
  step :record_telemetry do
    argument :actor, result(:load_actor)
    argument :processing_result, result(:process_signal)
    
    run fn args, _context ->
      frames = [
        %{
          bitactor_id: args.actor.id,
          metric_name: "processing_time",
          value: args.processing_result.processing_time_ns / 1_000_000.0,  # Convert to ms
          unit: :ms
        },
        %{
          bitactor_id: args.actor.id,
          metric_name: "signals_processed",
          value: 1,
          unit: :count
        }
      ]
      
      # Record telemetry frames
      Enum.each(frames, &BitActor.Ash.Resources.TelemetryFrame.record_telemetry!/1)
      
      {:ok, %{telemetry_recorded: length(frames)}}
    end
  end

  # Step 7: Check for TTL violations
  step :check_ttl_violation do
    argument :actor, result(:load_actor)
    argument :signal, result(:load_signal)
    argument :processing_result, result(:process_signal)
    argument :ttl_budget, result(:validate_ttl, [:ttl_budget_ns])
    
    run fn args, _context ->
      if not args.processing_result.ttl_compliant do
        violation = %{
          actor_id: args.actor.id,
          signal_id: args.signal.id,
          expected_ttl_ns: args.ttl_budget,
          actual_time_ns: args.processing_result.processing_time_ns,
          violation_amount_ns: args.processing_result.processing_time_ns - args.ttl_budget
        }
        
        {:ok, _} = BitActor.Ash.Resources.TTLViolation.record_violation(violation)
        
        {:ok, %{violation_recorded: true}}
      else
        {:ok, %{violation_recorded: false}}
      end
    end
  end

  return :process_signal
end

# =============================================================================
# Swarm Coordination Workflow
# =============================================================================

defmodule BitActor.Reactor.SwarmCoordinationWorkflow do
  @moduledoc """
  Reactor workflow for coordinating BitActor swarms
  """
  use Reactor

  input :swarm_config_id
  input :operation
  input :parameters, default: %{}

  # Step 1: Load swarm configuration
  step :load_config do
    argument :config_id, input(:swarm_config_id)
    
    run fn %{config_id: config_id}, _context ->
      case BitActor.Ash.Resources.SwarmConfiguration.get(config_id) do
        {:ok, config} -> {:ok, config}
        {:error, _} -> {:error, "Swarm configuration not found"}
      end
    end
  end

  # Step 2: Load swarm actors
  step :load_actors do
    argument :config, result(:load_config)
    
    run fn %{config: config}, _context ->
      # In real implementation, this would query actors belonging to swarm
      actors = BitActor.Ash.Resources.Actor.list_active_actors!()
      {:ok, actors}
    end
  end

  # Step 3: Execute swarm operation
  step :execute_operation do
    argument :operation, input(:operation)
    argument :parameters, input(:parameters)
    argument :config, result(:load_config)
    argument :actors, result(:load_actors)
    
    run fn args, _context ->
      case args.operation do
        :scale_up ->
          scale_up_swarm(args.config, args.parameters)
          
        :scale_down ->
          scale_down_swarm(args.config, args.parameters)
          
        :rebalance ->
          rebalance_swarm(args.config, args.actors)
          
        :health_check ->
          check_swarm_health(args.actors)
          
        _ ->
          {:error, "Unknown operation: #{args.operation}"}
      end
    end
  end

  # Step 4: Update swarm metrics
  step :update_swarm_metrics do
    argument :config, result(:load_config)
    argument :actors, result(:load_actors)
    argument :operation_result, result(:execute_operation)
    
    run fn args, _context ->
      metrics = calculate_swarm_metrics(args.actors)
      
      telemetry_frames = [
        %{
          bitactor_id: args.config.id,  # Use swarm ID as actor ID
          metric_name: "swarm_size",
          value: length(args.actors),
          unit: :count
        },
        %{
          bitactor_id: args.config.id,
          metric_name: "swarm_active_actors",
          value: Enum.count(args.actors, & &1.status == :active),
          unit: :count
        }
      ]
      
      Enum.each(telemetry_frames, &BitActor.Ash.Resources.TelemetryFrame.record_telemetry!/1)
      
      {:ok, metrics}
    end
  end

  return :execute_operation

  # Private helper functions
  defp scale_up_swarm(config, parameters) do
    count = Map.get(parameters, :count, 1)
    current_actors = BitActor.Ash.Resources.Actor.list_actors!()
    
    if length(current_actors) + count <= config.max_actors do
      # Create new actors
      new_actors = Enum.map(1..count, fn i ->
        BitActor.Ash.Resources.Actor.create_actor!(%{
          name: "SwarmActor_#{System.unique_integer()}",
          ttl_budget_ms: config.actor_ttl_budget_ms,
          status: :active
        })
      end)
      
      {:ok, %{scaled_up: count, new_actors: new_actors}}
    else
      {:error, "Cannot scale beyond max_actors limit"}
    end
  end

  defp scale_down_swarm(config, parameters) do
    count = Map.get(parameters, :count, 1)
    
    # Select actors to terminate (prefer idle ones)
    actors_to_remove = BitActor.Ash.Resources.Actor.list_actors!()
    |> Enum.filter(& &1.status == :inactive)
    |> Enum.take(count)
    
    # Terminate selected actors
    Enum.each(actors_to_remove, fn actor ->
      BitActor.GenServer.update_status(
        {:via, Registry, {BitActor.Registry, actor.id}},
        :terminated
      )
    end)
    
    {:ok, %{scaled_down: length(actors_to_remove)}}
  end

  defp rebalance_swarm(config, actors) do
    # Redistribute load based on topology
    case config.topology do
      :hierarchical ->
        rebalance_hierarchical(actors)
      :mesh ->
        rebalance_mesh(actors)
      :ring ->
        rebalance_ring(actors)
      :star ->
        rebalance_star(actors)
    end
  end

  defp check_swarm_health(actors) do
    health_metrics = %{
      total_actors: length(actors),
      active_actors: Enum.count(actors, & &1.status == :active),
      error_actors: Enum.count(actors, & &1.status == :error),
      avg_success_rate: calculate_avg_success_rate(actors),
      avg_ttl_utilization: calculate_avg_ttl_utilization(actors)
    }
    
    health_status = if health_metrics.error_actors > 0 or
                       health_metrics.avg_success_rate < 90.0 do
      :unhealthy
    else
      :healthy
    end
    
    {:ok, %{status: health_status, metrics: health_metrics}}
  end

  defp calculate_swarm_metrics(actors) do
    %{
      actor_count: length(actors),
      total_signals_processed: Enum.sum(Enum.map(actors, & &1.signals_processed)),
      total_signals_failed: Enum.sum(Enum.map(actors, & &1.signals_failed)),
      active_ratio: Enum.count(actors, & &1.status == :active) / max(length(actors), 1)
    }
  end

  defp calculate_avg_success_rate(actors) do
    rates = Enum.map(actors, fn actor ->
      total = actor.signals_processed + actor.signals_failed
      if total > 0 do
        actor.signals_processed / total * 100
      else
        100.0
      end
    end)
    
    if Enum.empty?(rates), do: 0.0, else: Enum.sum(rates) / length(rates)
  end

  defp calculate_avg_ttl_utilization(actors) do
    utilizations = Enum.map(actors, fn actor ->
      if actor.last_processing_time_ns && actor.ttl_budget_ms > 0 do
        (actor.last_processing_time_ns / (actor.ttl_budget_ms * 1_000_000)) * 100
      else
        0.0
      end
    end)
    
    if Enum.empty?(utilizations), do: 0.0, else: Enum.sum(utilizations) / length(utilizations)
  end

  defp rebalance_hierarchical(actors), do: {:ok, %{rebalanced: true, topology: :hierarchical}}
  defp rebalance_mesh(actors), do: {:ok, %{rebalanced: true, topology: :mesh}}
  defp rebalance_ring(actors), do: {:ok, %{rebalanced: true, topology: :ring}}
  defp rebalance_star(actors), do: {:ok, %{rebalanced: true, topology: :star}}
end

# =============================================================================
# TTL Enforcement Workflow
# =============================================================================

defmodule BitActor.Reactor.TTLEnforcementWorkflow do
  @moduledoc """
  Reactor workflow for strict TTL enforcement
  """
  use Reactor

  input :actor_id
  input :operation
  input :ttl_budget_ns

  # Step 1: Start TTL timer
  step :start_timer do
    argument :ttl_budget_ns, input(:ttl_budget_ns)
    
    run fn %{ttl_budget_ns: ttl_budget_ns}, context ->
      start_time = System.monotonic_time(:nanosecond)
      deadline = start_time + ttl_budget_ns
      
      {:ok, %{
        start_time: start_time,
        deadline: deadline,
        timer_ref: make_ref()
      }}
    end
  end

  # Step 2: Execute operation with deadline
  step :execute_with_deadline do
    argument :operation, input(:operation)
    argument :actor_id, input(:actor_id)
    argument :timer, result(:start_timer)
    
    run fn args, _context ->
      # Check if we still have time
      current_time = System.monotonic_time(:nanosecond)
      
      if current_time < args.timer.deadline do
        # Execute the operation
        remaining_ns = args.timer.deadline - current_time
        
        task = Task.async(fn ->
          args.operation.()
        end)
        
        timeout_ms = div(remaining_ns, 1_000_000)
        
        case Task.yield(task, timeout_ms) || Task.shutdown(task) do
          {:ok, result} -> result
          nil -> {:error, :ttl_exceeded}
        end
      else
        {:error, :ttl_already_exceeded}
      end
    end
  end

  # Step 3: Record execution time
  step :record_execution_time do
    argument :timer, result(:start_timer)
    argument :result, result(:execute_with_deadline)
    
    run fn args, _context ->
      end_time = System.monotonic_time(:nanosecond)
      execution_time = end_time - args.timer.start_time
      
      {:ok, %{
        execution_time_ns: execution_time,
        within_budget: execution_time <= args.timer.deadline - args.timer.start_time
      }}
    end
  end

  return :execute_with_deadline
end