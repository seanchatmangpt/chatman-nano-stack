defmodule CNSForge.Workflows.ProcessDirective do
  @moduledoc """
  Reactor workflow for processing high-level directives into BitActor mesh execution
  
  Implements the CNS Forge vision of translating outcome-based directives into
  a self-optimizing fabric of deterministic, atomic operations.
  """
  
  use Reactor

  @doc """
  Main workflow for processing a directive through the BitActor mesh
  
  Input token structure:
  %{
    directive: "Create user with premium subscription",
    ttl: 8,
    transaction_id: "abc123...",
    timestamp: DateTime.utc_now()
  }
  """
  
  # Step 1: Create stimulus BitActor (HTTP ingress)
  step :create_stimulus do
    argument :directive_token, input(:directive_token)
    
    run fn args, _context ->
      CNSForge.BitActor.create!(%{
        type: :stimulus,
        transaction_id: args.directive_token.transaction_id,
        token: args.directive_token,
        ttl: args.directive_token.ttl
      })
    end
  end
  
  # Step 2: Parse and decode directive (parallel with validation)
  step :parse_directive do
    argument :stimulus, result(:create_stimulus)
    
    run fn args, _context ->
      input_token = args.stimulus.token
      
      # Check TTL before proceeding
      if input_token.ttl <= 0 do
        {:error, :ttl_expired}
      else
        CNSForge.BitActor.execute_hop!(args.stimulus, %{
          input_token: input_token,
          operation: :decode_params
        })
      end
    end
    
    compensate fn _error, args, _context ->
      # Saga compensation - cleanup on failure
      CNSForge.BitActor.fail!(args.stimulus, %{
        error_message: "Directive parsing failed"
      })
    end
  end
  
  # Step 3: Validate parsed directive  
  step :validate_directive do
    argument :parsed_result, result(:parse_directive)
    
    run fn args, _context ->
      if args.parsed_result.ttl <= 0 do
        CNSForge.BitActor.expire_ttl!(args.parsed_result)
        {:error, :ttl_expired}
      else
        CNSForge.BitActor.execute_hop!(args.parsed_result, %{
          input_token: args.parsed_result.result,
          operation: :validate_input
        })
      end
    end
  end
  
  # Step 4: Route to appropriate workflow engine
  step :route_to_workflow do
    argument :validated_result, result(:validate_directive)
    
    run fn args, _context ->
      case determine_workflow_type(args.validated_result.result) do
        :user_management -> 
          execute_user_workflow(args.validated_result)
        :subscription_management ->
          execute_subscription_workflow(args.validated_result) 
        :system_management ->
          execute_system_workflow(args.validated_result)
        _ ->
          {:error, :unknown_workflow_type}
      end
    end
  end
  
  # Step 5: Execute business logic (memory layer interaction)
  step :execute_business_logic do
    argument :workflow_result, result(:route_to_workflow)
    
    run fn args, _context ->
      # This would interact with Mnesia via Ash.DataLayer.Mnesia
      # for transactional state management
      transaction do
        CNSForge.BitActor.execute_hop!(args.workflow_result, %{
          input_token: args.workflow_result.result,
          operation: :process_signal
        })
      end
    end
    
    undo fn result, _args, _context ->
      # Saga undo - reverse the business logic changes
      CNSForge.BitActor.fail!(result, %{
        error_message: "Business logic execution failed - rolling back"
      })
    end
  end
  
  # Step 6: Generate response/actuation
  step :generate_response do
    argument :business_result, result(:execute_business_logic)
    
    run fn args, _context ->
      final_result = CNSForge.BitActor.execute_hop!(args.business_result, %{
        input_token: args.business_result.result,
        operation: :emit_response
      })
      
      # Final pulse log for complete transaction
      :telemetry.execute(
        [:cns_forge, :transaction, :completed],
        %{total_hops: 8 - final_result.ttl},
        %{
          transaction_id: final_result.transaction_id,
          status: :success,
          final_token: final_result.result
        }
      )
      
      final_result
    end
  end

  # Private helper functions
  
  defp determine_workflow_type(token) do
    case token.directive do
      directive when is_binary(directive) ->
        cond do
          String.contains?(directive, ["user", "account"]) -> :user_management
          String.contains?(directive, ["subscription", "billing"]) -> :subscription_management
          String.contains?(directive, ["system", "alert"]) -> :system_management
          true -> :unknown
        end
      _ -> :unknown
    end
  end
  
  defp execute_user_workflow(bit_actor) do
    # Simplified user workflow execution
    result_token = bit_actor.result
    |> Map.put(:workflow_type, :user_management)
    |> Map.put(:executed_at, DateTime.utc_now())
    
    CNSForge.BitActor.complete!(bit_actor, %{result_token: result_token})
  end
  
  defp execute_subscription_workflow(bit_actor) do
    # Simplified subscription workflow execution  
    result_token = bit_actor.result
    |> Map.put(:workflow_type, :subscription_management)
    |> Map.put(:executed_at, DateTime.utc_now())
    
    CNSForge.BitActor.complete!(bit_actor, %{result_token: result_token})
  end
  
  defp execute_system_workflow(bit_actor) do
    # Simplified system workflow execution
    result_token = bit_actor.result
    |> Map.put(:workflow_type, :system_management) 
    |> Map.put(:executed_at, DateTime.utc_now())
    
    CNSForge.BitActor.complete!(bit_actor, %{result_token: result_token})
  end
end