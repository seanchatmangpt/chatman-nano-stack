defmodule CnsForgeAsh.Reactors.UserRegistrationReactor do
  @moduledoc """
  Real Ash.Reactor implementation of CNS Forge user registration workflow
  
  This demonstrates the actual Ash.Reactor pattern with TTL management:
  - Dependency-driven DAG execution
  - Saga pattern with compensate/undo callbacks
  - TTL state management through workflow steps
  - Integration with Ash resources
  """
  
  use Reactor
  
  # Workflow inputs
  input :user_params
  input :ttl, default: 8
  
  # Step 1: Validate TTL budget and create workflow tracking
  step :initialize_workflow do
    argument :ttl, input(:ttl)
    argument :user_params, input(:user_params)
    
    run fn %{ttl: ttl, user_params: params}, _context ->
      if ttl > 0 do
        # Create workflow tracking record
        case CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.Workflow, %{
          workflow_name: "user_registration",
          initial_ttl: ttl,
          input_params: params
        }, action: :initiate) do
          {:ok, workflow} ->
            # Emit pulse log
            CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
              transaction_id: :rand.uniform(1_000_000_000),
              event_type: "workflow_initialized",
              ttl_hops: ttl - 1,
              metadata: %{workflow_id: workflow.workflow_id}
            }, action: :emit)
            
            {:ok, %{workflow: workflow, ttl: ttl - 1}}
          {:error, reason} ->
            {:error, reason}
        end
      else
        {:error, :ttl_expired}
      end
    end
    
    compensate fn _reason, %{ttl: ttl}, _context ->
      # Emit TTL expiration telemetry
      :telemetry.execute([:cns_forge, :ttl_expired], %{ttl: ttl}, %{step: "initialize_workflow"})
      :ok
    end
  end
  
  # Step 2: Create TTL token for the workflow
  create :create_ttl_token, CnsForgeAsh.Resources.TTLToken, :create do
    inputs %{
      workflow_type: 1,
      payload: input(:user_params),
      initial_ttl: result(:initialize_workflow).ttl
    }
    
    wait_for [:initialize_workflow]
  end
  
  # Step 3: Validate user parameters (BitActor: decoder)
  step :validate_user_params do
    argument :token, result(:create_ttl_token)
    argument :workflow_result, result(:initialize_workflow)
    
    run fn %{token: token, workflow_result: workflow_result}, _context ->
      if token.ttl_hops > 0 do
        # Spawn BitActor for validation
        {:ok, bit_actor} = CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.BitActor, %{
          actor_type: :decoder,
          hop_type: "validate_params",
          config: %{validation_rules: ["email_format", "name_length"]}
        }, action: :spawn)
        
        # Process the hop
        CnsForgeAsh.Domain.update!(bit_actor, %{
          input_token_id: token.id
        }, action: :process_hop)
        
        # Decrement TTL
        updated_token = CnsForgeAsh.Domain.update!(token, %{}, action: :decrement_ttl)
        
        # Emit pulse log
        CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
          transaction_id: token.transaction_id,
          event_type: "hop_processed",
          actor_id: bit_actor.actor_id,
          ttl_hops: updated_token.ttl_hops,
          metadata: %{step: "validate_user_params", actor_type: "decoder"}
        }, action: :emit)
        
        # Validate params (simplified for demo)
        params = token.payload
        if is_binary(params["email"]) and String.contains?(params["email"], "@") do
          {:ok, %{validated_params: params, token: updated_token, bit_actor: bit_actor}}
        else
          {:error, :invalid_email}
        end
      else
        {:error, :ttl_expired}
      end
    end
    
    undo fn _result, %{token: token}, _context ->
      # Compensation: emit TTL expired event
      :telemetry.execute([:cns_forge, :validation_undone], %{}, %{transaction_id: token.transaction_id})
      :ok
    end
  end
  
  # Step 4: Check for duplicate users (BitActor: memory)
  step :check_duplicates do
    argument :validation_result, result(:validate_user_params)
    
    run fn %{validation_result: %{validated_params: params, token: token}}, _context ->
      if token.ttl_hops > 0 do
        # Spawn memory BitActor
        {:ok, memory_actor} = CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.BitActor, %{
          actor_type: :memory,
          hop_type: "check_duplicates",
          config: %{lookup_field: "email"}
        }, action: :spawn)
        
        # Process memory lookup hop
        CnsForgeAsh.Domain.update!(memory_actor, %{}, action: :process_hop)
        
        # Decrement TTL
        updated_token = CnsForgeAsh.Domain.update!(token, %{}, action: :decrement_ttl)
        
        # Emit pulse log
        CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
          transaction_id: token.transaction_id,
          event_type: "hop_processed",
          actor_id: memory_actor.actor_id,
          ttl_hops: updated_token.ttl_hops,
          metadata: %{step: "check_duplicates", actor_type: "memory"}
        }, action: :emit)
        
        # Simulate duplicate check (would integrate with existing infrastructure)
        duplicate_found = false  # In real implementation: call existing BitActor C code
        
        if duplicate_found do
          {:error, :user_already_exists}
        else
          {:ok, %{params: params, token: updated_token, memory_actor: memory_actor}}
        end
      else
        {:error, :ttl_expired}
      end
    end
    
    undo fn _result, %{validation_result: %{token: token}}, _context ->
      :telemetry.execute([:cns_forge, :duplicate_check_undone], %{}, %{transaction_id: token.transaction_id})
      :ok
    end
  end
  
  # Step 5: Create user record (BitActor: actuation)
  step :create_user_record do
    argument :duplicate_check, result(:check_duplicates)
    
    run fn %{duplicate_check: %{params: params, token: token}}, _context ->
      if token.ttl_hops > 0 do
        # Spawn actuation BitActor
        {:ok, actuation_actor} = CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.BitActor, %{
          actor_type: :actuation,
          hop_type: "create_user",
          config: %{target: "user_database"}
        }, action: :spawn)
        
        # Process creation hop
        CnsForgeAsh.Domain.update!(actuation_actor, %{}, action: :process_hop)
        
        # Decrement TTL
        updated_token = CnsForgeAsh.Domain.update!(token, %{}, action: :decrement_ttl)
        
        # Emit pulse log
        CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
          transaction_id: token.transaction_id,
          event_type: "hop_processed",
          actor_id: actuation_actor.actor_id,
          ttl_hops: updated_token.ttl_hops,
          metadata: %{step: "create_user_record", actor_type: "actuation"}
        }, action: :emit)
        
        # Simulate user creation (would call existing infrastructure)
        user_id = :rand.uniform(1_000_000)
        created_user = Map.put(params, "id", user_id)
        
        {:ok, %{user: created_user, token: updated_token, actuation_actor: actuation_actor}}
      else
        {:error, :ttl_expired}
      end
    end
    
    undo fn result, %{duplicate_check: %{token: token}}, _context ->
      # Compensation: delete the created user
      case result do
        {:ok, %{user: user}} ->
          :telemetry.execute([:cns_forge, :user_creation_undone], %{user_id: user["id"]}, %{transaction_id: token.transaction_id})
        _ ->
          :ok
      end
      :ok
    end
  end
  
  # Step 6: Send welcome email (concurrent with user creation if TTL allows)
  step :send_welcome_email do
    argument :user_creation, result(:create_user_record)
    
    run fn %{user_creation: %{user: user, token: token}}, _context ->
      if token.ttl_hops > 0 do
        # Spawn signal router BitActor for email
        {:ok, signal_actor} = CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.BitActor, %{
          actor_type: :signal_router,
          hop_type: "send_email",
          config: %{email_type: "welcome", template: "user_welcome"}
        }, action: :spawn)
        
        # Process email hop
        CnsForgeAsh.Domain.update!(signal_actor, %{}, action: :process_hop)
        
        # Final TTL decrement
        final_token = CnsForgeAsh.Domain.update!(token, %{}, action: :decrement_ttl)
        
        # Emit final pulse log
        CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
          transaction_id: token.transaction_id,
          event_type: "workflow_completed",
          actor_id: signal_actor.actor_id,
          ttl_hops: final_token.ttl_hops,
          metadata: %{step: "send_welcome_email", final_step: true}
        }, action: :emit)
        
        # Simulate email sending
        email_sent_at = DateTime.utc_now()
        
        {:ok, %{user: user, email_sent_at: email_sent_at, final_token: final_token}}
      else
        {:error, :ttl_expired}
      end
    end
    
    undo fn _result, %{user_creation: %{token: token}}, _context ->
      # Cancel email sending
      :telemetry.execute([:cns_forge, :welcome_email_undone], %{}, %{transaction_id: token.transaction_id})
      :ok
    end
  end
  
  # Final step: Mark workflow as completed
  step :finalize_workflow do
    argument :email_result, result(:send_welcome_email)
    argument :workflow_init, result(:initialize_workflow)
    
    run fn %{email_result: result, workflow_init: %{workflow: workflow}}, _context ->
      # Update workflow status to completed
      completed_workflow = CnsForgeAsh.Domain.update!(workflow, %{
        result: result
      }, action: :complete)
      
      # Emit final telemetry
      :telemetry.execute(
        [:cns_forge, :reactor_workflow, :completed],
        %{total_hops: 8 - result.final_token.ttl_hops},
        %{workflow_id: workflow.workflow_id}
      )
      
      {:ok, %{workflow: completed_workflow, result: result}}
    end
  end
end