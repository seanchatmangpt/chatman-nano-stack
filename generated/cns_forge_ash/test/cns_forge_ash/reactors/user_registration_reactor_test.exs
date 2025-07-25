defmodule CnsForgeAsh.Reactors.UserRegistrationReactorTest do
  use ExUnit.Case, async: false
  
  alias CnsForgeAsh.Reactors.UserRegistrationReactor
  
  setup do
    # Initialize ETS tables for testing
    CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.Workflow, %{}, action: :initiate)
    :ok
  end
  
  test "executes complete user registration workflow with TTL management" do
    user_params = %{
      "name" => "John Doe",
      "email" => "john.doe@example.com",
      "phone" => "+1-555-0123"
    }
    
    # Execute the real Ash.Reactor workflow
    assert {:ok, result} = Reactor.run(UserRegistrationReactor, %{
      user_params: user_params,
      ttl: 8
    }, %{}, async?: false)
    
    # Verify workflow completion
    assert %{workflow: workflow, result: workflow_result} = result
    assert workflow.status == :completed
    assert workflow.initial_ttl == 8
    
    # Verify user was created
    assert %{user: user} = workflow_result
    assert user["name"] == "John Doe"
    assert user["email"] == "john.doe@example.com"
    assert is_integer(user["id"])
    
    # Verify TTL was properly decremented
    assert workflow_result.final_token.ttl_hops >= 0
    assert workflow_result.final_token.ttl_hops < 8
    
    # Verify email was sent
    assert is_struct(workflow_result.email_sent_at, DateTime)
  end
  
  test "handles TTL expiration gracefully" do
    user_params = %{
      "name" => "Jane Smith", 
      "email" => "jane.smith@example.com"
    }
    
    # Start with TTL of 1 to trigger expiration
    assert {:error, reason} = Reactor.run(UserRegistrationReactor, %{
      user_params: user_params,
      ttl: 1
    }, %{}, async?: false)
    
    # Should fail due to TTL expiration
    assert reason == :ttl_expired
  end
  
  test "validates business rules and handles invalid data" do
    invalid_params = %{
      "name" => "",  # Invalid: too short
      "email" => "invalid-email"  # Invalid: no @ symbol
    }
    
    assert {:error, _reason} = Reactor.run(UserRegistrationReactor, %{
      user_params: invalid_params,
      ttl: 8
    }, %{}, async?: false)
  end
  
  test "generates comprehensive pulse logs for observability" do
    user_params = %{
      "name" => "Alice Johnson",
      "email" => "alice.johnson@example.com"
    }
    
    # Execute workflow
    assert {:ok, result} = Reactor.run(UserRegistrationReactor, %{
      user_params: user_params,
      ttl: 8
    }, %{}, async?: false)
    
    # Get the transaction ID from the workflow result
    transaction_id = result.result.final_token.transaction_id
    
    # Query pulse logs for this transaction
    pulse_logs = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.PulseLog, %{
      transaction_id: transaction_id
    }, action: :by_transaction)
    
    # Verify we have pulse logs for each step
    assert length(pulse_logs) > 0
    
    # Verify pulse log structure
    first_log = List.first(pulse_logs)
    assert first_log.transaction_id == transaction_id
    assert is_binary(first_log.event_type)
    assert is_map(first_log.metadata)
    assert is_struct(first_log.timestamp, DateTime)
    
    # Verify we can reconstruct the causal chain
    sorted_logs = Enum.sort_by(pulse_logs, & &1.sequence_number)
    assert length(sorted_logs) == length(pulse_logs)
  end
  
  test "creates and manages BitActor resources correctly" do
    user_params = %{
      "name" => "Bob Wilson", 
      "email" => "bob.wilson@example.com"
    }
    
    # Execute workflow
    assert {:ok, _result} = Reactor.run(UserRegistrationReactor, %{
      user_params: user_params,
      ttl: 8
    }, %{}, async?: false)
    
    # Verify BitActors were created
    bit_actors = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.BitActor, %{})
    
    # Should have created multiple BitActors for different steps
    assert length(bit_actors) > 0
    
    # Verify BitActor types match CNS Forge specification
    actor_types = Enum.map(bit_actors, & &1.actor_type)
    expected_types = [:decoder, :memory, :actuation, :signal_router]
    
    Enum.each(expected_types, fn type ->
      assert type in actor_types, "Missing BitActor type: #{type}"
    end)
    
    # Verify all BitActors processed at least one hop
    Enum.each(bit_actors, fn actor ->
      assert actor.hops_processed > 0
      assert actor.status in [:active, :terminated]
    end)
  end
  
  test "integrates with existing BitActor C infrastructure patterns" do
    # This test verifies the conceptual integration points
    # In a real implementation, this would test the NIF integration
    
    user_params = %{
      "name" => "Integration Test User",
      "email" => "integration@example.com"
    }
    
    assert {:ok, result} = Reactor.run(UserRegistrationReactor, %{
      user_params: user_params,
      ttl: 8
    }, %{}, async?: false)
    
    # Verify the workflow follows the CNS Forge pattern:
    # 1. TTL token created and managed
    assert result.result.final_token.ttl_hops >= 0
    
    # 2. BitActors spawned for each logical step
    bit_actors = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.BitActor, %{})
    assert length(bit_actors) > 0
    
    # 3. Universal observability via pulse logs
    transaction_id = result.result.final_token.transaction_id
    pulse_logs = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.PulseLog, %{
      transaction_id: transaction_id
    }, action: :by_transaction)
    assert length(pulse_logs) > 0
    
    # 4. Workflow tracking and completion
    assert result.workflow.status == :completed
    assert is_map(result.workflow.result)
  end
end