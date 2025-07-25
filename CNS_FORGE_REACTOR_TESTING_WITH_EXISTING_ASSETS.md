defmodule CnsForgeExistingBridgeTest do
  @moduledoc """
  Test the existing CNS Forge Ash.Reactor bridge implementation
  Location: /Users/sac/cns/cns_forge_ash_reactor_bridge.erl
  
  This tests the EXISTING infrastructure I should have used instead of recreating.
  """
  
  use ExUnit.Case, async: false
  
  # Test the existing Erlang bridge
  test "existing bridge starts and initializes correctly" do
    # Test the existing bridge implementation
    assert {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # Verify telemetry initialization
    telemetry = :cns_forge_ash_reactor_bridge.get_telemetry()
    assert is_map(telemetry)
    assert Map.has_key?(telemetry, :workflows_executed)
    assert Map.has_key?(telemetry, :bitactors_spawned)
    
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  test "existing bridge executes TTL-driven workflows" do
    {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # Test workflow execution using existing implementation
    workflow_type = "user_registration"
    initial_payload = %{name: "Test User", email: "test@example.com"}
    
    assert {:ok, workflow_id} = :cns_forge_ash_reactor_bridge.execute_workflow(
      workflow_type, 
      initial_payload
    )
    
    # Verify workflow was created
    assert is_binary(workflow_id)
    
    # Check workflow status
    assert {:ok, status} = :cns_forge_ash_reactor_bridge.get_workflow_status(workflow_id)
    assert Map.has_key?(status, :ttl_remaining)
    assert Map.has_key?(status, :status)
    
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  test "existing bridge handles TTL expiration correctly" do
    {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # Create workflow with minimal TTL
    workflow_id = create_test_workflow_with_ttl(1)
    
    # Wait for TTL expiration
    :timer.sleep(100)
    
    # Verify TTL expiration handling
    {:ok, status} = :cns_forge_ash_reactor_bridge.get_workflow_status(workflow_id)
    assert status.status in [:failed, :completed]
    
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  test "existing bridge integrates with BitActor infrastructure" do
    {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # Test BitActor step spawning
    workflow_id = create_test_workflow()
    step_type = :decode_params
    step_data = %{validation_rules: ["email_format"]}
    
    assert {:ok, step_ref} = :cns_forge_ash_reactor_bridge.spawn_bitactor_step(
      workflow_id,
      step_type, 
      step_data
    )
    
    assert is_reference(step_ref)
    
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  test "existing bridge emits pulse logs for observability" do
    {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # Set up telemetry event capture
    telemetry_events = []
    
    :telemetry.attach(
      "test-pulse-log-handler",
      [:cns_forge, :workflow, :pulse],
      fn event, measurements, metadata, _ ->
        send(self(), {:pulse_log, event, measurements, metadata})
      end,
      nil
    )
    
    # Execute workflow to generate pulse logs
    workflow_id = create_test_workflow()
    
    # Verify pulse logs are emitted
    assert_receive {:pulse_log, [:cns_forge, :workflow, :pulse], _measurements, metadata}
    assert Map.has_key?(metadata, :workflow_id)
    
    :telemetry.detach("test-pulse-log-handler")
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  # Helper functions for testing existing bridge
  defp create_test_workflow(ttl \\ 8) do
    workflow_type = "test_workflow"
    payload = %{test: true, ttl: ttl}
    
    {:ok, workflow_id} = :cns_forge_ash_reactor_bridge.execute_workflow(
      workflow_type,
      payload
    )
    
    workflow_id
  end
  
  defp create_test_workflow_with_ttl(ttl) do
    create_test_workflow(ttl)
  end
end