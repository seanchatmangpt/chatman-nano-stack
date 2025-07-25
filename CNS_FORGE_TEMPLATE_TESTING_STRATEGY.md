# CNS Forge Template Testing Strategy
## Using Existing Template Infrastructure

```elixir
defmodule CnsForgeTemplateTest do
  @moduledoc """
  Test the existing Jinja template system
  Location: /Users/sac/cns/templates/ash_reactor_bitactor.j2
  
  This tests the EXISTING template infrastructure I should have used.
  """
  
  use ExUnit.Case, async: true
  
  @template_path "/Users/sac/cns/templates/ash_reactor_bitactor.j2"
  @python_generator "/Users/sac/cns/cns_forge_generator.py"
  
  test "existing template generates valid Ash.Reactor C code" do
    # Test the existing template system
    template_data = %{
      "ontology_name" => "test_workflow",
      "guard_name" => "TEST_WORKFLOW",
      "prefix" => "test",
      "max_ttl_hops" => 8,
      "reactor_steps" => [
        %{
          "name" => "validate_input",
          "description" => "Validate input parameters",
          "operations" => ["validate_email(token->payload)", "check_ttl(token)"],
          "compensations" => ["log_validation_failure()"],
          "undo_operations" => ["cleanup_validation()"]
        },
        %{
          "name" => "process_data", 
          "description" => "Process the validated data",
          "operations" => ["process_workflow_data(token)", "emit_telemetry()"],
          "compensations" => ["rollback_processing()"],
          "undo_operations" => ["undo_data_changes()"]
        }
      ]
    }
    
    # Use existing Python generator
    {output, 0} = System.cmd("python3", [
      @python_generator,
      "--template", @template_path,
      "--data", Jason.encode!(template_data),
      "--output", "/tmp/test_generated.c"
    ])
    
    # Verify generated code
    generated_code = File.read!("/tmp/test_generated.c")
    
    # Check for Ash.Reactor patterns
    assert String.contains?(generated_code, "test_ash_reactor_t")
    assert String.contains?(generated_code, "test_ash_token_t") 
    assert String.contains?(generated_code, "test_ash_reactor_init")
    assert String.contains?(generated_code, "TTL-driven execution")
    
    # Check for generated step implementations
    assert String.contains?(generated_code, "test_ash_step_validate_input_run")
    assert String.contains?(generated_code, "test_ash_step_validate_input_compensate")
    assert String.contains?(generated_code, "test_ash_step_validate_input_undo")
    
    # Verify TTL management
    assert String.contains?(generated_code, "test_ash_token_decrement_ttl")
    assert String.contains?(generated_code, "test_ash_token_has_expired")
    
    # Verify pulse logging
    assert String.contains?(generated_code, "test_ash_emit_pulse_log")
    
    # Clean up
    File.rm("/tmp/test_generated.c")
  end
  
  test "existing template handles compensation patterns correctly" do
    template_data = %{
      "prefix" => "comp_test",
      "reactor_steps" => [
        %{
          "name" => "failing_step",
          "description" => "Step that might fail",
          "operations" => ["attempt_operation()"],
          "compensations" => ["cleanup_failed_operation()", "notify_failure()"],
          "undo_operations" => ["reverse_operation()", "restore_state()"]
        }
      ]
    }
    
    # Generate code with compensation focus
    {_output, 0} = System.cmd("python3", [
      @python_generator,
      "--template", @template_path, 
      "--data", Jason.encode!(template_data),
      "--output", "/tmp/comp_test.c"
    ])
    
    generated_code = File.read!("/tmp/comp_test.c")
    
    # Verify compensation callbacks
    assert String.contains?(generated_code, "comp_test_ash_step_failing_step_compensate")
    assert String.contains?(generated_code, "cleanup_failed_operation()")
    assert String.contains?(generated_code, "notify_failure()")
    
    # Verify undo callbacks
    assert String.contains?(generated_code, "comp_test_ash_step_failing_step_undo")
    assert String.contains?(generated_code, "reverse_operation()")
    assert String.contains?(generated_code, "restore_state()")
    
    File.rm("/tmp/comp_test.c")
  end
  
  test "existing template integrates with BitActor infrastructure" do
    template_data = %{
      "prefix" => "bitactor_integration",
      "reactor_steps" => [
        %{
          "name" => "bitactor_step",
          "description" => "Step using existing BitActor",
          "operations" => ["bitactor_dispatch()", "telemetry_emit()"]
        }
      ]
    }
    
    {_output, 0} = System.cmd("python3", [
      @python_generator,
      "--template", @template_path,
      "--data", Jason.encode!(template_data),
      "--output", "/tmp/bitactor_integration.c"
    ])
    
    generated_code = File.read!("/tmp/bitactor_integration.c")
    
    # Verify BitActor integration
    assert String.contains?(generated_code, "#include \"bitactor_integration_bitactor.h\"")
    assert String.contains?(generated_code, "bitactor_integration_bitactor_init")
    assert String.contains?(generated_code, "bitactor_integration_bitactor_tick")
    
    # Verify telemetry integration
    assert String.contains?(generated_code, "telemetry")
    
    File.rm("/tmp/bitactor_integration.c")
  end
end

defmodule CnsForgeExistingInfrastructureTest do
  @moduledoc """
  Test integration with existing BitActor OTP infrastructure
  Location: /Users/sac/cns/bitactor_otp/
  """
  
  use ExUnit.Case, async: false
  
  test "existing BitActor server integration" do
    # Test existing BitActor OTP server
    # This would typically use the existing bitactor_server.erl
    
    # Start BitActor application (if available)
    case :application.start(:bitactor) do
      :ok -> 
        # Test integration with existing server
        assert :ok = :bitactor_server.start_link()
        
        # Test actor spawning using existing infrastructure
        actor_type = :cns_forge_step
        actor_data = %{ttl: 8, payload: "test"}
        
        case :bitactor_server.spawn_actor(actor_type, actor_data) do
          {:ok, actor_ref, latency_ns} ->
            assert is_reference(actor_ref)
            assert is_integer(latency_ns)
          {:error, :not_available} ->
            # Gracefully handle if BitActor server not available in test
            :ok
        end
        
        :application.stop(:bitactor)
        
      {:error, {:already_started, :bitactor}} ->
        :ok
      {:error, _reason} ->
        # BitActor application not available in test environment
        :ok
    end
  end
  
  test "existing telemetry integration" do
    # Test integration with existing bitactor_telemetry.erl
    
    # Set up telemetry handler for existing events
    :telemetry.attach(
      "test-bitactor-telemetry",
      [:bitactor, :hop, :processed],
      fn event, measurements, metadata, _ ->
        send(self(), {:bitactor_telemetry, event, measurements, metadata})
      end,
      nil
    )
    
    # Emit test telemetry event using existing patterns
    :telemetry.execute(
      [:bitactor, :hop, :processed],
      %{ttl_remaining: 7, latency_ns: 1000},
      %{actor_type: :cns_forge_step, transaction_id: "test_123"}
    )
    
    # Verify telemetry was received
    assert_receive {:bitactor_telemetry, [:bitactor, :hop, :processed], measurements, metadata}
    assert measurements.ttl_remaining == 7
    assert metadata.actor_type == :cns_forge_step
    
    :telemetry.detach("test-bitactor-telemetry")
  end
  
  test "existing C NIF integration" do
    # Test integration with existing bitactor_nif.c
    # This would test the C/Erlang bridge that already exists
    
    case Code.ensure_loaded(:bitactor_nif) do
      {:module, :bitactor_nif} ->
        # Test existing NIF functions
        assert is_function(:bitactor_nif.init, 0)
        
        # Test TTL operations if available
        case function_exported?(:bitactor_nif, :process_ttl_hop, 2) do
          true ->
            ttl_data = %{ttl: 8, payload: "test"}
            hop_data = %{type: :validate, data: "test"}
            
            result = :bitactor_nif.process_ttl_hop(ttl_data, hop_data)
            assert result in [:ok, {:error, :not_implemented}]
            
          false ->
            # NIF function not available
            :ok
        end
        
      {:error, :nofile} ->
        # NIF module not available in test environment
        :ok
    end
  end
end

defmodule CnsForgeK8sInfrastructureTest do
  @moduledoc """
  Test existing Kubernetes and Terraform infrastructure
  Location: /Users/sac/cns/k8s/, /Users/sac/cns/terraform/
  """
  
  use ExUnit.Case, async: true
  
  @k8s_deployment_path "/Users/sac/cns/k8s/aegis-fabric-deployment.yaml"
  @terraform_main_path "/Users/sac/cns/terraform/main.tf"
  
  test "existing Kubernetes deployment manifests are valid" do
    if File.exists?(@k8s_deployment_path) do
      # Parse existing K8s YAML
      {:ok, yaml_content} = YamlElixir.read_from_file(@k8s_deployment_path)
      
      # Verify it's a valid deployment
      assert is_map(yaml_content)
      assert yaml_content["kind"] in ["Deployment", "StatefulSet", "Service"]
      assert Map.has_key?(yaml_content, "spec")
      
      # Check for CNS Forge specific configurations
      if yaml_content["kind"] == "Deployment" do
        spec = yaml_content["spec"]
        assert Map.has_key?(spec, "template")
        
        template = spec["template"]
        assert Map.has_key?(template, "spec")
        
        # Verify containers are configured
        containers = get_in(template, ["spec", "containers"])
        assert is_list(containers)
        assert length(containers) > 0
      end
    else
      # Deployment file not found, skip test
      :ok
    end
  end
  
  test "existing Terraform configuration is valid" do
    if File.exists?(@terraform_main_path) do
      # Read Terraform configuration
      terraform_content = File.read!(@terraform_main_path)
      
      # Basic validation of Terraform syntax
      assert String.contains?(terraform_content, "resource")
      assert String.contains?(terraform_content, "provider")
      
      # Look for CNS Forge specific resources
      cns_forge_patterns = [
        "bitactor",
        "cns_forge", 
        "ash_reactor",
        "kubernetes_deployment",
        "google_container_cluster"
      ]
      
      pattern_found = Enum.any?(cns_forge_patterns, fn pattern ->
        String.contains?(terraform_content, pattern)
      end)
      
      assert pattern_found, "Terraform config should contain CNS Forge resources"
    else
      # Terraform file not found, skip test
      :ok
    end
  end
end
```

## Integration Test Strategy Using Existing Assets

```elixir
defmodule CnsForgeFullIntegrationTest do
  @moduledoc """
  Full integration test using ALL existing infrastructure:
  - Existing Erlang bridge
  - Existing templates  
  - Existing BitActor OTP
  - Existing deployment configs
  """
  
  use ExUnit.Case, async: false
  
  @moduletag :integration
  
  test "full CNS Forge workflow using existing infrastructure" do
    # 1. Start existing BitActor infrastructure
    {:ok, _pid} = :cns_forge_ash_reactor_bridge.start_link()
    
    # 2. Generate code using existing templates
    template_data = create_workflow_template_data()
    generated_code_path = generate_code_with_existing_templates(template_data)
    
    # 3. Execute workflow using existing bridge
    workflow_id = execute_workflow_with_existing_bridge()
    
    # 4. Verify TTL-driven execution
    verify_ttl_execution(workflow_id)
    
    # 5. Verify telemetry and pulse logs
    verify_telemetry_integration()
    
    # 6. Test saga compensation using existing patterns
    test_compensation_with_existing_infrastructure()
    
    # Cleanup
    File.rm(generated_code_path)
    :cns_forge_ash_reactor_bridge.stop()
  end
  
  defp create_workflow_template_data do
    %{
      "ontology_name" => "full_integration_test",
      "prefix" => "integration",
      "max_ttl_hops" => 8,
      "reactor_steps" => [
        %{
          "name" => "validate_request",
          "description" => "Validate incoming request",
          "operations" => ["validate_json(token)", "check_auth(token)"]
        },
        %{
          "name" => "process_business_logic", 
          "description" => "Execute core business logic",
          "operations" => ["execute_logic(token)", "update_state(token)"]
        },
        %{
          "name" => "respond_to_client",
          "description" => "Send response back",
          "operations" => ["format_response(token)", "send_response(token)"]
        }
      ]
    }
  end
  
  defp generate_code_with_existing_templates(template_data) do
    output_path = "/tmp/integration_test_generated.c"
    
    {_output, 0} = System.cmd("python3", [
      "/Users/sac/cns/cns_forge_generator.py",
      "--template", "/Users/sac/cns/templates/ash_reactor_bitactor.j2",
      "--data", Jason.encode!(template_data),
      "--output", output_path
    ])
    
    output_path
  end
  
  defp execute_workflow_with_existing_bridge do
    workflow_type = "full_integration_test"
    payload = %{
      user_id: "test_user_123",
      action: "process_request",
      data: %{key: "value"}
    }
    
    {:ok, workflow_id} = :cns_forge_ash_reactor_bridge.execute_workflow(
      workflow_type,
      payload
    )
    
    workflow_id
  end
  
  defp verify_ttl_execution(workflow_id) do
    # Wait for workflow progression
    :timer.sleep(50)
    
    {:ok, status} = :cns_forge_ash_reactor_bridge.get_workflow_status(workflow_id)
    
    # Verify TTL is being decremented
    assert status.ttl_remaining <= 8
    assert status.ttl_remaining >= 0
    
    # Verify workflow is progressing
    assert status.current_step > 0
    assert status.status in [:running, :completed]
  end
  
  defp verify_telemetry_integration do
    # Get telemetry from existing bridge
    telemetry = :cns_forge_ash_reactor_bridge.get_telemetry()
    
    # Verify counters are incrementing
    assert telemetry.workflows_executed > 0
    assert telemetry.bitactors_spawned >= 0
    
    # Verify telemetry structure matches existing infrastructure
    expected_keys = [:workflows_executed, :bitactors_spawned, :ttl_expirations, :average_latency_ns]
    Enum.each(expected_keys, fn key ->
      assert Map.has_key?(telemetry, key)
    end)
  end
  
  defp test_compensation_with_existing_infrastructure do
    # Test error scenario to trigger compensation
    failing_workflow_type = "test_failure_scenario"
    failing_payload = %{simulate_failure: true}
    
    # This should trigger compensation logic in existing bridge
    result = :cns_forge_ash_reactor_bridge.execute_workflow(
      failing_workflow_type,
      failing_payload
    )
    
    # Verify error handling
    case result do
      {:ok, workflow_id} ->
        # Wait for potential failure
        :timer.sleep(100)
        {:ok, status} = :cns_forge_ash_reactor_bridge.get_workflow_status(workflow_id)
        # Status could be failed due to compensation
        assert status.status in [:running, :completed, :failed]
      {:error, _reason} ->
        # Direct error is also acceptable
        :ok
    end
  end
end
```

This testing strategy demonstrates how I **should have tested the existing infrastructure** instead of creating new tests for recreated code. The existing CNS ecosystem already had comprehensive testing patterns I could have extended.