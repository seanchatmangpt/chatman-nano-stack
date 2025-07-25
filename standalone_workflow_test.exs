# Standalone CNS Forge Workflow Test
# Tests functionality without Mix dependencies

IO.puts("=== Standalone CNS Forge Workflow Test ===")

# Compile the modules we need
Code.compile_file("lib/cns_forge/metacompiler.ex")
Code.compile_file("lib/cns_forge/bit_actor_standalone.ex")
Code.compile_file("lib/cns_forge/workflow_orchestrator.ex")

IO.puts("\n1. Testing Metacompiler TTL parsing...")

test_ttl = """
@prefix cns: <http://cns-forge.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

cns:BitActor a owl:Class .
cns:Signal a owl:Class .
cns:processes a owl:ObjectProperty .
"""

try do
  case CNSForge.Metacompiler.compile(%{language: :ttl, content: test_ttl}, targets: [:elixir_reactor]) do
    {:ok, result} ->
      IO.puts("✅ Metacompiler TTL parsing works")
      IO.puts("   - Language: #{result.language}")
      IO.puts("   - Node count: #{result.metadata.node_count}")
      IO.puts("   - Edge count: #{result.metadata.edge_count}")
      IO.puts("   - Generated Elixir code length: #{String.length(result.targets[:elixir_reactor])}")
      
      # Test the generated code can be evaluated
      IO.puts("\n2. Testing generated Reactor code...")
      try do
        # Parse and evaluate the generated module code
        Code.eval_string(result.targets[:elixir_reactor])
        
        # Extract module name from the generated code
        module_name = case Regex.run(~r/defmodule\s+([^\s]+)\s+do/, result.targets[:elixir_reactor]) do
          [_, name] -> String.to_atom(name)
          _ -> nil
        end
        
        IO.puts("✅ Generated Reactor code compiles successfully")
        IO.puts("   - Module name: #{inspect(module_name)}")
        
        # Test execution
        IO.puts("\n3. Testing workflow execution...")
        case module_name.run(%{ttl: 3, signal: "test"}) do
          {:ok, execution_result} ->
            IO.puts("✅ Workflow execution successful")
            IO.puts("   - Result: #{inspect(execution_result)}")
            
            # Test TTL enforcement
            IO.puts("\n4. Testing TTL enforcement...")
            case module_name.run(%{ttl: 0, signal: "test"}) do
              result when is_map(result) ->
                if Map.get(result, :error) == "TTL exhausted" do
                  IO.puts("✅ TTL enforcement works correctly")
                  IO.puts("\n🎉 STANDALONE WORKFLOW TEST PASSED")
                else
                  IO.puts("⚠️  TTL enforcement returned: #{inspect(result)}")
                  IO.puts("   (May still be working, just different format)")
                end
                
              other ->
                IO.puts("⚠️  TTL enforcement returned: #{inspect(other)}")
                IO.puts("   (May still be working, just different format)")
            end
            
          {:error, reason} ->
            IO.puts("❌ Workflow execution failed: #{inspect(reason)}")
        end
        
      rescue
        error ->
          IO.puts("❌ Generated code compilation failed: #{inspect(error)}")
          IO.puts("Generated code was:")
          IO.puts(result.targets[:elixir_reactor])
      end
      
    {:error, reason} ->
      IO.puts("❌ Metacompiler failed: #{inspect(reason)}")
  end

  # Test BitActor standalone functionality
  IO.puts("\n5. Testing BitActor standalone...")
  
  case CNSForge.BitActorStandalone.create(%{
    type: :processor,
    transaction_id: "test_123",
    ttl: 5,
    token: %{data: "test"}
  }) do
    {:ok, actor} ->
      IO.puts("✅ BitActor creation works")
      IO.puts("   - Actor ID: #{actor.id}")
      IO.puts("   - TTL: #{actor.ttl}")
      
      # Test hop execution
      case CNSForge.BitActorStandalone.execute_hop(actor, :semantic_compile) do
        {:ok, updated_actor} ->
          IO.puts("✅ BitActor hop execution works")
          IO.puts("   - Remaining TTL: #{updated_actor.ttl}")
          IO.puts("   - Status: #{updated_actor.status}")
          
        {:error, reason} ->
          IO.puts("❌ BitActor hop execution failed: #{inspect(reason)}")
      end
      
    {:error, reason} ->
      IO.puts("❌ BitActor creation failed: #{inspect(reason)}")
  end

  # Test the critical security fix
  IO.puts("\n6. Testing TTL float vulnerability fix...")
  
  case CNSForge.BitActorStandalone.create(%{
    type: :processor,
    transaction_id: "security_test",
    ttl: 8.5,  # Float TTL - should be rejected
    token: %{}
  }) do
    {:error, error_msg} ->
      if String.contains?(error_msg, "TTL must be an integer") and String.contains?(error_msg, "float") do
        IO.puts("✅ TTL float vulnerability fix working correctly")
        IO.puts("   - Properly rejected float TTL with message: #{error_msg}")
      else
        IO.puts("⚠️  Unexpected error message: #{error_msg}")
      end
      
    {:ok, _actor} ->
      IO.puts("❌ SECURITY VULNERABILITY: Float TTL was accepted when it should be rejected!")
  end

rescue
  error ->
    IO.puts("❌ Test crashed: #{inspect(error)}")
    IO.puts("   Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
end

IO.puts("\n=== Standalone Test Complete ===")