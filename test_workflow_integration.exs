# CNS Forge 80/20 Workflow Integration Test
# Tests the complete pipeline: TTL → Metacompiler → ReactorBuilder → Execution

IO.puts("=== CNS Forge Workflow Integration Test ===")

# Test the complete workflow integration
try do
  # First test the MetaCompiler with working TTL parsing
  IO.puts("\n1. Testing Metacompiler TTL parsing...")
  
  test_ttl = """
  @prefix cns: <http://cns-forge.org/ontology#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .
  
  cns:BitActor a owl:Class .
  cns:Signal a owl:Class .
  cns:processes a owl:ObjectProperty .
  """
  
  case CNSForge.Metacompiler.compile(%{language: :ttl, content: test_ttl}, targets: [:elixir_reactor]) do
    {:ok, result} ->
      IO.puts("✅ Metacompiler TTL parsing works")
      IO.puts("   - Language: #{result.language}")
      IO.puts("   - Node count: #{result.metadata.node_count}")
      IO.puts("   - Targets: #{Enum.join(Map.keys(result.targets), ", ")}")
      
      # Test 2: ReactorBuilder integration
      IO.puts("\n2. Testing ReactorBuilder integration...")
      
      reactor_spec = %{
        id: "test_integration_workflow",
        inputs: [:signal, :ttl],
        middleware: [:telemetry],
        steps: [
          %{
            name: :process_signal,
            type: :transform,
            arguments: [
              {:signal, quote do input(:signal) end},
              {:ttl, quote do input(:ttl) end}
            ],
            run: quote do
              fn %{signal: signal, ttl: ttl} ->
                if ttl > 0 do
                  {:ok, %{processed_signal: signal, remaining_ttl: ttl - 1}}
                else
                  {:error, :ttl_exhausted}
                end
              end
            end
          }
        ],
        return: :processed_signal
      }
      
      case CNSForge.ReactorBuilder.build(reactor_spec) do
        {:ok, reactor_module} ->
          IO.puts("✅ ReactorBuilder works")
          IO.puts("   - Generated module: #{inspect(reactor_module)}")
          
          # Test 3: End-to-end execution
          IO.puts("\n3. Testing end-to-end execution...")
          
          case reactor_module.run(%{signal: "test_signal", ttl: 3}) do
            {:ok, execution_result} ->
              IO.puts("✅ End-to-end execution works")
              IO.puts("   - Result: #{inspect(execution_result)}")
              
              # Test 4: TTL enforcement
              IO.puts("\n4. Testing TTL enforcement...")
              
              case reactor_module.run(%{signal: "test_signal", ttl: 0}) do
                {:error, :ttl_exhausted} ->
                  IO.puts("✅ TTL enforcement works correctly")
                  
                  # Test 5: Workflow Orchestrator
                  IO.puts("\n5. Testing WorkflowOrchestrator...")
                  
                  case CNSForge.WorkflowOrchestrator.test_end_to_end() do
                    {:ok, orchestrator_result} ->
                      IO.puts("✅ WorkflowOrchestrator works")
                      IO.puts("   - Test status: #{orchestrator_result.test_status}")
                      
                      IO.puts("\n🎉 ALL TESTS PASSED - WORKFLOW INTEGRATION WORKING")
                      
                    {:error, reason} ->
                      IO.puts("❌ WorkflowOrchestrator failed: #{inspect(reason)}")
                  end
                  
                other ->
                  IO.puts("❌ TTL enforcement test returned unexpected: #{inspect(other)}")
              end
              
            {:error, reason} ->
              IO.puts("❌ End-to-end execution failed: #{inspect(reason)}")
          end
          
        {:error, reason} ->
          IO.puts("❌ ReactorBuilder failed: #{inspect(reason)}")
      end
      
    {:error, reason} ->
      IO.puts("❌ Metacompiler failed: #{inspect(reason)}")
  end

rescue
  error ->
    IO.puts("❌ Test crashed: #{inspect(error)}")
    IO.puts("   Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
end

IO.puts("\n=== Test Complete ===")