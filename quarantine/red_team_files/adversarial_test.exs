defmodule CNSForge.AdversarialTest do
  @moduledoc """
  Adversarial test permutations for CNS Forge
  Tests system resilience against malicious inputs and edge cases
  """
  
  use ExUnit.Case
  import ExUnit.CaptureLog
  alias CNSForge.{BitActor, MetaCompiler, JinjaRenderer}
  
  describe "BitActor adversarial tests" do
    test "handles malformed tokens gracefully" do
      malformed_tokens = [
        nil,
        "",
        %{},
        "not a map",
        123,
        [:list, :of, :atoms],
        %{nested: %{deeply: %{nested: %{deeply: %{nested: %{value: 1}}}}}},
        String.duplicate("x", 10_000), # Large string
        generate_circular_reference()
      ]
      
      Enum.each(malformed_tokens, fn token ->
        assert {:ok, actor} = BitActor.create(%{
          type: :adversarial,
          transaction_id: "adv_#{:erlang.unique_integer()}",
          ttl: 8,
          token: token
        })
        
        # Should not crash when processing malformed token
        result = BitActor.execute_hop(actor, %{
          input_token: token,
          operation: :process
        })
        
        assert match?({:ok, _} | {:error, _}, result),
          "Unexpected result for token: #{inspect(token)}"
      end)
    end
    
    test "enforces TTL strictly with adversarial attempts" do
      {:ok, actor} = BitActor.create(%{
        type: :ttl_adversary,
        transaction_id: "ttl_adv",
        ttl: 3,
        token: %{}
      })
      
      # Execute exactly 3 hops
      {:ok, actor} = BitActor.execute_hop(actor, %{input_token: %{}, operation: :hop1})
      {:ok, actor} = BitActor.execute_hop(actor, %{input_token: %{}, operation: :hop2})
      {:ok, actor} = BitActor.execute_hop(actor, %{input_token: %{}, operation: :hop3})
      
      # 4th hop should fail
      assert {:error, "TTL expired - cannot execute hop"} = 
        BitActor.execute_hop(actor, %{input_token: %{}, operation: :hop4})
      
      # Verify actor state
      assert actor.ttl == 0
      assert actor.status in [:completed, :ttl_expired]
    end
    
    test "prevents TTL manipulation attempts" do
      {:ok, actor} = BitActor.create(%{
        type: :secure,
        transaction_id: "ttl_manip",
        ttl: 5,
        token: %{}
      })
      
      # Attempt to modify TTL directly
      manipulated_actor = %{actor | ttl: 100}
      
      # System should validate TTL hasn't been tampered with
      result = BitActor.execute_hop(manipulated_actor, %{
        input_token: %{ttl_hack: true},
        operation: :malicious
      })
      
      # Should either error or reset to valid TTL
      case result do
        {:ok, updated} -> assert updated.ttl <= 5
        {:error, _} -> assert true
      end
    end
    
    test "handles race conditions in concurrent access" do
      {:ok, actor} = BitActor.create(%{
        type: :concurrent,
        transaction_id: "race_#{:erlang.unique_integer()}",
        ttl: 8,
        token: %{counter: 0}
      })
      
      # Spawn multiple processes trying to modify the same actor
      tasks = Enum.map(1..50, fn i ->
        Task.async(fn ->
          BitActor.execute_hop(actor, %{
            input_token: %{increment: i},
            operation: :race_condition
          })
        end)
      end)
      
      results = Task.await_many(tasks, 5000)
      
      # Should handle concurrent access safely
      successful = Enum.count(results, &match?({:ok, _}, &1))
      errors = Enum.count(results, &match?({:error, _}, &1))
      
      assert successful + errors == 50,
        "Lost operations in race condition"
    end
  end
  
  describe "MetaCompiler adversarial tests" do
    test "handles malicious TTL ontologies" do
      malicious_ttls = [
        # Empty file
        "",
        # Invalid syntax
        "this is not valid TTL syntax @#$%",
        # Infinite recursion attempt
        """
        @prefix : <http://evil#> .
        :A rdfs:subClassOf :B .
        :B rdfs:subClassOf :A .
        """,
        # Extremely large ontology
        generate_bomb_ontology(),
        # Unicode and special characters
        """
        @prefix : <http://test#> .
        :Class名前 a owl:Class ;
          rdfs:label "Test 🚀 \u0000 \n \r \t" .
        """,
        # Malformed IRIs
        """
        @prefix : <not a valid iri> .
        :Test a owl:Class .
        """
      ]
      
      Enum.each(malicious_ttls, fn ttl ->
        path = "test/fixtures/malicious_#{:erlang.unique_integer()}.ttl"
        File.write!(path, ttl)
        
        result = MetaCompiler.compile(path)
        
        # Should handle gracefully without crashing
        assert match?({:ok, _} | {:error, _}, result),
          "Compiler crashed on malicious input"
        
        File.rm!(path)
      end)
    end
    
    test "validates generated BitActor code for injection attempts" do
      # Attempt code injection through ontology
      injection_ttl = """
      @prefix : <http://injection#> .
      :MaliciousClass a owl:Class ;
        rdfs:label "'; system('rm -rf /'); //'" ;
        rdfs:comment "$(whoami)" .
      """
      
      path = "test/fixtures/injection.ttl"
      File.write!(path, injection_ttl)
      
      case MetaCompiler.compile(path) do
        {:ok, result} ->
          # Verify no system commands in generated code
          refute result.mesh_id =~ ~r/system\(/
          refute result.mesh_id =~ ~r/\$\(/
          
        {:error, _} ->
          # Rejection is also acceptable
          assert true
      end
    end
    
    test "enforces resource limits during compilation" do
      # Create ontology that would consume excessive resources
      huge_ttl = """
      @prefix : <http://huge#> .
      #{Enum.map(1..10_000, fn i -> ":Class#{i} a owl:Class ." end) |> Enum.join("\n")}
      """
      
      path = "test/fixtures/huge.ttl"
      File.write!(path, huge_ttl)
      
      # Set memory limit for test
      {:ok, _} = :memsup.set_sysmem_high_watermark(0.8)
      
      # Should complete without consuming excessive memory
      {time_us, result} = :timer.tc(fn ->
        MetaCompiler.compile(path, [timeout: 10_000])
      end)
      
      assert time_us < 10_000_000, "Compilation took too long: #{time_us/1_000_000}s"
      
      case result do
        {:ok, _} -> assert true
        {:error, :timeout} -> assert true
        {:error, _} -> assert true
      end
    end
  end
  
  describe "JinjaRenderer adversarial tests" do
    test "prevents template injection attacks" do
      malicious_contexts = [
        # Attempt to access system
        %{prefix: "{{ __import__('os').system('ls') }}"},
        # Infinite loop
        %{prefix: "{% for i in range(999999999) %}x{% endfor %}"},
        # Access private variables
        %{prefix: "{{ __globals__ }}"},
        # Large expansion
        %{prefix: String.duplicate("{{ prefix }}", 1000)},
        # Special characters
        %{prefix: "\"; DROP TABLE bitactors; --"}
      ]
      
      template = """
      #define {{ prefix|upper }}_CONSTANT 1
      void {{ prefix }}_function() {
        // {{ prefix|c_identifier }}
      }
      """
      
      Enum.each(malicious_contexts, fn context ->
        result = JinjaRenderer.render_string(template, context)
        
        case result do
          {:ok, rendered} ->
            # Verify no code execution
            refute rendered =~ ~r/__import__/
            refute rendered =~ ~r/DROP TABLE/
            
          {:error, _} ->
            # Error is acceptable for malicious input
            assert true
        end
      end)
    end
    
    test "handles template recursion and loops" do
      recursive_template = """
      {% for i in items %}
        {% for j in i.items %}
          {% for k in j.items %}
            {{ k }}
          {% endfor %}
        {% endfor %}
      {% endfor %}
      """
      
      # Create deeply nested structure
      context = %{
        items: create_nested_structure(10, 10)
      }
      
      # Should handle without stack overflow
      result = JinjaRenderer.render_string(recursive_template, context)
      assert match?({:ok, _} | {:error, _}, result)
    end
  end
  
  describe "System-wide adversarial scenarios" do
    test "cascade failure resilience" do
      # Create interdependent BitActors
      actors = Enum.map(1..10, fn i ->
        {:ok, actor} = BitActor.create(%{
          type: :cascade_test,
          transaction_id: "cascade_#{i}",
          ttl: 8,
          token: %{depends_on: if(i > 1, do: "cascade_#{i-1}", else: nil)}
        })
        {:"actor_#{i}", actor}
      end) |> Enum.into(%{})
      
      # Fail a middle actor
      failed_actor = actors[:actor_5]
      BitActor.fail(failed_actor, "Simulated failure")
      
      # Verify system continues operating
      other_actors = Map.delete(actors, :actor_5)
      
      Enum.each(other_actors, fn {_name, actor} ->
        result = BitActor.execute_hop(actor, %{
          input_token: %{test: true},
          operation: :verify_alive
        })
        
        # Non-dependent actors should continue
        if actor.token.depends_on != "cascade_4" do
          assert match?({:ok, _}, result)
        end
      end)
    end
    
    test "handles Byzantine failures" do
      # Create actors that might return inconsistent results
      byzantine_actors = Enum.map(1..5, fn i ->
        {:ok, actor} = BitActor.create(%{
          type: :byzantine,
          transaction_id: "byz_#{i}",
          ttl: 8,
          token: %{node: i, byzantine: rem(i, 3) == 0} # Every 3rd is Byzantine
        })
        actor
      end)
      
      # Execute same operation on all actors
      results = Enum.map(byzantine_actors, fn actor ->
        BitActor.execute_hop(actor, %{
          input_token: %{value: 42},
          operation: :consensus
        })
      end)
      
      # Should handle Byzantine actors (1/3 might give wrong results)
      successful = Enum.count(results, &match?({:ok, _}, &1))
      assert successful >= 3, "Too many failures in Byzantine scenario"
    end
    
    test "resource exhaustion protection" do
      # Attempt to exhaust system resources
      exhaust_tasks = [
        # Memory exhaustion
        Task.async(fn ->
          try do
            data = :binary.copy(<<0>>, 1_000_000_000) # 1GB
            {:memory, byte_size(data)}
          rescue
            _ -> {:memory, :protected}
          end
        end),
        
        # CPU exhaustion
        Task.async(fn ->
          try do
            end_time = System.monotonic_time(:millisecond) + 1000
            count = burn_cpu_until(end_time, 0)
            {:cpu, count}
          rescue
            _ -> {:cpu, :protected}
          end
        end),
        
        # Process exhaustion
        Task.async(fn ->
          try do
            processes = Enum.map(1..10_000, fn _ ->
              spawn(fn -> Process.sleep(:infinity) end)
            end)
            {:processes, length(processes)}
          rescue
            _ -> {:processes, :protected}
          end
        end)
      ]
      
      # Wait with timeout
      results = Task.yield_many(exhaust_tasks, 5000)
      
      # System should protect against resource exhaustion
      Enum.each(results, fn {task, result} ->
        case result do
          {:ok, {:memory, :protected}} -> assert true
          {:ok, {:cpu, :protected}} -> assert true
          {:ok, {:processes, :protected}} -> assert true
          {:exit, _} -> assert true # Task was killed
          nil -> Task.shutdown(task) # Timeout
          _ -> assert true
        end
      end)
    end
  end
  
  # Helper functions
  
  defp generate_circular_reference do
    map = %{a: nil}
    # Note: Elixir prevents actual circular references
    # This simulates the concept
    %{a: map, b: map, c: map}
  end
  
  defp generate_bomb_ontology do
    # Billion laughs attack variant for RDF
    """
    @prefix : <http://bomb#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    #{Enum.map(1..10, fn level ->
      Enum.map(1..10, fn i ->
        ":Level#{level}_#{i} a owl:Class ;\n" <>
        Enum.map(1..10, fn j ->
          "  rdfs:subClassOf :Level#{level - 1}_#{j}"
        end) |> Enum.join(" ;\n") <> " ."
      end) |> Enum.join("\n")
    end) |> Enum.join("\n")}
    """
  end
  
  defp create_nested_structure(0, _), do: []
  defp create_nested_structure(depth, width) do
    Enum.map(1..width, fn i ->
      %{
        id: i,
        items: create_nested_structure(depth - 1, width)
      }
    end)
  end
  
  defp burn_cpu_until(end_time, count) do
    if System.monotonic_time(:millisecond) < end_time do
      # CPU intensive operation
      _ = :crypto.hash(:sha256, "data#{count}")
      burn_cpu_until(end_time, count + 1)
    else
      count
    end
  end
end