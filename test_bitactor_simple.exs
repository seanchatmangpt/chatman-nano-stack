#!/usr/bin/env elixir

# Simple test of BitActor concepts from documentation
# Testing TTL-bounded execution units as described in the docs

defmodule TestBitActor do
  @moduledoc """
  Test the BitActor concepts described in documentation
  """

  defstruct [:id, :type, :transaction_id, :ttl, :token, :status, :created_at]

  def create(attrs) do
    # Test example from docs/cns-forge-documentation.md
    # {:ok, actor} = CNSForge.BitActor.create(%{
    #   type: :processor,
    #   ttl: 10,
    #   token: %{data: "payload"},
    #   transaction_id: "txn_123"
    # })
    
    id = :crypto.strong_rand_bytes(16) |> Base.encode16()
    
    actor = %TestBitActor{
      id: id,
      type: Map.get(attrs, :type, :processor),
      transaction_id: Map.get(attrs, :transaction_id),
      ttl: Map.get(attrs, :ttl, 8),
      token: Map.get(attrs, :token, %{}),
      status: :active,
      created_at: DateTime.utc_now()
    }
    
    {:ok, actor}
  end

  def execute(actor, operation) do
    IO.puts("\nExecuting operation: #{operation}")
    IO.puts("Current TTL: #{actor.ttl}")
    
    if actor.ttl <= 0 do
      {:error, :ttl_exhausted}
    else
      # Simulate operation execution
      updated_actor = %{actor | 
        ttl: actor.ttl - 1,
        token: Map.put(actor.token, :last_operation, operation)
      }
      
      IO.puts("Operation completed. New TTL: #{updated_actor.ttl}")
      {:ok, updated_actor}
    end
  end

  def hop(actor, next_token) do
    IO.puts("\nPerforming hop...")
    IO.puts("Current TTL: #{actor.ttl}")
    
    if actor.ttl <= 0 do
      {:error, :ttl_exhausted}
    else
      updated_actor = %{actor | 
        ttl: actor.ttl - 1,
        token: Map.merge(actor.token, next_token)
      }
      
      IO.puts("Hop completed. New TTL: #{updated_actor.ttl}")
      {:ok, updated_actor}
    end
  end

  def check_ttl(actor) do
    if actor.ttl > 0 do
      {:ok, actor.ttl}
    else
      {:error, :ttl_exhausted}
    end
  end

  def test_actor_creation do
    IO.puts("Testing BitActor creation...")
    
    # Test the example from documentation
    {:ok, actor} = create(%{
      type: :processor,
      ttl: 10,
      token: %{data: "payload"},
      transaction_id: "txn_123"
    })
    
    IO.puts("Created BitActor:")
    IO.inspect(actor, pretty: true)
    
    # Validate it matches documented structure
    assert actor.type == :processor
    assert actor.ttl == 10
    assert actor.token == %{data: "payload"}
    assert actor.transaction_id == "txn_123"
    assert actor.status == :active
    
    IO.puts("✅ BitActor creation matches documentation")
    {:ok, actor}
  end

  def test_actor_execution do
    IO.puts("\nTesting BitActor execution...")
    
    {:ok, actor} = create(%{
      type: :processor,
      ttl: 5,
      token: %{amount: 100.00, currency: "USD"},
      transaction_id: "txn_456"
    })
    
    # Test execution as shown in docs
    {:ok, result} = execute(actor, :process_payment)
    
    assert result.ttl == 4  # TTL decremented
    assert result.token.last_operation == :process_payment
    
    IO.puts("✅ BitActor execution works as documented")
    {:ok, result}
  end

  def test_ttl_exhaustion do
    IO.puts("\nTesting TTL exhaustion...")
    
    {:ok, actor} = create(%{type: :processor, ttl: 2})
    
    # Execute until TTL exhausted
    {:ok, actor1} = execute(actor, :operation1)
    {:ok, actor2} = execute(actor1, :operation2)
    
    # This should fail due to TTL exhaustion
    result = execute(actor2, :operation3)
    
    assert result == {:error, :ttl_exhausted}
    
    IO.puts("✅ TTL exhaustion prevention works as documented")
    :ok
  end

  def test_hop_operation do
    IO.puts("\nTesting hop operation...")
    
    {:ok, actor} = create(%{
      type: :processor,
      ttl: 5,
      token: %{step: 1, data: "initial"}
    })
    
    # Test hop as documented
    {:ok, hopped_actor} = hop(actor, %{step: 2, processed: true})
    
    assert hopped_actor.ttl == 4  # TTL decremented
    assert hopped_actor.token.step == 2
    assert hopped_actor.token.processed == true
    assert hopped_actor.token.data == "initial"  # Original data preserved
    
    IO.puts("✅ Hop operation works as documented")
    {:ok, hopped_actor}
  end

  def test_ttl_check do
    IO.puts("\nTesting TTL check...")
    
    {:ok, actor} = create(%{ttl: 3})
    
    {:ok, ttl} = check_ttl(actor)
    assert ttl == 3
    
    # Exhaust TTL
    {:ok, actor1} = execute(actor, :op1)
    {:ok, actor2} = execute(actor1, :op2)
    {:ok, actor3} = execute(actor2, :op3)
    
    # Should be exhausted
    result = check_ttl(actor3)
    assert result == {:error, :ttl_exhausted}
    
    IO.puts("✅ TTL check works as documented")
    :ok
  end

  def test_bitactor_pipeline do
    IO.puts(String.duplicate("=", 50))
    IO.puts("TESTING BITACTOR PIPELINE")
    IO.puts(String.duplicate("=", 50))
    
    with {:ok, _actor} <- test_actor_creation(),
         {:ok, _result} <- test_actor_execution(),
         :ok <- test_ttl_exhaustion(),
         {:ok, _hopped} <- test_hop_operation(),
         :ok <- test_ttl_check() do
      
      IO.puts("\n✅ BITACTOR PIPELINE SUCCESS")
      IO.puts("All BitActor operations work as documented:")
      IO.puts("- Creation with TTL budget")
      IO.puts("- Execution with TTL decrement") 
      IO.puts("- TTL exhaustion prevention")
      IO.puts("- Hop operations with token passing")
      IO.puts("- TTL checking")
      
      {:ok, :all_tests_passed}
    else
      error ->
        IO.puts("❌ BITACTOR PIPELINE FAILED")
        IO.inspect(error)
        {:error, :tests_failed}
    end
  end

  # Simple assertion helper
  defp assert(true), do: :ok
  defp assert(false), do: raise("Assertion failed")
end

# Run the tests
case TestBitActor.test_bitactor_pipeline() do
  {:ok, :all_tests_passed} ->
    IO.puts("\n🎉 ALL BITACTOR TESTS PASSED")
    System.halt(0)
  
  {:error, reason} ->
    IO.puts("\n💥 BITACTOR TESTS FAILED: #{reason}")
    System.halt(1)
end