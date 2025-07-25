defmodule CNSForge.BitActorTest do
  use ExUnit.Case, async: true
  
  alias CNSForge.BitActor
  
  setup do
    # Start Ash for testing
    {:ok, _} = Application.ensure_all_started(:ash)
    
    # Clean up any existing BitActors
    BitActor.read!()
    |> Enum.each(&BitActor.destroy!/1)
    
    :ok
  end
  
  describe "BitActor creation" do
    test "creates BitActor with valid attributes" do
      assert {:ok, actor} = BitActor.create(%{
        type: :stimulus,
        transaction_id: "test_123",
        ttl: 8,
        token: %{data: "test"}
      })
      
      assert actor.type == :stimulus
      assert actor.transaction_id == "test_123"
      assert actor.ttl == 8
      assert actor.token == %{data: "test"}
      assert actor.status == :pending
      assert actor.created_at != nil
    end
    
    test "enforces required attributes" do
      assert {:error, error} = BitActor.create(%{})
      assert error.errors != []
    end
    
    test "uses default TTL when not specified" do
      assert {:ok, actor} = BitActor.create(%{
        type: :decoder,
        transaction_id: "test_456"
      })
      
      assert actor.ttl == 8
    end
  end
  
  describe "BitActor hop execution" do
    setup do
      {:ok, actor} = BitActor.create(%{
        type: :workflow,
        transaction_id: "hop_test",
        ttl: 5
      })
      
      {:ok, actor: actor}
    end
    
    test "executes hop successfully", %{actor: actor} do
      assert {:ok, updated} = BitActor.execute_hop(
        actor,
        %{input_token: %{value: 42}, operation: :process_signal}
      )
      
      assert updated.status == :completed
      assert updated.ttl == 4  # Decremented
      assert updated.result != nil
      assert updated.result.processed == true
    end
    
    test "fails when TTL is exhausted", %{actor: actor} do
      # Exhaust TTL
      exhausted = %{actor | ttl: 0}
      
      assert {:error, error} = BitActor.execute_hop(
        exhausted,
        %{input_token: %{}, operation: :decode_params}
      )
      
      assert error.errors != []
    end
    
    test "emits telemetry during hop execution", %{actor: actor} do
      handler_id = "test_telemetry_#{System.unique_integer()}"
      
      :telemetry.attach(
        handler_id,
        [:cns_forge, :bit_actor, :hop],
        fn _event_name, measurements, metadata, _config ->
          send(self(), {:hop_executed, measurements, metadata})
        end,
        nil
      )
      
      BitActor.execute_hop(actor, %{
        input_token: %{test: true},
        operation: :validate_input
      })
      
      assert_receive {:hop_executed, measurements, metadata}, 1000
      assert measurements.ttl_remaining == 5
      assert metadata.bit_actor_id == actor.id
      assert metadata.operation == :validate_input
      
      :telemetry.detach(handler_id)
    end
  end
  
  describe "BitActor lifecycle" do
    setup do
      {:ok, actor} = BitActor.create(%{
        type: :action,
        transaction_id: "lifecycle_test"
      })
      
      {:ok, actor: actor}
    end
    
    test "completes BitActor successfully", %{actor: actor} do
      assert {:ok, completed} = BitActor.complete(
        actor,
        %{result_token: %{success: true, data: "result"}}
      )
      
      assert completed.status == :completed
      assert completed.completed_at != nil
      assert completed.result == %{success: true, data: "result"}
    end
    
    test "fails BitActor with error", %{actor: actor} do
      assert {:ok, failed} = BitActor.fail(
        actor,
        %{error_message: "Processing failed"}
      )
      
      assert failed.status == :failed
      assert failed.completed_at != nil
      assert failed.error == "Processing failed"
    end
    
    test "expires BitActor TTL", %{actor: actor} do
      assert {:ok, expired} = BitActor.expire_ttl(actor)
      
      assert expired.status == :ttl_expired
      assert expired.completed_at != nil
      assert expired.error == "TTL budget exhausted"
    end
  end
  
  describe "BitActor queries" do
    setup do
      # Create test BitActors
      {:ok, _} = BitActor.create(%{
        type: :stimulus,
        transaction_id: "query_test_1",
        status: :completed
      })
      
      {:ok, _} = BitActor.create(%{
        type: :decoder,
        transaction_id: "query_test_1",
        status: :pending
      })
      
      {:ok, _} = BitActor.create(%{
        type: :workflow,
        transaction_id: "query_test_2",
        status: :failed
      })
      
      :ok
    end
    
    test "queries BitActors by transaction_id" do
      actors = BitActor.read!()
      |> Enum.filter(&(&1.transaction_id == "query_test_1"))
      
      assert length(actors) == 2
      assert Enum.all?(actors, &(&1.transaction_id == "query_test_1"))
    end
    
    test "queries BitActors by status" do
      completed = BitActor.read!()
      |> Enum.filter(&(&1.status == :completed))
      
      assert length(completed) >= 1
      assert Enum.all?(completed, &(&1.status == :completed))
    end
    
    test "queries BitActors by type" do
      workflows = BitActor.read!()
      |> Enum.filter(&(&1.type == :workflow))
      
      assert length(workflows) >= 1
      assert Enum.all?(workflows, &(&1.type == :workflow))
    end
  end
  
  describe "BitActor concurrency" do
    test "handles concurrent BitActor creation" do
      transaction_id = "concurrent_test"
      
      tasks = for i <- 1..10 do
        Task.async(fn ->
          BitActor.create(%{
            type: :stimulus,
            transaction_id: "#{transaction_id}_#{i}"
          })
        end)
      end
      
      results = Task.await_many(tasks)
      
      assert Enum.all?(results, fn result ->
        match?({:ok, _}, result)
      end)
      
      created_count = results
      |> Enum.filter(&match?({:ok, _}, &1))
      |> length()
      
      assert created_count == 10
    end
    
    test "handles concurrent hop execution" do
      {:ok, actor} = BitActor.create(%{
        type: :workflow,
        transaction_id: "concurrent_hops",
        ttl: 8
      })
      
      # Note: Ash handles updates sequentially, so we test
      # that multiple updates don't cause issues
      results = for i <- 1..3 do
        BitActor.execute_hop(actor, %{
          input_token: %{index: i},
          operation: :process_signal
        })
      end
      
      successful = Enum.filter(results, &match?({:ok, _}, &1))
      assert length(successful) >= 1
    end
  end
  
  describe "BitActor performance" do
    test "creates many BitActors efficiently" do
      start_time = System.monotonic_time(:millisecond)
      
      for i <- 1..100 do
        BitActor.create(%{
          type: :stimulus,
          transaction_id: "perf_test_#{i}"
        })
      end
      
      duration = System.monotonic_time(:millisecond) - start_time
      
      # Should complete within reasonable time (adjust as needed)
      assert duration < 5000
      
      # Verify all were created
      all_actors = BitActor.read!()
      perf_actors = Enum.filter(all_actors, fn actor ->
        String.starts_with?(actor.transaction_id, "perf_test_")
      end)
      
      assert length(perf_actors) == 100
    end
  end
  
  describe "BitActor TTL management" do
    test "enforces TTL budget across multiple hops" do
      {:ok, actor} = BitActor.create(%{
        type: :workflow,
        transaction_id: "ttl_test",
        ttl: 3
      })
      
      # Execute 3 hops successfully
      {:ok, actor} = BitActor.execute_hop(actor, %{
        input_token: %{step: 1},
        operation: :decode_params
      })
      assert actor.ttl == 2
      
      {:ok, actor} = BitActor.execute_hop(actor, %{
        input_token: %{step: 2},
        operation: :validate_input
      })
      assert actor.ttl == 1
      
      {:ok, actor} = BitActor.execute_hop(actor, %{
        input_token: %{step: 3},
        operation: :process_signal
      })
      assert actor.ttl == 0
      
      # Fourth hop should fail
      assert {:error, _} = BitActor.execute_hop(actor, %{
        input_token: %{step: 4},
        operation: :emit_response
      })
    end
  end
end