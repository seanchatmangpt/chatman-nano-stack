defmodule Ultrathink.Workflows.MainWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.MainWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "main workflow execution" do
    test "executes process operation successfully" do
      input = %{
        operation: :process,
        data: %{test: "data"},
        context: %{user_id: "test"}
      }
      
      assert {:ok, result} = Reactor.run(MainWorkflow, input)
      assert result.operation == :process
      assert result.status == :success
      assert %DateTime{} = result.completed_at
    end
    
    test "validates operation input" do
      input = %{operation: :invalid_operation}
      
      assert {:error, _} = Reactor.run(MainWorkflow, input)
    end
    
    test "executes create operation" do
      input = %{
        operation: :create,
        data: %{resource: "test", attributes: %{name: "Test"}}
      }
      
      assert {:ok, result} = Reactor.run(MainWorkflow, input)
      assert result.operation == :create
      assert result.status == :success
    end
  end
end
