defmodule Ultrathink.Workflows.SignalWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.SignalWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "signal workflow" do
    test "creates signal through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Signal",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SignalWorkflow, input)
      assert result.action == :create
      assert result.resource == "signal"
    end
    
    test "reads signal through workflow" do
      signal = TestHelper.create_test_data(Ultrathink.Resources.Signal)
      
      input = %{
        action: :read,
        resource_id: signal.id
      }
      
      assert {:ok, result} = Reactor.run(SignalWorkflow, input)
      assert result.action == :read
      assert result.resource == "signal"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SignalWorkflow, input)
    end
  end
end
