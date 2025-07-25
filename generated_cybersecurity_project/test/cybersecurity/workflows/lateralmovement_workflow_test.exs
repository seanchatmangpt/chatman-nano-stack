defmodule Cybersecurity.Workflows.LateralMovementWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.LateralMovementWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "lateralmovement workflow" do
    test "creates lateralmovement through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test LateralMovement",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(LateralMovementWorkflow, input)
      assert result.action == :create
      assert result.resource == "lateralmovement"
    end
    
    test "reads lateralmovement through workflow" do
      lateralmovement = TestHelper.create_test_data(Cybersecurity.Resources.LateralMovement)
      
      input = %{
        action: :read,
        resource_id: lateralmovement.id
      }
      
      assert {:ok, result} = Reactor.run(LateralMovementWorkflow, input)
      assert result.action == :read
      assert result.resource == "lateralmovement"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(LateralMovementWorkflow, input)
    end
  end
end
