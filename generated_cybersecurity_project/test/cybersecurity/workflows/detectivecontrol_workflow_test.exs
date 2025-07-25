defmodule Cybersecurity.Workflows.DetectiveControlWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.DetectiveControlWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "detectivecontrol workflow" do
    test "creates detectivecontrol through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test DetectiveControl",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(DetectiveControlWorkflow, input)
      assert result.action == :create
      assert result.resource == "detectivecontrol"
    end
    
    test "reads detectivecontrol through workflow" do
      detectivecontrol = TestHelper.create_test_data(Cybersecurity.Resources.DetectiveControl)
      
      input = %{
        action: :read,
        resource_id: detectivecontrol.id
      }
      
      assert {:ok, result} = Reactor.run(DetectiveControlWorkflow, input)
      assert result.action == :read
      assert result.resource == "detectivecontrol"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(DetectiveControlWorkflow, input)
    end
  end
end
