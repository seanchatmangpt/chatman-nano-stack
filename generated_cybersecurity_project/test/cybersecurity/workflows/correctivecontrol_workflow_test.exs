defmodule Cybersecurity.Workflows.CorrectiveControlWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.CorrectiveControlWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "correctivecontrol workflow" do
    test "creates correctivecontrol through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test CorrectiveControl",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(CorrectiveControlWorkflow, input)
      assert result.action == :create
      assert result.resource == "correctivecontrol"
    end
    
    test "reads correctivecontrol through workflow" do
      correctivecontrol = TestHelper.create_test_data(Cybersecurity.Resources.CorrectiveControl)
      
      input = %{
        action: :read,
        resource_id: correctivecontrol.id
      }
      
      assert {:ok, result} = Reactor.run(CorrectiveControlWorkflow, input)
      assert result.action == :read
      assert result.resource == "correctivecontrol"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(CorrectiveControlWorkflow, input)
    end
  end
end
