defmodule Cybersecurity.Workflows.PreventiveControlWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PreventiveControlWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "preventivecontrol workflow" do
    test "creates preventivecontrol through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PreventiveControl",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PreventiveControlWorkflow, input)
      assert result.action == :create
      assert result.resource == "preventivecontrol"
    end
    
    test "reads preventivecontrol through workflow" do
      preventivecontrol = TestHelper.create_test_data(Cybersecurity.Resources.PreventiveControl)
      
      input = %{
        action: :read,
        resource_id: preventivecontrol.id
      }
      
      assert {:ok, result} = Reactor.run(PreventiveControlWorkflow, input)
      assert result.action == :read
      assert result.resource == "preventivecontrol"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PreventiveControlWorkflow, input)
    end
  end
end
