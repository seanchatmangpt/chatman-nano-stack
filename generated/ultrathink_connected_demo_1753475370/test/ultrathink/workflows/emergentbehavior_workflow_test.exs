defmodule Ultrathink.Workflows.EmergentBehaviorWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.EmergentBehaviorWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "emergentbehavior workflow" do
    test "creates emergentbehavior through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test EmergentBehavior",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(EmergentBehaviorWorkflow, input)
      assert result.action == :create
      assert result.resource == "emergentbehavior"
    end
    
    test "reads emergentbehavior through workflow" do
      emergentbehavior = TestHelper.create_test_data(Ultrathink.Resources.EmergentBehavior)
      
      input = %{
        action: :read,
        resource_id: emergentbehavior.id
      }
      
      assert {:ok, result} = Reactor.run(EmergentBehaviorWorkflow, input)
      assert result.action == :read
      assert result.resource == "emergentbehavior"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(EmergentBehaviorWorkflow, input)
    end
  end
end
