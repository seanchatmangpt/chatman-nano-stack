defmodule Cybersecurity.Workflows.SwitchWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SwitchWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "switch workflow" do
    test "creates switch through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Switch",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SwitchWorkflow, input)
      assert result.action == :create
      assert result.resource == "switch"
    end
    
    test "reads switch through workflow" do
      switch = TestHelper.create_test_data(Cybersecurity.Resources.Switch)
      
      input = %{
        action: :read,
        resource_id: switch.id
      }
      
      assert {:ok, result} = Reactor.run(SwitchWorkflow, input)
      assert result.action == :read
      assert result.resource == "switch"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SwitchWorkflow, input)
    end
  end
end
