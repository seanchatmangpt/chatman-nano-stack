defmodule Cybersecurity.Workflows.WormWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.WormWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "worm workflow" do
    test "creates worm through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Worm",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(WormWorkflow, input)
      assert result.action == :create
      assert result.resource == "worm"
    end
    
    test "reads worm through workflow" do
      worm = TestHelper.create_test_data(Cybersecurity.Resources.Worm)
      
      input = %{
        action: :read,
        resource_id: worm.id
      }
      
      assert {:ok, result} = Reactor.run(WormWorkflow, input)
      assert result.action == :read
      assert result.resource == "worm"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(WormWorkflow, input)
    end
  end
end
