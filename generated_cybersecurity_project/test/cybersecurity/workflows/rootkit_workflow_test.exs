defmodule Cybersecurity.Workflows.RootkitWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.RootkitWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "rootkit workflow" do
    test "creates rootkit through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Rootkit",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(RootkitWorkflow, input)
      assert result.action == :create
      assert result.resource == "rootkit"
    end
    
    test "reads rootkit through workflow" do
      rootkit = TestHelper.create_test_data(Cybersecurity.Resources.Rootkit)
      
      input = %{
        action: :read,
        resource_id: rootkit.id
      }
      
      assert {:ok, result} = Reactor.run(RootkitWorkflow, input)
      assert result.action == :read
      assert result.resource == "rootkit"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(RootkitWorkflow, input)
    end
  end
end
