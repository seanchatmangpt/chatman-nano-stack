defmodule Cybersecurity.Workflows.EDRWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.EDRWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "edr workflow" do
    test "creates edr through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test EDR",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(EDRWorkflow, input)
      assert result.action == :create
      assert result.resource == "edr"
    end
    
    test "reads edr through workflow" do
      edr = TestHelper.create_test_data(Cybersecurity.Resources.EDR)
      
      input = %{
        action: :read,
        resource_id: edr.id
      }
      
      assert {:ok, result} = Reactor.run(EDRWorkflow, input)
      assert result.action == :read
      assert result.resource == "edr"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(EDRWorkflow, input)
    end
  end
end
