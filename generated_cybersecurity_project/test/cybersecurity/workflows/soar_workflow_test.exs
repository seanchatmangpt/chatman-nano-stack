defmodule Cybersecurity.Workflows.SOARWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SOARWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "soar workflow" do
    test "creates soar through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SOAR",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SOARWorkflow, input)
      assert result.action == :create
      assert result.resource == "soar"
    end
    
    test "reads soar through workflow" do
      soar = TestHelper.create_test_data(Cybersecurity.Resources.SOAR)
      
      input = %{
        action: :read,
        resource_id: soar.id
      }
      
      assert {:ok, result} = Reactor.run(SOARWorkflow, input)
      assert result.action == :read
      assert result.resource == "soar"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SOARWorkflow, input)
    end
  end
end
