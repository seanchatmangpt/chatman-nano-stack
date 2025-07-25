defmodule Cybersecurity.Workflows.AlertWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.AlertWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "alert workflow" do
    test "creates alert through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Alert",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(AlertWorkflow, input)
      assert result.action == :create
      assert result.resource == "alert"
    end
    
    test "reads alert through workflow" do
      alert = TestHelper.create_test_data(Cybersecurity.Resources.Alert)
      
      input = %{
        action: :read,
        resource_id: alert.id
      }
      
      assert {:ok, result} = Reactor.run(AlertWorkflow, input)
      assert result.action == :read
      assert result.resource == "alert"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(AlertWorkflow, input)
    end
  end
end
