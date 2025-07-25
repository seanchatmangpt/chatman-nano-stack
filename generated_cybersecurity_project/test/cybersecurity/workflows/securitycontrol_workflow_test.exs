defmodule Cybersecurity.Workflows.SecurityControlWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SecurityControlWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "securitycontrol workflow" do
    test "creates securitycontrol through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SecurityControl",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SecurityControlWorkflow, input)
      assert result.action == :create
      assert result.resource == "securitycontrol"
    end
    
    test "reads securitycontrol through workflow" do
      securitycontrol = TestHelper.create_test_data(Cybersecurity.Resources.SecurityControl)
      
      input = %{
        action: :read,
        resource_id: securitycontrol.id
      }
      
      assert {:ok, result} = Reactor.run(SecurityControlWorkflow, input)
      assert result.action == :read
      assert result.resource == "securitycontrol"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SecurityControlWorkflow, input)
    end
  end
end
