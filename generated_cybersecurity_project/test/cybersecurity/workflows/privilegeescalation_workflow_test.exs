defmodule Cybersecurity.Workflows.PrivilegeEscalationWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PrivilegeEscalationWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "privilegeescalation workflow" do
    test "creates privilegeescalation through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PrivilegeEscalation",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PrivilegeEscalationWorkflow, input)
      assert result.action == :create
      assert result.resource == "privilegeescalation"
    end
    
    test "reads privilegeescalation through workflow" do
      privilegeescalation = TestHelper.create_test_data(Cybersecurity.Resources.PrivilegeEscalation)
      
      input = %{
        action: :read,
        resource_id: privilegeescalation.id
      }
      
      assert {:ok, result} = Reactor.run(PrivilegeEscalationWorkflow, input)
      assert result.action == :read
      assert result.resource == "privilegeescalation"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PrivilegeEscalationWorkflow, input)
    end
  end
end
