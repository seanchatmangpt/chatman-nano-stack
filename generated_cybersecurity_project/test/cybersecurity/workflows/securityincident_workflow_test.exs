defmodule Cybersecurity.Workflows.SecurityIncidentWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SecurityIncidentWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "securityincident workflow" do
    test "creates securityincident through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SecurityIncident",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SecurityIncidentWorkflow, input)
      assert result.action == :create
      assert result.resource == "securityincident"
    end
    
    test "reads securityincident through workflow" do
      securityincident = TestHelper.create_test_data(Cybersecurity.Resources.SecurityIncident)
      
      input = %{
        action: :read,
        resource_id: securityincident.id
      }
      
      assert {:ok, result} = Reactor.run(SecurityIncidentWorkflow, input)
      assert result.action == :read
      assert result.resource == "securityincident"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SecurityIncidentWorkflow, input)
    end
  end
end
