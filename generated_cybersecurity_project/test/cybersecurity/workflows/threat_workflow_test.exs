defmodule Cybersecurity.Workflows.ThreatWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.ThreatWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "threat workflow" do
    test "creates threat through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Threat",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(ThreatWorkflow, input)
      assert result.action == :create
      assert result.resource == "threat"
    end
    
    test "reads threat through workflow" do
      threat = TestHelper.create_test_data(Cybersecurity.Resources.Threat)
      
      input = %{
        action: :read,
        resource_id: threat.id
      }
      
      assert {:ok, result} = Reactor.run(ThreatWorkflow, input)
      assert result.action == :read
      assert result.resource == "threat"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(ThreatWorkflow, input)
    end
  end
end
