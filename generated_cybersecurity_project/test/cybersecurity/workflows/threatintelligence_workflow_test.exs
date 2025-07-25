defmodule Cybersecurity.Workflows.ThreatIntelligenceWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.ThreatIntelligenceWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "threatintelligence workflow" do
    test "creates threatintelligence through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test ThreatIntelligence",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(ThreatIntelligenceWorkflow, input)
      assert result.action == :create
      assert result.resource == "threatintelligence"
    end
    
    test "reads threatintelligence through workflow" do
      threatintelligence = TestHelper.create_test_data(Cybersecurity.Resources.ThreatIntelligence)
      
      input = %{
        action: :read,
        resource_id: threatintelligence.id
      }
      
      assert {:ok, result} = Reactor.run(ThreatIntelligenceWorkflow, input)
      assert result.action == :read
      assert result.resource == "threatintelligence"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(ThreatIntelligenceWorkflow, input)
    end
  end
end
