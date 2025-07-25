defmodule Cybersecurity.Workflows.VirusWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.VirusWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "virus workflow" do
    test "creates virus through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Virus",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(VirusWorkflow, input)
      assert result.action == :create
      assert result.resource == "virus"
    end
    
    test "reads virus through workflow" do
      virus = TestHelper.create_test_data(Cybersecurity.Resources.Virus)
      
      input = %{
        action: :read,
        resource_id: virus.id
      }
      
      assert {:ok, result} = Reactor.run(VirusWorkflow, input)
      assert result.action == :read
      assert result.resource == "virus"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(VirusWorkflow, input)
    end
  end
end
