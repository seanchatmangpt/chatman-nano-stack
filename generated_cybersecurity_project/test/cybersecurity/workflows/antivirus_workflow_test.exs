defmodule Cybersecurity.Workflows.AntivirusWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.AntivirusWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "antivirus workflow" do
    test "creates antivirus through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Antivirus",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(AntivirusWorkflow, input)
      assert result.action == :create
      assert result.resource == "antivirus"
    end
    
    test "reads antivirus through workflow" do
      antivirus = TestHelper.create_test_data(Cybersecurity.Resources.Antivirus)
      
      input = %{
        action: :read,
        resource_id: antivirus.id
      }
      
      assert {:ok, result} = Reactor.run(AntivirusWorkflow, input)
      assert result.action == :read
      assert result.resource == "antivirus"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(AntivirusWorkflow, input)
    end
  end
end
