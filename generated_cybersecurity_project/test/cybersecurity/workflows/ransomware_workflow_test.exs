defmodule Cybersecurity.Workflows.RansomwareWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.RansomwareWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "ransomware workflow" do
    test "creates ransomware through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Ransomware",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(RansomwareWorkflow, input)
      assert result.action == :create
      assert result.resource == "ransomware"
    end
    
    test "reads ransomware through workflow" do
      ransomware = TestHelper.create_test_data(Cybersecurity.Resources.Ransomware)
      
      input = %{
        action: :read,
        resource_id: ransomware.id
      }
      
      assert {:ok, result} = Reactor.run(RansomwareWorkflow, input)
      assert result.action == :read
      assert result.resource == "ransomware"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(RansomwareWorkflow, input)
    end
  end
end
