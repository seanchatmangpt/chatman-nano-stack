defmodule Cybersecurity.Workflows.TrojanWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.TrojanWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "trojan workflow" do
    test "creates trojan through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Trojan",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(TrojanWorkflow, input)
      assert result.action == :create
      assert result.resource == "trojan"
    end
    
    test "reads trojan through workflow" do
      trojan = TestHelper.create_test_data(Cybersecurity.Resources.Trojan)
      
      input = %{
        action: :read,
        resource_id: trojan.id
      }
      
      assert {:ok, result} = Reactor.run(TrojanWorkflow, input)
      assert result.action == :read
      assert result.resource == "trojan"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(TrojanWorkflow, input)
    end
  end
end
