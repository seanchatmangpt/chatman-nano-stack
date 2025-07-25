defmodule Cybersecurity.Workflows.PublicNetworkWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PublicNetworkWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "publicnetwork workflow" do
    test "creates publicnetwork through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PublicNetwork",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PublicNetworkWorkflow, input)
      assert result.action == :create
      assert result.resource == "publicnetwork"
    end
    
    test "reads publicnetwork through workflow" do
      publicnetwork = TestHelper.create_test_data(Cybersecurity.Resources.PublicNetwork)
      
      input = %{
        action: :read,
        resource_id: publicnetwork.id
      }
      
      assert {:ok, result} = Reactor.run(PublicNetworkWorkflow, input)
      assert result.action == :read
      assert result.resource == "publicnetwork"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PublicNetworkWorkflow, input)
    end
  end
end
