defmodule Cybersecurity.Workflows.InternalNetworkWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.InternalNetworkWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "internalnetwork workflow" do
    test "creates internalnetwork through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test InternalNetwork",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(InternalNetworkWorkflow, input)
      assert result.action == :create
      assert result.resource == "internalnetwork"
    end
    
    test "reads internalnetwork through workflow" do
      internalnetwork = TestHelper.create_test_data(Cybersecurity.Resources.InternalNetwork)
      
      input = %{
        action: :read,
        resource_id: internalnetwork.id
      }
      
      assert {:ok, result} = Reactor.run(InternalNetworkWorkflow, input)
      assert result.action == :read
      assert result.resource == "internalnetwork"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(InternalNetworkWorkflow, input)
    end
  end
end
