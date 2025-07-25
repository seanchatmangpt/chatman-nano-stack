defmodule Cybersecurity.Workflows.NetworkDeviceWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.NetworkDeviceWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "networkdevice workflow" do
    test "creates networkdevice through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test NetworkDevice",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(NetworkDeviceWorkflow, input)
      assert result.action == :create
      assert result.resource == "networkdevice"
    end
    
    test "reads networkdevice through workflow" do
      networkdevice = TestHelper.create_test_data(Cybersecurity.Resources.NetworkDevice)
      
      input = %{
        action: :read,
        resource_id: networkdevice.id
      }
      
      assert {:ok, result} = Reactor.run(NetworkDeviceWorkflow, input)
      assert result.action == :read
      assert result.resource == "networkdevice"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(NetworkDeviceWorkflow, input)
    end
  end
end
