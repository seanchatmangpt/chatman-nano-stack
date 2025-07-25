defmodule Cybersecurity.Workflows.VPNWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.VPNWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "vpn workflow" do
    test "creates vpn through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test VPN",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(VPNWorkflow, input)
      assert result.action == :create
      assert result.resource == "vpn"
    end
    
    test "reads vpn through workflow" do
      vpn = TestHelper.create_test_data(Cybersecurity.Resources.VPN)
      
      input = %{
        action: :read,
        resource_id: vpn.id
      }
      
      assert {:ok, result} = Reactor.run(VPNWorkflow, input)
      assert result.action == :read
      assert result.resource == "vpn"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(VPNWorkflow, input)
    end
  end
end
