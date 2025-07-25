defmodule Cybersecurity.Workflows.FirewallWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.FirewallWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "firewall workflow" do
    test "creates firewall through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Firewall",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(FirewallWorkflow, input)
      assert result.action == :create
      assert result.resource == "firewall"
    end
    
    test "reads firewall through workflow" do
      firewall = TestHelper.create_test_data(Cybersecurity.Resources.Firewall)
      
      input = %{
        action: :read,
        resource_id: firewall.id
      }
      
      assert {:ok, result} = Reactor.run(FirewallWorkflow, input)
      assert result.action == :read
      assert result.resource == "firewall"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(FirewallWorkflow, input)
    end
  end
end
