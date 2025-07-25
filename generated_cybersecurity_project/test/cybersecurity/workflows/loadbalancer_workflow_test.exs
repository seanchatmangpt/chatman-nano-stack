defmodule Cybersecurity.Workflows.LoadBalancerWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.LoadBalancerWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "loadbalancer workflow" do
    test "creates loadbalancer through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test LoadBalancer",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(LoadBalancerWorkflow, input)
      assert result.action == :create
      assert result.resource == "loadbalancer"
    end
    
    test "reads loadbalancer through workflow" do
      loadbalancer = TestHelper.create_test_data(Cybersecurity.Resources.LoadBalancer)
      
      input = %{
        action: :read,
        resource_id: loadbalancer.id
      }
      
      assert {:ok, result} = Reactor.run(LoadBalancerWorkflow, input)
      assert result.action == :read
      assert result.resource == "loadbalancer"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(LoadBalancerWorkflow, input)
    end
  end
end
