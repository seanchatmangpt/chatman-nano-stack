defmodule Cybersecurity.Workflows.IPSWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.IPSWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "ips workflow" do
    test "creates ips through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test IPS",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(IPSWorkflow, input)
      assert result.action == :create
      assert result.resource == "ips"
    end
    
    test "reads ips through workflow" do
      ips = TestHelper.create_test_data(Cybersecurity.Resources.IPS)
      
      input = %{
        action: :read,
        resource_id: ips.id
      }
      
      assert {:ok, result} = Reactor.run(IPSWorkflow, input)
      assert result.action == :read
      assert result.resource == "ips"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(IPSWorkflow, input)
    end
  end
end
