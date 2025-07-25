defmodule Cybersecurity.Workflows.PortScanWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PortScanWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "portscan workflow" do
    test "creates portscan through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PortScan",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PortScanWorkflow, input)
      assert result.action == :create
      assert result.resource == "portscan"
    end
    
    test "reads portscan through workflow" do
      portscan = TestHelper.create_test_data(Cybersecurity.Resources.PortScan)
      
      input = %{
        action: :read,
        resource_id: portscan.id
      }
      
      assert {:ok, result} = Reactor.run(PortScanWorkflow, input)
      assert result.action == :read
      assert result.resource == "portscan"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PortScanWorkflow, input)
    end
  end
end
