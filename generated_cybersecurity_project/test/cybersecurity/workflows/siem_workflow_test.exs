defmodule Cybersecurity.Workflows.SIEMWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SIEMWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "siem workflow" do
    test "creates siem through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SIEM",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SIEMWorkflow, input)
      assert result.action == :create
      assert result.resource == "siem"
    end
    
    test "reads siem through workflow" do
      siem = TestHelper.create_test_data(Cybersecurity.Resources.SIEM)
      
      input = %{
        action: :read,
        resource_id: siem.id
      }
      
      assert {:ok, result} = Reactor.run(SIEMWorkflow, input)
      assert result.action == :read
      assert result.resource == "siem"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SIEMWorkflow, input)
    end
  end
end
