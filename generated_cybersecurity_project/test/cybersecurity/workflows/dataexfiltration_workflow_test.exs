defmodule Cybersecurity.Workflows.DataExfiltrationWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.DataExfiltrationWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "dataexfiltration workflow" do
    test "creates dataexfiltration through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test DataExfiltration",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(DataExfiltrationWorkflow, input)
      assert result.action == :create
      assert result.resource == "dataexfiltration"
    end
    
    test "reads dataexfiltration through workflow" do
      dataexfiltration = TestHelper.create_test_data(Cybersecurity.Resources.DataExfiltration)
      
      input = %{
        action: :read,
        resource_id: dataexfiltration.id
      }
      
      assert {:ok, result} = Reactor.run(DataExfiltrationWorkflow, input)
      assert result.action == :read
      assert result.resource == "dataexfiltration"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(DataExfiltrationWorkflow, input)
    end
  end
end
