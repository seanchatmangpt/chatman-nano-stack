defmodule Cybersecurity.Workflows.IOCWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.IOCWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "ioc workflow" do
    test "creates ioc through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test IOC",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(IOCWorkflow, input)
      assert result.action == :create
      assert result.resource == "ioc"
    end
    
    test "reads ioc through workflow" do
      ioc = TestHelper.create_test_data(Cybersecurity.Resources.IOC)
      
      input = %{
        action: :read,
        resource_id: ioc.id
      }
      
      assert {:ok, result} = Reactor.run(IOCWorkflow, input)
      assert result.action == :read
      assert result.resource == "ioc"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(IOCWorkflow, input)
    end
  end
end
