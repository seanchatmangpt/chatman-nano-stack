defmodule Cybersecurity.Workflows.SpywareWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SpywareWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "spyware workflow" do
    test "creates spyware through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Spyware",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SpywareWorkflow, input)
      assert result.action == :create
      assert result.resource == "spyware"
    end
    
    test "reads spyware through workflow" do
      spyware = TestHelper.create_test_data(Cybersecurity.Resources.Spyware)
      
      input = %{
        action: :read,
        resource_id: spyware.id
      }
      
      assert {:ok, result} = Reactor.run(SpywareWorkflow, input)
      assert result.action == :read
      assert result.resource == "spyware"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SpywareWorkflow, input)
    end
  end
end
