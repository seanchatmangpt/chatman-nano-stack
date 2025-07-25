defmodule Cybersecurity.Workflows.RouterWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.RouterWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "router workflow" do
    test "creates router through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Router",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(RouterWorkflow, input)
      assert result.action == :create
      assert result.resource == "router"
    end
    
    test "reads router through workflow" do
      router = TestHelper.create_test_data(Cybersecurity.Resources.Router)
      
      input = %{
        action: :read,
        resource_id: router.id
      }
      
      assert {:ok, result} = Reactor.run(RouterWorkflow, input)
      assert result.action == :read
      assert result.resource == "router"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(RouterWorkflow, input)
    end
  end
end
