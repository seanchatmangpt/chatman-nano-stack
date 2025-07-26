defmodule Ultrathink.Workflows.CoordinationReactorWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.CoordinationReactorWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "coordinationreactor workflow" do
    test "creates coordinationreactor through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test CoordinationReactor",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(CoordinationReactorWorkflow, input)
      assert result.action == :create
      assert result.resource == "coordinationreactor"
    end
    
    test "reads coordinationreactor through workflow" do
      coordinationreactor = TestHelper.create_test_data(Ultrathink.Resources.CoordinationReactor)
      
      input = %{
        action: :read,
        resource_id: coordinationreactor.id
      }
      
      assert {:ok, result} = Reactor.run(CoordinationReactorWorkflow, input)
      assert result.action == :read
      assert result.resource == "coordinationreactor"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(CoordinationReactorWorkflow, input)
    end
  end
end
