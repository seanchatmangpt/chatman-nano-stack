defmodule Ultrathink.Workflows.BitActorWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.BitActorWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "bitactor workflow" do
    test "creates bitactor through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test BitActor",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(BitActorWorkflow, input)
      assert result.action == :create
      assert result.resource == "bitactor"
    end
    
    test "reads bitactor through workflow" do
      bitactor = TestHelper.create_test_data(Ultrathink.Resources.BitActor)
      
      input = %{
        action: :read,
        resource_id: bitactor.id
      }
      
      assert {:ok, result} = Reactor.run(BitActorWorkflow, input)
      assert result.action == :read
      assert result.resource == "bitactor"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(BitActorWorkflow, input)
    end
  end
end
