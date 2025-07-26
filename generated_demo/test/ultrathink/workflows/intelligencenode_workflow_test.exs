defmodule Ultrathink.Workflows.IntelligenceNodeWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Workflows.IntelligenceNodeWorkflow
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "intelligencenode workflow" do
    test "creates intelligencenode through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test IntelligenceNode",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(IntelligenceNodeWorkflow, input)
      assert result.action == :create
      assert result.resource == "intelligencenode"
    end
    
    test "reads intelligencenode through workflow" do
      intelligencenode = TestHelper.create_test_data(Ultrathink.Resources.IntelligenceNode)
      
      input = %{
        action: :read,
        resource_id: intelligencenode.id
      }
      
      assert {:ok, result} = Reactor.run(IntelligenceNodeWorkflow, input)
      assert result.action == :read
      assert result.resource == "intelligencenode"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(IntelligenceNodeWorkflow, input)
    end
  end
end
