defmodule Cybersecurity.Workflows.NetworkSegmentWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.NetworkSegmentWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "networksegment workflow" do
    test "creates networksegment through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test NetworkSegment",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(NetworkSegmentWorkflow, input)
      assert result.action == :create
      assert result.resource == "networksegment"
    end
    
    test "reads networksegment through workflow" do
      networksegment = TestHelper.create_test_data(Cybersecurity.Resources.NetworkSegment)
      
      input = %{
        action: :read,
        resource_id: networksegment.id
      }
      
      assert {:ok, result} = Reactor.run(NetworkSegmentWorkflow, input)
      assert result.action == :read
      assert result.resource == "networksegment"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(NetworkSegmentWorkflow, input)
    end
  end
end
