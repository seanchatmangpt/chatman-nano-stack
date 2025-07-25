defmodule Cybersecurity.Workflows.DMZWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.DMZWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "dmz workflow" do
    test "creates dmz through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test DMZ",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(DMZWorkflow, input)
      assert result.action == :create
      assert result.resource == "dmz"
    end
    
    test "reads dmz through workflow" do
      dmz = TestHelper.create_test_data(Cybersecurity.Resources.DMZ)
      
      input = %{
        action: :read,
        resource_id: dmz.id
      }
      
      assert {:ok, result} = Reactor.run(DMZWorkflow, input)
      assert result.action == :read
      assert result.resource == "dmz"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(DMZWorkflow, input)
    end
  end
end
