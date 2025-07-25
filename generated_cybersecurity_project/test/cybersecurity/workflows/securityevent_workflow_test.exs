defmodule Cybersecurity.Workflows.SecurityEventWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SecurityEventWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "securityevent workflow" do
    test "creates securityevent through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SecurityEvent",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SecurityEventWorkflow, input)
      assert result.action == :create
      assert result.resource == "securityevent"
    end
    
    test "reads securityevent through workflow" do
      securityevent = TestHelper.create_test_data(Cybersecurity.Resources.SecurityEvent)
      
      input = %{
        action: :read,
        resource_id: securityevent.id
      }
      
      assert {:ok, result} = Reactor.run(SecurityEventWorkflow, input)
      assert result.action == :read
      assert result.resource == "securityevent"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SecurityEventWorkflow, input)
    end
  end
end
