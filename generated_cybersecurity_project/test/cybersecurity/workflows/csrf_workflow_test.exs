defmodule Cybersecurity.Workflows.CSRFWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.CSRFWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "csrf workflow" do
    test "creates csrf through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test CSRF",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(CSRFWorkflow, input)
      assert result.action == :create
      assert result.resource == "csrf"
    end
    
    test "reads csrf through workflow" do
      csrf = TestHelper.create_test_data(Cybersecurity.Resources.CSRF)
      
      input = %{
        action: :read,
        resource_id: csrf.id
      }
      
      assert {:ok, result} = Reactor.run(CSRFWorkflow, input)
      assert result.action == :read
      assert result.resource == "csrf"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(CSRFWorkflow, input)
    end
  end
end
