defmodule Cybersecurity.Workflows.XSSWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.XSSWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "xss workflow" do
    test "creates xss through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test XSS",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(XSSWorkflow, input)
      assert result.action == :create
      assert result.resource == "xss"
    end
    
    test "reads xss through workflow" do
      xss = TestHelper.create_test_data(Cybersecurity.Resources.XSS)
      
      input = %{
        action: :read,
        resource_id: xss.id
      }
      
      assert {:ok, result} = Reactor.run(XSSWorkflow, input)
      assert result.action == :read
      assert result.resource == "xss"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(XSSWorkflow, input)
    end
  end
end
