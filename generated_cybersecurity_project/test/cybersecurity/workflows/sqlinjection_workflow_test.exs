defmodule Cybersecurity.Workflows.SQLInjectionWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SQLInjectionWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "sqlinjection workflow" do
    test "creates sqlinjection through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SQLInjection",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SQLInjectionWorkflow, input)
      assert result.action == :create
      assert result.resource == "sqlinjection"
    end
    
    test "reads sqlinjection through workflow" do
      sqlinjection = TestHelper.create_test_data(Cybersecurity.Resources.SQLInjection)
      
      input = %{
        action: :read,
        resource_id: sqlinjection.id
      }
      
      assert {:ok, result} = Reactor.run(SQLInjectionWorkflow, input)
      assert result.action == :read
      assert result.resource == "sqlinjection"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SQLInjectionWorkflow, input)
    end
  end
end
