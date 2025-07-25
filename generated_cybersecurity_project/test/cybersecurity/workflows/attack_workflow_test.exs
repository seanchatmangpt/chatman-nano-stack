defmodule Cybersecurity.Workflows.AttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.AttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "attack workflow" do
    test "creates attack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Attack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(AttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "attack"
    end
    
    test "reads attack through workflow" do
      attack = TestHelper.create_test_data(Cybersecurity.Resources.Attack)
      
      input = %{
        action: :read,
        resource_id: attack.id
      }
      
      assert {:ok, result} = Reactor.run(AttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "attack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(AttackWorkflow, input)
    end
  end
end
