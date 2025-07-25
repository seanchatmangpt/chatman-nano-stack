defmodule Cybersecurity.Workflows.NetworkAttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.NetworkAttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "networkattack workflow" do
    test "creates networkattack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test NetworkAttack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(NetworkAttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "networkattack"
    end
    
    test "reads networkattack through workflow" do
      networkattack = TestHelper.create_test_data(Cybersecurity.Resources.NetworkAttack)
      
      input = %{
        action: :read,
        resource_id: networkattack.id
      }
      
      assert {:ok, result} = Reactor.run(NetworkAttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "networkattack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(NetworkAttackWorkflow, input)
    end
  end
end
