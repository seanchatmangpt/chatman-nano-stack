defmodule Cybersecurity.Workflows.ManInTheMiddleAttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.ManInTheMiddleAttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "maninthemiddleattack workflow" do
    test "creates maninthemiddleattack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test ManInTheMiddleAttack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(ManInTheMiddleAttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "maninthemiddleattack"
    end
    
    test "reads maninthemiddleattack through workflow" do
      maninthemiddleattack = TestHelper.create_test_data(Cybersecurity.Resources.ManInTheMiddleAttack)
      
      input = %{
        action: :read,
        resource_id: maninthemiddleattack.id
      }
      
      assert {:ok, result} = Reactor.run(ManInTheMiddleAttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "maninthemiddleattack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(ManInTheMiddleAttackWorkflow, input)
    end
  end
end
