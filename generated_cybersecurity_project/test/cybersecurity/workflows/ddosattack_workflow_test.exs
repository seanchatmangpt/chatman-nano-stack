defmodule Cybersecurity.Workflows.DDoSAttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.DDoSAttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "ddosattack workflow" do
    test "creates ddosattack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test DDoSAttack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(DDoSAttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "ddosattack"
    end
    
    test "reads ddosattack through workflow" do
      ddosattack = TestHelper.create_test_data(Cybersecurity.Resources.DDoSAttack)
      
      input = %{
        action: :read,
        resource_id: ddosattack.id
      }
      
      assert {:ok, result} = Reactor.run(DDoSAttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "ddosattack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(DDoSAttackWorkflow, input)
    end
  end
end
