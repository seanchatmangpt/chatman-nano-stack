defmodule Cybersecurity.Workflows.PhishingAttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PhishingAttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "phishingattack workflow" do
    test "creates phishingattack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PhishingAttack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PhishingAttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "phishingattack"
    end
    
    test "reads phishingattack through workflow" do
      phishingattack = TestHelper.create_test_data(Cybersecurity.Resources.PhishingAttack)
      
      input = %{
        action: :read,
        resource_id: phishingattack.id
      }
      
      assert {:ok, result} = Reactor.run(PhishingAttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "phishingattack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PhishingAttackWorkflow, input)
    end
  end
end
