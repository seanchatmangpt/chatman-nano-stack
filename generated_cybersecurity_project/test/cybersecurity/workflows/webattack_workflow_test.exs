defmodule Cybersecurity.Workflows.WebAttackWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.WebAttackWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "webattack workflow" do
    test "creates webattack through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test WebAttack",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(WebAttackWorkflow, input)
      assert result.action == :create
      assert result.resource == "webattack"
    end
    
    test "reads webattack through workflow" do
      webattack = TestHelper.create_test_data(Cybersecurity.Resources.WebAttack)
      
      input = %{
        action: :read,
        resource_id: webattack.id
      }
      
      assert {:ok, result} = Reactor.run(WebAttackWorkflow, input)
      assert result.action == :read
      assert result.resource == "webattack"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(WebAttackWorkflow, input)
    end
  end
end
