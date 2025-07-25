defmodule Cybersecurity.Workflows.BotnetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.BotnetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "botnet workflow" do
    test "creates botnet through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Botnet",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(BotnetWorkflow, input)
      assert result.action == :create
      assert result.resource == "botnet"
    end
    
    test "reads botnet through workflow" do
      botnet = TestHelper.create_test_data(Cybersecurity.Resources.Botnet)
      
      input = %{
        action: :read,
        resource_id: botnet.id
      }
      
      assert {:ok, result} = Reactor.run(BotnetWorkflow, input)
      assert result.action == :read
      assert result.resource == "botnet"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(BotnetWorkflow, input)
    end
  end
end
