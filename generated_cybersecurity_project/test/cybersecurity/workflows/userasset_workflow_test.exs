defmodule Cybersecurity.Workflows.UserAssetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.UserAssetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "userasset workflow" do
    test "creates userasset through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test UserAsset",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(UserAssetWorkflow, input)
      assert result.action == :create
      assert result.resource == "userasset"
    end
    
    test "reads userasset through workflow" do
      userasset = TestHelper.create_test_data(Cybersecurity.Resources.UserAsset)
      
      input = %{
        action: :read,
        resource_id: userasset.id
      }
      
      assert {:ok, result} = Reactor.run(UserAssetWorkflow, input)
      assert result.action == :read
      assert result.resource == "userasset"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(UserAssetWorkflow, input)
    end
  end
end
