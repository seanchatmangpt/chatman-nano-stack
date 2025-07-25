defmodule Cybersecurity.Workflows.AssetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.AssetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "asset workflow" do
    test "creates asset through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test Asset",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(AssetWorkflow, input)
      assert result.action == :create
      assert result.resource == "asset"
    end
    
    test "reads asset through workflow" do
      asset = TestHelper.create_test_data(Cybersecurity.Resources.Asset)
      
      input = %{
        action: :read,
        resource_id: asset.id
      }
      
      assert {:ok, result} = Reactor.run(AssetWorkflow, input)
      assert result.action == :read
      assert result.resource == "asset"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(AssetWorkflow, input)
    end
  end
end
