defmodule Cybersecurity.Workflows.DataAssetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.DataAssetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "dataasset workflow" do
    test "creates dataasset through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test DataAsset",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(DataAssetWorkflow, input)
      assert result.action == :create
      assert result.resource == "dataasset"
    end
    
    test "reads dataasset through workflow" do
      dataasset = TestHelper.create_test_data(Cybersecurity.Resources.DataAsset)
      
      input = %{
        action: :read,
        resource_id: dataasset.id
      }
      
      assert {:ok, result} = Reactor.run(DataAssetWorkflow, input)
      assert result.action == :read
      assert result.resource == "dataasset"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(DataAssetWorkflow, input)
    end
  end
end
