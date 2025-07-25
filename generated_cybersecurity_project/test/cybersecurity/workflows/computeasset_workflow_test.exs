defmodule Cybersecurity.Workflows.ComputeAssetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.ComputeAssetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "computeasset workflow" do
    test "creates computeasset through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test ComputeAsset",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(ComputeAssetWorkflow, input)
      assert result.action == :create
      assert result.resource == "computeasset"
    end
    
    test "reads computeasset through workflow" do
      computeasset = TestHelper.create_test_data(Cybersecurity.Resources.ComputeAsset)
      
      input = %{
        action: :read,
        resource_id: computeasset.id
      }
      
      assert {:ok, result} = Reactor.run(ComputeAssetWorkflow, input)
      assert result.action == :read
      assert result.resource == "computeasset"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(ComputeAssetWorkflow, input)
    end
  end
end
