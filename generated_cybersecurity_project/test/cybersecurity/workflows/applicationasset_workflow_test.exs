defmodule Cybersecurity.Workflows.ApplicationAssetWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.ApplicationAssetWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "applicationasset workflow" do
    test "creates applicationasset through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test ApplicationAsset",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(ApplicationAssetWorkflow, input)
      assert result.action == :create
      assert result.resource == "applicationasset"
    end
    
    test "reads applicationasset through workflow" do
      applicationasset = TestHelper.create_test_data(Cybersecurity.Resources.ApplicationAsset)
      
      input = %{
        action: :read,
        resource_id: applicationasset.id
      }
      
      assert {:ok, result} = Reactor.run(ApplicationAssetWorkflow, input)
      assert result.action == :read
      assert result.resource == "applicationasset"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(ApplicationAssetWorkflow, input)
    end
  end
end
