defmodule Cybersecurity.Workflows.SpearPhishingWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SpearPhishingWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "spearphishing workflow" do
    test "creates spearphishing through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SpearPhishing",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SpearPhishingWorkflow, input)
      assert result.action == :create
      assert result.resource == "spearphishing"
    end
    
    test "reads spearphishing through workflow" do
      spearphishing = TestHelper.create_test_data(Cybersecurity.Resources.SpearPhishing)
      
      input = %{
        action: :read,
        resource_id: spearphishing.id
      }
      
      assert {:ok, result} = Reactor.run(SpearPhishingWorkflow, input)
      assert result.action == :read
      assert result.resource == "spearphishing"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SpearPhishingWorkflow, input)
    end
  end
end
