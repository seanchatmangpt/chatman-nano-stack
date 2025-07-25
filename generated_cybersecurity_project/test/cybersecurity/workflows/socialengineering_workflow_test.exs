defmodule Cybersecurity.Workflows.SocialEngineeringWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.SocialEngineeringWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "socialengineering workflow" do
    test "creates socialengineering through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test SocialEngineering",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(SocialEngineeringWorkflow, input)
      assert result.action == :create
      assert result.resource == "socialengineering"
    end
    
    test "reads socialengineering through workflow" do
      socialengineering = TestHelper.create_test_data(Cybersecurity.Resources.SocialEngineering)
      
      input = %{
        action: :read,
        resource_id: socialengineering.id
      }
      
      assert {:ok, result} = Reactor.run(SocialEngineeringWorkflow, input)
      assert result.action == :read
      assert result.resource == "socialengineering"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(SocialEngineeringWorkflow, input)
    end
  end
end
