defmodule Cybersecurity.Workflows.PacketSniffingWorkflowTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Workflows.PacketSniffingWorkflow
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "packetsniffing workflow" do
    test "creates packetsniffing through workflow" do
      input = %{
        action: :create,
        resource_data: %{
          name: "Workflow Test PacketSniffing",
          description: "Created via workflow"
        }
      }
      
      assert {:ok, result} = Reactor.run(PacketSniffingWorkflow, input)
      assert result.action == :create
      assert result.resource == "packetsniffing"
    end
    
    test "reads packetsniffing through workflow" do
      packetsniffing = TestHelper.create_test_data(Cybersecurity.Resources.PacketSniffing)
      
      input = %{
        action: :read,
        resource_id: packetsniffing.id
      }
      
      assert {:ok, result} = Reactor.run(PacketSniffingWorkflow, input)
      assert result.action == :read
      assert result.resource == "packetsniffing"
    end
    
    test "validates workflow actions" do
      input = %{action: :invalid_action}
      
      assert {:error, _} = Reactor.run(PacketSniffingWorkflow, input)
    end
  end
end
