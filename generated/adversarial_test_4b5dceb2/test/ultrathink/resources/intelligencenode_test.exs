defmodule Ultrathink.Resources.IntelligenceNodeTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Resources.IntelligenceNode
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates intelligencenode with valid attributes" do
      attrs = %{
        name: "Test IntelligenceNode",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, intelligencenode} = Ash.create(IntelligenceNode, attrs)
      assert intelligencenode.name == "Test IntelligenceNode"
      assert intelligencenode.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(IntelligenceNode, attrs)
    end
  end

  describe "read action" do
    test "reads existing intelligencenode" do
      intelligencenode = TestHelper.create_test_data(IntelligenceNode)
      
      assert {:ok, found_intelligencenode} = Ash.get(IntelligenceNode, intelligencenode.id)
      assert found_intelligencenode.id == intelligencenode.id
    end
    
    test "lists all intelligencenodes" do
      TestHelper.create_test_data(IntelligenceNode, %{name: "IntelligenceNode 1"})
      TestHelper.create_test_data(IntelligenceNode, %{name: "IntelligenceNode 2"})
      
      assert {:ok, intelligencenodes} = Ash.read(IntelligenceNode)
      assert length(intelligencenodes) >= 2
    end
    
    test "filters by status" do
      active_intelligencenode = TestHelper.create_test_data(IntelligenceNode, %{status: :active})
      _inactive_intelligencenode = TestHelper.create_test_data(IntelligenceNode, %{status: :inactive})
      
      assert {:ok, [intelligencenode]} = Ash.read(IntelligenceNode, action: :by_status, status: :active)
      assert intelligencenode.id == active_intelligencenode.id
    end
  end

  describe "update action" do
    test "updates intelligencenode attributes" do
      intelligencenode = TestHelper.create_test_data(IntelligenceNode)
      
      assert {:ok, updated_intelligencenode} = Ash.update(intelligencenode, %{name: "Updated Name"})
      assert updated_intelligencenode.name == "Updated Name"
    end
    
    test "activates intelligencenode" do
      intelligencenode = TestHelper.create_test_data(IntelligenceNode, %{status: :inactive})
      
      assert {:ok, activated_intelligencenode} = Ash.update(intelligencenode, action: :activate)
      assert activated_intelligencenode.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing intelligencenode" do
      intelligencenode = TestHelper.create_test_data(IntelligenceNode)
      
      assert :ok = Ash.destroy(intelligencenode)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(IntelligenceNode, intelligencenode.id)
    end
  end
end
