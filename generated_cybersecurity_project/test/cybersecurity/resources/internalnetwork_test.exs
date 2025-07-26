defmodule Cybersecurity.Resources.InternalNetworkTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.InternalNetwork
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates internalnetwork with valid attributes" do
      attrs = %{
        name: "Test InternalNetwork",
        description: "Test description",
        status: :active
      }
      
      InternalNetwork.init_storage()
      assert {:ok, internalnetwork} = InternalNetwork.create(attrs)
      assert internalnetwork.name == "Test InternalNetwork"
      assert internalnetwork.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = InternalNetwork.create(InternalNetwork, attrs)
    end
  end

  describe "read action" do
    test "reads existing internalnetwork" do
      internalnetwork = TestHelper.create_test_data(InternalNetwork)
      
      assert {:ok, found_internalnetwork} = InternalNetwork.get(InternalNetwork, internalnetwork.id)
      assert found_internalnetwork.id == internalnetwork.id
    end
    
    test "lists all internalnetworks" do
      TestHelper.create_test_data(InternalNetwork, %{name: "InternalNetwork 1"})
      TestHelper.create_test_data(InternalNetwork, %{name: "InternalNetwork 2"})
      
      assert {:ok, internalnetworks} = InternalNetwork.list(InternalNetwork)
      assert length(internalnetworks) >= 2
    end
    
    test "filters by status" do
      active_internalnetwork = TestHelper.create_test_data(InternalNetwork, %{status: :active})
      _inactive_internalnetwork = TestHelper.create_test_data(InternalNetwork, %{status: :inactive})
      
      assert {:ok, [internalnetwork]} = InternalNetwork.list(InternalNetwork, action: :by_status, status: :active)
      assert internalnetwork.id == active_internalnetwork.id
    end
  end

  describe "update action" do
    test "updates internalnetwork attributes" do
      internalnetwork = TestHelper.create_test_data(InternalNetwork)
      
      assert {:ok, updated_internalnetwork} = InternalNetwork.update(internalnetwork, %{name: "Updated Name"})
      assert updated_internalnetwork.name == "Updated Name"
    end
    
    test "activates internalnetwork" do
      internalnetwork = TestHelper.create_test_data(InternalNetwork, %{status: :inactive})
      
      assert {:ok, activated_internalnetwork} = InternalNetwork.update(internalnetwork, action: :activate)
      assert activated_internalnetwork.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing internalnetwork" do
      internalnetwork = TestHelper.create_test_data(InternalNetwork)
      
      assert :ok = InternalNetwork.delete(internalnetwork)
      assert {:error, %Ash.Error.Invalid{}} = InternalNetwork.get(InternalNetwork, internalnetwork.id)
    end
  end
end
