defmodule Cybersecurity.Resources.NetworkAssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.NetworkAsset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates networkasset with valid attributes" do
      attrs = %{
        name: "Test NetworkAsset",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, networkasset} = Ash.create(NetworkAsset, attrs)
      assert networkasset.name == "Test NetworkAsset"
      assert networkasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(NetworkAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing networkasset" do
      networkasset = TestHelper.create_test_data(NetworkAsset)
      
      assert {:ok, found_networkasset} = Ash.get(NetworkAsset, networkasset.id)
      assert found_networkasset.id == networkasset.id
    end
    
    test "lists all networkassets" do
      TestHelper.create_test_data(NetworkAsset, %{name: "NetworkAsset 1"})
      TestHelper.create_test_data(NetworkAsset, %{name: "NetworkAsset 2"})
      
      assert {:ok, networkassets} = Ash.read(NetworkAsset)
      assert length(networkassets) >= 2
    end
    
    test "filters by status" do
      active_networkasset = TestHelper.create_test_data(NetworkAsset, %{status: :active})
      _inactive_networkasset = TestHelper.create_test_data(NetworkAsset, %{status: :inactive})
      
      assert {:ok, [networkasset]} = Ash.read(NetworkAsset, action: :by_status, status: :active)
      assert networkasset.id == active_networkasset.id
    end
  end

  describe "update action" do
    test "updates networkasset attributes" do
      networkasset = TestHelper.create_test_data(NetworkAsset)
      
      assert {:ok, updated_networkasset} = Ash.update(networkasset, %{name: "Updated Name"})
      assert updated_networkasset.name == "Updated Name"
    end
    
    test "activates networkasset" do
      networkasset = TestHelper.create_test_data(NetworkAsset, %{status: :inactive})
      
      assert {:ok, activated_networkasset} = Ash.update(networkasset, action: :activate)
      assert activated_networkasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing networkasset" do
      networkasset = TestHelper.create_test_data(NetworkAsset)
      
      assert :ok = Ash.destroy(networkasset)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(NetworkAsset, networkasset.id)
    end
  end
end
