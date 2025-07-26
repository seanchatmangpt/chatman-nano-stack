defmodule Cybersecurity.Resources.DataAssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.DataAsset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates dataasset with valid attributes" do
      attrs = %{
        name: "Test DataAsset",
        description: "Test description",
        status: :active
      }
      
      DataAsset.init_storage()
      assert {:ok, dataasset} = DataAsset.create(attrs)
      assert dataasset.name == "Test DataAsset"
      assert dataasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = DataAsset.create(DataAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert {:ok, found_dataasset} = DataAsset.get(DataAsset, dataasset.id)
      assert found_dataasset.id == dataasset.id
    end
    
    test "lists all dataassets" do
      TestHelper.create_test_data(DataAsset, %{name: "DataAsset 1"})
      TestHelper.create_test_data(DataAsset, %{name: "DataAsset 2"})
      
      assert {:ok, dataassets} = DataAsset.list(DataAsset)
      assert length(dataassets) >= 2
    end
    
    test "filters by status" do
      active_dataasset = TestHelper.create_test_data(DataAsset, %{status: :active})
      _inactive_dataasset = TestHelper.create_test_data(DataAsset, %{status: :inactive})
      
      assert {:ok, [dataasset]} = DataAsset.list(DataAsset, action: :by_status, status: :active)
      assert dataasset.id == active_dataasset.id
    end
  end

  describe "update action" do
    test "updates dataasset attributes" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert {:ok, updated_dataasset} = DataAsset.update(dataasset, %{name: "Updated Name"})
      assert updated_dataasset.name == "Updated Name"
    end
    
    test "activates dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset, %{status: :inactive})
      
      assert {:ok, activated_dataasset} = DataAsset.update(dataasset, action: :activate)
      assert activated_dataasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert :ok = DataAsset.delete(dataasset)
      assert {:error, %Ash.Error.Invalid{}} = DataAsset.get(DataAsset, dataasset.id)
    end
  end
end
