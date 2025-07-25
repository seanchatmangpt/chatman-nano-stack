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
      
      assert {:ok, dataasset} = Ash.create(DataAsset, attrs)
      assert dataasset.name == "Test DataAsset"
      assert dataasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(DataAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert {:ok, found_dataasset} = Ash.get(DataAsset, dataasset.id)
      assert found_dataasset.id == dataasset.id
    end
    
    test "lists all dataassets" do
      TestHelper.create_test_data(DataAsset, %{name: "DataAsset 1"})
      TestHelper.create_test_data(DataAsset, %{name: "DataAsset 2"})
      
      assert {:ok, dataassets} = Ash.read(DataAsset)
      assert length(dataassets) >= 2
    end
    
    test "filters by status" do
      active_dataasset = TestHelper.create_test_data(DataAsset, %{status: :active})
      _inactive_dataasset = TestHelper.create_test_data(DataAsset, %{status: :inactive})
      
      assert {:ok, [dataasset]} = Ash.read(DataAsset, action: :by_status, status: :active)
      assert dataasset.id == active_dataasset.id
    end
  end

  describe "update action" do
    test "updates dataasset attributes" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert {:ok, updated_dataasset} = Ash.update(dataasset, %{name: "Updated Name"})
      assert updated_dataasset.name == "Updated Name"
    end
    
    test "activates dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset, %{status: :inactive})
      
      assert {:ok, activated_dataasset} = Ash.update(dataasset, action: :activate)
      assert activated_dataasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing dataasset" do
      dataasset = TestHelper.create_test_data(DataAsset)
      
      assert :ok = Ash.destroy(dataasset)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(DataAsset, dataasset.id)
    end
  end
end
