defmodule Cybersecurity.Resources.AssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Asset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates asset with valid attributes" do
      attrs = %{
        name: "Test Asset",
        description: "Test description",
        status: :active
      }
      
      Asset.init_storage()
      assert {:ok, asset} = Asset.create(attrs)
      assert asset.name == "Test Asset"
      assert asset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Asset.create(Asset, attrs)
    end
  end

  describe "read action" do
    test "reads existing asset" do
      asset = TestHelper.create_test_data(Asset)
      
      assert {:ok, found_asset} = Asset.get(Asset, asset.id)
      assert found_asset.id == asset.id
    end
    
    test "lists all assets" do
      TestHelper.create_test_data(Asset, %{name: "Asset 1"})
      TestHelper.create_test_data(Asset, %{name: "Asset 2"})
      
      assert {:ok, assets} = Asset.list(Asset)
      assert length(assets) >= 2
    end
    
    test "filters by status" do
      active_asset = TestHelper.create_test_data(Asset, %{status: :active})
      _inactive_asset = TestHelper.create_test_data(Asset, %{status: :inactive})
      
      assert {:ok, [asset]} = Asset.list(Asset, action: :by_status, status: :active)
      assert asset.id == active_asset.id
    end
  end

  describe "update action" do
    test "updates asset attributes" do
      asset = TestHelper.create_test_data(Asset)
      
      assert {:ok, updated_asset} = Asset.update(asset, %{name: "Updated Name"})
      assert updated_asset.name == "Updated Name"
    end
    
    test "activates asset" do
      asset = TestHelper.create_test_data(Asset, %{status: :inactive})
      
      assert {:ok, activated_asset} = Asset.update(asset, action: :activate)
      assert activated_asset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing asset" do
      asset = TestHelper.create_test_data(Asset)
      
      assert :ok = Asset.delete(asset)
      assert {:error, %Ash.Error.Invalid{}} = Asset.get(Asset, asset.id)
    end
  end
end
