defmodule Cybersecurity.Resources.UserAssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.UserAsset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates userasset with valid attributes" do
      attrs = %{
        name: "Test UserAsset",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, userasset} = Ash.create(UserAsset, attrs)
      assert userasset.name == "Test UserAsset"
      assert userasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(UserAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing userasset" do
      userasset = TestHelper.create_test_data(UserAsset)
      
      assert {:ok, found_userasset} = Ash.get(UserAsset, userasset.id)
      assert found_userasset.id == userasset.id
    end
    
    test "lists all userassets" do
      TestHelper.create_test_data(UserAsset, %{name: "UserAsset 1"})
      TestHelper.create_test_data(UserAsset, %{name: "UserAsset 2"})
      
      assert {:ok, userassets} = Ash.read(UserAsset)
      assert length(userassets) >= 2
    end
    
    test "filters by status" do
      active_userasset = TestHelper.create_test_data(UserAsset, %{status: :active})
      _inactive_userasset = TestHelper.create_test_data(UserAsset, %{status: :inactive})
      
      assert {:ok, [userasset]} = Ash.read(UserAsset, action: :by_status, status: :active)
      assert userasset.id == active_userasset.id
    end
  end

  describe "update action" do
    test "updates userasset attributes" do
      userasset = TestHelper.create_test_data(UserAsset)
      
      assert {:ok, updated_userasset} = Ash.update(userasset, %{name: "Updated Name"})
      assert updated_userasset.name == "Updated Name"
    end
    
    test "activates userasset" do
      userasset = TestHelper.create_test_data(UserAsset, %{status: :inactive})
      
      assert {:ok, activated_userasset} = Ash.update(userasset, action: :activate)
      assert activated_userasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing userasset" do
      userasset = TestHelper.create_test_data(UserAsset)
      
      assert :ok = Ash.destroy(userasset)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(UserAsset, userasset.id)
    end
  end
end
