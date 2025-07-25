defmodule Cybersecurity.Resources.ComputeAssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.ComputeAsset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates computeasset with valid attributes" do
      attrs = %{
        name: "Test ComputeAsset",
        description: "Test description",
        status: :active
      }
      
      ComputeAsset.init_storage()
      assert {:ok, computeasset} = ComputeAsset.create(attrs)
      assert computeasset.name == "Test ComputeAsset"
      assert computeasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = ComputeAsset.create(ComputeAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing computeasset" do
      computeasset = TestHelper.create_test_data(ComputeAsset)
      
      assert {:ok, found_computeasset} = ComputeAsset.get(ComputeAsset, computeasset.id)
      assert found_computeasset.id == computeasset.id
    end
    
    test "lists all computeassets" do
      TestHelper.create_test_data(ComputeAsset, %{name: "ComputeAsset 1"})
      TestHelper.create_test_data(ComputeAsset, %{name: "ComputeAsset 2"})
      
      assert {:ok, computeassets} = ComputeAsset.list(ComputeAsset)
      assert length(computeassets) >= 2
    end
    
    test "filters by status" do
      active_computeasset = TestHelper.create_test_data(ComputeAsset, %{status: :active})
      _inactive_computeasset = TestHelper.create_test_data(ComputeAsset, %{status: :inactive})
      
      assert {:ok, [computeasset]} = ComputeAsset.list(ComputeAsset, action: :by_status, status: :active)
      assert computeasset.id == active_computeasset.id
    end
  end

  describe "update action" do
    test "updates computeasset attributes" do
      computeasset = TestHelper.create_test_data(ComputeAsset)
      
      assert {:ok, updated_computeasset} = ComputeAsset.update(computeasset, %{name: "Updated Name"})
      assert updated_computeasset.name == "Updated Name"
    end
    
    test "activates computeasset" do
      computeasset = TestHelper.create_test_data(ComputeAsset, %{status: :inactive})
      
      assert {:ok, activated_computeasset} = ComputeAsset.update(computeasset, action: :activate)
      assert activated_computeasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing computeasset" do
      computeasset = TestHelper.create_test_data(ComputeAsset)
      
      assert :ok = ComputeAsset.delete(computeasset)
      assert {:error, %Ash.Error.Invalid{}} = ComputeAsset.get(ComputeAsset, computeasset.id)
    end
  end
end
