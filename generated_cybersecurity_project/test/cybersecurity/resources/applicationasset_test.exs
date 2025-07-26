defmodule Cybersecurity.Resources.ApplicationAssetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.ApplicationAsset
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates applicationasset with valid attributes" do
      attrs = %{
        name: "Test ApplicationAsset",
        description: "Test description",
        status: :active
      }
      
      ApplicationAsset.init_storage()
      assert {:ok, applicationasset} = ApplicationAsset.create(attrs)
      assert applicationasset.name == "Test ApplicationAsset"
      assert applicationasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = ApplicationAsset.create(ApplicationAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert {:ok, found_applicationasset} = ApplicationAsset.get(ApplicationAsset, applicationasset.id)
      assert found_applicationasset.id == applicationasset.id
    end
    
    test "lists all applicationassets" do
      TestHelper.create_test_data(ApplicationAsset, %{name: "ApplicationAsset 1"})
      TestHelper.create_test_data(ApplicationAsset, %{name: "ApplicationAsset 2"})
      
      assert {:ok, applicationassets} = ApplicationAsset.list(ApplicationAsset)
      assert length(applicationassets) >= 2
    end
    
    test "filters by status" do
      active_applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :active})
      _inactive_applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :inactive})
      
      assert {:ok, [applicationasset]} = ApplicationAsset.list(ApplicationAsset, action: :by_status, status: :active)
      assert applicationasset.id == active_applicationasset.id
    end
  end

  describe "update action" do
    test "updates applicationasset attributes" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert {:ok, updated_applicationasset} = ApplicationAsset.update(applicationasset, %{name: "Updated Name"})
      assert updated_applicationasset.name == "Updated Name"
    end
    
    test "activates applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :inactive})
      
      assert {:ok, activated_applicationasset} = ApplicationAsset.update(applicationasset, action: :activate)
      assert activated_applicationasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert :ok = ApplicationAsset.delete(applicationasset)
      assert {:error, %Ash.Error.Invalid{}} = ApplicationAsset.get(ApplicationAsset, applicationasset.id)
    end
  end
end
