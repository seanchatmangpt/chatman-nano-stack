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
      
      assert {:ok, applicationasset} = Ash.create(ApplicationAsset, attrs)
      assert applicationasset.name == "Test ApplicationAsset"
      assert applicationasset.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(ApplicationAsset, attrs)
    end
  end

  describe "read action" do
    test "reads existing applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert {:ok, found_applicationasset} = Ash.get(ApplicationAsset, applicationasset.id)
      assert found_applicationasset.id == applicationasset.id
    end
    
    test "lists all applicationassets" do
      TestHelper.create_test_data(ApplicationAsset, %{name: "ApplicationAsset 1"})
      TestHelper.create_test_data(ApplicationAsset, %{name: "ApplicationAsset 2"})
      
      assert {:ok, applicationassets} = Ash.read(ApplicationAsset)
      assert length(applicationassets) >= 2
    end
    
    test "filters by status" do
      active_applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :active})
      _inactive_applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :inactive})
      
      assert {:ok, [applicationasset]} = Ash.read(ApplicationAsset, action: :by_status, status: :active)
      assert applicationasset.id == active_applicationasset.id
    end
  end

  describe "update action" do
    test "updates applicationasset attributes" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert {:ok, updated_applicationasset} = Ash.update(applicationasset, %{name: "Updated Name"})
      assert updated_applicationasset.name == "Updated Name"
    end
    
    test "activates applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset, %{status: :inactive})
      
      assert {:ok, activated_applicationasset} = Ash.update(applicationasset, action: :activate)
      assert activated_applicationasset.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing applicationasset" do
      applicationasset = TestHelper.create_test_data(ApplicationAsset)
      
      assert :ok = Ash.destroy(applicationasset)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(ApplicationAsset, applicationasset.id)
    end
  end
end
