defmodule Cybersecurity.Resources.NetworkDeviceTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.NetworkDevice
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates networkdevice with valid attributes" do
      attrs = %{
        name: "Test NetworkDevice",
        description: "Test description",
        status: :active
      }
      
      NetworkDevice.init_storage()
      assert {:ok, networkdevice} = NetworkDevice.create(attrs)
      assert networkdevice.name == "Test NetworkDevice"
      assert networkdevice.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = NetworkDevice.create(NetworkDevice, attrs)
    end
  end

  describe "read action" do
    test "reads existing networkdevice" do
      networkdevice = TestHelper.create_test_data(NetworkDevice)
      
      assert {:ok, found_networkdevice} = NetworkDevice.get(NetworkDevice, networkdevice.id)
      assert found_networkdevice.id == networkdevice.id
    end
    
    test "lists all networkdevices" do
      TestHelper.create_test_data(NetworkDevice, %{name: "NetworkDevice 1"})
      TestHelper.create_test_data(NetworkDevice, %{name: "NetworkDevice 2"})
      
      assert {:ok, networkdevices} = NetworkDevice.list(NetworkDevice)
      assert length(networkdevices) >= 2
    end
    
    test "filters by status" do
      active_networkdevice = TestHelper.create_test_data(NetworkDevice, %{status: :active})
      _inactive_networkdevice = TestHelper.create_test_data(NetworkDevice, %{status: :inactive})
      
      assert {:ok, [networkdevice]} = NetworkDevice.list(NetworkDevice, action: :by_status, status: :active)
      assert networkdevice.id == active_networkdevice.id
    end
  end

  describe "update action" do
    test "updates networkdevice attributes" do
      networkdevice = TestHelper.create_test_data(NetworkDevice)
      
      assert {:ok, updated_networkdevice} = NetworkDevice.update(networkdevice, %{name: "Updated Name"})
      assert updated_networkdevice.name == "Updated Name"
    end
    
    test "activates networkdevice" do
      networkdevice = TestHelper.create_test_data(NetworkDevice, %{status: :inactive})
      
      assert {:ok, activated_networkdevice} = NetworkDevice.update(networkdevice, action: :activate)
      assert activated_networkdevice.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing networkdevice" do
      networkdevice = TestHelper.create_test_data(NetworkDevice)
      
      assert :ok = NetworkDevice.delete(networkdevice)
      assert {:error, %Ash.Error.Invalid{}} = NetworkDevice.get(NetworkDevice, networkdevice.id)
    end
  end
end
