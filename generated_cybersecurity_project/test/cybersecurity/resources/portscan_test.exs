defmodule Cybersecurity.Resources.PortScanTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PortScan
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates portscan with valid attributes" do
      attrs = %{
        name: "Test PortScan",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, portscan} = Ash.create(PortScan, attrs)
      assert portscan.name == "Test PortScan"
      assert portscan.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(PortScan, attrs)
    end
  end

  describe "read action" do
    test "reads existing portscan" do
      portscan = TestHelper.create_test_data(PortScan)
      
      assert {:ok, found_portscan} = Ash.get(PortScan, portscan.id)
      assert found_portscan.id == portscan.id
    end
    
    test "lists all portscans" do
      TestHelper.create_test_data(PortScan, %{name: "PortScan 1"})
      TestHelper.create_test_data(PortScan, %{name: "PortScan 2"})
      
      assert {:ok, portscans} = Ash.read(PortScan)
      assert length(portscans) >= 2
    end
    
    test "filters by status" do
      active_portscan = TestHelper.create_test_data(PortScan, %{status: :active})
      _inactive_portscan = TestHelper.create_test_data(PortScan, %{status: :inactive})
      
      assert {:ok, [portscan]} = Ash.read(PortScan, action: :by_status, status: :active)
      assert portscan.id == active_portscan.id
    end
  end

  describe "update action" do
    test "updates portscan attributes" do
      portscan = TestHelper.create_test_data(PortScan)
      
      assert {:ok, updated_portscan} = Ash.update(portscan, %{name: "Updated Name"})
      assert updated_portscan.name == "Updated Name"
    end
    
    test "activates portscan" do
      portscan = TestHelper.create_test_data(PortScan, %{status: :inactive})
      
      assert {:ok, activated_portscan} = Ash.update(portscan, action: :activate)
      assert activated_portscan.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing portscan" do
      portscan = TestHelper.create_test_data(PortScan)
      
      assert :ok = Ash.destroy(portscan)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(PortScan, portscan.id)
    end
  end
end
