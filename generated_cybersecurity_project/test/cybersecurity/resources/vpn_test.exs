defmodule Cybersecurity.Resources.VPNTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.VPN
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates vpn with valid attributes" do
      attrs = %{
        name: "Test VPN",
        description: "Test description",
        status: :active
      }
      
      VPN.init_storage()
      assert {:ok, vpn} = VPN.create(attrs)
      assert vpn.name == "Test VPN"
      assert vpn.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = VPN.create(VPN, attrs)
    end
  end

  describe "read action" do
    test "reads existing vpn" do
      vpn = TestHelper.create_test_data(VPN)
      
      assert {:ok, found_vpn} = VPN.get(VPN, vpn.id)
      assert found_vpn.id == vpn.id
    end
    
    test "lists all vpns" do
      TestHelper.create_test_data(VPN, %{name: "VPN 1"})
      TestHelper.create_test_data(VPN, %{name: "VPN 2"})
      
      assert {:ok, vpns} = VPN.list(VPN)
      assert length(vpns) >= 2
    end
    
    test "filters by status" do
      active_vpn = TestHelper.create_test_data(VPN, %{status: :active})
      _inactive_vpn = TestHelper.create_test_data(VPN, %{status: :inactive})
      
      assert {:ok, [vpn]} = VPN.list(VPN, action: :by_status, status: :active)
      assert vpn.id == active_vpn.id
    end
  end

  describe "update action" do
    test "updates vpn attributes" do
      vpn = TestHelper.create_test_data(VPN)
      
      assert {:ok, updated_vpn} = VPN.update(vpn, %{name: "Updated Name"})
      assert updated_vpn.name == "Updated Name"
    end
    
    test "activates vpn" do
      vpn = TestHelper.create_test_data(VPN, %{status: :inactive})
      
      assert {:ok, activated_vpn} = VPN.update(vpn, action: :activate)
      assert activated_vpn.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing vpn" do
      vpn = TestHelper.create_test_data(VPN)
      
      assert :ok = VPN.delete(vpn)
      assert {:error, %Ash.Error.Invalid{}} = VPN.get(VPN, vpn.id)
    end
  end
end
