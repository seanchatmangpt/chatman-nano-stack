defmodule Cybersecurity.Resources.FirewallTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Firewall
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates firewall with valid attributes" do
      attrs = %{
        name: "Test Firewall",
        description: "Test description",
        status: :active
      }
      
      Firewall.init_storage()
      assert {:ok, firewall} = Firewall.create(attrs)
      assert firewall.name == "Test Firewall"
      assert firewall.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Firewall.create(Firewall, attrs)
    end
  end

  describe "read action" do
    test "reads existing firewall" do
      firewall = TestHelper.create_test_data(Firewall)
      
      assert {:ok, found_firewall} = Firewall.get(Firewall, firewall.id)
      assert found_firewall.id == firewall.id
    end
    
    test "lists all firewalls" do
      TestHelper.create_test_data(Firewall, %{name: "Firewall 1"})
      TestHelper.create_test_data(Firewall, %{name: "Firewall 2"})
      
      assert {:ok, firewalls} = Firewall.list(Firewall)
      assert length(firewalls) >= 2
    end
    
    test "filters by status" do
      active_firewall = TestHelper.create_test_data(Firewall, %{status: :active})
      _inactive_firewall = TestHelper.create_test_data(Firewall, %{status: :inactive})
      
      assert {:ok, [firewall]} = Firewall.list(Firewall, action: :by_status, status: :active)
      assert firewall.id == active_firewall.id
    end
  end

  describe "update action" do
    test "updates firewall attributes" do
      firewall = TestHelper.create_test_data(Firewall)
      
      assert {:ok, updated_firewall} = Firewall.update(firewall, %{name: "Updated Name"})
      assert updated_firewall.name == "Updated Name"
    end
    
    test "activates firewall" do
      firewall = TestHelper.create_test_data(Firewall, %{status: :inactive})
      
      assert {:ok, activated_firewall} = Firewall.update(firewall, action: :activate)
      assert activated_firewall.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing firewall" do
      firewall = TestHelper.create_test_data(Firewall)
      
      assert :ok = Firewall.delete(firewall)
      assert {:error, %Ash.Error.Invalid{}} = Firewall.get(Firewall, firewall.id)
    end
  end
end
