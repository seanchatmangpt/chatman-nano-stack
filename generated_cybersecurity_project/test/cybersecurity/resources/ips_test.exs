defmodule Cybersecurity.Resources.IPSTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.IPS
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates ips with valid attributes" do
      attrs = %{
        name: "Test IPS",
        description: "Test description",
        status: :active
      }
      
      IPS.init_storage()
      assert {:ok, ips} = IPS.create(attrs)
      assert ips.name == "Test IPS"
      assert ips.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = IPS.create(IPS, attrs)
    end
  end

  describe "read action" do
    test "reads existing ips" do
      ips = TestHelper.create_test_data(IPS)
      
      assert {:ok, found_ips} = IPS.get(IPS, ips.id)
      assert found_ips.id == ips.id
    end
    
    test "lists all ipss" do
      TestHelper.create_test_data(IPS, %{name: "IPS 1"})
      TestHelper.create_test_data(IPS, %{name: "IPS 2"})
      
      assert {:ok, ipss} = IPS.list(IPS)
      assert length(ipss) >= 2
    end
    
    test "filters by status" do
      active_ips = TestHelper.create_test_data(IPS, %{status: :active})
      _inactive_ips = TestHelper.create_test_data(IPS, %{status: :inactive})
      
      assert {:ok, [ips]} = IPS.list(IPS, action: :by_status, status: :active)
      assert ips.id == active_ips.id
    end
  end

  describe "update action" do
    test "updates ips attributes" do
      ips = TestHelper.create_test_data(IPS)
      
      assert {:ok, updated_ips} = IPS.update(ips, %{name: "Updated Name"})
      assert updated_ips.name == "Updated Name"
    end
    
    test "activates ips" do
      ips = TestHelper.create_test_data(IPS, %{status: :inactive})
      
      assert {:ok, activated_ips} = IPS.update(ips, action: :activate)
      assert activated_ips.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ips" do
      ips = TestHelper.create_test_data(IPS)
      
      assert :ok = IPS.delete(ips)
      assert {:error, %Ash.Error.Invalid{}} = IPS.get(IPS, ips.id)
    end
  end
end
