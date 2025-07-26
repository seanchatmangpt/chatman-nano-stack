defmodule Cybersecurity.Resources.IOCTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.IOC
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates ioc with valid attributes" do
      attrs = %{
        name: "Test IOC",
        description: "Test description",
        status: :active
      }
      
      IOC.init_storage()
      assert {:ok, ioc} = IOC.create(attrs)
      assert ioc.name == "Test IOC"
      assert ioc.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = IOC.create(IOC, attrs)
    end
  end

  describe "read action" do
    test "reads existing ioc" do
      ioc = TestHelper.create_test_data(IOC)
      
      assert {:ok, found_ioc} = IOC.get(IOC, ioc.id)
      assert found_ioc.id == ioc.id
    end
    
    test "lists all iocs" do
      TestHelper.create_test_data(IOC, %{name: "IOC 1"})
      TestHelper.create_test_data(IOC, %{name: "IOC 2"})
      
      assert {:ok, iocs} = IOC.list(IOC)
      assert length(iocs) >= 2
    end
    
    test "filters by status" do
      active_ioc = TestHelper.create_test_data(IOC, %{status: :active})
      _inactive_ioc = TestHelper.create_test_data(IOC, %{status: :inactive})
      
      assert {:ok, [ioc]} = IOC.list(IOC, action: :by_status, status: :active)
      assert ioc.id == active_ioc.id
    end
  end

  describe "update action" do
    test "updates ioc attributes" do
      ioc = TestHelper.create_test_data(IOC)
      
      assert {:ok, updated_ioc} = IOC.update(ioc, %{name: "Updated Name"})
      assert updated_ioc.name == "Updated Name"
    end
    
    test "activates ioc" do
      ioc = TestHelper.create_test_data(IOC, %{status: :inactive})
      
      assert {:ok, activated_ioc} = IOC.update(ioc, action: :activate)
      assert activated_ioc.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ioc" do
      ioc = TestHelper.create_test_data(IOC)
      
      assert :ok = IOC.delete(ioc)
      assert {:error, %Ash.Error.Invalid{}} = IOC.get(IOC, ioc.id)
    end
  end
end
