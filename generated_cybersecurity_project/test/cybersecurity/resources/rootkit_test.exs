defmodule Cybersecurity.Resources.RootkitTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Rootkit
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates rootkit with valid attributes" do
      attrs = %{
        name: "Test Rootkit",
        description: "Test description",
        status: :active
      }
      
      Rootkit.init_storage()
      assert {:ok, rootkit} = Rootkit.create(attrs)
      assert rootkit.name == "Test Rootkit"
      assert rootkit.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Rootkit.create(Rootkit, attrs)
    end
  end

  describe "read action" do
    test "reads existing rootkit" do
      rootkit = TestHelper.create_test_data(Rootkit)
      
      assert {:ok, found_rootkit} = Rootkit.get(Rootkit, rootkit.id)
      assert found_rootkit.id == rootkit.id
    end
    
    test "lists all rootkits" do
      TestHelper.create_test_data(Rootkit, %{name: "Rootkit 1"})
      TestHelper.create_test_data(Rootkit, %{name: "Rootkit 2"})
      
      assert {:ok, rootkits} = Rootkit.list(Rootkit)
      assert length(rootkits) >= 2
    end
    
    test "filters by status" do
      active_rootkit = TestHelper.create_test_data(Rootkit, %{status: :active})
      _inactive_rootkit = TestHelper.create_test_data(Rootkit, %{status: :inactive})
      
      assert {:ok, [rootkit]} = Rootkit.list(Rootkit, action: :by_status, status: :active)
      assert rootkit.id == active_rootkit.id
    end
  end

  describe "update action" do
    test "updates rootkit attributes" do
      rootkit = TestHelper.create_test_data(Rootkit)
      
      assert {:ok, updated_rootkit} = Rootkit.update(rootkit, %{name: "Updated Name"})
      assert updated_rootkit.name == "Updated Name"
    end
    
    test "activates rootkit" do
      rootkit = TestHelper.create_test_data(Rootkit, %{status: :inactive})
      
      assert {:ok, activated_rootkit} = Rootkit.update(rootkit, action: :activate)
      assert activated_rootkit.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing rootkit" do
      rootkit = TestHelper.create_test_data(Rootkit)
      
      assert :ok = Rootkit.delete(rootkit)
      assert {:error, %Ash.Error.Invalid{}} = Rootkit.get(Rootkit, rootkit.id)
    end
  end
end
