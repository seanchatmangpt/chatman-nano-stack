defmodule Cybersecurity.Resources.SpywareTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Spyware
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates spyware with valid attributes" do
      attrs = %{
        name: "Test Spyware",
        description: "Test description",
        status: :active
      }
      
      Spyware.init_storage()
      assert {:ok, spyware} = Spyware.create(attrs)
      assert spyware.name == "Test Spyware"
      assert spyware.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Spyware.create(Spyware, attrs)
    end
  end

  describe "read action" do
    test "reads existing spyware" do
      spyware = TestHelper.create_test_data(Spyware)
      
      assert {:ok, found_spyware} = Spyware.get(Spyware, spyware.id)
      assert found_spyware.id == spyware.id
    end
    
    test "lists all spywares" do
      TestHelper.create_test_data(Spyware, %{name: "Spyware 1"})
      TestHelper.create_test_data(Spyware, %{name: "Spyware 2"})
      
      assert {:ok, spywares} = Spyware.list(Spyware)
      assert length(spywares) >= 2
    end
    
    test "filters by status" do
      active_spyware = TestHelper.create_test_data(Spyware, %{status: :active})
      _inactive_spyware = TestHelper.create_test_data(Spyware, %{status: :inactive})
      
      assert {:ok, [spyware]} = Spyware.list(Spyware, action: :by_status, status: :active)
      assert spyware.id == active_spyware.id
    end
  end

  describe "update action" do
    test "updates spyware attributes" do
      spyware = TestHelper.create_test_data(Spyware)
      
      assert {:ok, updated_spyware} = Spyware.update(spyware, %{name: "Updated Name"})
      assert updated_spyware.name == "Updated Name"
    end
    
    test "activates spyware" do
      spyware = TestHelper.create_test_data(Spyware, %{status: :inactive})
      
      assert {:ok, activated_spyware} = Spyware.update(spyware, action: :activate)
      assert activated_spyware.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing spyware" do
      spyware = TestHelper.create_test_data(Spyware)
      
      assert :ok = Spyware.delete(spyware)
      assert {:error, %Ash.Error.Invalid{}} = Spyware.get(Spyware, spyware.id)
    end
  end
end
