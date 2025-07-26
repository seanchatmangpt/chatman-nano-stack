defmodule Cybersecurity.Resources.WormTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Worm
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates worm with valid attributes" do
      attrs = %{
        name: "Test Worm",
        description: "Test description",
        status: :active
      }
      
      Worm.init_storage()
      assert {:ok, worm} = Worm.create(attrs)
      assert worm.name == "Test Worm"
      assert worm.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Worm.create(Worm, attrs)
    end
  end

  describe "read action" do
    test "reads existing worm" do
      worm = TestHelper.create_test_data(Worm)
      
      assert {:ok, found_worm} = Worm.get(Worm, worm.id)
      assert found_worm.id == worm.id
    end
    
    test "lists all worms" do
      TestHelper.create_test_data(Worm, %{name: "Worm 1"})
      TestHelper.create_test_data(Worm, %{name: "Worm 2"})
      
      assert {:ok, worms} = Worm.list(Worm)
      assert length(worms) >= 2
    end
    
    test "filters by status" do
      active_worm = TestHelper.create_test_data(Worm, %{status: :active})
      _inactive_worm = TestHelper.create_test_data(Worm, %{status: :inactive})
      
      assert {:ok, [worm]} = Worm.list(Worm, action: :by_status, status: :active)
      assert worm.id == active_worm.id
    end
  end

  describe "update action" do
    test "updates worm attributes" do
      worm = TestHelper.create_test_data(Worm)
      
      assert {:ok, updated_worm} = Worm.update(worm, %{name: "Updated Name"})
      assert updated_worm.name == "Updated Name"
    end
    
    test "activates worm" do
      worm = TestHelper.create_test_data(Worm, %{status: :inactive})
      
      assert {:ok, activated_worm} = Worm.update(worm, action: :activate)
      assert activated_worm.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing worm" do
      worm = TestHelper.create_test_data(Worm)
      
      assert :ok = Worm.delete(worm)
      assert {:error, %Ash.Error.Invalid{}} = Worm.get(Worm, worm.id)
    end
  end
end
