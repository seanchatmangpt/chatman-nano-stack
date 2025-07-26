defmodule Ultrathink.Resources.BitActorTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Resources.BitActor
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates bitactor with valid attributes" do
      attrs = %{
        name: "Test BitActor",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, bitactor} = Ash.create(BitActor, attrs)
      assert bitactor.name == "Test BitActor"
      assert bitactor.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(BitActor, attrs)
    end
  end

  describe "read action" do
    test "reads existing bitactor" do
      bitactor = TestHelper.create_test_data(BitActor)
      
      assert {:ok, found_bitactor} = Ash.get(BitActor, bitactor.id)
      assert found_bitactor.id == bitactor.id
    end
    
    test "lists all bitactors" do
      TestHelper.create_test_data(BitActor, %{name: "BitActor 1"})
      TestHelper.create_test_data(BitActor, %{name: "BitActor 2"})
      
      assert {:ok, bitactors} = Ash.read(BitActor)
      assert length(bitactors) >= 2
    end
    
    test "filters by status" do
      active_bitactor = TestHelper.create_test_data(BitActor, %{status: :active})
      _inactive_bitactor = TestHelper.create_test_data(BitActor, %{status: :inactive})
      
      assert {:ok, [bitactor]} = Ash.read(BitActor, action: :by_status, status: :active)
      assert bitactor.id == active_bitactor.id
    end
  end

  describe "update action" do
    test "updates bitactor attributes" do
      bitactor = TestHelper.create_test_data(BitActor)
      
      assert {:ok, updated_bitactor} = Ash.update(bitactor, %{name: "Updated Name"})
      assert updated_bitactor.name == "Updated Name"
    end
    
    test "activates bitactor" do
      bitactor = TestHelper.create_test_data(BitActor, %{status: :inactive})
      
      assert {:ok, activated_bitactor} = Ash.update(bitactor, action: :activate)
      assert activated_bitactor.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing bitactor" do
      bitactor = TestHelper.create_test_data(BitActor)
      
      assert :ok = Ash.destroy(bitactor)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(BitActor, bitactor.id)
    end
  end
end
