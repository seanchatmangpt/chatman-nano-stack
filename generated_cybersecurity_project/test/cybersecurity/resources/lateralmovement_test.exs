defmodule Cybersecurity.Resources.LateralMovementTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.LateralMovement
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates lateralmovement with valid attributes" do
      attrs = %{
        name: "Test LateralMovement",
        description: "Test description",
        status: :active
      }
      
      LateralMovement.init_storage()
      assert {:ok, lateralmovement} = LateralMovement.create(attrs)
      assert lateralmovement.name == "Test LateralMovement"
      assert lateralmovement.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = LateralMovement.create(LateralMovement, attrs)
    end
  end

  describe "read action" do
    test "reads existing lateralmovement" do
      lateralmovement = TestHelper.create_test_data(LateralMovement)
      
      assert {:ok, found_lateralmovement} = LateralMovement.get(LateralMovement, lateralmovement.id)
      assert found_lateralmovement.id == lateralmovement.id
    end
    
    test "lists all lateralmovements" do
      TestHelper.create_test_data(LateralMovement, %{name: "LateralMovement 1"})
      TestHelper.create_test_data(LateralMovement, %{name: "LateralMovement 2"})
      
      assert {:ok, lateralmovements} = LateralMovement.list(LateralMovement)
      assert length(lateralmovements) >= 2
    end
    
    test "filters by status" do
      active_lateralmovement = TestHelper.create_test_data(LateralMovement, %{status: :active})
      _inactive_lateralmovement = TestHelper.create_test_data(LateralMovement, %{status: :inactive})
      
      assert {:ok, [lateralmovement]} = LateralMovement.list(LateralMovement, action: :by_status, status: :active)
      assert lateralmovement.id == active_lateralmovement.id
    end
  end

  describe "update action" do
    test "updates lateralmovement attributes" do
      lateralmovement = TestHelper.create_test_data(LateralMovement)
      
      assert {:ok, updated_lateralmovement} = LateralMovement.update(lateralmovement, %{name: "Updated Name"})
      assert updated_lateralmovement.name == "Updated Name"
    end
    
    test "activates lateralmovement" do
      lateralmovement = TestHelper.create_test_data(LateralMovement, %{status: :inactive})
      
      assert {:ok, activated_lateralmovement} = LateralMovement.update(lateralmovement, action: :activate)
      assert activated_lateralmovement.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing lateralmovement" do
      lateralmovement = TestHelper.create_test_data(LateralMovement)
      
      assert :ok = LateralMovement.delete(lateralmovement)
      assert {:error, %Ash.Error.Invalid{}} = LateralMovement.get(LateralMovement, lateralmovement.id)
    end
  end
end
