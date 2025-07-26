defmodule Cybersecurity.Resources.DetectiveControlTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.DetectiveControl
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates detectivecontrol with valid attributes" do
      attrs = %{
        name: "Test DetectiveControl",
        description: "Test description",
        status: :active
      }
      
      DetectiveControl.init_storage()
      assert {:ok, detectivecontrol} = DetectiveControl.create(attrs)
      assert detectivecontrol.name == "Test DetectiveControl"
      assert detectivecontrol.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = DetectiveControl.create(DetectiveControl, attrs)
    end
  end

  describe "read action" do
    test "reads existing detectivecontrol" do
      detectivecontrol = TestHelper.create_test_data(DetectiveControl)
      
      assert {:ok, found_detectivecontrol} = DetectiveControl.get(DetectiveControl, detectivecontrol.id)
      assert found_detectivecontrol.id == detectivecontrol.id
    end
    
    test "lists all detectivecontrols" do
      TestHelper.create_test_data(DetectiveControl, %{name: "DetectiveControl 1"})
      TestHelper.create_test_data(DetectiveControl, %{name: "DetectiveControl 2"})
      
      assert {:ok, detectivecontrols} = DetectiveControl.list(DetectiveControl)
      assert length(detectivecontrols) >= 2
    end
    
    test "filters by status" do
      active_detectivecontrol = TestHelper.create_test_data(DetectiveControl, %{status: :active})
      _inactive_detectivecontrol = TestHelper.create_test_data(DetectiveControl, %{status: :inactive})
      
      assert {:ok, [detectivecontrol]} = DetectiveControl.list(DetectiveControl, action: :by_status, status: :active)
      assert detectivecontrol.id == active_detectivecontrol.id
    end
  end

  describe "update action" do
    test "updates detectivecontrol attributes" do
      detectivecontrol = TestHelper.create_test_data(DetectiveControl)
      
      assert {:ok, updated_detectivecontrol} = DetectiveControl.update(detectivecontrol, %{name: "Updated Name"})
      assert updated_detectivecontrol.name == "Updated Name"
    end
    
    test "activates detectivecontrol" do
      detectivecontrol = TestHelper.create_test_data(DetectiveControl, %{status: :inactive})
      
      assert {:ok, activated_detectivecontrol} = DetectiveControl.update(detectivecontrol, action: :activate)
      assert activated_detectivecontrol.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing detectivecontrol" do
      detectivecontrol = TestHelper.create_test_data(DetectiveControl)
      
      assert :ok = DetectiveControl.delete(detectivecontrol)
      assert {:error, %Ash.Error.Invalid{}} = DetectiveControl.get(DetectiveControl, detectivecontrol.id)
    end
  end
end
