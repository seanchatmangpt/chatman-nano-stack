defmodule Cybersecurity.Resources.PreventiveControlTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PreventiveControl
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates preventivecontrol with valid attributes" do
      attrs = %{
        name: "Test PreventiveControl",
        description: "Test description",
        status: :active
      }
      
      PreventiveControl.init_storage()
      assert {:ok, preventivecontrol} = PreventiveControl.create(attrs)
      assert preventivecontrol.name == "Test PreventiveControl"
      assert preventivecontrol.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = PreventiveControl.create(PreventiveControl, attrs)
    end
  end

  describe "read action" do
    test "reads existing preventivecontrol" do
      preventivecontrol = TestHelper.create_test_data(PreventiveControl)
      
      assert {:ok, found_preventivecontrol} = PreventiveControl.get(PreventiveControl, preventivecontrol.id)
      assert found_preventivecontrol.id == preventivecontrol.id
    end
    
    test "lists all preventivecontrols" do
      TestHelper.create_test_data(PreventiveControl, %{name: "PreventiveControl 1"})
      TestHelper.create_test_data(PreventiveControl, %{name: "PreventiveControl 2"})
      
      assert {:ok, preventivecontrols} = PreventiveControl.list(PreventiveControl)
      assert length(preventivecontrols) >= 2
    end
    
    test "filters by status" do
      active_preventivecontrol = TestHelper.create_test_data(PreventiveControl, %{status: :active})
      _inactive_preventivecontrol = TestHelper.create_test_data(PreventiveControl, %{status: :inactive})
      
      assert {:ok, [preventivecontrol]} = PreventiveControl.list(PreventiveControl, action: :by_status, status: :active)
      assert preventivecontrol.id == active_preventivecontrol.id
    end
  end

  describe "update action" do
    test "updates preventivecontrol attributes" do
      preventivecontrol = TestHelper.create_test_data(PreventiveControl)
      
      assert {:ok, updated_preventivecontrol} = PreventiveControl.update(preventivecontrol, %{name: "Updated Name"})
      assert updated_preventivecontrol.name == "Updated Name"
    end
    
    test "activates preventivecontrol" do
      preventivecontrol = TestHelper.create_test_data(PreventiveControl, %{status: :inactive})
      
      assert {:ok, activated_preventivecontrol} = PreventiveControl.update(preventivecontrol, action: :activate)
      assert activated_preventivecontrol.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing preventivecontrol" do
      preventivecontrol = TestHelper.create_test_data(PreventiveControl)
      
      assert :ok = PreventiveControl.delete(preventivecontrol)
      assert {:error, %Ash.Error.Invalid{}} = PreventiveControl.get(PreventiveControl, preventivecontrol.id)
    end
  end
end
