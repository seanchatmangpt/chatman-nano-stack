defmodule Cybersecurity.Resources.CorrectiveControlTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.CorrectiveControl
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates correctivecontrol with valid attributes" do
      attrs = %{
        name: "Test CorrectiveControl",
        description: "Test description",
        status: :active
      }
      
      CorrectiveControl.init_storage()
      assert {:ok, correctivecontrol} = CorrectiveControl.create(attrs)
      assert correctivecontrol.name == "Test CorrectiveControl"
      assert correctivecontrol.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = CorrectiveControl.create(CorrectiveControl, attrs)
    end
  end

  describe "read action" do
    test "reads existing correctivecontrol" do
      correctivecontrol = TestHelper.create_test_data(CorrectiveControl)
      
      assert {:ok, found_correctivecontrol} = CorrectiveControl.get(CorrectiveControl, correctivecontrol.id)
      assert found_correctivecontrol.id == correctivecontrol.id
    end
    
    test "lists all correctivecontrols" do
      TestHelper.create_test_data(CorrectiveControl, %{name: "CorrectiveControl 1"})
      TestHelper.create_test_data(CorrectiveControl, %{name: "CorrectiveControl 2"})
      
      assert {:ok, correctivecontrols} = CorrectiveControl.list(CorrectiveControl)
      assert length(correctivecontrols) >= 2
    end
    
    test "filters by status" do
      active_correctivecontrol = TestHelper.create_test_data(CorrectiveControl, %{status: :active})
      _inactive_correctivecontrol = TestHelper.create_test_data(CorrectiveControl, %{status: :inactive})
      
      assert {:ok, [correctivecontrol]} = CorrectiveControl.list(CorrectiveControl, action: :by_status, status: :active)
      assert correctivecontrol.id == active_correctivecontrol.id
    end
  end

  describe "update action" do
    test "updates correctivecontrol attributes" do
      correctivecontrol = TestHelper.create_test_data(CorrectiveControl)
      
      assert {:ok, updated_correctivecontrol} = CorrectiveControl.update(correctivecontrol, %{name: "Updated Name"})
      assert updated_correctivecontrol.name == "Updated Name"
    end
    
    test "activates correctivecontrol" do
      correctivecontrol = TestHelper.create_test_data(CorrectiveControl, %{status: :inactive})
      
      assert {:ok, activated_correctivecontrol} = CorrectiveControl.update(correctivecontrol, action: :activate)
      assert activated_correctivecontrol.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing correctivecontrol" do
      correctivecontrol = TestHelper.create_test_data(CorrectiveControl)
      
      assert :ok = CorrectiveControl.delete(correctivecontrol)
      assert {:error, %Ash.Error.Invalid{}} = CorrectiveControl.get(CorrectiveControl, correctivecontrol.id)
    end
  end
end
