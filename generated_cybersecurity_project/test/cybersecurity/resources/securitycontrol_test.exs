defmodule Cybersecurity.Resources.SecurityControlTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SecurityControl
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates securitycontrol with valid attributes" do
      attrs = %{
        name: "Test SecurityControl",
        description: "Test description",
        status: :active
      }
      
      SecurityControl.init_storage()
      assert {:ok, securitycontrol} = SecurityControl.create(attrs)
      assert securitycontrol.name == "Test SecurityControl"
      assert securitycontrol.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SecurityControl.create(SecurityControl, attrs)
    end
  end

  describe "read action" do
    test "reads existing securitycontrol" do
      securitycontrol = TestHelper.create_test_data(SecurityControl)
      
      assert {:ok, found_securitycontrol} = SecurityControl.get(SecurityControl, securitycontrol.id)
      assert found_securitycontrol.id == securitycontrol.id
    end
    
    test "lists all securitycontrols" do
      TestHelper.create_test_data(SecurityControl, %{name: "SecurityControl 1"})
      TestHelper.create_test_data(SecurityControl, %{name: "SecurityControl 2"})
      
      assert {:ok, securitycontrols} = SecurityControl.list(SecurityControl)
      assert length(securitycontrols) >= 2
    end
    
    test "filters by status" do
      active_securitycontrol = TestHelper.create_test_data(SecurityControl, %{status: :active})
      _inactive_securitycontrol = TestHelper.create_test_data(SecurityControl, %{status: :inactive})
      
      assert {:ok, [securitycontrol]} = SecurityControl.list(SecurityControl, action: :by_status, status: :active)
      assert securitycontrol.id == active_securitycontrol.id
    end
  end

  describe "update action" do
    test "updates securitycontrol attributes" do
      securitycontrol = TestHelper.create_test_data(SecurityControl)
      
      assert {:ok, updated_securitycontrol} = SecurityControl.update(securitycontrol, %{name: "Updated Name"})
      assert updated_securitycontrol.name == "Updated Name"
    end
    
    test "activates securitycontrol" do
      securitycontrol = TestHelper.create_test_data(SecurityControl, %{status: :inactive})
      
      assert {:ok, activated_securitycontrol} = SecurityControl.update(securitycontrol, action: :activate)
      assert activated_securitycontrol.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing securitycontrol" do
      securitycontrol = TestHelper.create_test_data(SecurityControl)
      
      assert :ok = SecurityControl.delete(securitycontrol)
      assert {:error, %Ash.Error.Invalid{}} = SecurityControl.get(SecurityControl, securitycontrol.id)
    end
  end
end
