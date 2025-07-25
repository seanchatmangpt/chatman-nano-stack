defmodule Cybersecurity.Resources.PrivilegeEscalationTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PrivilegeEscalation
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates privilegeescalation with valid attributes" do
      attrs = %{
        name: "Test PrivilegeEscalation",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, privilegeescalation} = Ash.create(PrivilegeEscalation, attrs)
      assert privilegeescalation.name == "Test PrivilegeEscalation"
      assert privilegeescalation.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(PrivilegeEscalation, attrs)
    end
  end

  describe "read action" do
    test "reads existing privilegeescalation" do
      privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation)
      
      assert {:ok, found_privilegeescalation} = Ash.get(PrivilegeEscalation, privilegeescalation.id)
      assert found_privilegeescalation.id == privilegeescalation.id
    end
    
    test "lists all privilegeescalations" do
      TestHelper.create_test_data(PrivilegeEscalation, %{name: "PrivilegeEscalation 1"})
      TestHelper.create_test_data(PrivilegeEscalation, %{name: "PrivilegeEscalation 2"})
      
      assert {:ok, privilegeescalations} = Ash.read(PrivilegeEscalation)
      assert length(privilegeescalations) >= 2
    end
    
    test "filters by status" do
      active_privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation, %{status: :active})
      _inactive_privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation, %{status: :inactive})
      
      assert {:ok, [privilegeescalation]} = Ash.read(PrivilegeEscalation, action: :by_status, status: :active)
      assert privilegeescalation.id == active_privilegeescalation.id
    end
  end

  describe "update action" do
    test "updates privilegeescalation attributes" do
      privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation)
      
      assert {:ok, updated_privilegeescalation} = Ash.update(privilegeescalation, %{name: "Updated Name"})
      assert updated_privilegeescalation.name == "Updated Name"
    end
    
    test "activates privilegeescalation" do
      privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation, %{status: :inactive})
      
      assert {:ok, activated_privilegeescalation} = Ash.update(privilegeescalation, action: :activate)
      assert activated_privilegeescalation.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing privilegeescalation" do
      privilegeescalation = TestHelper.create_test_data(PrivilegeEscalation)
      
      assert :ok = Ash.destroy(privilegeescalation)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(PrivilegeEscalation, privilegeescalation.id)
    end
  end
end
