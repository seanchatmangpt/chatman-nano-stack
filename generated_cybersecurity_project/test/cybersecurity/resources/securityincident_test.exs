defmodule Cybersecurity.Resources.SecurityIncidentTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SecurityIncident
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates securityincident with valid attributes" do
      attrs = %{
        name: "Test SecurityIncident",
        description: "Test description",
        status: :active
      }
      
      SecurityIncident.init_storage()
      assert {:ok, securityincident} = SecurityIncident.create(attrs)
      assert securityincident.name == "Test SecurityIncident"
      assert securityincident.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SecurityIncident.create(SecurityIncident, attrs)
    end
  end

  describe "read action" do
    test "reads existing securityincident" do
      securityincident = TestHelper.create_test_data(SecurityIncident)
      
      assert {:ok, found_securityincident} = SecurityIncident.get(SecurityIncident, securityincident.id)
      assert found_securityincident.id == securityincident.id
    end
    
    test "lists all securityincidents" do
      TestHelper.create_test_data(SecurityIncident, %{name: "SecurityIncident 1"})
      TestHelper.create_test_data(SecurityIncident, %{name: "SecurityIncident 2"})
      
      assert {:ok, securityincidents} = SecurityIncident.list(SecurityIncident)
      assert length(securityincidents) >= 2
    end
    
    test "filters by status" do
      active_securityincident = TestHelper.create_test_data(SecurityIncident, %{status: :active})
      _inactive_securityincident = TestHelper.create_test_data(SecurityIncident, %{status: :inactive})
      
      assert {:ok, [securityincident]} = SecurityIncident.list(SecurityIncident, action: :by_status, status: :active)
      assert securityincident.id == active_securityincident.id
    end
  end

  describe "update action" do
    test "updates securityincident attributes" do
      securityincident = TestHelper.create_test_data(SecurityIncident)
      
      assert {:ok, updated_securityincident} = SecurityIncident.update(securityincident, %{name: "Updated Name"})
      assert updated_securityincident.name == "Updated Name"
    end
    
    test "activates securityincident" do
      securityincident = TestHelper.create_test_data(SecurityIncident, %{status: :inactive})
      
      assert {:ok, activated_securityincident} = SecurityIncident.update(securityincident, action: :activate)
      assert activated_securityincident.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing securityincident" do
      securityincident = TestHelper.create_test_data(SecurityIncident)
      
      assert :ok = SecurityIncident.delete(securityincident)
      assert {:error, %Ash.Error.Invalid{}} = SecurityIncident.get(SecurityIncident, securityincident.id)
    end
  end
end
