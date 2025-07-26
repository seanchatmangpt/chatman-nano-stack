defmodule Cybersecurity.Resources.SecurityEventTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SecurityEvent
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates securityevent with valid attributes" do
      attrs = %{
        name: "Test SecurityEvent",
        description: "Test description",
        status: :active
      }
      
      SecurityEvent.init_storage()
      assert {:ok, securityevent} = SecurityEvent.create(attrs)
      assert securityevent.name == "Test SecurityEvent"
      assert securityevent.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SecurityEvent.create(SecurityEvent, attrs)
    end
  end

  describe "read action" do
    test "reads existing securityevent" do
      securityevent = TestHelper.create_test_data(SecurityEvent)
      
      assert {:ok, found_securityevent} = SecurityEvent.get(SecurityEvent, securityevent.id)
      assert found_securityevent.id == securityevent.id
    end
    
    test "lists all securityevents" do
      TestHelper.create_test_data(SecurityEvent, %{name: "SecurityEvent 1"})
      TestHelper.create_test_data(SecurityEvent, %{name: "SecurityEvent 2"})
      
      assert {:ok, securityevents} = SecurityEvent.list(SecurityEvent)
      assert length(securityevents) >= 2
    end
    
    test "filters by status" do
      active_securityevent = TestHelper.create_test_data(SecurityEvent, %{status: :active})
      _inactive_securityevent = TestHelper.create_test_data(SecurityEvent, %{status: :inactive})
      
      assert {:ok, [securityevent]} = SecurityEvent.list(SecurityEvent, action: :by_status, status: :active)
      assert securityevent.id == active_securityevent.id
    end
  end

  describe "update action" do
    test "updates securityevent attributes" do
      securityevent = TestHelper.create_test_data(SecurityEvent)
      
      assert {:ok, updated_securityevent} = SecurityEvent.update(securityevent, %{name: "Updated Name"})
      assert updated_securityevent.name == "Updated Name"
    end
    
    test "activates securityevent" do
      securityevent = TestHelper.create_test_data(SecurityEvent, %{status: :inactive})
      
      assert {:ok, activated_securityevent} = SecurityEvent.update(securityevent, action: :activate)
      assert activated_securityevent.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing securityevent" do
      securityevent = TestHelper.create_test_data(SecurityEvent)
      
      assert :ok = SecurityEvent.delete(securityevent)
      assert {:error, %Ash.Error.Invalid{}} = SecurityEvent.get(SecurityEvent, securityevent.id)
    end
  end
end
