defmodule Cybersecurity.Resources.AlertTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Alert
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates alert with valid attributes" do
      attrs = %{
        name: "Test Alert",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, alert} = Ash.create(Alert, attrs)
      assert alert.name == "Test Alert"
      assert alert.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(Alert, attrs)
    end
  end

  describe "read action" do
    test "reads existing alert" do
      alert = TestHelper.create_test_data(Alert)
      
      assert {:ok, found_alert} = Ash.get(Alert, alert.id)
      assert found_alert.id == alert.id
    end
    
    test "lists all alerts" do
      TestHelper.create_test_data(Alert, %{name: "Alert 1"})
      TestHelper.create_test_data(Alert, %{name: "Alert 2"})
      
      assert {:ok, alerts} = Ash.read(Alert)
      assert length(alerts) >= 2
    end
    
    test "filters by status" do
      active_alert = TestHelper.create_test_data(Alert, %{status: :active})
      _inactive_alert = TestHelper.create_test_data(Alert, %{status: :inactive})
      
      assert {:ok, [alert]} = Ash.read(Alert, action: :by_status, status: :active)
      assert alert.id == active_alert.id
    end
  end

  describe "update action" do
    test "updates alert attributes" do
      alert = TestHelper.create_test_data(Alert)
      
      assert {:ok, updated_alert} = Ash.update(alert, %{name: "Updated Name"})
      assert updated_alert.name == "Updated Name"
    end
    
    test "activates alert" do
      alert = TestHelper.create_test_data(Alert, %{status: :inactive})
      
      assert {:ok, activated_alert} = Ash.update(alert, action: :activate)
      assert activated_alert.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing alert" do
      alert = TestHelper.create_test_data(Alert)
      
      assert :ok = Ash.destroy(alert)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Alert, alert.id)
    end
  end
end
