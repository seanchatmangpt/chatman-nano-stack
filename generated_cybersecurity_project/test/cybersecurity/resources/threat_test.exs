defmodule Cybersecurity.Resources.ThreatTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Threat
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates threat with valid attributes" do
      attrs = %{
        name: "Test Threat",
        description: "Test description",
        status: :active
      }
      
      Threat.init_storage()
      assert {:ok, threat} = Threat.create(attrs)
      assert threat.name == "Test Threat"
      assert threat.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Threat.create(Threat, attrs)
    end
  end

  describe "read action" do
    test "reads existing threat" do
      threat = TestHelper.create_test_data(Threat)
      
      assert {:ok, found_threat} = Threat.get(Threat, threat.id)
      assert found_threat.id == threat.id
    end
    
    test "lists all threats" do
      TestHelper.create_test_data(Threat, %{name: "Threat 1"})
      TestHelper.create_test_data(Threat, %{name: "Threat 2"})
      
      assert {:ok, threats} = Threat.list(Threat)
      assert length(threats) >= 2
    end
    
    test "filters by status" do
      active_threat = TestHelper.create_test_data(Threat, %{status: :active})
      _inactive_threat = TestHelper.create_test_data(Threat, %{status: :inactive})
      
      assert {:ok, [threat]} = Threat.list(Threat, action: :by_status, status: :active)
      assert threat.id == active_threat.id
    end
  end

  describe "update action" do
    test "updates threat attributes" do
      threat = TestHelper.create_test_data(Threat)
      
      assert {:ok, updated_threat} = Threat.update(threat, %{name: "Updated Name"})
      assert updated_threat.name == "Updated Name"
    end
    
    test "activates threat" do
      threat = TestHelper.create_test_data(Threat, %{status: :inactive})
      
      assert {:ok, activated_threat} = Threat.update(threat, action: :activate)
      assert activated_threat.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing threat" do
      threat = TestHelper.create_test_data(Threat)
      
      assert :ok = Threat.delete(threat)
      assert {:error, %Ash.Error.Invalid{}} = Threat.get(Threat, threat.id)
    end
  end
end
