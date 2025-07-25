defmodule Cybersecurity.Resources.SwitchTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Switch
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates switch with valid attributes" do
      attrs = %{
        name: "Test Switch",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, switch} = Ash.create(Switch, attrs)
      assert switch.name == "Test Switch"
      assert switch.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(Switch, attrs)
    end
  end

  describe "read action" do
    test "reads existing switch" do
      switch = TestHelper.create_test_data(Switch)
      
      assert {:ok, found_switch} = Ash.get(Switch, switch.id)
      assert found_switch.id == switch.id
    end
    
    test "lists all switchs" do
      TestHelper.create_test_data(Switch, %{name: "Switch 1"})
      TestHelper.create_test_data(Switch, %{name: "Switch 2"})
      
      assert {:ok, switchs} = Ash.read(Switch)
      assert length(switchs) >= 2
    end
    
    test "filters by status" do
      active_switch = TestHelper.create_test_data(Switch, %{status: :active})
      _inactive_switch = TestHelper.create_test_data(Switch, %{status: :inactive})
      
      assert {:ok, [switch]} = Ash.read(Switch, action: :by_status, status: :active)
      assert switch.id == active_switch.id
    end
  end

  describe "update action" do
    test "updates switch attributes" do
      switch = TestHelper.create_test_data(Switch)
      
      assert {:ok, updated_switch} = Ash.update(switch, %{name: "Updated Name"})
      assert updated_switch.name == "Updated Name"
    end
    
    test "activates switch" do
      switch = TestHelper.create_test_data(Switch, %{status: :inactive})
      
      assert {:ok, activated_switch} = Ash.update(switch, action: :activate)
      assert activated_switch.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing switch" do
      switch = TestHelper.create_test_data(Switch)
      
      assert :ok = Ash.destroy(switch)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Switch, switch.id)
    end
  end
end
