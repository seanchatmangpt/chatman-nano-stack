defmodule Cybersecurity.Resources.DMZTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.DMZ
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates dmz with valid attributes" do
      attrs = %{
        name: "Test DMZ",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, dmz} = Ash.create(DMZ, attrs)
      assert dmz.name == "Test DMZ"
      assert dmz.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(DMZ, attrs)
    end
  end

  describe "read action" do
    test "reads existing dmz" do
      dmz = TestHelper.create_test_data(DMZ)
      
      assert {:ok, found_dmz} = Ash.get(DMZ, dmz.id)
      assert found_dmz.id == dmz.id
    end
    
    test "lists all dmzs" do
      TestHelper.create_test_data(DMZ, %{name: "DMZ 1"})
      TestHelper.create_test_data(DMZ, %{name: "DMZ 2"})
      
      assert {:ok, dmzs} = Ash.read(DMZ)
      assert length(dmzs) >= 2
    end
    
    test "filters by status" do
      active_dmz = TestHelper.create_test_data(DMZ, %{status: :active})
      _inactive_dmz = TestHelper.create_test_data(DMZ, %{status: :inactive})
      
      assert {:ok, [dmz]} = Ash.read(DMZ, action: :by_status, status: :active)
      assert dmz.id == active_dmz.id
    end
  end

  describe "update action" do
    test "updates dmz attributes" do
      dmz = TestHelper.create_test_data(DMZ)
      
      assert {:ok, updated_dmz} = Ash.update(dmz, %{name: "Updated Name"})
      assert updated_dmz.name == "Updated Name"
    end
    
    test "activates dmz" do
      dmz = TestHelper.create_test_data(DMZ, %{status: :inactive})
      
      assert {:ok, activated_dmz} = Ash.update(dmz, action: :activate)
      assert activated_dmz.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing dmz" do
      dmz = TestHelper.create_test_data(DMZ)
      
      assert :ok = Ash.destroy(dmz)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(DMZ, dmz.id)
    end
  end
end
