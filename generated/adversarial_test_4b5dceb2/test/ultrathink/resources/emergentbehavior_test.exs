defmodule Ultrathink.Resources.EmergentBehaviorTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Resources.EmergentBehavior
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates emergentbehavior with valid attributes" do
      attrs = %{
        name: "Test EmergentBehavior",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, emergentbehavior} = Ash.create(EmergentBehavior, attrs)
      assert emergentbehavior.name == "Test EmergentBehavior"
      assert emergentbehavior.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(EmergentBehavior, attrs)
    end
  end

  describe "read action" do
    test "reads existing emergentbehavior" do
      emergentbehavior = TestHelper.create_test_data(EmergentBehavior)
      
      assert {:ok, found_emergentbehavior} = Ash.get(EmergentBehavior, emergentbehavior.id)
      assert found_emergentbehavior.id == emergentbehavior.id
    end
    
    test "lists all emergentbehaviors" do
      TestHelper.create_test_data(EmergentBehavior, %{name: "EmergentBehavior 1"})
      TestHelper.create_test_data(EmergentBehavior, %{name: "EmergentBehavior 2"})
      
      assert {:ok, emergentbehaviors} = Ash.read(EmergentBehavior)
      assert length(emergentbehaviors) >= 2
    end
    
    test "filters by status" do
      active_emergentbehavior = TestHelper.create_test_data(EmergentBehavior, %{status: :active})
      _inactive_emergentbehavior = TestHelper.create_test_data(EmergentBehavior, %{status: :inactive})
      
      assert {:ok, [emergentbehavior]} = Ash.read(EmergentBehavior, action: :by_status, status: :active)
      assert emergentbehavior.id == active_emergentbehavior.id
    end
  end

  describe "update action" do
    test "updates emergentbehavior attributes" do
      emergentbehavior = TestHelper.create_test_data(EmergentBehavior)
      
      assert {:ok, updated_emergentbehavior} = Ash.update(emergentbehavior, %{name: "Updated Name"})
      assert updated_emergentbehavior.name == "Updated Name"
    end
    
    test "activates emergentbehavior" do
      emergentbehavior = TestHelper.create_test_data(EmergentBehavior, %{status: :inactive})
      
      assert {:ok, activated_emergentbehavior} = Ash.update(emergentbehavior, action: :activate)
      assert activated_emergentbehavior.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing emergentbehavior" do
      emergentbehavior = TestHelper.create_test_data(EmergentBehavior)
      
      assert :ok = Ash.destroy(emergentbehavior)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(EmergentBehavior, emergentbehavior.id)
    end
  end
end
