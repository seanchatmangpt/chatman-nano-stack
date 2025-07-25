defmodule Cybersecurity.Resources.AttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Attack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates attack with valid attributes" do
      attrs = %{
        name: "Test Attack",
        description: "Test description",
        status: :active
      }
      
      Attack.init_storage()
      assert {:ok, attack} = Attack.create(attrs)
      assert attack.name == "Test Attack"
      assert attack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Attack.create(Attack, attrs)
    end
  end

  describe "read action" do
    test "reads existing attack" do
      attack = TestHelper.create_test_data(Attack)
      
      assert {:ok, found_attack} = Attack.get(Attack, attack.id)
      assert found_attack.id == attack.id
    end
    
    test "lists all attacks" do
      TestHelper.create_test_data(Attack, %{name: "Attack 1"})
      TestHelper.create_test_data(Attack, %{name: "Attack 2"})
      
      assert {:ok, attacks} = Attack.list(Attack)
      assert length(attacks) >= 2
    end
    
    test "filters by status" do
      active_attack = TestHelper.create_test_data(Attack, %{status: :active})
      _inactive_attack = TestHelper.create_test_data(Attack, %{status: :inactive})
      
      assert {:ok, [attack]} = Attack.list(Attack, action: :by_status, status: :active)
      assert attack.id == active_attack.id
    end
  end

  describe "update action" do
    test "updates attack attributes" do
      attack = TestHelper.create_test_data(Attack)
      
      assert {:ok, updated_attack} = Attack.update(attack, %{name: "Updated Name"})
      assert updated_attack.name == "Updated Name"
    end
    
    test "activates attack" do
      attack = TestHelper.create_test_data(Attack, %{status: :inactive})
      
      assert {:ok, activated_attack} = Attack.update(attack, action: :activate)
      assert activated_attack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing attack" do
      attack = TestHelper.create_test_data(Attack)
      
      assert :ok = Attack.delete(attack)
      assert {:error, %Ash.Error.Invalid{}} = Attack.get(Attack, attack.id)
    end
  end
end
