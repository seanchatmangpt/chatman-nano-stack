defmodule Cybersecurity.Resources.PhishingAttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PhishingAttack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates phishingattack with valid attributes" do
      attrs = %{
        name: "Test PhishingAttack",
        description: "Test description",
        status: :active
      }
      
      PhishingAttack.init_storage()
      assert {:ok, phishingattack} = PhishingAttack.create(attrs)
      assert phishingattack.name == "Test PhishingAttack"
      assert phishingattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = PhishingAttack.create(PhishingAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing phishingattack" do
      phishingattack = TestHelper.create_test_data(PhishingAttack)
      
      assert {:ok, found_phishingattack} = PhishingAttack.get(PhishingAttack, phishingattack.id)
      assert found_phishingattack.id == phishingattack.id
    end
    
    test "lists all phishingattacks" do
      TestHelper.create_test_data(PhishingAttack, %{name: "PhishingAttack 1"})
      TestHelper.create_test_data(PhishingAttack, %{name: "PhishingAttack 2"})
      
      assert {:ok, phishingattacks} = PhishingAttack.list(PhishingAttack)
      assert length(phishingattacks) >= 2
    end
    
    test "filters by status" do
      active_phishingattack = TestHelper.create_test_data(PhishingAttack, %{status: :active})
      _inactive_phishingattack = TestHelper.create_test_data(PhishingAttack, %{status: :inactive})
      
      assert {:ok, [phishingattack]} = PhishingAttack.list(PhishingAttack, action: :by_status, status: :active)
      assert phishingattack.id == active_phishingattack.id
    end
  end

  describe "update action" do
    test "updates phishingattack attributes" do
      phishingattack = TestHelper.create_test_data(PhishingAttack)
      
      assert {:ok, updated_phishingattack} = PhishingAttack.update(phishingattack, %{name: "Updated Name"})
      assert updated_phishingattack.name == "Updated Name"
    end
    
    test "activates phishingattack" do
      phishingattack = TestHelper.create_test_data(PhishingAttack, %{status: :inactive})
      
      assert {:ok, activated_phishingattack} = PhishingAttack.update(phishingattack, action: :activate)
      assert activated_phishingattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing phishingattack" do
      phishingattack = TestHelper.create_test_data(PhishingAttack)
      
      assert :ok = PhishingAttack.delete(phishingattack)
      assert {:error, %Ash.Error.Invalid{}} = PhishingAttack.get(PhishingAttack, phishingattack.id)
    end
  end
end
