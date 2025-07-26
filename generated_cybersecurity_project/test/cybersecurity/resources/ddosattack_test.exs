defmodule Cybersecurity.Resources.DDoSAttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.DDoSAttack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates ddosattack with valid attributes" do
      attrs = %{
        name: "Test DDoSAttack",
        description: "Test description",
        status: :active
      }
      
      DDoSAttack.init_storage()
      assert {:ok, ddosattack} = DDoSAttack.create(attrs)
      assert ddosattack.name == "Test DDoSAttack"
      assert ddosattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = DDoSAttack.create(DDoSAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert {:ok, found_ddosattack} = DDoSAttack.get(DDoSAttack, ddosattack.id)
      assert found_ddosattack.id == ddosattack.id
    end
    
    test "lists all ddosattacks" do
      TestHelper.create_test_data(DDoSAttack, %{name: "DDoSAttack 1"})
      TestHelper.create_test_data(DDoSAttack, %{name: "DDoSAttack 2"})
      
      assert {:ok, ddosattacks} = DDoSAttack.list(DDoSAttack)
      assert length(ddosattacks) >= 2
    end
    
    test "filters by status" do
      active_ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :active})
      _inactive_ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :inactive})
      
      assert {:ok, [ddosattack]} = DDoSAttack.list(DDoSAttack, action: :by_status, status: :active)
      assert ddosattack.id == active_ddosattack.id
    end
  end

  describe "update action" do
    test "updates ddosattack attributes" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert {:ok, updated_ddosattack} = DDoSAttack.update(ddosattack, %{name: "Updated Name"})
      assert updated_ddosattack.name == "Updated Name"
    end
    
    test "activates ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :inactive})
      
      assert {:ok, activated_ddosattack} = DDoSAttack.update(ddosattack, action: :activate)
      assert activated_ddosattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert :ok = DDoSAttack.delete(ddosattack)
      assert {:error, %Ash.Error.Invalid{}} = DDoSAttack.get(DDoSAttack, ddosattack.id)
    end
  end
end
