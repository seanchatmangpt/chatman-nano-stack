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
      
      assert {:ok, ddosattack} = Ash.create(DDoSAttack, attrs)
      assert ddosattack.name == "Test DDoSAttack"
      assert ddosattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(DDoSAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert {:ok, found_ddosattack} = Ash.get(DDoSAttack, ddosattack.id)
      assert found_ddosattack.id == ddosattack.id
    end
    
    test "lists all ddosattacks" do
      TestHelper.create_test_data(DDoSAttack, %{name: "DDoSAttack 1"})
      TestHelper.create_test_data(DDoSAttack, %{name: "DDoSAttack 2"})
      
      assert {:ok, ddosattacks} = Ash.read(DDoSAttack)
      assert length(ddosattacks) >= 2
    end
    
    test "filters by status" do
      active_ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :active})
      _inactive_ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :inactive})
      
      assert {:ok, [ddosattack]} = Ash.read(DDoSAttack, action: :by_status, status: :active)
      assert ddosattack.id == active_ddosattack.id
    end
  end

  describe "update action" do
    test "updates ddosattack attributes" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert {:ok, updated_ddosattack} = Ash.update(ddosattack, %{name: "Updated Name"})
      assert updated_ddosattack.name == "Updated Name"
    end
    
    test "activates ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack, %{status: :inactive})
      
      assert {:ok, activated_ddosattack} = Ash.update(ddosattack, action: :activate)
      assert activated_ddosattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ddosattack" do
      ddosattack = TestHelper.create_test_data(DDoSAttack)
      
      assert :ok = Ash.destroy(ddosattack)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(DDoSAttack, ddosattack.id)
    end
  end
end
