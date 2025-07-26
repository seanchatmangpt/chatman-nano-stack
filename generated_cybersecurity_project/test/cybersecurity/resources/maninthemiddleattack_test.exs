defmodule Cybersecurity.Resources.ManInTheMiddleAttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.ManInTheMiddleAttack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates maninthemiddleattack with valid attributes" do
      attrs = %{
        name: "Test ManInTheMiddleAttack",
        description: "Test description",
        status: :active
      }
      
      ManInTheMiddleAttack.init_storage()
      assert {:ok, maninthemiddleattack} = ManInTheMiddleAttack.create(attrs)
      assert maninthemiddleattack.name == "Test ManInTheMiddleAttack"
      assert maninthemiddleattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = ManInTheMiddleAttack.create(ManInTheMiddleAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing maninthemiddleattack" do
      maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack)
      
      assert {:ok, found_maninthemiddleattack} = ManInTheMiddleAttack.get(ManInTheMiddleAttack, maninthemiddleattack.id)
      assert found_maninthemiddleattack.id == maninthemiddleattack.id
    end
    
    test "lists all maninthemiddleattacks" do
      TestHelper.create_test_data(ManInTheMiddleAttack, %{name: "ManInTheMiddleAttack 1"})
      TestHelper.create_test_data(ManInTheMiddleAttack, %{name: "ManInTheMiddleAttack 2"})
      
      assert {:ok, maninthemiddleattacks} = ManInTheMiddleAttack.list(ManInTheMiddleAttack)
      assert length(maninthemiddleattacks) >= 2
    end
    
    test "filters by status" do
      active_maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack, %{status: :active})
      _inactive_maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack, %{status: :inactive})
      
      assert {:ok, [maninthemiddleattack]} = ManInTheMiddleAttack.list(ManInTheMiddleAttack, action: :by_status, status: :active)
      assert maninthemiddleattack.id == active_maninthemiddleattack.id
    end
  end

  describe "update action" do
    test "updates maninthemiddleattack attributes" do
      maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack)
      
      assert {:ok, updated_maninthemiddleattack} = ManInTheMiddleAttack.update(maninthemiddleattack, %{name: "Updated Name"})
      assert updated_maninthemiddleattack.name == "Updated Name"
    end
    
    test "activates maninthemiddleattack" do
      maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack, %{status: :inactive})
      
      assert {:ok, activated_maninthemiddleattack} = ManInTheMiddleAttack.update(maninthemiddleattack, action: :activate)
      assert activated_maninthemiddleattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing maninthemiddleattack" do
      maninthemiddleattack = TestHelper.create_test_data(ManInTheMiddleAttack)
      
      assert :ok = ManInTheMiddleAttack.delete(maninthemiddleattack)
      assert {:error, %Ash.Error.Invalid{}} = ManInTheMiddleAttack.get(ManInTheMiddleAttack, maninthemiddleattack.id)
    end
  end
end
