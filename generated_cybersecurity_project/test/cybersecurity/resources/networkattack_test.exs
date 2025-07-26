defmodule Cybersecurity.Resources.NetworkAttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.NetworkAttack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates networkattack with valid attributes" do
      attrs = %{
        name: "Test NetworkAttack",
        description: "Test description",
        status: :active
      }
      
      NetworkAttack.init_storage()
      assert {:ok, networkattack} = NetworkAttack.create(attrs)
      assert networkattack.name == "Test NetworkAttack"
      assert networkattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = NetworkAttack.create(NetworkAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing networkattack" do
      networkattack = TestHelper.create_test_data(NetworkAttack)
      
      assert {:ok, found_networkattack} = NetworkAttack.get(NetworkAttack, networkattack.id)
      assert found_networkattack.id == networkattack.id
    end
    
    test "lists all networkattacks" do
      TestHelper.create_test_data(NetworkAttack, %{name: "NetworkAttack 1"})
      TestHelper.create_test_data(NetworkAttack, %{name: "NetworkAttack 2"})
      
      assert {:ok, networkattacks} = NetworkAttack.list(NetworkAttack)
      assert length(networkattacks) >= 2
    end
    
    test "filters by status" do
      active_networkattack = TestHelper.create_test_data(NetworkAttack, %{status: :active})
      _inactive_networkattack = TestHelper.create_test_data(NetworkAttack, %{status: :inactive})
      
      assert {:ok, [networkattack]} = NetworkAttack.list(NetworkAttack, action: :by_status, status: :active)
      assert networkattack.id == active_networkattack.id
    end
  end

  describe "update action" do
    test "updates networkattack attributes" do
      networkattack = TestHelper.create_test_data(NetworkAttack)
      
      assert {:ok, updated_networkattack} = NetworkAttack.update(networkattack, %{name: "Updated Name"})
      assert updated_networkattack.name == "Updated Name"
    end
    
    test "activates networkattack" do
      networkattack = TestHelper.create_test_data(NetworkAttack, %{status: :inactive})
      
      assert {:ok, activated_networkattack} = NetworkAttack.update(networkattack, action: :activate)
      assert activated_networkattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing networkattack" do
      networkattack = TestHelper.create_test_data(NetworkAttack)
      
      assert :ok = NetworkAttack.delete(networkattack)
      assert {:error, %Ash.Error.Invalid{}} = NetworkAttack.get(NetworkAttack, networkattack.id)
    end
  end
end
