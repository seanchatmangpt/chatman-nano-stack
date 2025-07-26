defmodule Cybersecurity.Resources.TrojanTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Trojan
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates trojan with valid attributes" do
      attrs = %{
        name: "Test Trojan",
        description: "Test description",
        status: :active
      }
      
      Trojan.init_storage()
      assert {:ok, trojan} = Trojan.create(attrs)
      assert trojan.name == "Test Trojan"
      assert trojan.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Trojan.create(Trojan, attrs)
    end
  end

  describe "read action" do
    test "reads existing trojan" do
      trojan = TestHelper.create_test_data(Trojan)
      
      assert {:ok, found_trojan} = Trojan.get(Trojan, trojan.id)
      assert found_trojan.id == trojan.id
    end
    
    test "lists all trojans" do
      TestHelper.create_test_data(Trojan, %{name: "Trojan 1"})
      TestHelper.create_test_data(Trojan, %{name: "Trojan 2"})
      
      assert {:ok, trojans} = Trojan.list(Trojan)
      assert length(trojans) >= 2
    end
    
    test "filters by status" do
      active_trojan = TestHelper.create_test_data(Trojan, %{status: :active})
      _inactive_trojan = TestHelper.create_test_data(Trojan, %{status: :inactive})
      
      assert {:ok, [trojan]} = Trojan.list(Trojan, action: :by_status, status: :active)
      assert trojan.id == active_trojan.id
    end
  end

  describe "update action" do
    test "updates trojan attributes" do
      trojan = TestHelper.create_test_data(Trojan)
      
      assert {:ok, updated_trojan} = Trojan.update(trojan, %{name: "Updated Name"})
      assert updated_trojan.name == "Updated Name"
    end
    
    test "activates trojan" do
      trojan = TestHelper.create_test_data(Trojan, %{status: :inactive})
      
      assert {:ok, activated_trojan} = Trojan.update(trojan, action: :activate)
      assert activated_trojan.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing trojan" do
      trojan = TestHelper.create_test_data(Trojan)
      
      assert :ok = Trojan.delete(trojan)
      assert {:error, %Ash.Error.Invalid{}} = Trojan.get(Trojan, trojan.id)
    end
  end
end
