defmodule Cybersecurity.Resources.AntivirusTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Antivirus
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates antivirus with valid attributes" do
      attrs = %{
        name: "Test Antivirus",
        description: "Test description",
        status: :active
      }
      
      Antivirus.init_storage()
      assert {:ok, antivirus} = Antivirus.create(attrs)
      assert antivirus.name == "Test Antivirus"
      assert antivirus.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Antivirus.create(Antivirus, attrs)
    end
  end

  describe "read action" do
    test "reads existing antivirus" do
      antivirus = TestHelper.create_test_data(Antivirus)
      
      assert {:ok, found_antivirus} = Antivirus.get(Antivirus, antivirus.id)
      assert found_antivirus.id == antivirus.id
    end
    
    test "lists all antiviruss" do
      TestHelper.create_test_data(Antivirus, %{name: "Antivirus 1"})
      TestHelper.create_test_data(Antivirus, %{name: "Antivirus 2"})
      
      assert {:ok, antiviruss} = Antivirus.list(Antivirus)
      assert length(antiviruss) >= 2
    end
    
    test "filters by status" do
      active_antivirus = TestHelper.create_test_data(Antivirus, %{status: :active})
      _inactive_antivirus = TestHelper.create_test_data(Antivirus, %{status: :inactive})
      
      assert {:ok, [antivirus]} = Antivirus.list(Antivirus, action: :by_status, status: :active)
      assert antivirus.id == active_antivirus.id
    end
  end

  describe "update action" do
    test "updates antivirus attributes" do
      antivirus = TestHelper.create_test_data(Antivirus)
      
      assert {:ok, updated_antivirus} = Antivirus.update(antivirus, %{name: "Updated Name"})
      assert updated_antivirus.name == "Updated Name"
    end
    
    test "activates antivirus" do
      antivirus = TestHelper.create_test_data(Antivirus, %{status: :inactive})
      
      assert {:ok, activated_antivirus} = Antivirus.update(antivirus, action: :activate)
      assert activated_antivirus.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing antivirus" do
      antivirus = TestHelper.create_test_data(Antivirus)
      
      assert :ok = Antivirus.delete(antivirus)
      assert {:error, %Ash.Error.Invalid{}} = Antivirus.get(Antivirus, antivirus.id)
    end
  end
end
