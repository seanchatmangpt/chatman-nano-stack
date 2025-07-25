defmodule Cybersecurity.Resources.VirusTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Virus
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates virus with valid attributes" do
      attrs = %{
        name: "Test Virus",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, virus} = Ash.create(Virus, attrs)
      assert virus.name == "Test Virus"
      assert virus.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(Virus, attrs)
    end
  end

  describe "read action" do
    test "reads existing virus" do
      virus = TestHelper.create_test_data(Virus)
      
      assert {:ok, found_virus} = Ash.get(Virus, virus.id)
      assert found_virus.id == virus.id
    end
    
    test "lists all viruss" do
      TestHelper.create_test_data(Virus, %{name: "Virus 1"})
      TestHelper.create_test_data(Virus, %{name: "Virus 2"})
      
      assert {:ok, viruss} = Ash.read(Virus)
      assert length(viruss) >= 2
    end
    
    test "filters by status" do
      active_virus = TestHelper.create_test_data(Virus, %{status: :active})
      _inactive_virus = TestHelper.create_test_data(Virus, %{status: :inactive})
      
      assert {:ok, [virus]} = Ash.read(Virus, action: :by_status, status: :active)
      assert virus.id == active_virus.id
    end
  end

  describe "update action" do
    test "updates virus attributes" do
      virus = TestHelper.create_test_data(Virus)
      
      assert {:ok, updated_virus} = Ash.update(virus, %{name: "Updated Name"})
      assert updated_virus.name == "Updated Name"
    end
    
    test "activates virus" do
      virus = TestHelper.create_test_data(Virus, %{status: :inactive})
      
      assert {:ok, activated_virus} = Ash.update(virus, action: :activate)
      assert activated_virus.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing virus" do
      virus = TestHelper.create_test_data(Virus)
      
      assert :ok = Ash.destroy(virus)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Virus, virus.id)
    end
  end
end
