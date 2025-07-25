defmodule Cybersecurity.Resources.SpearPhishingTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SpearPhishing
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates spearphishing with valid attributes" do
      attrs = %{
        name: "Test SpearPhishing",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, spearphishing} = Ash.create(SpearPhishing, attrs)
      assert spearphishing.name == "Test SpearPhishing"
      assert spearphishing.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(SpearPhishing, attrs)
    end
  end

  describe "read action" do
    test "reads existing spearphishing" do
      spearphishing = TestHelper.create_test_data(SpearPhishing)
      
      assert {:ok, found_spearphishing} = Ash.get(SpearPhishing, spearphishing.id)
      assert found_spearphishing.id == spearphishing.id
    end
    
    test "lists all spearphishings" do
      TestHelper.create_test_data(SpearPhishing, %{name: "SpearPhishing 1"})
      TestHelper.create_test_data(SpearPhishing, %{name: "SpearPhishing 2"})
      
      assert {:ok, spearphishings} = Ash.read(SpearPhishing)
      assert length(spearphishings) >= 2
    end
    
    test "filters by status" do
      active_spearphishing = TestHelper.create_test_data(SpearPhishing, %{status: :active})
      _inactive_spearphishing = TestHelper.create_test_data(SpearPhishing, %{status: :inactive})
      
      assert {:ok, [spearphishing]} = Ash.read(SpearPhishing, action: :by_status, status: :active)
      assert spearphishing.id == active_spearphishing.id
    end
  end

  describe "update action" do
    test "updates spearphishing attributes" do
      spearphishing = TestHelper.create_test_data(SpearPhishing)
      
      assert {:ok, updated_spearphishing} = Ash.update(spearphishing, %{name: "Updated Name"})
      assert updated_spearphishing.name == "Updated Name"
    end
    
    test "activates spearphishing" do
      spearphishing = TestHelper.create_test_data(SpearPhishing, %{status: :inactive})
      
      assert {:ok, activated_spearphishing} = Ash.update(spearphishing, action: :activate)
      assert activated_spearphishing.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing spearphishing" do
      spearphishing = TestHelper.create_test_data(SpearPhishing)
      
      assert :ok = Ash.destroy(spearphishing)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(SpearPhishing, spearphishing.id)
    end
  end
end
