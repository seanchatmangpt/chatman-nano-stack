defmodule Cybersecurity.Resources.IDSTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.IDS
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates ids with valid attributes" do
      attrs = %{
        name: "Test IDS",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, ids} = Ash.create(IDS, attrs)
      assert ids.name == "Test IDS"
      assert ids.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(IDS, attrs)
    end
  end

  describe "read action" do
    test "reads existing ids" do
      ids = TestHelper.create_test_data(IDS)
      
      assert {:ok, found_ids} = Ash.get(IDS, ids.id)
      assert found_ids.id == ids.id
    end
    
    test "lists all idss" do
      TestHelper.create_test_data(IDS, %{name: "IDS 1"})
      TestHelper.create_test_data(IDS, %{name: "IDS 2"})
      
      assert {:ok, idss} = Ash.read(IDS)
      assert length(idss) >= 2
    end
    
    test "filters by status" do
      active_ids = TestHelper.create_test_data(IDS, %{status: :active})
      _inactive_ids = TestHelper.create_test_data(IDS, %{status: :inactive})
      
      assert {:ok, [ids]} = Ash.read(IDS, action: :by_status, status: :active)
      assert ids.id == active_ids.id
    end
  end

  describe "update action" do
    test "updates ids attributes" do
      ids = TestHelper.create_test_data(IDS)
      
      assert {:ok, updated_ids} = Ash.update(ids, %{name: "Updated Name"})
      assert updated_ids.name == "Updated Name"
    end
    
    test "activates ids" do
      ids = TestHelper.create_test_data(IDS, %{status: :inactive})
      
      assert {:ok, activated_ids} = Ash.update(ids, action: :activate)
      assert activated_ids.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ids" do
      ids = TestHelper.create_test_data(IDS)
      
      assert :ok = Ash.destroy(ids)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(IDS, ids.id)
    end
  end
end
