defmodule Ultrathink.Resources.CoordinationReactorTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Resources.CoordinationReactor
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates coordinationreactor with valid attributes" do
      attrs = %{
        name: "Test CoordinationReactor",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, coordinationreactor} = Ash.create(CoordinationReactor, attrs)
      assert coordinationreactor.name == "Test CoordinationReactor"
      assert coordinationreactor.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(CoordinationReactor, attrs)
    end
  end

  describe "read action" do
    test "reads existing coordinationreactor" do
      coordinationreactor = TestHelper.create_test_data(CoordinationReactor)
      
      assert {:ok, found_coordinationreactor} = Ash.get(CoordinationReactor, coordinationreactor.id)
      assert found_coordinationreactor.id == coordinationreactor.id
    end
    
    test "lists all coordinationreactors" do
      TestHelper.create_test_data(CoordinationReactor, %{name: "CoordinationReactor 1"})
      TestHelper.create_test_data(CoordinationReactor, %{name: "CoordinationReactor 2"})
      
      assert {:ok, coordinationreactors} = Ash.read(CoordinationReactor)
      assert length(coordinationreactors) >= 2
    end
    
    test "filters by status" do
      active_coordinationreactor = TestHelper.create_test_data(CoordinationReactor, %{status: :active})
      _inactive_coordinationreactor = TestHelper.create_test_data(CoordinationReactor, %{status: :inactive})
      
      assert {:ok, [coordinationreactor]} = Ash.read(CoordinationReactor, action: :by_status, status: :active)
      assert coordinationreactor.id == active_coordinationreactor.id
    end
  end

  describe "update action" do
    test "updates coordinationreactor attributes" do
      coordinationreactor = TestHelper.create_test_data(CoordinationReactor)
      
      assert {:ok, updated_coordinationreactor} = Ash.update(coordinationreactor, %{name: "Updated Name"})
      assert updated_coordinationreactor.name == "Updated Name"
    end
    
    test "activates coordinationreactor" do
      coordinationreactor = TestHelper.create_test_data(CoordinationReactor, %{status: :inactive})
      
      assert {:ok, activated_coordinationreactor} = Ash.update(coordinationreactor, action: :activate)
      assert activated_coordinationreactor.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing coordinationreactor" do
      coordinationreactor = TestHelper.create_test_data(CoordinationReactor)
      
      assert :ok = Ash.destroy(coordinationreactor)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(CoordinationReactor, coordinationreactor.id)
    end
  end
end
