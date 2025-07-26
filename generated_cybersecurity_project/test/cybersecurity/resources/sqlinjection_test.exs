defmodule Cybersecurity.Resources.SQLInjectionTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SQLInjection
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates sqlinjection with valid attributes" do
      attrs = %{
        name: "Test SQLInjection",
        description: "Test description",
        status: :active
      }
      
      SQLInjection.init_storage()
      assert {:ok, sqlinjection} = SQLInjection.create(attrs)
      assert sqlinjection.name == "Test SQLInjection"
      assert sqlinjection.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SQLInjection.create(SQLInjection, attrs)
    end
  end

  describe "read action" do
    test "reads existing sqlinjection" do
      sqlinjection = TestHelper.create_test_data(SQLInjection)
      
      assert {:ok, found_sqlinjection} = SQLInjection.get(SQLInjection, sqlinjection.id)
      assert found_sqlinjection.id == sqlinjection.id
    end
    
    test "lists all sqlinjections" do
      TestHelper.create_test_data(SQLInjection, %{name: "SQLInjection 1"})
      TestHelper.create_test_data(SQLInjection, %{name: "SQLInjection 2"})
      
      assert {:ok, sqlinjections} = SQLInjection.list(SQLInjection)
      assert length(sqlinjections) >= 2
    end
    
    test "filters by status" do
      active_sqlinjection = TestHelper.create_test_data(SQLInjection, %{status: :active})
      _inactive_sqlinjection = TestHelper.create_test_data(SQLInjection, %{status: :inactive})
      
      assert {:ok, [sqlinjection]} = SQLInjection.list(SQLInjection, action: :by_status, status: :active)
      assert sqlinjection.id == active_sqlinjection.id
    end
  end

  describe "update action" do
    test "updates sqlinjection attributes" do
      sqlinjection = TestHelper.create_test_data(SQLInjection)
      
      assert {:ok, updated_sqlinjection} = SQLInjection.update(sqlinjection, %{name: "Updated Name"})
      assert updated_sqlinjection.name == "Updated Name"
    end
    
    test "activates sqlinjection" do
      sqlinjection = TestHelper.create_test_data(SQLInjection, %{status: :inactive})
      
      assert {:ok, activated_sqlinjection} = SQLInjection.update(sqlinjection, action: :activate)
      assert activated_sqlinjection.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing sqlinjection" do
      sqlinjection = TestHelper.create_test_data(SQLInjection)
      
      assert :ok = SQLInjection.delete(sqlinjection)
      assert {:error, %Ash.Error.Invalid{}} = SQLInjection.get(SQLInjection, sqlinjection.id)
    end
  end
end
