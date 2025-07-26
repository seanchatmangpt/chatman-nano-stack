defmodule Cybersecurity.Resources.CSRFTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.CSRF
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates csrf with valid attributes" do
      attrs = %{
        name: "Test CSRF",
        description: "Test description",
        status: :active
      }
      
      CSRF.init_storage()
      assert {:ok, csrf} = CSRF.create(attrs)
      assert csrf.name == "Test CSRF"
      assert csrf.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = CSRF.create(CSRF, attrs)
    end
  end

  describe "read action" do
    test "reads existing csrf" do
      csrf = TestHelper.create_test_data(CSRF)
      
      assert {:ok, found_csrf} = CSRF.get(CSRF, csrf.id)
      assert found_csrf.id == csrf.id
    end
    
    test "lists all csrfs" do
      TestHelper.create_test_data(CSRF, %{name: "CSRF 1"})
      TestHelper.create_test_data(CSRF, %{name: "CSRF 2"})
      
      assert {:ok, csrfs} = CSRF.list(CSRF)
      assert length(csrfs) >= 2
    end
    
    test "filters by status" do
      active_csrf = TestHelper.create_test_data(CSRF, %{status: :active})
      _inactive_csrf = TestHelper.create_test_data(CSRF, %{status: :inactive})
      
      assert {:ok, [csrf]} = CSRF.list(CSRF, action: :by_status, status: :active)
      assert csrf.id == active_csrf.id
    end
  end

  describe "update action" do
    test "updates csrf attributes" do
      csrf = TestHelper.create_test_data(CSRF)
      
      assert {:ok, updated_csrf} = CSRF.update(csrf, %{name: "Updated Name"})
      assert updated_csrf.name == "Updated Name"
    end
    
    test "activates csrf" do
      csrf = TestHelper.create_test_data(CSRF, %{status: :inactive})
      
      assert {:ok, activated_csrf} = CSRF.update(csrf, action: :activate)
      assert activated_csrf.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing csrf" do
      csrf = TestHelper.create_test_data(CSRF)
      
      assert :ok = CSRF.delete(csrf)
      assert {:error, %Ash.Error.Invalid{}} = CSRF.get(CSRF, csrf.id)
    end
  end
end
