defmodule Cybersecurity.Resources.RouterTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Router
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates router with valid attributes" do
      attrs = %{
        name: "Test Router",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, router} = Ash.create(Router, attrs)
      assert router.name == "Test Router"
      assert router.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(Router, attrs)
    end
  end

  describe "read action" do
    test "reads existing router" do
      router = TestHelper.create_test_data(Router)
      
      assert {:ok, found_router} = Ash.get(Router, router.id)
      assert found_router.id == router.id
    end
    
    test "lists all routers" do
      TestHelper.create_test_data(Router, %{name: "Router 1"})
      TestHelper.create_test_data(Router, %{name: "Router 2"})
      
      assert {:ok, routers} = Ash.read(Router)
      assert length(routers) >= 2
    end
    
    test "filters by status" do
      active_router = TestHelper.create_test_data(Router, %{status: :active})
      _inactive_router = TestHelper.create_test_data(Router, %{status: :inactive})
      
      assert {:ok, [router]} = Ash.read(Router, action: :by_status, status: :active)
      assert router.id == active_router.id
    end
  end

  describe "update action" do
    test "updates router attributes" do
      router = TestHelper.create_test_data(Router)
      
      assert {:ok, updated_router} = Ash.update(router, %{name: "Updated Name"})
      assert updated_router.name == "Updated Name"
    end
    
    test "activates router" do
      router = TestHelper.create_test_data(Router, %{status: :inactive})
      
      assert {:ok, activated_router} = Ash.update(router, action: :activate)
      assert activated_router.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing router" do
      router = TestHelper.create_test_data(Router)
      
      assert :ok = Ash.destroy(router)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Router, router.id)
    end
  end
end
