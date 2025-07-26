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
      
      Router.init_storage()
      assert {:ok, router} = Router.create(attrs)
      assert router.name == "Test Router"
      assert router.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Router.create(Router, attrs)
    end
  end

  describe "read action" do
    test "reads existing router" do
      router = TestHelper.create_test_data(Router)
      
      assert {:ok, found_router} = Router.get(Router, router.id)
      assert found_router.id == router.id
    end
    
    test "lists all routers" do
      TestHelper.create_test_data(Router, %{name: "Router 1"})
      TestHelper.create_test_data(Router, %{name: "Router 2"})
      
      assert {:ok, routers} = Router.list(Router)
      assert length(routers) >= 2
    end
    
    test "filters by status" do
      active_router = TestHelper.create_test_data(Router, %{status: :active})
      _inactive_router = TestHelper.create_test_data(Router, %{status: :inactive})
      
      assert {:ok, [router]} = Router.list(Router, action: :by_status, status: :active)
      assert router.id == active_router.id
    end
  end

  describe "update action" do
    test "updates router attributes" do
      router = TestHelper.create_test_data(Router)
      
      assert {:ok, updated_router} = Router.update(router, %{name: "Updated Name"})
      assert updated_router.name == "Updated Name"
    end
    
    test "activates router" do
      router = TestHelper.create_test_data(Router, %{status: :inactive})
      
      assert {:ok, activated_router} = Router.update(router, action: :activate)
      assert activated_router.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing router" do
      router = TestHelper.create_test_data(Router)
      
      assert :ok = Router.delete(router)
      assert {:error, %Ash.Error.Invalid{}} = Router.get(Router, router.id)
    end
  end
end
