defmodule Cybersecurity.Resources.LoadBalancerTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.LoadBalancer
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates loadbalancer with valid attributes" do
      attrs = %{
        name: "Test LoadBalancer",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, loadbalancer} = Ash.create(LoadBalancer, attrs)
      assert loadbalancer.name == "Test LoadBalancer"
      assert loadbalancer.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(LoadBalancer, attrs)
    end
  end

  describe "read action" do
    test "reads existing loadbalancer" do
      loadbalancer = TestHelper.create_test_data(LoadBalancer)
      
      assert {:ok, found_loadbalancer} = Ash.get(LoadBalancer, loadbalancer.id)
      assert found_loadbalancer.id == loadbalancer.id
    end
    
    test "lists all loadbalancers" do
      TestHelper.create_test_data(LoadBalancer, %{name: "LoadBalancer 1"})
      TestHelper.create_test_data(LoadBalancer, %{name: "LoadBalancer 2"})
      
      assert {:ok, loadbalancers} = Ash.read(LoadBalancer)
      assert length(loadbalancers) >= 2
    end
    
    test "filters by status" do
      active_loadbalancer = TestHelper.create_test_data(LoadBalancer, %{status: :active})
      _inactive_loadbalancer = TestHelper.create_test_data(LoadBalancer, %{status: :inactive})
      
      assert {:ok, [loadbalancer]} = Ash.read(LoadBalancer, action: :by_status, status: :active)
      assert loadbalancer.id == active_loadbalancer.id
    end
  end

  describe "update action" do
    test "updates loadbalancer attributes" do
      loadbalancer = TestHelper.create_test_data(LoadBalancer)
      
      assert {:ok, updated_loadbalancer} = Ash.update(loadbalancer, %{name: "Updated Name"})
      assert updated_loadbalancer.name == "Updated Name"
    end
    
    test "activates loadbalancer" do
      loadbalancer = TestHelper.create_test_data(LoadBalancer, %{status: :inactive})
      
      assert {:ok, activated_loadbalancer} = Ash.update(loadbalancer, action: :activate)
      assert activated_loadbalancer.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing loadbalancer" do
      loadbalancer = TestHelper.create_test_data(LoadBalancer)
      
      assert :ok = Ash.destroy(loadbalancer)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(LoadBalancer, loadbalancer.id)
    end
  end
end
