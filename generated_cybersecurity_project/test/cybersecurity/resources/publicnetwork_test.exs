defmodule Cybersecurity.Resources.PublicNetworkTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PublicNetwork
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates publicnetwork with valid attributes" do
      attrs = %{
        name: "Test PublicNetwork",
        description: "Test description",
        status: :active
      }
      
      PublicNetwork.init_storage()
      assert {:ok, publicnetwork} = PublicNetwork.create(attrs)
      assert publicnetwork.name == "Test PublicNetwork"
      assert publicnetwork.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = PublicNetwork.create(PublicNetwork, attrs)
    end
  end

  describe "read action" do
    test "reads existing publicnetwork" do
      publicnetwork = TestHelper.create_test_data(PublicNetwork)
      
      assert {:ok, found_publicnetwork} = PublicNetwork.get(PublicNetwork, publicnetwork.id)
      assert found_publicnetwork.id == publicnetwork.id
    end
    
    test "lists all publicnetworks" do
      TestHelper.create_test_data(PublicNetwork, %{name: "PublicNetwork 1"})
      TestHelper.create_test_data(PublicNetwork, %{name: "PublicNetwork 2"})
      
      assert {:ok, publicnetworks} = PublicNetwork.list(PublicNetwork)
      assert length(publicnetworks) >= 2
    end
    
    test "filters by status" do
      active_publicnetwork = TestHelper.create_test_data(PublicNetwork, %{status: :active})
      _inactive_publicnetwork = TestHelper.create_test_data(PublicNetwork, %{status: :inactive})
      
      assert {:ok, [publicnetwork]} = PublicNetwork.list(PublicNetwork, action: :by_status, status: :active)
      assert publicnetwork.id == active_publicnetwork.id
    end
  end

  describe "update action" do
    test "updates publicnetwork attributes" do
      publicnetwork = TestHelper.create_test_data(PublicNetwork)
      
      assert {:ok, updated_publicnetwork} = PublicNetwork.update(publicnetwork, %{name: "Updated Name"})
      assert updated_publicnetwork.name == "Updated Name"
    end
    
    test "activates publicnetwork" do
      publicnetwork = TestHelper.create_test_data(PublicNetwork, %{status: :inactive})
      
      assert {:ok, activated_publicnetwork} = PublicNetwork.update(publicnetwork, action: :activate)
      assert activated_publicnetwork.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing publicnetwork" do
      publicnetwork = TestHelper.create_test_data(PublicNetwork)
      
      assert :ok = PublicNetwork.delete(publicnetwork)
      assert {:error, %Ash.Error.Invalid{}} = PublicNetwork.get(PublicNetwork, publicnetwork.id)
    end
  end
end
