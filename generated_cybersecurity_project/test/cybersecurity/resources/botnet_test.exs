defmodule Cybersecurity.Resources.BotnetTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Botnet
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates botnet with valid attributes" do
      attrs = %{
        name: "Test Botnet",
        description: "Test description",
        status: :active
      }
      
      Botnet.init_storage()
      assert {:ok, botnet} = Botnet.create(attrs)
      assert botnet.name == "Test Botnet"
      assert botnet.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Botnet.create(Botnet, attrs)
    end
  end

  describe "read action" do
    test "reads existing botnet" do
      botnet = TestHelper.create_test_data(Botnet)
      
      assert {:ok, found_botnet} = Botnet.get(Botnet, botnet.id)
      assert found_botnet.id == botnet.id
    end
    
    test "lists all botnets" do
      TestHelper.create_test_data(Botnet, %{name: "Botnet 1"})
      TestHelper.create_test_data(Botnet, %{name: "Botnet 2"})
      
      assert {:ok, botnets} = Botnet.list(Botnet)
      assert length(botnets) >= 2
    end
    
    test "filters by status" do
      active_botnet = TestHelper.create_test_data(Botnet, %{status: :active})
      _inactive_botnet = TestHelper.create_test_data(Botnet, %{status: :inactive})
      
      assert {:ok, [botnet]} = Botnet.list(Botnet, action: :by_status, status: :active)
      assert botnet.id == active_botnet.id
    end
  end

  describe "update action" do
    test "updates botnet attributes" do
      botnet = TestHelper.create_test_data(Botnet)
      
      assert {:ok, updated_botnet} = Botnet.update(botnet, %{name: "Updated Name"})
      assert updated_botnet.name == "Updated Name"
    end
    
    test "activates botnet" do
      botnet = TestHelper.create_test_data(Botnet, %{status: :inactive})
      
      assert {:ok, activated_botnet} = Botnet.update(botnet, action: :activate)
      assert activated_botnet.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing botnet" do
      botnet = TestHelper.create_test_data(Botnet)
      
      assert :ok = Botnet.delete(botnet)
      assert {:error, %Ash.Error.Invalid{}} = Botnet.get(Botnet, botnet.id)
    end
  end
end
