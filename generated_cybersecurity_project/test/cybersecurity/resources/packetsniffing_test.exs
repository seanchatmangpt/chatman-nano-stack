defmodule Cybersecurity.Resources.PacketSniffingTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.PacketSniffing
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates packetsniffing with valid attributes" do
      attrs = %{
        name: "Test PacketSniffing",
        description: "Test description",
        status: :active
      }
      
      PacketSniffing.init_storage()
      assert {:ok, packetsniffing} = PacketSniffing.create(attrs)
      assert packetsniffing.name == "Test PacketSniffing"
      assert packetsniffing.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = PacketSniffing.create(PacketSniffing, attrs)
    end
  end

  describe "read action" do
    test "reads existing packetsniffing" do
      packetsniffing = TestHelper.create_test_data(PacketSniffing)
      
      assert {:ok, found_packetsniffing} = PacketSniffing.get(PacketSniffing, packetsniffing.id)
      assert found_packetsniffing.id == packetsniffing.id
    end
    
    test "lists all packetsniffings" do
      TestHelper.create_test_data(PacketSniffing, %{name: "PacketSniffing 1"})
      TestHelper.create_test_data(PacketSniffing, %{name: "PacketSniffing 2"})
      
      assert {:ok, packetsniffings} = PacketSniffing.list(PacketSniffing)
      assert length(packetsniffings) >= 2
    end
    
    test "filters by status" do
      active_packetsniffing = TestHelper.create_test_data(PacketSniffing, %{status: :active})
      _inactive_packetsniffing = TestHelper.create_test_data(PacketSniffing, %{status: :inactive})
      
      assert {:ok, [packetsniffing]} = PacketSniffing.list(PacketSniffing, action: :by_status, status: :active)
      assert packetsniffing.id == active_packetsniffing.id
    end
  end

  describe "update action" do
    test "updates packetsniffing attributes" do
      packetsniffing = TestHelper.create_test_data(PacketSniffing)
      
      assert {:ok, updated_packetsniffing} = PacketSniffing.update(packetsniffing, %{name: "Updated Name"})
      assert updated_packetsniffing.name == "Updated Name"
    end
    
    test "activates packetsniffing" do
      packetsniffing = TestHelper.create_test_data(PacketSniffing, %{status: :inactive})
      
      assert {:ok, activated_packetsniffing} = PacketSniffing.update(packetsniffing, action: :activate)
      assert activated_packetsniffing.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing packetsniffing" do
      packetsniffing = TestHelper.create_test_data(PacketSniffing)
      
      assert :ok = PacketSniffing.delete(packetsniffing)
      assert {:error, %Ash.Error.Invalid{}} = PacketSniffing.get(PacketSniffing, packetsniffing.id)
    end
  end
end
