defmodule Cybersecurity.Resources.RansomwareTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.Ransomware
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates ransomware with valid attributes" do
      attrs = %{
        name: "Test Ransomware",
        description: "Test description",
        status: :active
      }
      
      Ransomware.init_storage()
      assert {:ok, ransomware} = Ransomware.create(attrs)
      assert ransomware.name == "Test Ransomware"
      assert ransomware.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ransomware.create(Ransomware, attrs)
    end
  end

  describe "read action" do
    test "reads existing ransomware" do
      ransomware = TestHelper.create_test_data(Ransomware)
      
      assert {:ok, found_ransomware} = Ransomware.get(Ransomware, ransomware.id)
      assert found_ransomware.id == ransomware.id
    end
    
    test "lists all ransomwares" do
      TestHelper.create_test_data(Ransomware, %{name: "Ransomware 1"})
      TestHelper.create_test_data(Ransomware, %{name: "Ransomware 2"})
      
      assert {:ok, ransomwares} = Ransomware.list(Ransomware)
      assert length(ransomwares) >= 2
    end
    
    test "filters by status" do
      active_ransomware = TestHelper.create_test_data(Ransomware, %{status: :active})
      _inactive_ransomware = TestHelper.create_test_data(Ransomware, %{status: :inactive})
      
      assert {:ok, [ransomware]} = Ransomware.list(Ransomware, action: :by_status, status: :active)
      assert ransomware.id == active_ransomware.id
    end
  end

  describe "update action" do
    test "updates ransomware attributes" do
      ransomware = TestHelper.create_test_data(Ransomware)
      
      assert {:ok, updated_ransomware} = Ransomware.update(ransomware, %{name: "Updated Name"})
      assert updated_ransomware.name == "Updated Name"
    end
    
    test "activates ransomware" do
      ransomware = TestHelper.create_test_data(Ransomware, %{status: :inactive})
      
      assert {:ok, activated_ransomware} = Ransomware.update(ransomware, action: :activate)
      assert activated_ransomware.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing ransomware" do
      ransomware = TestHelper.create_test_data(Ransomware)
      
      assert :ok = Ransomware.delete(ransomware)
      assert {:error, %Ash.Error.Invalid{}} = Ransomware.get(Ransomware, ransomware.id)
    end
  end
end
