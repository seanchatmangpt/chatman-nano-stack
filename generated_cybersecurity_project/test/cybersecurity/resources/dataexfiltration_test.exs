defmodule Cybersecurity.Resources.DataExfiltrationTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.DataExfiltration
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates dataexfiltration with valid attributes" do
      attrs = %{
        name: "Test DataExfiltration",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, dataexfiltration} = Ash.create(DataExfiltration, attrs)
      assert dataexfiltration.name == "Test DataExfiltration"
      assert dataexfiltration.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(DataExfiltration, attrs)
    end
  end

  describe "read action" do
    test "reads existing dataexfiltration" do
      dataexfiltration = TestHelper.create_test_data(DataExfiltration)
      
      assert {:ok, found_dataexfiltration} = Ash.get(DataExfiltration, dataexfiltration.id)
      assert found_dataexfiltration.id == dataexfiltration.id
    end
    
    test "lists all dataexfiltrations" do
      TestHelper.create_test_data(DataExfiltration, %{name: "DataExfiltration 1"})
      TestHelper.create_test_data(DataExfiltration, %{name: "DataExfiltration 2"})
      
      assert {:ok, dataexfiltrations} = Ash.read(DataExfiltration)
      assert length(dataexfiltrations) >= 2
    end
    
    test "filters by status" do
      active_dataexfiltration = TestHelper.create_test_data(DataExfiltration, %{status: :active})
      _inactive_dataexfiltration = TestHelper.create_test_data(DataExfiltration, %{status: :inactive})
      
      assert {:ok, [dataexfiltration]} = Ash.read(DataExfiltration, action: :by_status, status: :active)
      assert dataexfiltration.id == active_dataexfiltration.id
    end
  end

  describe "update action" do
    test "updates dataexfiltration attributes" do
      dataexfiltration = TestHelper.create_test_data(DataExfiltration)
      
      assert {:ok, updated_dataexfiltration} = Ash.update(dataexfiltration, %{name: "Updated Name"})
      assert updated_dataexfiltration.name == "Updated Name"
    end
    
    test "activates dataexfiltration" do
      dataexfiltration = TestHelper.create_test_data(DataExfiltration, %{status: :inactive})
      
      assert {:ok, activated_dataexfiltration} = Ash.update(dataexfiltration, action: :activate)
      assert activated_dataexfiltration.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing dataexfiltration" do
      dataexfiltration = TestHelper.create_test_data(DataExfiltration)
      
      assert :ok = Ash.destroy(dataexfiltration)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(DataExfiltration, dataexfiltration.id)
    end
  end
end
