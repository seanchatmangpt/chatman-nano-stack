defmodule Cybersecurity.Resources.EDRTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.EDR
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates edr with valid attributes" do
      attrs = %{
        name: "Test EDR",
        description: "Test description",
        status: :active
      }
      
      EDR.init_storage()
      assert {:ok, edr} = EDR.create(attrs)
      assert edr.name == "Test EDR"
      assert edr.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = EDR.create(EDR, attrs)
    end
  end

  describe "read action" do
    test "reads existing edr" do
      edr = TestHelper.create_test_data(EDR)
      
      assert {:ok, found_edr} = EDR.get(EDR, edr.id)
      assert found_edr.id == edr.id
    end
    
    test "lists all edrs" do
      TestHelper.create_test_data(EDR, %{name: "EDR 1"})
      TestHelper.create_test_data(EDR, %{name: "EDR 2"})
      
      assert {:ok, edrs} = EDR.list(EDR)
      assert length(edrs) >= 2
    end
    
    test "filters by status" do
      active_edr = TestHelper.create_test_data(EDR, %{status: :active})
      _inactive_edr = TestHelper.create_test_data(EDR, %{status: :inactive})
      
      assert {:ok, [edr]} = EDR.list(EDR, action: :by_status, status: :active)
      assert edr.id == active_edr.id
    end
  end

  describe "update action" do
    test "updates edr attributes" do
      edr = TestHelper.create_test_data(EDR)
      
      assert {:ok, updated_edr} = EDR.update(edr, %{name: "Updated Name"})
      assert updated_edr.name == "Updated Name"
    end
    
    test "activates edr" do
      edr = TestHelper.create_test_data(EDR, %{status: :inactive})
      
      assert {:ok, activated_edr} = EDR.update(edr, action: :activate)
      assert activated_edr.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing edr" do
      edr = TestHelper.create_test_data(EDR)
      
      assert :ok = EDR.delete(edr)
      assert {:error, %Ash.Error.Invalid{}} = EDR.get(EDR, edr.id)
    end
  end
end
