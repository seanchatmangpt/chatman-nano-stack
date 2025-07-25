defmodule Cybersecurity.Resources.SIEMTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SIEM
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates siem with valid attributes" do
      attrs = %{
        name: "Test SIEM",
        description: "Test description",
        status: :active
      }
      
      SIEM.init_storage()
      assert {:ok, siem} = SIEM.create(attrs)
      assert siem.name == "Test SIEM"
      assert siem.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SIEM.create(SIEM, attrs)
    end
  end

  describe "read action" do
    test "reads existing siem" do
      siem = TestHelper.create_test_data(SIEM)
      
      assert {:ok, found_siem} = SIEM.get(SIEM, siem.id)
      assert found_siem.id == siem.id
    end
    
    test "lists all siems" do
      TestHelper.create_test_data(SIEM, %{name: "SIEM 1"})
      TestHelper.create_test_data(SIEM, %{name: "SIEM 2"})
      
      assert {:ok, siems} = SIEM.list(SIEM)
      assert length(siems) >= 2
    end
    
    test "filters by status" do
      active_siem = TestHelper.create_test_data(SIEM, %{status: :active})
      _inactive_siem = TestHelper.create_test_data(SIEM, %{status: :inactive})
      
      assert {:ok, [siem]} = SIEM.list(SIEM, action: :by_status, status: :active)
      assert siem.id == active_siem.id
    end
  end

  describe "update action" do
    test "updates siem attributes" do
      siem = TestHelper.create_test_data(SIEM)
      
      assert {:ok, updated_siem} = SIEM.update(siem, %{name: "Updated Name"})
      assert updated_siem.name == "Updated Name"
    end
    
    test "activates siem" do
      siem = TestHelper.create_test_data(SIEM, %{status: :inactive})
      
      assert {:ok, activated_siem} = SIEM.update(siem, action: :activate)
      assert activated_siem.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing siem" do
      siem = TestHelper.create_test_data(SIEM)
      
      assert :ok = SIEM.delete(siem)
      assert {:error, %Ash.Error.Invalid{}} = SIEM.get(SIEM, siem.id)
    end
  end
end
