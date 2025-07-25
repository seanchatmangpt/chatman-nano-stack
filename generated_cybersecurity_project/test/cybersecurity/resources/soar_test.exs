defmodule Cybersecurity.Resources.SOARTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SOAR
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates soar with valid attributes" do
      attrs = %{
        name: "Test SOAR",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, soar} = Ash.create(SOAR, attrs)
      assert soar.name == "Test SOAR"
      assert soar.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(SOAR, attrs)
    end
  end

  describe "read action" do
    test "reads existing soar" do
      soar = TestHelper.create_test_data(SOAR)
      
      assert {:ok, found_soar} = Ash.get(SOAR, soar.id)
      assert found_soar.id == soar.id
    end
    
    test "lists all soars" do
      TestHelper.create_test_data(SOAR, %{name: "SOAR 1"})
      TestHelper.create_test_data(SOAR, %{name: "SOAR 2"})
      
      assert {:ok, soars} = Ash.read(SOAR)
      assert length(soars) >= 2
    end
    
    test "filters by status" do
      active_soar = TestHelper.create_test_data(SOAR, %{status: :active})
      _inactive_soar = TestHelper.create_test_data(SOAR, %{status: :inactive})
      
      assert {:ok, [soar]} = Ash.read(SOAR, action: :by_status, status: :active)
      assert soar.id == active_soar.id
    end
  end

  describe "update action" do
    test "updates soar attributes" do
      soar = TestHelper.create_test_data(SOAR)
      
      assert {:ok, updated_soar} = Ash.update(soar, %{name: "Updated Name"})
      assert updated_soar.name == "Updated Name"
    end
    
    test "activates soar" do
      soar = TestHelper.create_test_data(SOAR, %{status: :inactive})
      
      assert {:ok, activated_soar} = Ash.update(soar, action: :activate)
      assert activated_soar.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing soar" do
      soar = TestHelper.create_test_data(SOAR)
      
      assert :ok = Ash.destroy(soar)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(SOAR, soar.id)
    end
  end
end
