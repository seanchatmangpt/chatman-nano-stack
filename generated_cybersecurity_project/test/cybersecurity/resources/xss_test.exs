defmodule Cybersecurity.Resources.XSSTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.XSS
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates xss with valid attributes" do
      attrs = %{
        name: "Test XSS",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, xss} = Ash.create(XSS, attrs)
      assert xss.name == "Test XSS"
      assert xss.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(XSS, attrs)
    end
  end

  describe "read action" do
    test "reads existing xss" do
      xss = TestHelper.create_test_data(XSS)
      
      assert {:ok, found_xss} = Ash.get(XSS, xss.id)
      assert found_xss.id == xss.id
    end
    
    test "lists all xsss" do
      TestHelper.create_test_data(XSS, %{name: "XSS 1"})
      TestHelper.create_test_data(XSS, %{name: "XSS 2"})
      
      assert {:ok, xsss} = Ash.read(XSS)
      assert length(xsss) >= 2
    end
    
    test "filters by status" do
      active_xss = TestHelper.create_test_data(XSS, %{status: :active})
      _inactive_xss = TestHelper.create_test_data(XSS, %{status: :inactive})
      
      assert {:ok, [xss]} = Ash.read(XSS, action: :by_status, status: :active)
      assert xss.id == active_xss.id
    end
  end

  describe "update action" do
    test "updates xss attributes" do
      xss = TestHelper.create_test_data(XSS)
      
      assert {:ok, updated_xss} = Ash.update(xss, %{name: "Updated Name"})
      assert updated_xss.name == "Updated Name"
    end
    
    test "activates xss" do
      xss = TestHelper.create_test_data(XSS, %{status: :inactive})
      
      assert {:ok, activated_xss} = Ash.update(xss, action: :activate)
      assert activated_xss.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing xss" do
      xss = TestHelper.create_test_data(XSS)
      
      assert :ok = Ash.destroy(xss)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(XSS, xss.id)
    end
  end
end
