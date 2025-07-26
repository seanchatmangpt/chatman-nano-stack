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
      
      XSS.init_storage()
      assert {:ok, xss} = XSS.create(attrs)
      assert xss.name == "Test XSS"
      assert xss.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = XSS.create(XSS, attrs)
    end
  end

  describe "read action" do
    test "reads existing xss" do
      xss = TestHelper.create_test_data(XSS)
      
      assert {:ok, found_xss} = XSS.get(XSS, xss.id)
      assert found_xss.id == xss.id
    end
    
    test "lists all xsss" do
      TestHelper.create_test_data(XSS, %{name: "XSS 1"})
      TestHelper.create_test_data(XSS, %{name: "XSS 2"})
      
      assert {:ok, xsss} = XSS.list(XSS)
      assert length(xsss) >= 2
    end
    
    test "filters by status" do
      active_xss = TestHelper.create_test_data(XSS, %{status: :active})
      _inactive_xss = TestHelper.create_test_data(XSS, %{status: :inactive})
      
      assert {:ok, [xss]} = XSS.list(XSS, action: :by_status, status: :active)
      assert xss.id == active_xss.id
    end
  end

  describe "update action" do
    test "updates xss attributes" do
      xss = TestHelper.create_test_data(XSS)
      
      assert {:ok, updated_xss} = XSS.update(xss, %{name: "Updated Name"})
      assert updated_xss.name == "Updated Name"
    end
    
    test "activates xss" do
      xss = TestHelper.create_test_data(XSS, %{status: :inactive})
      
      assert {:ok, activated_xss} = XSS.update(xss, action: :activate)
      assert activated_xss.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing xss" do
      xss = TestHelper.create_test_data(XSS)
      
      assert :ok = XSS.delete(xss)
      assert {:error, %Ash.Error.Invalid{}} = XSS.get(XSS, xss.id)
    end
  end
end
