defmodule Cybersecurity.Resources.WebAttackTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.WebAttack
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates webattack with valid attributes" do
      attrs = %{
        name: "Test WebAttack",
        description: "Test description",
        status: :active
      }
      
      WebAttack.init_storage()
      assert {:ok, webattack} = WebAttack.create(attrs)
      assert webattack.name == "Test WebAttack"
      assert webattack.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = WebAttack.create(WebAttack, attrs)
    end
  end

  describe "read action" do
    test "reads existing webattack" do
      webattack = TestHelper.create_test_data(WebAttack)
      
      assert {:ok, found_webattack} = WebAttack.get(WebAttack, webattack.id)
      assert found_webattack.id == webattack.id
    end
    
    test "lists all webattacks" do
      TestHelper.create_test_data(WebAttack, %{name: "WebAttack 1"})
      TestHelper.create_test_data(WebAttack, %{name: "WebAttack 2"})
      
      assert {:ok, webattacks} = WebAttack.list(WebAttack)
      assert length(webattacks) >= 2
    end
    
    test "filters by status" do
      active_webattack = TestHelper.create_test_data(WebAttack, %{status: :active})
      _inactive_webattack = TestHelper.create_test_data(WebAttack, %{status: :inactive})
      
      assert {:ok, [webattack]} = WebAttack.list(WebAttack, action: :by_status, status: :active)
      assert webattack.id == active_webattack.id
    end
  end

  describe "update action" do
    test "updates webattack attributes" do
      webattack = TestHelper.create_test_data(WebAttack)
      
      assert {:ok, updated_webattack} = WebAttack.update(webattack, %{name: "Updated Name"})
      assert updated_webattack.name == "Updated Name"
    end
    
    test "activates webattack" do
      webattack = TestHelper.create_test_data(WebAttack, %{status: :inactive})
      
      assert {:ok, activated_webattack} = WebAttack.update(webattack, action: :activate)
      assert activated_webattack.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing webattack" do
      webattack = TestHelper.create_test_data(WebAttack)
      
      assert :ok = WebAttack.delete(webattack)
      assert {:error, %Ash.Error.Invalid{}} = WebAttack.get(WebAttack, webattack.id)
    end
  end
end
