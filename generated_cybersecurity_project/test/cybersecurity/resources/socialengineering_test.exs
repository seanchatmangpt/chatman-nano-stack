defmodule Cybersecurity.Resources.SocialEngineeringTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.SocialEngineering
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates socialengineering with valid attributes" do
      attrs = %{
        name: "Test SocialEngineering",
        description: "Test description",
        status: :active
      }
      
      SocialEngineering.init_storage()
      assert {:ok, socialengineering} = SocialEngineering.create(attrs)
      assert socialengineering.name == "Test SocialEngineering"
      assert socialengineering.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = SocialEngineering.create(SocialEngineering, attrs)
    end
  end

  describe "read action" do
    test "reads existing socialengineering" do
      socialengineering = TestHelper.create_test_data(SocialEngineering)
      
      assert {:ok, found_socialengineering} = SocialEngineering.get(SocialEngineering, socialengineering.id)
      assert found_socialengineering.id == socialengineering.id
    end
    
    test "lists all socialengineerings" do
      TestHelper.create_test_data(SocialEngineering, %{name: "SocialEngineering 1"})
      TestHelper.create_test_data(SocialEngineering, %{name: "SocialEngineering 2"})
      
      assert {:ok, socialengineerings} = SocialEngineering.list(SocialEngineering)
      assert length(socialengineerings) >= 2
    end
    
    test "filters by status" do
      active_socialengineering = TestHelper.create_test_data(SocialEngineering, %{status: :active})
      _inactive_socialengineering = TestHelper.create_test_data(SocialEngineering, %{status: :inactive})
      
      assert {:ok, [socialengineering]} = SocialEngineering.list(SocialEngineering, action: :by_status, status: :active)
      assert socialengineering.id == active_socialengineering.id
    end
  end

  describe "update action" do
    test "updates socialengineering attributes" do
      socialengineering = TestHelper.create_test_data(SocialEngineering)
      
      assert {:ok, updated_socialengineering} = SocialEngineering.update(socialengineering, %{name: "Updated Name"})
      assert updated_socialengineering.name == "Updated Name"
    end
    
    test "activates socialengineering" do
      socialengineering = TestHelper.create_test_data(SocialEngineering, %{status: :inactive})
      
      assert {:ok, activated_socialengineering} = SocialEngineering.update(socialengineering, action: :activate)
      assert activated_socialengineering.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing socialengineering" do
      socialengineering = TestHelper.create_test_data(SocialEngineering)
      
      assert :ok = SocialEngineering.delete(socialengineering)
      assert {:error, %Ash.Error.Invalid{}} = SocialEngineering.get(SocialEngineering, socialengineering.id)
    end
  end
end
