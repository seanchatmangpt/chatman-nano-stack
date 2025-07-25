defmodule Cybersecurity.Resources.NetworkSegmentTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.NetworkSegment
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates networksegment with valid attributes" do
      attrs = %{
        name: "Test NetworkSegment",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, networksegment} = Ash.create(NetworkSegment, attrs)
      assert networksegment.name == "Test NetworkSegment"
      assert networksegment.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(NetworkSegment, attrs)
    end
  end

  describe "read action" do
    test "reads existing networksegment" do
      networksegment = TestHelper.create_test_data(NetworkSegment)
      
      assert {:ok, found_networksegment} = Ash.get(NetworkSegment, networksegment.id)
      assert found_networksegment.id == networksegment.id
    end
    
    test "lists all networksegments" do
      TestHelper.create_test_data(NetworkSegment, %{name: "NetworkSegment 1"})
      TestHelper.create_test_data(NetworkSegment, %{name: "NetworkSegment 2"})
      
      assert {:ok, networksegments} = Ash.read(NetworkSegment)
      assert length(networksegments) >= 2
    end
    
    test "filters by status" do
      active_networksegment = TestHelper.create_test_data(NetworkSegment, %{status: :active})
      _inactive_networksegment = TestHelper.create_test_data(NetworkSegment, %{status: :inactive})
      
      assert {:ok, [networksegment]} = Ash.read(NetworkSegment, action: :by_status, status: :active)
      assert networksegment.id == active_networksegment.id
    end
  end

  describe "update action" do
    test "updates networksegment attributes" do
      networksegment = TestHelper.create_test_data(NetworkSegment)
      
      assert {:ok, updated_networksegment} = Ash.update(networksegment, %{name: "Updated Name"})
      assert updated_networksegment.name == "Updated Name"
    end
    
    test "activates networksegment" do
      networksegment = TestHelper.create_test_data(NetworkSegment, %{status: :inactive})
      
      assert {:ok, activated_networksegment} = Ash.update(networksegment, action: :activate)
      assert activated_networksegment.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing networksegment" do
      networksegment = TestHelper.create_test_data(NetworkSegment)
      
      assert :ok = Ash.destroy(networksegment)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(NetworkSegment, networksegment.id)
    end
  end
end
