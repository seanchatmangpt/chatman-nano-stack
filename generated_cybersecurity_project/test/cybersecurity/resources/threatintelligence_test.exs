defmodule Cybersecurity.Resources.ThreatIntelligenceTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Resources.ThreatIntelligence
  alias Cybersecurity.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates threatintelligence with valid attributes" do
      attrs = %{
        name: "Test ThreatIntelligence",
        description: "Test description",
        status: :active
      }
      
      ThreatIntelligence.init_storage()
      assert {:ok, threatintelligence} = ThreatIntelligence.create(attrs)
      assert threatintelligence.name == "Test ThreatIntelligence"
      assert threatintelligence.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = ThreatIntelligence.create(ThreatIntelligence, attrs)
    end
  end

  describe "read action" do
    test "reads existing threatintelligence" do
      threatintelligence = TestHelper.create_test_data(ThreatIntelligence)
      
      assert {:ok, found_threatintelligence} = ThreatIntelligence.get(ThreatIntelligence, threatintelligence.id)
      assert found_threatintelligence.id == threatintelligence.id
    end
    
    test "lists all threatintelligences" do
      TestHelper.create_test_data(ThreatIntelligence, %{name: "ThreatIntelligence 1"})
      TestHelper.create_test_data(ThreatIntelligence, %{name: "ThreatIntelligence 2"})
      
      assert {:ok, threatintelligences} = ThreatIntelligence.list(ThreatIntelligence)
      assert length(threatintelligences) >= 2
    end
    
    test "filters by status" do
      active_threatintelligence = TestHelper.create_test_data(ThreatIntelligence, %{status: :active})
      _inactive_threatintelligence = TestHelper.create_test_data(ThreatIntelligence, %{status: :inactive})
      
      assert {:ok, [threatintelligence]} = ThreatIntelligence.list(ThreatIntelligence, action: :by_status, status: :active)
      assert threatintelligence.id == active_threatintelligence.id
    end
  end

  describe "update action" do
    test "updates threatintelligence attributes" do
      threatintelligence = TestHelper.create_test_data(ThreatIntelligence)
      
      assert {:ok, updated_threatintelligence} = ThreatIntelligence.update(threatintelligence, %{name: "Updated Name"})
      assert updated_threatintelligence.name == "Updated Name"
    end
    
    test "activates threatintelligence" do
      threatintelligence = TestHelper.create_test_data(ThreatIntelligence, %{status: :inactive})
      
      assert {:ok, activated_threatintelligence} = ThreatIntelligence.update(threatintelligence, action: :activate)
      assert activated_threatintelligence.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing threatintelligence" do
      threatintelligence = TestHelper.create_test_data(ThreatIntelligence)
      
      assert :ok = ThreatIntelligence.delete(threatintelligence)
      assert {:error, %Ash.Error.Invalid{}} = ThreatIntelligence.get(ThreatIntelligence, threatintelligence.id)
    end
  end
end
