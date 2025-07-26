defmodule Ultrathink.Resources.SignalTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Resources.Signal
  alias Ultrathink.TestHelper

  setup do
    TestHelper.start_sandbox()
    on_exit(&TestHelper.stop_sandbox/0)
  end

  describe "create action" do
    test "creates signal with valid attributes" do
      attrs = %{
        name: "Test Signal",
        description: "Test description",
        status: :active
      }
      
      assert {:ok, signal} = Ash.create(Signal, attrs)
      assert signal.name == "Test Signal"
      assert signal.status == :active
    end
    
    test "fails with invalid attributes" do
      attrs = %{description: "Missing name"}
      
      assert {:error, %Ash.Error.Invalid{}} = Ash.create(Signal, attrs)
    end
  end

  describe "read action" do
    test "reads existing signal" do
      signal = TestHelper.create_test_data(Signal)
      
      assert {:ok, found_signal} = Ash.get(Signal, signal.id)
      assert found_signal.id == signal.id
    end
    
    test "lists all signals" do
      TestHelper.create_test_data(Signal, %{name: "Signal 1"})
      TestHelper.create_test_data(Signal, %{name: "Signal 2"})
      
      assert {:ok, signals} = Ash.read(Signal)
      assert length(signals) >= 2
    end
    
    test "filters by status" do
      active_signal = TestHelper.create_test_data(Signal, %{status: :active})
      _inactive_signal = TestHelper.create_test_data(Signal, %{status: :inactive})
      
      assert {:ok, [signal]} = Ash.read(Signal, action: :by_status, status: :active)
      assert signal.id == active_signal.id
    end
  end

  describe "update action" do
    test "updates signal attributes" do
      signal = TestHelper.create_test_data(Signal)
      
      assert {:ok, updated_signal} = Ash.update(signal, %{name: "Updated Name"})
      assert updated_signal.name == "Updated Name"
    end
    
    test "activates signal" do
      signal = TestHelper.create_test_data(Signal, %{status: :inactive})
      
      assert {:ok, activated_signal} = Ash.update(signal, action: :activate)
      assert activated_signal.status == :active
    end
  end

  describe "destroy action" do
    test "destroys existing signal" do
      signal = TestHelper.create_test_data(Signal)
      
      assert :ok = Ash.destroy(signal)
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Signal, signal.id)
    end
  end
end
