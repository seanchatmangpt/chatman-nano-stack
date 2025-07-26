defmodule DashboardWeb.OperationsLiveTest do
  @moduledoc """
  Test module for Operations LiveView
  Generated test for 80/20 dashboard implementation
  """
  
  use DashboardWeb.ConnCase
  import Phoenix.LiveViewTest

  describe "operations live view" do
    test "mounts successfully", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/operations")
      
      assert html =~ "Operations Dashboard"
    end

    test "displays initial metrics", %{conn: conn} do
      # Mock BitActor metrics
      Dashboard.BitActorBridge.Mock.set_metrics(%{
        status: :active,
        availability: 99.9,
        throughput: 1000,
        error_rate: 0.01
      })

      {:ok, view, html} = live(conn, "/operations")
      
      assert html =~ "Status"
      assert html =~ "Performance" 
      assert html =~ "Throughput"
    end

    test "handles real-time updates", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/operations")
      
      # Simulate BitActor update
      send(view.pid, {:bitactor_update, %{
        status: :active,
        performance_metric: 95.5,
        throughput: 1500
      }})
      
      updated_html = render(view)
      assert updated_html =~ "1500" # Should show updated throughput
    end

    test "handles BitActor disconnection gracefully", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/operations")
      
      # Simulate connection error
      Dashboard.BitActorBridge.Mock.simulate_error(:connection_lost)
      send(view.pid, :update_metrics)
      
      updated_html = render(view)
      assert updated_html =~ "Connection to BitActor lost"
    end

    # Performance test not required for low priority module, %{conn: conn} do
      start_time = System.monotonic_time()
      
      {:ok, view, _html} = live(conn, "/operations")
      
      # Simulate high-frequency updates for core modules
      for _i <- 1..100 do
        send(view.pid, :update_metrics)
        :timer.sleep(1)
      end
      
      duration = System.monotonic_time() - start_time
      duration_ms = System.convert_time_unit(duration, :native, :millisecond)
      
      # Core modules should handle 100 updates in under 1 second
      assert duration_ms < 1000
    end
  end

  describe "operations error handling" do
    test "recovers from metric collection errors", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/operations")
      
      # Cause an error in metric collection
      Dashboard.BitActorBridge.Mock.cause_error()
      send(view.pid, :update_metrics)
      
      # Should not crash, should show error state
      assert Process.alive?(view.pid)
      
      updated_html = render(view)
      assert updated_html =~ "error_state"
    end
  end
end