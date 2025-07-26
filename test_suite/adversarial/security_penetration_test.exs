defmodule CnsWeb.SecurityPenetrationTest do
  @moduledoc """
  Adversarial Security Tests for Ultrathink Swarm 80/20 System
  Tests authentication, authorization, input validation, and security vulnerabilities
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  
  describe "authentication bypass attempts" do
    test "rejects unauthenticated channel connections" do
      {:ok, socket} = connect(UserSocket, %{})
      
      # Attempt to join protected channel without authentication
      assert {:error, %{reason: "Authentication required"}} = 
        subscribe_and_join(socket, SwarmChannel, "swarm:protected_operations")
    end
    
    test "rejects invalid authentication tokens" do
      {:ok, socket} = connect(UserSocket, %{
        auth_token: "invalid_token_12345",
        user_id: "malicious_user"
      })
      
      assert {:error, %{reason: "Invalid authentication"}} = 
        subscribe_and_join(socket, SwarmChannel, "swarm:secure_pipeline")
    end
    
    test "prevents privilege escalation through channel parameters" do
      {:ok, socket} = connect(UserSocket, %{
        current_user: %{id: "regular_user", role: "viewer"}
      })
      
      # Attempt to join with elevated privileges
      assert {:error, %{reason: "Insufficient permissions"}} = 
        subscribe_and_join(socket, SwarmChannel, "swarm:admin_operations", %{
          "override_role" => "admin",
          "escalate_privileges" => true
        })
    end
    
    test "validates user role permissions for sensitive operations" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "test_user", role: "operator"}
        })
        |> subscribe_and_join(SwarmChannel, "swarm:role_test")
      
      # Attempt admin-only operation with operator role
      ref = push(socket, "admin:reset_all", %{})
      assert_reply ref, :error, %{reason: "Admin access required"}
      
      # Attempt to modify system configuration
      ref = push(socket, "system:config:update", %{
        "critical_settings" => %{"debug_mode" => true}
      })
      assert_reply ref, :error, %{reason: "Insufficient permissions"}
    end
  end
  
  describe "input validation and injection attacks" do
    test "prevents SQL injection through pipeline parameters" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:injection_test")
      
      # Attempt SQL injection in domain parameter
      malicious_payload = %{
        "domain" => "'; DROP TABLE users; --",
        "optimization_strategy" => "skip_non_critical"
      }
      
      ref = push(socket, "pipeline:execute", malicious_payload)
      assert_reply ref, :error, %{reason: "Invalid domain parameter"}
    end
    
    test "sanitizes script injection in notification messages" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:script_injection_test")
      
      # Attempt XSS injection in notification
      malicious_notification = %{
        "level" => "info",
        "channel" => "ash_resources",
        "message" => "<script>alert('XSS')</script>",
        "metadata" => %{
          "source" => "<img src=x onerror=alert('XSS')>"
        }
      }
      
      ref = push(socket, "notifications:send", malicious_notification)
      assert_reply ref, :error, %{reason: "Invalid message content"}
    end
    
    test "prevents command injection in reactor step execution" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:command_injection_test")
      
      # Attempt command injection in step inputs
      malicious_step = %{
        "step_name" => "malicious_step",
        "step_type" => "ash_action",
        "inputs" => %{
          "command" => "; rm -rf /",
          "file_path" => "/etc/passwd"
        }
      }
      
      ref = push(socket, "reactor:step:execute", malicious_step)
      assert_reply ref, :error, %{reason: "Invalid input parameters"}
    end
    
    test "validates JSON structure and prevents prototype pollution" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:json_validation_test")
      
      # Attempt prototype pollution
      malicious_json = %{
        "__proto__" => %{"isAdmin" => true},
        "constructor" => %{"prototype" => %{"admin" => true}},
        "optimization_strategy" => "skip_non_critical"
      }
      
      ref = push(socket, "pipeline:execute", malicious_json)
      assert_reply ref, :error, %{reason: "Invalid payload structure"}
    end
  end
  
  describe "rate limiting and DoS protection" do
    test "enforces rate limits on channel messages" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:rate_limit_test")
      
      # Send many requests rapidly to trigger rate limiting
      for i <- 1..200 do
        push(socket, "pipeline:status", %{"execution_id" => "test_#{i}"})
      end
      
      # Additional request should be rate limited
      ref = push(socket, "pipeline:status", %{"execution_id" => "test_201"})
      assert_reply ref, :error, %{reason: "Rate limit exceeded"}
    end
    
    test "prevents large payload attacks" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:payload_size_test")
      
      # Create oversized payload
      large_data = String.duplicate("A", 10_000_000)  # 10MB string
      
      oversized_payload = %{
        "domain" => "cybersecurity",
        "large_data" => large_data,
        "optimization_strategy" => "skip_non_critical"
      }
      
      ref = push(socket, "pipeline:execute", oversized_payload)
      assert_reply ref, :error, %{reason: "Payload too large"}
    end
    
    test "protects against connection flooding" do
      # Attempt to create many connections rapidly
      tasks = for i <- 1..50 do
        Task.async(fn ->
          try do
            {:ok, socket} = connect(UserSocket, %{
              current_user: %{id: "flood_user_#{i}", role: "operator"}
            })
            
            subscribe_and_join(socket, SwarmChannel, "swarm:flood_test_#{i}")
          rescue
            _ -> {:error, "Connection rejected"}
          end
        end)
      end
      
      results = Task.await_many(tasks, 5000)
      
      # Should reject some connections to prevent flooding
      rejections = Enum.count(results, &match?({:error, _}, &1))
      assert rejections > 0
    end
  end
  
  describe "data validation and boundary testing" do
    test "handles extreme parameter values" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:boundary_test")
      
      # Test with extreme complexity values
      extreme_payload = %{
        "complexity" => 999999999,
        "timeout" => -1,
        "batch_size" => 0,
        "optimization_strategy" => "invalid_strategy"
      }
      
      ref = push(socket, "pipeline:execute", extreme_payload)
      assert_reply ref, :error, %{reason: "Invalid parameter values"}
    end
    
    test "validates workflow step dependencies and prevents cycles" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:dependency_test")
      
      # Create workflow with circular dependencies
      circular_workflow = %{
        "name" => "circular_test",
        "steps" => [
          %{"name" => "step_a", "depends_on" => ["step_c"]},
          %{"name" => "step_b", "depends_on" => ["step_a"]},
          %{"name" => "step_c", "depends_on" => ["step_b"]}
        ]
      }
      
      ref = push(socket, "reactor:workflow:create", circular_workflow)
      assert_reply ref, :error, %{reason: "Circular dependency detected"}
    end
    
    test "prevents resource exhaustion through recursive operations" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:recursion_test")
      
      # Attempt deeply nested recursive structure
      recursive_payload = create_deep_nested_structure(1000)
      
      ref = push(socket, "swarm:process_nested", recursive_payload)
      assert_reply ref, :error, %{reason: "Structure too complex"}
    end
  end
  
  describe "session hijacking and CSRF protection" do
    test "validates session integrity across requests" do
      {:ok, _, socket1} =
        UserSocket
        |> socket("user1", %{current_user: %{id: "user1", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:session_test_1")
      
      {:ok, _, socket2} =
        UserSocket
        |> socket("user2", %{current_user: %{id: "user2", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:session_test_2")
      
      # User1 attempts to impersonate User2
      impersonation_payload = %{
        "user_id" => "user2",
        "session_override" => true,
        "operation" => "sensitive_action"
      }
      
      ref = push(socket1, "admin:impersonate", impersonation_payload)
      assert_reply ref, :error, %{reason: "Session validation failed"}
    end
    
    test "prevents cross-channel message injection" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:cross_channel_test")
      
      # Attempt to send message to different channel
      cross_channel_payload = %{
        "target_channel" => "swarm:admin_channel",
        "message" => "unauthorized_command",
        "inject" => true
      }
      
      ref = push(socket, "cross_channel:send", cross_channel_payload)
      assert_reply ref, :error, %{reason: "Cross-channel injection blocked"}
    end
  end
  
  describe "data leakage and information disclosure" do
    test "prevents exposure of sensitive system information" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:info_disclosure_test")
      
      # Attempt to access system internals
      ref = push(socket, "system:debug:info", %{})
      assert_reply ref, :error, %{reason: "Access denied"}
      
      # Attempt to read configuration
      ref = push(socket, "system:config:read", %{})
      assert_reply ref, :error, %{reason: "Configuration access denied"}
    end
    
    test "sanitizes error messages to prevent information leakage" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:error_leakage_test")
      
      # Trigger internal error
      ref = push(socket, "pipeline:execute", %{
        "internal_error_trigger" => true,
        "force_database_error" => true
      })
      
      assert_reply ref, :error, %{reason: reason}
      
      # Error message should be generic, not expose internal details
      refute String.contains?(reason, "database")
      refute String.contains?(reason, "connection")
      refute String.contains?(reason, "internal")
    end
    
    test "prevents unauthorized access to other users' data" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:data_access_test")
      
      # Attempt to access another user's execution data
      ref = push(socket, "pipeline:status", %{
        "execution_id" => "other_user_execution_123",
        "user_override" => "different_user"
      })
      
      assert_reply ref, :error, %{reason: "Unauthorized access"}
    end
  end
  
  describe "cryptographic security" do
    test "validates secure communication integrity" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:crypto_test")
      
      # Attempt to send tampered encrypted data
      tampered_payload = %{
        "encrypted_data" => "tampered_encryption_string",
        "signature" => "invalid_signature",
        "operation" => "sensitive_action"
      }
      
      ref = push(socket, "secure:operation", tampered_payload)
      assert_reply ref, :error, %{reason: "Cryptographic validation failed"}
    end
  end
  
  describe "business logic vulnerabilities" do
    test "prevents optimization bypass attacks" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:optimization_bypass_test")
      
      # Attempt to bypass 80/20 optimization
      bypass_payload = %{
        "optimization_strategy" => "skip_non_critical",
        "force_full_execution" => true,
        "bypass_filters" => true,
        "override_optimization" => true
      }
      
      ref = push(socket, "pipeline:execute", bypass_payload)
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should still apply optimization despite bypass attempts
      assert_broadcast "pipeline:started", %{
        execution_id: ^execution_id,
        optimization_applied: true,
        bypass_ignored: true
      }
    end
    
    test "validates workflow execution permissions" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:workflow_permission_test")
      
      # Attempt to execute restricted workflow
      restricted_workflow = %{
        "workflow_id" => "admin_only_workflow",
        "override_permissions" => true,
        "elevated_execution" => true
      }
      
      ref = push(socket, "reactor:workflow:execute", restricted_workflow)
      assert_reply ref, :error, %{reason: "Workflow access denied"}
    end
    
    test "prevents resource quota bypass" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:quota_test")
      
      # Attempt to exceed resource quotas
      quota_bypass_payload = %{
        "resource_request" => %{
          "cpu" => 999,
          "memory" => "unlimited",
          "concurrent_executions" => 1000
        },
        "bypass_quotas" => true
      }
      
      ref = push(socket, "resources:allocate", quota_bypass_payload)
      assert_reply ref, :error, %{reason: "Resource quota exceeded"}
    end
  end
  
  # Helper function to create deeply nested structure
  defp create_deep_nested_structure(depth) when depth <= 0, do: %{"end" => true}
  defp create_deep_nested_structure(depth) do
    %{
      "level" => depth,
      "nested" => create_deep_nested_structure(depth - 1)
    }
  end
end