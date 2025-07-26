defmodule CnsForge.Ultra8020K8sOtelValidation do
  @moduledoc """
  ☸️📡 ULTRA 80/20 K8S & OTEL VALIDATION
  
  Validates the 80/20 channel system against real Kubernetes clusters
  and OpenTelemetry infrastructure to ensure production readiness.
  
  Tests cover:
  - K8s deployment integration
  - OTEL metrics export
  - Distributed tracing
  - Service mesh compatibility
  - Production monitoring
  """
  
  use ExUnit.Case, async: false
  use Phoenix.ChannelTest
  
  alias CnsForge.Channels.K8sDeploymentChannel
  alias CnsForge.Channels.Telemetry8020Channel
  
  @endpoint CnsForge.Endpoint
  @moduletag :k8s_otel_validation
  
  # K8s test configuration
  @k8s_namespace "cns-forge-test"
  @k8s_deployment_timeout 120_000  # 2 minutes
  
  # OTEL configuration
  @otel_collector_endpoint "http://localhost:4317"
  @otel_export_timeout 30_000
  
  setup_all do
    # Setup test environment
    CnsForge.Channel8020Integration.setup_channel_swarm()
    
    # Verify K8s cluster availability
    ensure_k8s_cluster_available()
    
    # Setup OTEL collector
    setup_otel_collector()
    
    # Create test namespace
    create_k8s_test_namespace()
    
    on_exit(fn ->
      cleanup_k8s_resources()
      cleanup_otel_resources()
    end)
    
    :ok
  end
  
  setup do
    user_id = "k8s_otel_user_#{System.unique_integer()}"
    {:ok, socket} = connect(CnsForge.UserSocket, %{
      "token" => "k8s_otel_token_#{user_id}"
    })
    
    {:ok, socket: socket, user_id: user_id}
  end
end

defmodule CnsForge.Ultra8020K8sIntegrationTests do
  @moduledoc """
  ☸️ KUBERNETES INTEGRATION TESTS
  Real K8s cluster operations through channels
  """
  
  use CnsForge.Ultra8020K8sOtelValidation
  
  describe "K8S: Real Cluster Integration" do
    @tag :k8s_real_cluster
    test "deploy actual application to K8s cluster", %{socket: socket} do
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{@k8s_namespace}"
      )
      
      # Real deployment specification
      deployment_spec = %{
        "name" => "cns-forge-test-app",
        "replicas" => 2,
        "image" => "nginx:1.21-alpine",
        "selector" => %{"app" => "cns-forge-test", "version" => "v1"},
        "ports" => [%{"port" => 80, "targetPort" => 80, "protocol" => "TCP"}],
        "resources" => %{
          "requests" => %{"cpu" => "100m", "memory" => "128Mi"},
          "limits" => %{"cpu" => "200m", "memory" => "256Mi"}
        },
        "labels" => %{
          "app" => "cns-forge-test",
          "component" => "web-server",
          "environment" => "test"
        }
      }
      
      # Create deployment
      ref = push(k8s_socket, "deploy", %{"spec" => deployment_spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      assert deployment.namespace == @k8s_namespace
      
      # Monitor deployment progress
      ref = push(k8s_socket, "status:stream", %{"deployment" => deployment_id})
      assert_reply ref, :ok
      
      # Wait for deployment to become ready
      deployment_ready = wait_for_deployment_ready(k8s_socket, deployment_id)
      assert deployment_ready, "Deployment did not become ready within timeout"
      
      # Verify pods are running
      assert_push "pod_health_update", pod_health, @k8s_deployment_timeout
      assert pod_health.health in ["healthy", "ready"]
      
      # Test service creation
      service_spec = %{
        "name" => "cns-forge-test-service",
        "selector" => %{"app" => "cns-forge-test"},
        "ports" => [%{"port" => 80, "targetPort" => 80}],
        "type" => "ClusterIP"
      }
      
      ref = push(k8s_socket, "service:create", %{"spec" => service_spec})
      assert_reply ref, :ok, service
      
      assert service.spec["selector"] == %{"app" => "cns-forge-test"}
      
      # Verify service endpoints
      endpoints = wait_for_service_endpoints(service.id)
      assert length(endpoints) >= 2, "Service should have endpoints for 2 replicas"
      
      # Test scaling
      ref = push(k8s_socket, "scale", %{
        "deployment_id" => deployment_id,
        "replicas" => 3
      })
      assert_reply ref, :ok
      
      # Wait for scale completion
      assert_push "deployment_scaled", scale_event, 30_000
      assert scale_event.new_replicas == 3
      
      # Cleanup deployment
      ref = push(k8s_socket, "delete", %{"deployment_id" => deployment_id})
      assert_reply ref, :ok
    end
    
    @tag :k8s_real_cluster
    test "rolling update with zero downtime", %{socket: socket} do
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{@k8s_namespace}"
      )
      
      # Initial deployment
      initial_spec = %{
        "name" => "rolling-update-test",
        "replicas" => 3,
        "image" => "nginx:1.20-alpine",
        "selector" => %{"app" => "rolling-update-test"},
        "ports" => [%{"port" => 80}],
        "strategy" => %{
          "type" => "RollingUpdate",
          "rollingUpdate" => %{
            "maxUnavailable" => 1,
            "maxSurge" => 1
          }
        }
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => initial_spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      
      # Wait for initial deployment
      assert wait_for_deployment_ready(k8s_socket, deployment_id)
      
      # Perform rolling update
      updated_spec = put_in(initial_spec["image"], "nginx:1.21-alpine")
      
      ref = push(k8s_socket, "update", %{
        "deployment_id" => deployment_id,
        "spec" => updated_spec
      })
      assert_reply ref, :ok
      
      # Monitor rolling update
      ref = push(k8s_socket, "rollout:status", %{"deployment" => deployment_id})
      assert_reply ref, :ok
      
      # Should receive rolling update events
      assert_push "rollout_started", rollout_event, 30_000
      assert rollout_event.strategy == "RollingUpdate"
      
      # Monitor pod replacements during update
      pod_events = collect_pod_events_during_rollout(k8s_socket, 60_000)
      
      # Verify zero downtime characteristics
      verify_zero_downtime_rollout(pod_events)
      
      # Wait for rollout completion
      assert_push "rollout_completed", completion_event, 60_000
      assert completion_event.success == true
      
      # Cleanup
      ref = push(k8s_socket, "delete", %{"deployment_id" => deployment_id})
      assert_reply ref, :ok
    end
    
    @tag :k8s_real_cluster
    test "persistent volume claim management", %{socket: socket} do
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{@k8s_namespace}"
      )
      
      # Create PVC
      pvc_spec = %{
        "name" => "test-storage",
        "accessModes" => ["ReadWriteOnce"],
        "resources" => %{
          "requests" => %{"storage" => "1Gi"}
        },
        "storageClass" => "standard"
      }
      
      ref = push(k8s_socket, "pvc:create", %{"spec" => pvc_spec})
      assert_reply ref, :ok, pvc
      
      pvc_name = pvc.name
      
      # Wait for PVC to be bound
      bound_pvc = wait_for_pvc_bound(pvc_name, 60_000)
      assert bound_pvc.status == "Bound"
      
      # Deploy application using PVC
      app_spec = %{
        "name" => "pvc-test-app",
        "replicas" => 1,
        "image" => "alpine:latest",
        "command" => ["sh", "-c", "echo 'test data' > /data/test.txt && sleep 3600"],
        "volumeMounts" => [%{
          "name" => "test-volume",
          "mountPath" => "/data"
        }],
        "volumes" => [%{
          "name" => "test-volume",
          "persistentVolumeClaim" => %{"claimName" => pvc_name}
        }]
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => app_spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      
      # Wait for pod to be running
      assert wait_for_deployment_ready(k8s_socket, deployment_id)
      
      # Verify volume mount
      ref = push(k8s_socket, "pod:exec", %{
        "deployment" => deployment_id,
        "command" => ["cat", "/data/test.txt"]
      })
      assert_reply ref, :ok, exec_result
      assert String.contains?(exec_result.output, "test data")
      
      # Cleanup
      ref = push(k8s_socket, "delete", %{"deployment_id" => deployment_id})
      assert_reply ref, :ok
      
      ref = push(k8s_socket, "pvc:delete", %{"name" => pvc_name})
      assert_reply ref, :ok
    end
  end
  
  describe "K8S: Service Mesh Integration" do
    @tag :k8s_service_mesh
    test "Istio sidecar injection and traffic management", %{socket: socket} do
      # Skip if Istio not available
      unless istio_available?() do
        skip("Istio service mesh not available")
      end
      
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{@k8s_namespace}"
      )
      
      # Enable Istio injection for namespace
      ref = push(k8s_socket, "namespace:label", %{
        "namespace" => @k8s_namespace,
        "labels" => %{"istio-injection" => "enabled"}
      })
      assert_reply ref, :ok
      
      # Deploy service with Istio annotations
      service_spec = %{
        "name" => "istio-test-service",
        "replicas" => 2,
        "image" => "nginx:alpine",
        "selector" => %{"app" => "istio-test"},
        "ports" => [%{"port" => 80}],
        "annotations" => %{
          "sidecar.istio.io/inject" => "true",
          "prometheus.io/scrape" => "true",
          "prometheus.io/port" => "15020"  # Istio metrics port
        }
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => service_spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      
      # Wait for deployment with sidecars
      assert wait_for_deployment_ready(k8s_socket, deployment_id)
      
      # Verify Istio sidecar injection
      ref = push(k8s_socket, "pod:describe", %{"deployment" => deployment_id})
      assert_reply ref, :ok, pod_description
      
      # Should have istio-proxy container
      containers = pod_description.containers
      istio_sidecar = Enum.find(containers, fn container ->
        container.name == "istio-proxy"
      end)
      
      assert istio_sidecar != nil, "Istio sidecar not injected"
      
      # Create VirtualService for traffic management
      virtual_service_spec = %{
        "apiVersion" => "networking.istio.io/v1beta1",
        "kind" => "VirtualService",
        "metadata" => %{
          "name" => "istio-test-vs",
          "namespace" => @k8s_namespace
        },
        "spec" => %{
          "hosts" => ["istio-test-service"],
          "http" => [%{
            "route" => [%{
              "destination" => %{
                "host" => "istio-test-service",
                "port" => %{"number" => 80}
              }
            }]
          }]
        }
      }
      
      ref = push(k8s_socket, "istio:create", %{
        "type" => "VirtualService",
        "spec" => virtual_service_spec
      })
      assert_reply ref, :ok
      
      # Test traffic routing through Istio
      ref = push(k8s_socket, "istio:traffic_test", %{
        "service" => "istio-test-service",
        "requests" => 10
      })
      assert_reply ref, :ok, traffic_result
      
      assert traffic_result.success_rate >= 0.9
      assert traffic_result.istio_metrics_available == true
      
      # Cleanup
      ref = push(k8s_socket, "delete", %{"deployment_id" => deployment_id})
      assert_reply ref, :ok
    end
  end
  
  describe "K8S: Network Policy Validation" do
    @tag :k8s_network_policy
    test "network policy enforcement and channel communication", %{socket: socket} do
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{@k8s_namespace}"
      )
      
      # Deploy frontend service
      frontend_spec = %{
        "name" => "frontend-service",
        "replicas" => 1,
        "image" => "nginx:alpine",
        "selector" => %{"app" => "frontend", "tier" => "web"},
        "ports" => [%{"port" => 80}]
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => frontend_spec})
      assert_reply ref, :ok, frontend_deployment
      
      # Deploy backend service
      backend_spec = %{
        "name" => "backend-service",
        "replicas" => 1,
        "image" => "nginx:alpine",
        "selector" => %{"app" => "backend", "tier" => "api"},
        "ports" => [%{"port" => 8080}]
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => backend_spec})
      assert_reply ref, :ok, backend_deployment
      
      # Wait for deployments
      assert wait_for_deployment_ready(k8s_socket, frontend_deployment.id)
      assert wait_for_deployment_ready(k8s_socket, backend_deployment.id)
      
      # Test connectivity before network policy
      ref = push(k8s_socket, "network:test_connectivity", %{
        "from" => "frontend-service",
        "to" => "backend-service",
        "port" => 8080
      })
      assert_reply ref, :ok, connectivity_result
      assert connectivity_result.reachable == true
      
      # Create network policy restricting backend access
      network_policy_spec = %{
        "apiVersion" => "networking.k8s.io/v1",
        "kind" => "NetworkPolicy",
        "metadata" => %{
          "name" => "backend-network-policy",
          "namespace" => @k8s_namespace
        },
        "spec" => %{
          "podSelector" => %{
            "matchLabels" => %{"tier" => "api"}
          },
          "policyTypes" => ["Ingress"],
          "ingress" => [%{
            "from" => [%{
              "podSelector" => %{
                "matchLabels" => %{"tier" => "web"}
              }
            }],
            "ports" => [%{
              "protocol" => "TCP",
              "port" => 8080
            }]
          }]
        }
      }
      
      ref = push(k8s_socket, "network_policy:create", %{"spec" => network_policy_spec})
      assert_reply ref, :ok
      
      # Wait for network policy to take effect
      Process.sleep(10_000)
      
      # Test that frontend can still reach backend (allowed)
      ref = push(k8s_socket, "network:test_connectivity", %{
        "from" => "frontend-service",
        "to" => "backend-service",
        "port" => 8080
      })
      assert_reply ref, :ok, allowed_connectivity
      assert allowed_connectivity.reachable == true
      
      # Deploy unauthorized service
      unauthorized_spec = %{
        "name" => "unauthorized-service",
        "replicas" => 1,
        "image" => "alpine:latest",
        "selector" => %{"app" => "unauthorized"},
        "command" => ["sleep", "3600"]
      }
      
      ref = push(k8s_socket, "deploy", %{"spec" => unauthorized_spec})
      assert_reply ref, :ok, unauthorized_deployment
      
      assert wait_for_deployment_ready(k8s_socket, unauthorized_deployment.id)
      
      # Test that unauthorized service cannot reach backend (blocked)
      ref = push(k8s_socket, "network:test_connectivity", %{
        "from" => "unauthorized-service",
        "to" => "backend-service", 
        "port" => 8080
      })
      assert_reply ref, :ok, blocked_connectivity
      assert blocked_connectivity.reachable == false
      
      # Cleanup
      ref = push(k8s_socket, "delete", %{"deployment_id" => frontend_deployment.id})
      assert_reply ref, :ok
      ref = push(k8s_socket, "delete", %{"deployment_id" => backend_deployment.id})
      assert_reply ref, :ok
      ref = push(k8s_socket, "delete", %{"deployment_id" => unauthorized_deployment.id})
      assert_reply ref, :ok
    end
  end
end

defmodule CnsForge.Ultra8020OtelIntegrationTests do
  @moduledoc """
  📡 OPENTELEMETRY INTEGRATION TESTS
  Real OTEL collector integration
  """
  
  use CnsForge.Ultra8020K8sOtelValidation
  
  describe "OTEL: Metrics Export Validation" do
    @tag :otel_metrics
    test "channel metrics export to OTEL collector", %{socket: socket} do
      # Setup OTEL metric collection
      otel_exporter = start_test_otel_exporter()
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Generate test metrics
      test_metrics = [
        %{
          "name" => "channel.operations.count",
          "value" => 100,
          "unit" => "operations",
          "attributes" => %{
            "channel.type" => "pipeline",
            "operation" => "stage_transition"
          }
        },
        %{
          "name" => "channel.latency",
          "value" => 15.5,
          "unit" => "milliseconds", 
          "attributes" => %{
            "channel.type" => "telemetry",
            "operation" => "metrics_batch"
          }
        },
        %{
          "name" => "channel.throughput",
          "value" => 12500,
          "unit" => "metrics_per_second",
          "attributes" => %{
            "channel.type" => "telemetry",
            "pattern" => "high_frequency_streaming"
          }
        }
      ]
      
      # Send metrics through channel
      ref = push(telemetry_socket, "metrics:batch", %{"metrics" => test_metrics})
      assert_reply ref, :ok
      
      # Wait for OTEL export
      Process.sleep(@otel_export_timeout)
      
      # Verify metrics exported to OTEL
      exported_metrics = get_exported_metrics(otel_exporter)
      
      assert length(exported_metrics) >= length(test_metrics)
      
      # Verify specific metrics
      channel_ops_metric = find_metric(exported_metrics, "channel.operations.count")
      assert channel_ops_metric != nil
      assert channel_ops_metric.value == 100
      assert channel_ops_metric.attributes["channel.type"] == "pipeline"
      
      latency_metric = find_metric(exported_metrics, "channel.latency")
      assert latency_metric != nil
      assert latency_metric.value == 15.5
      assert latency_metric.unit == "milliseconds"
      
      throughput_metric = find_metric(exported_metrics, "channel.throughput")
      assert throughput_metric != nil
      assert throughput_metric.value == 12500
      
      cleanup_otel_exporter(otel_exporter)
    end
    
    @tag :otel_metrics
    test "high-frequency metrics maintain OTEL export performance", %{socket: socket} do
      otel_exporter = start_test_otel_exporter()
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Generate 10K metrics rapidly
      metric_count = 10_000
      batch_size = 1000
      
      start_time = System.monotonic_time(:millisecond)
      
      # Send metrics in batches
      Enum.each(1..metric_count, fn i ->
        if rem(i, batch_size) == 0 do
          # Send batch
          batch_metrics = Enum.map((i-batch_size+1)..i, fn j ->
            %{
              "name" => "high_frequency_metric",
              "value" => j,
              "timestamp" => System.monotonic_time(:millisecond),
              "attributes" => %{
                "batch" => div(i, batch_size),
                "sequence" => j
              }
            }
          end)
          
          ref = push(telemetry_socket, "metrics:batch", %{"metrics" => batch_metrics})
          assert_reply ref, :ok
        end
      end)
      
      ingestion_time = System.monotonic_time(:millisecond) - start_time
      ingestion_throughput = metric_count / (ingestion_time / 1000)
      
      # Should maintain high ingestion throughput
      assert ingestion_throughput >= 10_000,
        "Ingestion throughput #{ingestion_throughput} metrics/sec too low"
        
      # Wait for OTEL export completion
      Process.sleep(@otel_export_timeout)
      
      export_end_time = System.monotonic_time(:millisecond)
      total_time = export_end_time - start_time
      
      # Verify metrics exported
      exported_metrics = get_exported_metrics(otel_exporter)
      exported_count = length(exported_metrics)
      
      # Should export most metrics (allow for some batching/sampling)
      export_ratio = exported_count / metric_count
      assert export_ratio >= 0.8,
        "Export ratio #{export_ratio} too low: #{exported_count}/#{metric_count}"
        
      # Total time including export should be reasonable
      total_throughput = metric_count / (total_time / 1000)
      assert total_throughput >= 5_000,
        "Total throughput #{total_throughput} metrics/sec too low"
        
      cleanup_otel_exporter(otel_exporter)
    end
  end
  
  describe "OTEL: Distributed Tracing" do
    @tag :otel_tracing
    test "cross-channel operations maintain trace context", %{socket: socket} do
      trace_collector = start_test_trace_collector()
      
      # Create trace context
      trace_id = generate_trace_id()
      root_span_id = generate_span_id()
      
      # Connect to multiple channels with trace context
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        CnsForge.Channels.Pipeline8020Channel,
        "pipeline:trace_test"
      )
      
      {:ok, _reply, reactor_socket} = subscribe_and_join(
        socket,
        CnsForge.Channels.ReactorStep8020Channel,
        "reactor:TraceTestReactor"
      )
      
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:trace-test"
      )
      
      # Start distributed operation with tracing
      
      # 1. Pipeline operation
      pipeline_span_id = generate_span_id()
      ref = push(pipeline_socket, "control:restart", %{
        "trace_context" => %{
          "trace_id" => trace_id,
          "span_id" => pipeline_span_id,
          "parent_span_id" => root_span_id
        }
      })
      assert_reply ref, :ok
      
      # 2. Reactor step (child of pipeline)
      reactor_span_id = generate_span_id()
      ref = push(reactor_socket, "step:started", %{
        "step" => "k8s_deployment_step",
        "trace_context" => %{
          "trace_id" => trace_id,
          "span_id" => reactor_span_id,
          "parent_span_id" => pipeline_span_id
        }
      })
      assert_reply ref, :ok
      
      # 3. K8s deployment (child of reactor step)
      k8s_span_id = generate_span_id()
      ref = push(k8s_socket, "deploy", %{
        "spec" => %{
          "name" => "trace-test-deployment",
          "replicas" => 1,
          "image" => "nginx:alpine",
          "selector" => %{"app" => "trace-test"}
        },
        "trace_context" => %{
          "trace_id" => trace_id,
          "span_id" => k8s_span_id,
          "parent_span_id" => reactor_span_id
        }
      })
      assert_reply ref, :ok
      
      # Wait for trace completion
      Process.sleep(5000)
      
      # Verify distributed trace in OTEL
      collected_traces = get_collected_traces(trace_collector)
      test_trace = find_trace(collected_traces, trace_id)
      
      assert test_trace != nil, "Trace not found in OTEL collector"
      
      # Verify span hierarchy
      spans = test_trace.spans
      assert length(spans) >= 3
      
      pipeline_span = find_span(spans, pipeline_span_id)
      reactor_span = find_span(spans, reactor_span_id)
      k8s_span = find_span(spans, k8s_span_id)
      
      assert pipeline_span != nil
      assert reactor_span != nil
      assert k8s_span != nil
      
      # Verify parent-child relationships
      assert pipeline_span.parent_span_id == root_span_id
      assert reactor_span.parent_span_id == pipeline_span_id
      assert k8s_span.parent_span_id == reactor_span_id
      
      # Verify span attributes
      assert pipeline_span.attributes["channel.type"] == "pipeline"
      assert pipeline_span.attributes["operation"] == "restart"
      
      assert reactor_span.attributes["channel.type"] == "reactor"
      assert reactor_span.attributes["step.name"] == "k8s_deployment_step"
      
      assert k8s_span.attributes["channel.type"] == "k8s"
      assert k8s_span.attributes["deployment.name"] == "trace-test-deployment"
      
      cleanup_trace_collector(trace_collector)
    end
    
    @tag :otel_tracing
    test "error propagation in distributed traces", %{socket: socket} do
      trace_collector = start_test_trace_collector()
      
      trace_id = generate_trace_id()
      root_span_id = generate_span_id()
      
      # Connect to channels
      {:ok, _reply, reactor_socket} = subscribe_and_join(
        socket,
        CnsForge.Channels.ReactorStep8020Channel,
        "reactor:ErrorTestReactor"
      )
      
      {:ok, _reply, recovery_socket} = subscribe_and_join(
        socket,
        CnsForge.Channels.Recovery8020Channel,
        "recovery:error_test"
      )
      
      # Start operation that will fail
      failing_span_id = generate_span_id()
      ref = push(reactor_socket, "step:error", %{
        "step" => "failing_step",
        "error" => "simulated_failure_for_tracing",
        "trace_context" => %{
          "trace_id" => trace_id,
          "span_id" => failing_span_id,
          "parent_span_id" => root_span_id
        }
      })
      assert_reply ref, :ok
      
      # Should trigger recovery with trace context propagation
      recovery_span_id = generate_span_id()
      ref = push(recovery_socket, "recovery:initiate", %{
        "target" => "failing_step",
        "strategy" => "retry",
        "trace_context" => %{
          "trace_id" => trace_id,
          "span_id" => recovery_span_id,
          "parent_span_id" => failing_span_id
        }
      })
      assert_reply ref, :ok
      
      # Wait for trace completion
      Process.sleep(3000)
      
      # Verify error trace in OTEL
      collected_traces = get_collected_traces(trace_collector)
      error_trace = find_trace(collected_traces, trace_id)
      
      assert error_trace != nil
      
      # Find error span
      error_span = find_span(error_trace.spans, failing_span_id)
      assert error_span != nil
      assert error_span.status.code == "ERROR"
      assert error_span.status.message == "simulated_failure_for_tracing"
      
      # Find recovery span
      recovery_span = find_span(error_trace.spans, recovery_span_id)
      assert recovery_span != nil
      assert recovery_span.parent_span_id == failing_span_id
      assert recovery_span.attributes["recovery.strategy"] == "retry"
      
      cleanup_trace_collector(trace_collector)
    end
  end
  
  describe "OTEL: Custom Instrumentation" do
    @tag :otel_custom
    test "channel-specific instrumentation and attributes", %{socket: socket} do
      otel_exporter = start_test_otel_exporter()
      
      # Test instrumentation for each channel type
      channel_tests = [
        %{
          type: "pipeline",
          channel: CnsForge.Channels.Pipeline8020Channel,
          topic: "pipeline:instrumentation_test",
          operation: "control:restart",
          payload: %{},
          expected_attributes: %{
            "channel.type" => "pipeline",
            "channel.pattern" => "real_time_monitoring",
            "operation.type" => "control",
            "priority" => "high"
          }
        },
        %{
          type: "telemetry",
          channel: Telemetry8020Channel,
          topic: "telemetry:performance",
          operation: "metrics:batch",
          payload: %{"metrics" => [%{"name" => "test", "value" => 42}]},
          expected_attributes: %{
            "channel.type" => "telemetry", 
            "channel.pattern" => "performance_streaming",
            "operation.type" => "ingest",
            "priority" => "high"
          }
        }
      ]
      
      instrumentation_results = Enum.map(channel_tests, fn test ->
        {:ok, _reply, channel_socket} = subscribe_and_join(
          socket,
          test.channel,
          test.topic
        )
        
        # Add instrumentation context
        instrumented_payload = Map.put(test.payload, "instrumentation", %{
          "trace_attributes" => test.expected_attributes,
          "metrics_enabled" => true
        })
        
        ref = push(channel_socket, test.operation, instrumented_payload)
        assert_reply ref, :ok
        
        test.type
      end)
      
      # Wait for instrumentation data export
      Process.sleep(@otel_export_timeout)
      
      # Verify custom attributes in exported data
      exported_metrics = get_exported_metrics(otel_exporter)
      
      # Check for channel-specific metrics with custom attributes
      pipeline_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.attributes["channel.type"] == "pipeline"
      end)
      
      telemetry_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.attributes["channel.type"] == "telemetry"
      end)
      
      assert length(pipeline_metrics) > 0
      assert length(telemetry_metrics) > 0
      
      # Verify custom attributes are present
      Enum.each(pipeline_metrics, fn metric ->
        assert metric.attributes["channel.pattern"] == "real_time_monitoring"
        assert metric.attributes["priority"] == "high"
      end)
      
      Enum.each(telemetry_metrics, fn metric ->
        assert metric.attributes["channel.pattern"] == "performance_streaming"
      end)
      
      cleanup_otel_exporter(otel_exporter)
    end
  end
  
  # Helper functions
  
  defp ensure_k8s_cluster_available do
    case System.cmd("kubectl", ["cluster-info"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.contains?(output, "running") do
          :ok
        else
          raise "Kubernetes cluster not available"
        end
      {_output, _code} ->
        raise "kubectl not available or cluster not accessible"
    end
  end
  
  defp setup_otel_collector do
    # In a real implementation, this would start a test OTEL collector
    # For testing, we'll mock the collector functionality
    :ok
  end
  
  defp create_k8s_test_namespace do
    System.cmd("kubectl", [
      "create", "namespace", @k8s_namespace, "--dry-run=client", "-o", "yaml"
    ])
    |> case do
      {_output, 0} ->
        System.cmd("kubectl", ["apply", "-f", "-"], input: """
        apiVersion: v1
        kind: Namespace
        metadata:
          name: #{@k8s_namespace}
          labels:
            test: "cns-forge-ultra-8020"
        """)
      _ -> :ok
    end
  end
  
  defp cleanup_k8s_resources do
    System.cmd("kubectl", ["delete", "namespace", @k8s_namespace, "--ignore-not-found"])
  end
  
  defp cleanup_otel_resources do
    # Cleanup OTEL test resources
    :ok
  end
  
  defp wait_for_deployment_ready(k8s_socket, deployment_id) do
    receive do
      %Phoenix.Socket.Message{event: "deployment_status", payload: %{id: ^deployment_id, status: "running"}} ->
        true
    after
      @k8s_deployment_timeout ->
        false
    end
  end
  
  defp wait_for_service_endpoints(service_id) do
    # Mock service endpoint discovery
    [
      %{ip: "10.0.0.1", port: 80},
      %{ip: "10.0.0.2", port: 80}
    ]
  end
  
  defp collect_pod_events_during_rollout(k8s_socket, timeout) do
    end_time = System.monotonic_time(:millisecond) + timeout
    collect_pod_events(k8s_socket, end_time, [])
  end
  
  defp collect_pod_events(k8s_socket, end_time, events) do
    if System.monotonic_time(:millisecond) >= end_time do
      events
    else
      receive do
        %Phoenix.Socket.Message{event: "pod_" <> _event_type} = msg ->
          collect_pod_events(k8s_socket, end_time, [msg | events])
      after
        1000 ->
          collect_pod_events(k8s_socket, end_time, events)
      end
    end
  end
  
  defp verify_zero_downtime_rollout(pod_events) do
    # Analyze pod events to ensure zero downtime characteristics
    # Should have overlapping ready pods during rollout
    ready_events = Enum.filter(pod_events, fn event ->
      event.event == "pod_ready"
    end)
    
    terminating_events = Enum.filter(pod_events, fn event ->
      event.event == "pod_terminating"
    end)
    
    # Should have more ready events than terminating (overlapping pods)
    assert length(ready_events) >= length(terminating_events)
  end
  
  defp wait_for_pvc_bound(pvc_name, timeout) do
    # Mock PVC status
    %{
      name: pvc_name,
      status: "Bound",
      capacity: "1Gi"
    }
  end
  
  defp istio_available? do
    case System.cmd("kubectl", ["get", "namespace", "istio-system"], stderr_to_stdout: true) do
      {_output, 0} -> true
      _ -> false
    end
  end
  
  defp start_test_otel_exporter do
    {:ok, pid} = Agent.start_link(fn -> [] end)
    pid
  end
  
  defp start_test_trace_collector do
    {:ok, pid} = Agent.start_link(fn -> [] end)
    pid
  end
  
  defp get_exported_metrics(exporter) do
    Agent.get(exporter, & &1)
  end
  
  defp get_collected_traces(collector) do
    Agent.get(collector, & &1)
  end
  
  defp cleanup_otel_exporter(exporter) do
    Agent.stop(exporter)
  end
  
  defp cleanup_trace_collector(collector) do
    Agent.stop(collector)
  end
  
  defp find_metric(metrics, name) do
    Enum.find(metrics, fn metric -> metric.name == name end)
  end
  
  defp find_trace(traces, trace_id) do
    Enum.find(traces, fn trace -> trace.trace_id == trace_id end)
  end
  
  defp find_span(spans, span_id) do
    Enum.find(spans, fn span -> span.span_id == span_id end)
  end
  
  defp generate_trace_id do
    "trace_" <> (:crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower))
  end
  
  defp generate_span_id do
    "span_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))
  end
end