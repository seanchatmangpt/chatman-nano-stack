defmodule CnsWeb.Channels.K8sOtelValidationTest do
  @moduledoc """
  Validates ultrathink 80/20 swarm channels against OpenTelemetry and Kubernetes standards.
  Tests OTEL metric formats, K8s resource attributes, and observability integration.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Telemetry, K8s}
  
  @moduletag :k8s_otel_validation
  @moduletag timeout: 300_000  # 5 minutes for validation tests
  
  # OTEL standard metric names
  @otel_metrics [
    "system.cpu.utilization",
    "system.memory.utilization", 
    "system.network.io",
    "process.runtime.elixir.total_run_queue_lengths.total",
    "http.server.duration",
    "http.server.request.size",
    "http.server.response.size"
  ]
  
  # K8s resource attributes per OTEL semantic conventions
  @k8s_resource_attributes [
    "k8s.cluster.name",
    "k8s.namespace.name", 
    "k8s.deployment.name",
    "k8s.pod.name",
    "k8s.container.name",
    "service.name",
    "service.version",
    "deployment.environment"
  ]
  
  # OTEL status codes
  @otel_status_codes %{
    unset: 0,
    ok: 1,
    error: 2
  }
  
  setup_all do
    # Start OTEL collector simulator
    start_supervised!({Phoenix.PubSub, name: CnsWeb.PubSub})
    start_supervised!({Registry, keys: :unique, name: CnsWeb.OtelRegistry})
    
    # Mock K8s environment
    System.put_env("KUBERNETES_SERVICE_HOST", "10.96.0.1")
    System.put_env("KUBERNETES_SERVICE_PORT", "443")
    System.put_env("K8S_NAMESPACE", "cns-swarm")
    System.put_env("K8S_POD_NAME", "ultrathink-pipeline-test")
    System.put_env("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4317")
    
    # Create admin user for testing
    admin_user = %{
      id: 1,
      email: "otel_admin@test.com",
      role: "admin",
      active: true
    }
    
    # Create test swarm
    swarm = %{
      id: "k8s-otel-test-swarm",
      name: "K8s OTEL Validation Swarm",
      optimization_mode: "80_20"
    }
    
    {:ok, %{admin_user: admin_user, swarm: swarm}}
  end
  
  describe "OTEL Metric Format Validation" do
    test "validates metric naming conventions", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Subscribe to OTEL-compliant telemetry
      ref = push(channel_socket, "telemetry:subscribe", %{
        "format" => "otel",
        "metrics" => @otel_metrics
      })
      
      assert_reply ref, :ok, response
      assert response.format == "otel"
      
      # Execute pipeline to generate metrics
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "otel_enabled" => true,
        "input_data" => %{"generate_telemetry" => true}
      })
      
      assert_reply pipeline_ref, :ok, _result
      
      # Collect OTEL metrics
      metrics = collect_otel_metrics(10, 5000)
      
      # Validate metric naming conventions
      Enum.each(metrics, fn metric ->
        # Must follow OTEL naming convention (snake_case with dots)
        assert metric.name =~ ~r/^[a-z][a-z0-9_.]*[a-z0-9]$/
        
        # Must have required OTEL fields
        assert Map.has_key?(metric, :name)
        assert Map.has_key?(metric, :value)
        assert Map.has_key?(metric, :timestamp)
        assert Map.has_key?(metric, :attributes)
        assert Map.has_key?(metric, :resource)
        
        # Timestamp must be Unix nanoseconds
        assert is_integer(metric.timestamp)
        assert metric.timestamp > 1_600_000_000_000_000_000
        
        # Value must be numeric
        assert is_number(metric.value)
      end)
      
      # Validate specific OTEL metric formats
      cpu_metrics = Enum.filter(metrics, &(&1.name == "system.cpu.utilization"))
      assert length(cpu_metrics) > 0
      
      Enum.each(cpu_metrics, fn metric ->
        assert metric.value >= 0.0 and metric.value <= 1.0  # CPU as ratio
        assert Map.has_key?(metric.attributes, "cpu.state")
      end)
    end
    
    test "validates span and trace generation", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Subscribe to OTEL traces
      ref = push(channel_socket, "telemetry:traces:subscribe", %{
        "format" => "otel",
        "sample_rate" => 1.0
      })
      
      assert_reply ref, :ok, _response
      
      # Execute traced pipeline
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "trace_enabled" => true,
        "input_data" => %{"operation" => "trace_test"}
      })
      
      assert_reply pipeline_ref, :ok, result
      
      # Should include trace context
      assert Map.has_key?(result, :trace_id)
      assert Map.has_key?(result, :span_id)
      assert byte_size(result.trace_id) == 32  # 16 bytes hex encoded
      assert byte_size(result.span_id) == 16   # 8 bytes hex encoded
      
      # Collect OTEL spans
      spans = collect_otel_spans(5, 5000)
      
      # Validate span structure
      Enum.each(spans, fn span ->
        # Required OTEL span fields
        assert Map.has_key?(span, :trace_id)
        assert Map.has_key?(span, :span_id)
        assert Map.has_key?(span, :parent_span_id)
        assert Map.has_key?(span, :operation_name)
        assert Map.has_key?(span, :start_time)
        assert Map.has_key?(span, :end_time)
        assert Map.has_key?(span, :status)
        assert Map.has_key?(span, :attributes)
        
        # Validate span timing
        assert span.end_time >= span.start_time
        assert span.end_time - span.start_time < 10_000_000_000  # < 10 seconds
        
        # Validate OTEL status
        assert span.status.code in [@otel_status_codes.unset, @otel_status_codes.ok, @otel_status_codes.error]
        
        # Operation names should follow convention
        assert span.operation_name =~ ~r/^(pipeline|reactor|k8s)\./
      end)
      
      # Validate trace hierarchy
      trace_spans = Enum.filter(spans, &(&1.trace_id == result.trace_id))
      assert length(trace_spans) >= 3  # Pipeline + stages
      
      # Should have root span with no parent
      root_spans = Enum.filter(trace_spans, &is_nil(&1.parent_span_id))
      assert length(root_spans) == 1
    end
  end
  
  describe "K8s Resource Attributes Validation" do
    test "validates K8s resource attributes in metrics", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Execute pipeline with K8s resource detection
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "k8s_resource_detection" => true,
        "input_data" => %{"test" => "k8s_attributes"}
      })
      
      assert_reply pipeline_ref, :ok, _result
      
      # Collect metrics with resource attributes
      metrics = collect_otel_metrics(5, 3000)
      
      # Validate K8s resource attributes
      Enum.each(metrics, fn metric ->
        resource = metric.resource
        
        # Must include required K8s attributes when in K8s environment
        required_k8s_attrs = [
          "k8s.namespace.name",
          "k8s.pod.name", 
          "service.name"
        ]
        
        Enum.each(required_k8s_attrs, fn attr ->
          assert Map.has_key?(resource.attributes, attr), 
            "Missing required K8s attribute: #{attr}"
        end)
        
        # Validate K8s attribute values
        assert resource.attributes["k8s.namespace.name"] == "cns-swarm"
        assert resource.attributes["k8s.pod.name"] == "ultrathink-pipeline-test"
        assert resource.attributes["service.name"] =~ ~r/cns|ultrathink/
        
        # Should include deployment environment
        if Map.has_key?(resource.attributes, "deployment.environment") do
          assert resource.attributes["deployment.environment"] in ["test", "staging", "production"]
        end
      end)
    end
    
    test "validates K8s deployment integration", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Execute K8s deployment pipeline
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "target" => "k8s",
        "k8s_config" => %{
          "namespace" => "cns-swarm",
          "deployment" => "ultrathink-80-20",
          "replicas" => 3,
          "otel_enabled" => true
        },
        "input_data" => %{"deploy_with_otel" => true}
      })
      
      assert_reply pipeline_ref, :ok, result
      
      # Should generate K8s deployment events
      assert_broadcast "stage:k8s:deployment", k8s_event
      assert k8s_event.otel_enabled == true
      assert Map.has_key?(k8s_event, :trace_id)
      
      # Should include K8s-specific metrics
      k8s_metrics = collect_k8s_metrics(3000)
      
      # Validate K8s deployment metrics
      deployment_metrics = Enum.filter(k8s_metrics, fn metric ->
        metric.name =~ ~r/k8s\.deployment\./
      end)
      
      assert length(deployment_metrics) > 0
      
      Enum.each(deployment_metrics, fn metric ->
        assert Map.has_key?(metric.attributes, "k8s.deployment.name")
        assert Map.has_key?(metric.attributes, "k8s.namespace.name")
        assert metric.attributes["k8s.deployment.name"] == "ultrathink-80-20"
      end)
    end
  end
  
  describe "OTEL Collector Integration" do
    test "validates OTLP export format", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Configure OTLP export
      export_ref = push(channel_socket, "telemetry:export:configure", %{
        "protocol" => "otlp",
        "endpoint" => "http://otel-collector:4317",
        "format" => "protobuf",
        "compression" => "gzip"
      })
      
      assert_reply export_ref, :ok, config
      assert config.protocol == "otlp"
      assert config.format == "protobuf"
      
      # Execute pipeline with OTLP export
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "export_otel" => true,
        "input_data" => %{"test" => "otlp_export"}
      })
      
      assert_reply pipeline_ref, :ok, _result
      
      # Should receive export confirmation
      assert_broadcast "telemetry:export:completed", export_data
      assert export_data.protocol == "otlp"
      assert export_data.records_exported > 0
      assert export_data.status == "success"
      
      # Validate exported data format
      exported_metrics = export_data.sample_data.metrics
      
      # Should be in OTLP format
      assert is_list(exported_metrics)
      
      Enum.each(exported_metrics, fn metric ->
        # OTLP metric structure
        assert Map.has_key?(metric, :resource)
        assert Map.has_key?(metric, :scope_metrics)
        
        # Resource should have attributes
        assert Map.has_key?(metric.resource, :attributes)
        assert is_list(metric.resource.attributes)
        
        # Scope metrics should have metrics array
        Enum.each(metric.scope_metrics, fn scope ->
          assert Map.has_key?(scope, :scope)
          assert Map.has_key?(scope, :metrics)
          assert is_list(scope.metrics)
        end)
      end)
    end
    
    test "validates batch export configuration", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Configure batch export with 80/20 optimization
      batch_ref = push(channel_socket, "telemetry:batch:configure", %{
        "max_batch_size" => 512,      # OTEL recommended
        "export_timeout" => 30000,    # 30 seconds
        "schedule_delay" => 5000,     # 5 seconds
        "max_export_batch_size" => 512
      })
      
      assert_reply batch_ref, :ok, batch_config
      
      # In 80/20 mode, should optimize batch settings
      assert batch_config.optimized == true
      assert batch_config.max_batch_size <= 512
      assert batch_config.export_timeout <= 30000
      
      # Generate telemetry load to trigger batching
      for i <- 1..100 do
        push(channel_socket, "telemetry:data:generate", %{
          "metric" => "test.batch.metric.#{i}",
          "value" => :rand.uniform(100)
        })
      end
      
      # Should receive batch export events
      batches = collect_batch_exports(5, 10000)
      
      assert length(batches) > 0
      
      Enum.each(batches, fn batch ->
        assert batch.batch_size <= 512
        assert batch.export_duration < 30000
        assert Map.has_key?(batch, :compression_ratio)
      end)
    end
  end
  
  describe "OTEL Semantic Conventions" do
    test "validates HTTP semantic conventions", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Execute pipeline that generates HTTP spans
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "external_calls" => [
          %{"url" => "https://api.example.com/validate", "method" => "POST"},
          %{"url" => "https://k8s.example.com/deploy", "method" => "PUT"}
        ],
        "input_data" => %{"test" => "http_conventions"}
      })
      
      assert_reply pipeline_ref, :ok, _result
      
      # Collect HTTP spans
      http_spans = collect_http_spans(3000)
      
      assert length(http_spans) > 0
      
      Enum.each(http_spans, fn span ->
        attrs = span.attributes
        
        # Required HTTP attributes per OTEL semantic conventions
        assert Map.has_key?(attrs, "http.method")
        assert Map.has_key?(attrs, "http.url") or Map.has_key?(attrs, "http.target")
        assert Map.has_key?(attrs, "http.status_code")
        assert Map.has_key?(attrs, "http.user_agent")
        
        # Validate HTTP method
        assert attrs["http.method"] in ["GET", "POST", "PUT", "DELETE", "PATCH"]
        
        # Validate status code
        assert is_integer(attrs["http.status_code"])
        assert attrs["http.status_code"] >= 100 and attrs["http.status_code"] < 600
        
        # Span name should follow convention
        assert span.operation_name =~ ~r/^HTTP\s+[A-Z]+/
      end)
    end
    
    test "validates system resource semantic conventions", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Subscribe to system metrics
      metrics_ref = push(channel_socket, "telemetry:system:subscribe", %{
        "metrics" => ["cpu", "memory", "disk", "network"],
        "semantic_conventions" => true
      })
      
      assert_reply metrics_ref, :ok, _response
      
      # Generate system load
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "system_intensive" => true,
        "input_data" => %{"load_test" => true}
      })
      
      assert_reply pipeline_ref, :ok, _result
      
      # Collect system metrics
      system_metrics = collect_system_metrics(5000)
      
      # Validate CPU metrics semantic conventions
      cpu_metrics = Enum.filter(system_metrics, &(&1.name =~ ~r/system\.cpu\./))
      
      Enum.each(cpu_metrics, fn metric ->
        attrs = metric.attributes
        
        # CPU semantic conventions
        if metric.name == "system.cpu.utilization" do
          assert Map.has_key?(attrs, "cpu.state")
          assert attrs["cpu.state"] in ["idle", "user", "system", "nice", "iowait"]
        end
        
        # Value should be a ratio for utilization
        if String.ends_with?(metric.name, ".utilization") do
          assert metric.value >= 0.0 and metric.value <= 1.0
        end
      end)
      
      # Validate memory metrics semantic conventions
      memory_metrics = Enum.filter(system_metrics, &(&1.name =~ ~r/system\.memory\./))
      
      Enum.each(memory_metrics, fn metric ->
        attrs = metric.attributes
        
        if metric.name == "system.memory.usage" do
          assert Map.has_key?(attrs, "memory.state")
          assert attrs["memory.state"] in ["used", "free", "cached", "buffered"]
        end
      end)
    end
  end
  
  describe "Performance and Compliance" do
    test "validates telemetry overhead limits", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Measure baseline performance
      baseline_start = :os.system_time(:millisecond)
      
      baseline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "telemetry_enabled" => false,
        "input_data" => %{"baseline_test" => true}
      })
      
      assert_reply baseline_ref, :ok, _result
      baseline_duration = :os.system_time(:millisecond) - baseline_start
      
      # Measure performance with OTEL telemetry
      otel_start = :os.system_time(:millisecond)
      
      otel_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "telemetry_enabled" => true,
        "otel_enabled" => true,
        "input_data" => %{"otel_test" => true}
      })
      
      assert_reply otel_ref, :ok, _result
      otel_duration = :os.system_time(:millisecond) - otel_start
      
      # OTEL overhead should be minimal (< 10% per OTEL guidelines)
      overhead_ratio = (otel_duration - baseline_duration) / baseline_duration
      assert overhead_ratio < 0.10, 
        "OTEL overhead (#{overhead_ratio * 100}%) exceeds 10% guideline"
      
      # Validate resource consumption
      memory_before = :erlang.memory(:total)
      
      # Generate sustained telemetry load
      for _i <- 1..1000 do
        push(channel_socket, "telemetry:data:generate", %{
          "metric" => "load.test.metric",
          "value" => :rand.uniform(100)
        })
      end
      
      Process.sleep(2000)  # Allow processing
      
      memory_after = :erlang.memory(:total)
      memory_increase = (memory_after - memory_before) / (1024 * 1024)  # MB
      
      # Memory overhead should be reasonable
      assert memory_increase < 50, 
        "Memory overhead (#{memory_increase}MB) too high for telemetry load"
    end
    
    test "validates data retention and sampling compliance", %{admin_user: user, swarm: swarm} do
      {:ok, socket} = connect(UserSocket, %{
        "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
        "optimization" => "80_20"
      })
      
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}"
      )
      
      # Configure sampling with compliance settings
      sampling_ref = push(channel_socket, "telemetry:sampling:configure", %{
        "trace_sample_rate" => 0.1,     # 10% sampling
        "metric_sample_rate" => 1.0,    # All metrics (production would be lower)
        "retention_policy" => %{
          "traces" => "7d",
          "metrics" => "30d", 
          "logs" => "3d"
        },
        "compliance_mode" => "gdpr"
      })
      
      assert_reply sampling_ref, :ok, sampling_config
      
      assert sampling_config.trace_sample_rate == 0.1
      assert sampling_config.compliance_mode == "gdpr"
      
      # Generate telemetry data
      for i <- 1..100 do
        push(channel_socket, "pipeline:execute", %{
          "strategy" => "80_20",
          "trace_enabled" => true,
          "execution_id" => "sample_test_#{i}",
          "input_data" => %{"sample_iteration" => i}
        })
      end
      
      # Collect sampling statistics
      sampling_stats = collect_sampling_stats(10000)
      
      # Validate sampling rates
      trace_sample_rate = sampling_stats.traces_sampled / sampling_stats.traces_generated
      assert abs(trace_sample_rate - 0.1) < 0.05, 
        "Trace sampling rate (#{trace_sample_rate}) outside expected range"
      
      # Validate compliance features
      assert sampling_stats.gdpr_compliant == true
      assert Map.has_key?(sampling_stats, :pii_scrubbed)
      assert sampling_stats.retention_policies_applied == true
    end
  end
  
  # Helper functions
  
  defp collect_otel_metrics(count, timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_otel_metrics_loop([], count, end_time)
  end
  
  defp collect_otel_metrics_loop(metrics, remaining, end_time) do
    if remaining <= 0 or :os.system_time(:millisecond) >= end_time do
      metrics
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:otel:metric", payload: metric} ->
          collect_otel_metrics_loop([metric | metrics], remaining - 1, end_time)
      after
        100 -> collect_otel_metrics_loop(metrics, remaining, end_time)
      end
    end
  end
  
  defp collect_otel_spans(count, timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_otel_spans_loop([], count, end_time)
  end
  
  defp collect_otel_spans_loop(spans, remaining, end_time) do
    if remaining <= 0 or :os.system_time(:millisecond) >= end_time do
      spans
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:otel:span", payload: span} ->
          collect_otel_spans_loop([span | spans], remaining - 1, end_time)
      after
        100 -> collect_otel_spans_loop(spans, remaining, end_time)
      end
    end
  end
  
  defp collect_k8s_metrics(timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_k8s_metrics_loop([], end_time)
  end
  
  defp collect_k8s_metrics_loop(metrics, end_time) do
    if :os.system_time(:millisecond) >= end_time do
      metrics
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:k8s:" <> _, payload: metric} ->
          collect_k8s_metrics_loop([metric | metrics], end_time)
      after
        100 -> collect_k8s_metrics_loop(metrics, end_time)
      end
    end
  end
  
  defp collect_batch_exports(count, timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_batch_exports_loop([], count, end_time)
  end
  
  defp collect_batch_exports_loop(batches, remaining, end_time) do
    if remaining <= 0 or :os.system_time(:millisecond) >= end_time do
      batches
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:batch:exported", payload: batch} ->
          collect_batch_exports_loop([batch | batches], remaining - 1, end_time)
      after
        200 -> collect_batch_exports_loop(batches, remaining, end_time)
      end
    end
  end
  
  defp collect_http_spans(timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_http_spans_loop([], end_time)
  end
  
  defp collect_http_spans_loop(spans, end_time) do
    if :os.system_time(:millisecond) >= end_time do
      spans
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:otel:span", payload: span} ->
          if String.starts_with?(span.operation_name, "HTTP") do
            collect_http_spans_loop([span | spans], end_time)
          else
            collect_http_spans_loop(spans, end_time)
          end
      after
        100 -> collect_http_spans_loop(spans, end_time)
      end
    end
  end
  
  defp collect_system_metrics(timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_system_metrics_loop([], end_time)
  end
  
  defp collect_system_metrics_loop(metrics, end_time) do
    if :os.system_time(:millisecond) >= end_time do
      metrics
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:otel:metric", payload: metric} ->
          if String.starts_with?(metric.name, "system.") do
            collect_system_metrics_loop([metric | metrics], end_time)
          else
            collect_system_metrics_loop(metrics, end_time)
          end
      after
        100 -> collect_system_metrics_loop(metrics, end_time)
      end
    end
  end
  
  defp collect_sampling_stats(timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    
    receive do
      %Phoenix.Socket.Message{event: "telemetry:sampling:stats", payload: stats} ->
        stats
    after
      timeout -> %{
        traces_generated: 0,
        traces_sampled: 0,
        gdpr_compliant: false,
        retention_policies_applied: false
      }
    end
  end
end