# Cross-Service Benchmarking and Stress Testing Infrastructure
# Validates >100k RPS throughput and <10ms end-to-end latency across service mesh

# Performance Benchmarking Controller
resource "kubernetes_deployment" "performance_benchmark_controller" {
  metadata {
    name      = "performance-benchmark-controller"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-benchmark-controller"
      type        = "performance-testing"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "performance.testing/enabled" = "true"
    }
  }

  spec {
    replicas = 1  # Single controller for coordinated benchmarking

    selector {
      match_labels = {
        app  = "cns-benchmark-controller"
        type = "performance-testing"
      }
    }

    template {
      metadata {
        labels = {
          app  = "cns-benchmark-controller"
          type = "performance-testing"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9096"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
        
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          fs_group        = 2000
        }

        container {
          name  = "benchmark-controller"
          image = "cns-benchmark-controller:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "500m"
              memory = "1Gi"
            }
            limits = {
              cpu    = "2000m"
              memory = "2Gi"
            }
          }

          env {
            name  = "SERVICE_TYPE"
            value = "benchmark-controller"
          }
          
          env {
            name  = "CONTROLLER_PORT"
            value = "8086"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9096"
          }
          
          # Performance targets
          env {
            name  = "TARGET_RPS"
            value = "100000"
          }
          
          env {
            name  = "TARGET_LATENCY_MS"
            value = "10"
          }
          
          env {
            name  = "SERVICE_MESH_OVERHEAD_TARGET_MS"
            value = "1"
          }
          
          # Target services for benchmarking
          env {
            name  = "PROTECTION_SERVICE_ENDPOINT"
            value = "cns-protection-service:8080"
          }
          
          env {
            name  = "GATEWAY_SERVICE_ENDPOINT"
            value = "cns-gateway-service:8081"
          }
          
          env {
            name  = "ANALYTICS_SERVICE_ENDPOINT"
            value = "cns-analytics-service:8082"
          }
          
          env {
            name  = "MONITOR_SERVICE_ENDPOINT"
            value = "cns-monitor-service:8083"
          }
          
          # Benchmark configuration
          env {
            name  = "BENCHMARK_DURATION_SECONDS"
            value = "300"
          }
          
          env {
            name  = "WARMUP_DURATION_SECONDS"
            value = "60"
          }
          
          env {
            name  = "BENCHMARK_MODES"
            value = "latency,throughput,stress,endurance"
          }
          
          env {
            name  = "SERVICE_MESH_ENABLED"
            value = var.enable_service_mesh ? "true" : "false"
          }
          
          env {
            name  = "OTEL_EXPORTER_OTLP_ENDPOINT"
            value = "http://jaeger-collector:14268/api/traces"
          }

          port {
            name           = "controller"
            container_port = 8086
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9096
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "controller"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "controller"
            }
            initial_delay_seconds = 10
            period_seconds        = 5
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            run_as_user               = 1000
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "benchmark-config"
            mount_path = "/app/config/benchmark"
            read_only  = true
          }
        }

        volume {
          name = "benchmark-config"
          
          config_map {
            name = kubernetes_config_map.benchmark_stress_config.metadata[0].name
          }
        }
      }
    }
  }
}

# Load Generation Agents for Distributed Testing
resource "kubernetes_deployment" "load_generation_agents" {
  metadata {
    name      = "load-generation-agents"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-load-generators"
      type        = "load-testing"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    replicas = 10  # Multiple agents for distributed load generation

    selector {
      match_labels = {
        app  = "cns-load-generators"
        type = "load-testing"
      }
    }

    template {
      metadata {
        labels = {
          app  = "cns-load-generators"
          type = "load-testing"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9097"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
        
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          fs_group        = 2000
        }

        container {
          name  = "load-generator"
          image = "cns-load-generator:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "1000m"
              memory = "2Gi"
            }
            limits = {
              cpu    = "4000m"
              memory = "4Gi"
            }
          }

          env {
            name  = "SERVICE_TYPE"
            value = "load-generator"
          }
          
          env {
            name  = "GENERATOR_PORT"
            value = "8087"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9097"
          }
          
          env {
            name  = "CONTROLLER_ENDPOINT"
            value = "performance-benchmark-controller-service:8086"
          }
          
          # Load generation parameters
          env {
            name  = "MAX_RPS_PER_AGENT"
            value = "10000"
          }
          
          env {
            name  = "CONCURRENT_CONNECTIONS"
            value = "1000"
          }
          
          env {
            name  = "REQUEST_TIMEOUT_MS"
            value = "5000"
          }
          
          # Unique agent ID for coordination
          env {
            name = "AGENT_ID"
            value_from {
              field_ref {
                field_path = "metadata.name"
              }
            }
          }
          
          env {
            name = "POD_IP"
            value_from {
              field_ref {
                field_path = "status.podIP"
              }
            }
          }

          port {
            name           = "generator"
            container_port = 8087
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9097
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "generator"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "generator"
            }
            initial_delay_seconds = 10
            period_seconds        = 5
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            run_as_user               = 1000
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "benchmark-config"
            mount_path = "/app/config/benchmark"
            read_only  = true
          }
        }

        volume {
          name = "benchmark-config"
          
          config_map {
            name = kubernetes_config_map.benchmark_stress_config.metadata[0].name
          }
        }

        # Anti-affinity to spread load generators across nodes
        affinity {
          pod_anti_affinity {
            preferred_during_scheduling_ignored_during_execution {
              weight = 100
              
              pod_affinity_term {
                label_selector {
                  match_expressions {
                    key      = "app"
                    operator = "In"
                    values   = ["cns-load-generators"]
                  }
                }
                
                topology_key = "kubernetes.io/hostname"
              }
            }
          }
        }
      }
    }
  }
}

# Configuration for benchmarking and stress testing
resource "kubernetes_config_map" "benchmark_stress_config" {
  metadata {
    name      = "benchmark-stress-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-benchmark"
      type = "performance-configuration"
    }
  }

  data = {
    "benchmark_scenarios.yaml" = <<-EOT
      benchmark_scenarios:
        latency_benchmark:
          description: "End-to-end latency measurement across service chain"
          target_services: ["gateway", "protection", "analytics"]
          request_pattern: "sequential_chain"
          rps: 1000
          duration_seconds: 300
          success_criteria:
            p95_latency_ms: 10
            p99_latency_ms: 50
            success_rate: 99.9
            
        throughput_benchmark:
          description: "Maximum throughput validation (>100k RPS)"
          target_services: ["protection"]
          request_pattern: "parallel_flood"
          rps: 100000
          duration_seconds: 600
          success_criteria:
            sustained_rps: 100000
            cpu_utilization: 80
            memory_utilization: 70
            
        service_mesh_overhead:
          description: "Service mesh proxy latency measurement"
          target_services: ["gateway", "protection"]
          request_pattern: "direct_vs_mesh"
          rps: 10000
          duration_seconds: 180
          success_criteria:
            mesh_overhead_ms: 1
            proxy_cpu_overhead: 5
            
        connection_pooling_efficiency:
          description: "Connection pooling effectiveness testing"
          target_services: ["protection", "analytics"]
          request_pattern: "connection_reuse"
          rps: 5000
          duration_seconds: 240
          success_criteria:
            connection_reuse_rate: 95
            pool_utilization: 80
    EOT
    
    "stress_test_scenarios.yaml" = <<-EOT
      stress_test_scenarios:
        resource_exhaustion:
          description: "Test behavior under resource exhaustion"
          attack_vectors:
            - type: "cpu_exhaustion"
              target_utilization: 95
              duration_seconds: 300
              
            - type: "memory_exhaustion"
              target_utilization: 90
              duration_seconds: 180
              
            - type: "network_saturation"
              bandwidth_limit_mbps: 100
              duration_seconds: 240
              
        circuit_breaker_validation:
          description: "Circuit breaker behavior under extreme load"
          scenarios:
            - failure_rate: 50
              request_rate: 10000
              failure_duration_seconds: 60
              expected_breaker_activation: true
              
            - failure_rate: 10
              request_rate: 50000
              failure_duration_seconds: 30
              expected_breaker_activation: false
              
        service_mesh_partition:
          description: "Service mesh resilience during network partitions"
          partition_scenarios:
            - affected_services: ["protection"]
              partition_duration_seconds: 120
              expected_failover_time_ms: 100
              
            - affected_services: ["analytics", "monitor"]
              partition_duration_seconds: 180
              expected_degraded_service: true
              
        distributed_denial_of_service:
          description: "DDoS protection and rate limiting validation"
          attack_patterns:
            - type: "volumetric_attack"
              rps: 1000000
              duration_seconds: 60
              expected_rate_limiting: true
              
            - type: "application_layer_attack"
              rps: 50000
              malformed_requests: 30
              duration_seconds: 120
              expected_blocking: true
    EOT
    
    "performance_validation.yaml" = <<-EOT
      performance_validation:
        automated_regression_detection:
          enabled: true
          baseline_retention_days: 30
          regression_threshold_percent: 10
          alert_on_regression: true
          
        real_time_monitoring:
          metrics_collection_interval_seconds: 1
          dashboard_refresh_interval_seconds: 5
          alert_evaluation_interval_seconds: 10
          
        service_discovery_performance:
          dns_lookup_target_ms: 5
          endpoint_resolution_target_ms: 10
          cache_hit_rate_target: 95
          
        load_balancing_validation:
          fairness_coefficient_target: 0.95
          sticky_session_success_rate: 99
          failover_time_target_ms: 100
          
        otel_metrics_accuracy:
          trace_sampling_validation: true
          metric_accuracy_threshold: 99.5
          span_completeness_target: 99.9
          
        ci_cd_integration:
          performance_gate_enabled: true
          performance_regression_gate: true
          benchmark_on_pr: true
          performance_report_generation: true
    EOT
  }
}

# Services for benchmark and load testing
resource "kubernetes_service" "performance_benchmark_controller_service" {
  metadata {
    name      = "performance-benchmark-controller-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-benchmark-controller"
      type = "performance-testing"
    }
  }

  spec {
    selector = {
      app  = "cns-benchmark-controller"
      type = "performance-testing"
    }

    port {
      name        = "controller"
      port        = 8086
      target_port = 8086
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9096
      target_port = 9096
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

resource "kubernetes_service" "load_generation_agents_service" {
  metadata {
    name      = "load-generation-agents-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-load-generators"
      type = "load-testing"
    }
  }

  spec {
    selector = {
      app  = "cns-load-generators"
      type = "load-testing"
    }

    port {
      name        = "generator"
      port        = 8087
      target_port = 8087
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9097
      target_port = 9097
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# CronJob for continuous performance validation
resource "kubernetes_cron_job_v1" "continuous_performance_validation" {
  metadata {
    name      = "continuous-performance-validation"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-continuous-benchmark"
      type = "continuous-validation"
    }
  }

  spec {
    schedule = "0 */4 * * *"  # Run every 4 hours for continuous validation
    
    job_template {
      metadata {
        labels = {
          app  = "cns-continuous-benchmark"
          type = "continuous-validation"
        }
      }
      
      spec {
        template {
          metadata {
            labels = {
              app  = "cns-continuous-benchmark"
              type = "continuous-validation"
            }
          }
          
          spec {
            service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
            restart_policy       = "OnFailure"
            
            security_context {
              run_as_non_root = true
              run_as_user     = 1000
              fs_group        = 2000
            }

            container {
              name  = "continuous-benchmark"
              image = "cns-benchmark-controller:${var.environment}-latest"
              
              command = ["/app/continuous_benchmark"]
              args    = ["--controller", "performance-benchmark-controller-service:8086", "--full-suite"]
              
              resources {
                requests = {
                  cpu    = "500m"
                  memory = "1Gi"
                }
                limits = {
                  cpu    = "2000m"
                  memory = "2Gi"
                }
              }

              env {
                name  = "BENCHMARK_MODE"
                value = "continuous"
              }
              
              env {
                name  = "CONTROLLER_ENDPOINT"
                value = "performance-benchmark-controller-service:8086"
              }
              
              env {
                name  = "TARGET_RPS"
                value = "100000"
              }
              
              env {
                name  = "TARGET_LATENCY_MS"
                value = "10"
              }
              
              env {
                name  = "OTEL_EXPORTER_OTLP_ENDPOINT"
                value = "http://jaeger-collector:14268/api/traces"
              }

              security_context {
                allow_privilege_escalation = false
                read_only_root_filesystem  = true
                run_as_non_root           = true
                run_as_user               = 1000
                
                capabilities {
                  drop = ["ALL"]
                }
              }

              volume_mount {
                name       = "benchmark-config"
                mount_path = "/app/config/benchmark"
                read_only  = true
              }
            }

            volume {
              name = "benchmark-config"
              
              config_map {
                name = kubernetes_config_map.benchmark_stress_config.metadata[0].name
              }
            }
          }
        }
      }
    }
  }
}

# Performance Dashboard ConfigMap
resource "kubernetes_config_map" "performance_grafana_dashboard" {
  metadata {
    name      = "cns-performance-grafana-dashboard"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      grafana_dashboard = "1"
      dashboard_type    = "performance"
    }
  }

  data = {
    "cns-performance-dashboard.json" = jsonencode({
      annotations = {
        list = []
      }
      
      editable = true
      gnetId   = null
      graphTooltip = 0
      id       = null
      links    = []
      
      panels = [
        {
          # End-to-end latency panel
          title = "End-to-End Latency (Target: <10ms)"
          type  = "timeseries"
          gridPos = {
            h = 8
            w = 12
            x = 0
            y = 0
          }
          targets = [
            {
              expr         = "histogram_quantile(0.95, rate(cns_request_duration_seconds_bucket{service_chain=\"gateway->protection->analytics\"}[5m]))"
              legendFormat = "P95 End-to-End Latency"
            },
            {
              expr         = "histogram_quantile(0.99, rate(cns_request_duration_seconds_bucket{service_chain=\"gateway->protection->analytics\"}[5m]))"
              legendFormat = "P99 End-to-End Latency"
            }
          ]
          yAxes = [{
            unit = "s"
            max  = 0.05  # 50ms max for visualization
          }]
        },
        {
          # Throughput panel
          title = "Throughput (Target: >100k RPS)"
          type  = "timeseries"
          gridPos = {
            h = 8
            w = 12
            x = 12
            y = 0
          }
          targets = [
            {
              expr         = "sum(rate(cns_requests_total[5m]))"
              legendFormat = "Total RPS"
            },
            {
              expr         = "100000"
              legendFormat = "Target RPS"
            }
          ]
          yAxes = [{
            unit = "reqps"
            min  = 0
          }]
        },
        {
          # Service mesh overhead panel
          title = "Service Mesh Overhead (Target: <1ms)"
          type  = "timeseries"
          gridPos = {
            h = 8
            w = 12
            x = 0
            y = 8
          }
          targets = [
            {
              expr         = "histogram_quantile(0.95, rate(response_latency_ms_bucket{direction=\"inbound\"}[5m])) - histogram_quantile(0.95, rate(cns_direct_latency_ms_bucket[5m]))"
              legendFormat = "P95 Mesh Overhead"
            }
          ]
          yAxes = [{
            unit = "ms"
            max  = 5  # 5ms max for visualization
          }]
        },
        {
          # Circuit breaker status panel
          title = "Circuit Breaker Status"
          type  = "stat"
          gridPos = {
            h = 8
            w = 12
            x = 12
            y = 8
          }
          targets = [
            {
              expr = "sum(cns_circuit_breaker_state)"
            }
          ]
          thresholds = {
            steps = [
              { color = "green", value = 0 },
              { color = "red", value = 1 }
            ]
          }
        }
      ]
      
      refresh       = "5s"
      schemaVersion = 27
      style         = "dark"
      tags          = ["cns", "performance", "benchmarking"]
      time = {
        from = "now-1h"
        to   = "now"
      }
      title   = "CNS Performance Dashboard"
      uid     = "cns-performance"
      version = 1
    })
  }
}