# Comprehensive K8s Validation with SWARM Coordination
# Orchestrates full system validation with 90%+ adversarial survival rate requirement

# SWARM Validation Orchestrator - Central coordination for all validation activities
resource "kubernetes_deployment" "swarm_validation_orchestrator" {
  metadata {
    name      = "swarm-validation-orchestrator"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-swarm-orchestrator"
      type        = "validation-coordination"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "swarm.orchestration/enabled" = "true"
    }
  }

  spec {
    replicas = 1  # Single orchestrator for centralized coordination

    selector {
      match_labels = {
        app  = "cns-swarm-orchestrator"
        type = "validation-coordination"
      }
    }

    template {
      metadata {
        labels = {
          app  = "cns-swarm-orchestrator"
          type = "validation-coordination"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9098"
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
          name  = "swarm-orchestrator"
          image = "cns-swarm-orchestrator:${var.environment}-latest"
          
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
            value = "swarm-orchestrator"
          }
          
          env {
            name  = "ORCHESTRATOR_PORT"
            value = "8088"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9098"
          }
          
          # Validation targets and requirements
          env {
            name  = "REQUIRED_SURVIVAL_RATE"
            value = "90"
          }
          
          env {
            name  = "TARGET_LATENCY_MS"
            value = "10"
          }
          
          env {
            name  = "TARGET_RPS"
            value = "100000"
          }
          
          env {
            name  = "SERVICE_MESH_OVERHEAD_TARGET_MS"
            value = "1"
          }
          
          # Service endpoints for orchestration
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
          
          # Validation component endpoints
          env {
            name  = "ADVERSARIAL_COORDINATOR_ENDPOINT"
            value = "distributed-adversarial-coordinator-service:8084"
          }
          
          env {
            name  = "BENCHMARK_CONTROLLER_ENDPOINT"
            value = "performance-benchmark-controller-service:8086"
          }
          
          env {
            name  = "LOAD_GENERATORS_ENDPOINT"
            value = "load-generation-agents-service:8087"
          }
          
          # Validation phases
          env {
            name  = "VALIDATION_PHASES"
            value = "unit_tests,integration_tests,benchmark_tests,adversarial_tests,stress_tests,endurance_tests"
          }
          
          env {
            name  = "PARALLEL_VALIDATION_ENABLED"
            value = "true"
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
            name           = "orchestrator"
            container_port = 8088
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9098
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "orchestrator"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "orchestrator"
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
            name       = "validation-config"
            mount_path = "/app/config/validation"
            read_only  = true
          }
          
          volume_mount {
            name       = "test-results"
            mount_path = "/app/results"
          }
        }

        volume {
          name = "validation-config"
          
          config_map {
            name = kubernetes_config_map.comprehensive_validation_config.metadata[0].name
          }
        }
        
        volume {
          name = "test-results"
          
          empty_dir {
            size_limit = "10Gi"
          }
        }
      }
    }
  }
}

# Comprehensive Validation Configuration
resource "kubernetes_config_map" "comprehensive_validation_config" {
  metadata {
    name      = "comprehensive-validation-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-swarm-validation"
      type = "validation-configuration"
    }
  }

  data = {
    "validation_orchestration.yaml" = <<-EOT
      validation_orchestration:
        strategy: "swarm_coordinated"
        parallel_execution: true
        failure_fast: false
        comprehensive_reporting: true
        
        phases:
          unit_tests:
            priority: 1
            timeout_minutes: 15
            parallel_agents: 1
            success_criteria:
              pass_rate: 100
              execution_time_minutes: 10
              
          integration_tests:
            priority: 2
            timeout_minutes: 30
            parallel_agents: 3
            success_criteria:
              pass_rate: 100
              service_communication_success: 100
              
          benchmark_tests:
            priority: 3
            timeout_minutes: 45
            parallel_agents: 5
            success_criteria:
              target_rps: 100000
              p95_latency_ms: 10
              p99_latency_ms: 50
              
          adversarial_tests:
            priority: 4
            timeout_minutes: 60
            parallel_agents: 5
            success_criteria:
              survival_rate: 90
              attack_detection_rate: 95
              recovery_time_seconds: 60
              
          stress_tests:
            priority: 5
            timeout_minutes: 90
            parallel_agents: 10
            success_criteria:
              sustained_load_minutes: 60
              degradation_threshold: 20
              circuit_breaker_activation: true
              
          endurance_tests:
            priority: 6
            timeout_minutes: 240
            parallel_agents: 3
            success_criteria:
              uptime_hours: 4
              memory_leak_detection: false
              performance_degradation: 5
              
        coordination:
          agent_synchronization: true
          resource_sharing: false
          result_aggregation: true
          real_time_monitoring: true
          
        reporting:
          otel_integration: true
          prometheus_metrics: true
          grafana_dashboards: true
          junit_xml_export: true
          markdown_report: true
    EOT
    
    "swarm_coordination.yaml" = <<-EOT
      swarm_coordination:
        topology: "hierarchical"
        max_agents: 20
        agent_types:
          orchestrator:
            count: 1
            role: "coordination"
            capabilities: ["phase_management", "result_aggregation", "decision_making"]
            
          test_executors:
            count: 5
            role: "execution"
            capabilities: ["unit_testing", "integration_testing", "benchmark_execution"]
            
          adversarial_agents:
            count: 5
            role: "security_testing"
            capabilities: ["attack_simulation", "vulnerability_scanning", "chaos_engineering"]
            
          load_generators:
            count: 10
            role: "performance_testing"
            capabilities: ["load_generation", "stress_testing", "endurance_testing"]
            
        communication:
          protocol: "grpc"
          compression: true
          encryption: true
          heartbeat_interval_seconds: 10
          timeout_seconds: 30
          
        fault_tolerance:
          agent_failure_detection: true
          automatic_recovery: true
          work_redistribution: true
          checkpoint_interval_minutes: 5
          
        resource_management:
          cpu_allocation:
            orchestrator: "4000m"
            test_executors: "2000m"
            adversarial_agents: "1000m"
            load_generators: "4000m"
            
          memory_allocation:
            orchestrator: "4Gi"
            test_executors: "2Gi"
            adversarial_agents: "1Gi"
            load_generators: "4Gi"
    EOT
    
    "validation_scenarios.yaml" = <<-EOT
      validation_scenarios:
        scenario_1_enhanced_protection_validation:
          description: "Comprehensive enhanced protection system validation"
          components: ["protection", "gateway", "analytics", "monitor"]
          test_types: ["unit", "integration", "adversarial"]
          duration_minutes: 60
          success_criteria:
            enhanced_protection_tests: "12/12 passed"
            adversarial_survival_rate: 90
            circuit_breaker_functionality: true
            
        scenario_2_service_mesh_performance:
          description: "Service mesh performance and reliability validation"
          components: ["linkerd", "service_discovery", "load_balancing"]
          test_types: ["benchmark", "stress"]
          duration_minutes: 90
          success_criteria:
            service_discovery_latency_ms: 20
            load_balancing_overhead_ms: 1
            mesh_proxy_overhead_ms: 1
            
        scenario_3_distributed_adversarial_coordination:
          description: "Multi-service coordinated adversarial testing"
          components: ["all_services", "service_mesh", "network"]
          test_types: ["adversarial", "chaos"]
          duration_minutes: 120
          success_criteria:
            cross_service_attack_survival: 90
            service_isolation_effectiveness: 95
            recovery_coordination_time_ms: 100
            
        scenario_4_extreme_load_validation:
          description: "System behavior under extreme load conditions"
          components: ["all_services"]
          test_types: ["stress", "endurance"]
          duration_minutes: 240
          success_criteria:
            sustained_100k_rps: true
            resource_utilization_cpu: 80
            resource_utilization_memory: 70
            graceful_degradation: true
            
        scenario_5_end_to_end_production_simulation:
          description: "Full production workload simulation"
          components: ["all_services", "monitoring", "alerting"]
          test_types: ["integration", "benchmark", "adversarial", "endurance"]
          duration_minutes: 360
          success_criteria:
            overall_system_availability: 99.9
            end_to_end_latency_p95_ms: 10
            adversarial_survival_rate: 90
            monitoring_coverage: 100
    EOT
    
    "success_criteria.yaml" = <<-EOT
      success_criteria:
        system_level:
          overall_test_pass_rate: 95
          critical_test_pass_rate: 100
          performance_regression_threshold: 10
          security_vulnerability_count: 0
          
        service_level:
          protection_service:
            availability: 99.9
            response_time_p95_ms: 5
            adversarial_survival_rate: 95
            enhanced_protection_coverage: 100
            
          gateway_service:
            availability: 99.9
            response_time_p95_ms: 10
            rate_limiting_effectiveness: 100
            traffic_routing_accuracy: 99
            
          analytics_service:
            availability: 99.5
            response_time_p95_ms: 50
            data_processing_accuracy: 99.9
            metric_collection_coverage: 100
            
          monitor_service:
            availability: 99.5
            response_time_p95_ms: 100
            alert_detection_accuracy: 95
            health_check_coverage: 100
            
        infrastructure_level:
          service_mesh:
            proxy_overhead_ms: 1
            mtls_success_rate: 100
            traffic_split_accuracy: 99
            circuit_breaker_responsiveness_ms: 100
            
          kubernetes:
            pod_startup_time_seconds: 30
            service_discovery_time_ms: 20
            load_balancing_fairness: 95
            resource_utilization_efficiency: 80
            
        security_level:
          adversarial_testing:
            attack_vector_coverage: 100
            survival_rate_target: 90
            detection_rate_target: 95
            false_positive_rate: 5
            
          compliance:
            cis_compliance_score: 100
            security_policies_enforced: 100
            vulnerability_scan_pass: true
            penetration_test_pass: true
    EOT
  }
}

# Validation Results Aggregator Service
resource "kubernetes_service" "swarm_validation_orchestrator_service" {
  metadata {
    name      = "swarm-validation-orchestrator-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-swarm-orchestrator"
      type = "validation-coordination"
    }
  }

  spec {
    selector = {
      app  = "cns-swarm-orchestrator"
      type = "validation-coordination"
    }

    port {
      name        = "orchestrator"
      port        = 8088
      target_port = 8088
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9098
      target_port = 9098
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Master Validation Job - Runs complete system validation
resource "kubernetes_job" "master_validation_job" {
  metadata {
    name      = "master-validation-job"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-master-validation"
      type = "comprehensive-validation"
    }
    
    annotations = {
      "swarm.orchestration/enabled" = "true"
      "validation.comprehensive/enabled" = "true"
    }
  }

  spec {
    template {
      metadata {
        labels = {
          app  = "cns-master-validation"
          type = "comprehensive-validation"
        }
      }
      
      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
        restart_policy       = "Never"
        
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          fs_group        = 2000
        }

        container {
          name  = "master-validator"
          image = "cns-swarm-orchestrator:${var.environment}-latest"
          
          command = ["/app/master_validation"]
          args    = ["--orchestrator", "swarm-validation-orchestrator-service:8088", "--full-suite", "--survival-rate", "90"]
          
          resources {
            requests = {
              cpu    = "2000m"
              memory = "4Gi"
            }
            limits = {
              cpu    = "8000m"
              memory = "8Gi"
            }
          }

          env {
            name  = "VALIDATION_MODE"
            value = "comprehensive"
          }
            
          env {
            name  = "ORCHESTRATOR_ENDPOINT"
            value = "swarm-validation-orchestrator-service:8088"
          }
          
          env {
            name  = "REQUIRED_SURVIVAL_RATE"
            value = "90"
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
            name  = "SERVICE_MESH_ENABLED"
            value = var.enable_service_mesh ? "true" : "false"
          }
          
          env {
            name  = "VALIDATION_TIMEOUT_HOURS"
            value = "8"
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
            name       = "validation-config"
            mount_path = "/app/config/validation"
            read_only  = true
          }
          
          volume_mount {
            name       = "test-results"
            mount_path = "/app/results"
          }
        }

        volume {
          name = "validation-config"
          
          config_map {
            name = kubernetes_config_map.comprehensive_validation_config.metadata[0].name
          }
        }
        
        volume {
          name = "test-results"
          
          persistent_volume_claim {
            claim_name = kubernetes_persistent_volume_claim.validation_results_storage.metadata[0].name
          }
        }
      }
    }
    
    backoff_limit = 2
  }
}

# PVC for storing validation results
resource "kubernetes_persistent_volume_claim" "validation_results_storage" {
  metadata {
    name      = "validation-results-storage"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    
    resources {
      requests = {
        storage = "50Gi"
      }
    }
    
    storage_class_name = "standard"
  }
}

# Service Monitor for validation metrics
resource "kubernetes_manifest" "validation_metrics_monitor" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    
    metadata = {
      name      = "cns-validation-metrics"
      namespace = kubernetes_namespace.cns.metadata[0].name
      
      labels = {
        app = "cns-validation-monitor"
        monitoring = "swarm-coordination"
      }
    }
    
    spec = {
      selector = {
        matchLabels = {
          type = "validation-coordination"
        }
      }
      
      endpoints = [{
        port     = "metrics"
        interval = "10s"
        path     = "/metrics"
        
        metricRelabelings = [
          {
            sourceLabels = ["__name__"]
            regex        = "cns_validation_.*|cns_swarm_.*|cns_orchestration_.*"
            action       = "keep"
          }
        ]
      }]
    }
  }
}