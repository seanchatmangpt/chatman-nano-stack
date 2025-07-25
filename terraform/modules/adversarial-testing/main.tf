# Distributed Adversarial Testing Infrastructure for K8s Inter-Service Communication
# Tests service-to-service resilience under coordinated attacks with 90%+ survival rate requirement

# Distributed Adversarial Test Coordinator 
resource "kubernetes_deployment" "distributed_adversarial_coordinator" {
  metadata {
    name      = "distributed-adversarial-coordinator"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-adversarial-coordinator"
      type        = "distributed-testing"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "chaos.engineering/enabled" = "true"
    }
  }

  spec {
    replicas = 1  # Single coordinator for distributed tests

    selector {
      match_labels = {
        app  = "cns-adversarial-coordinator"
        type = "distributed-testing"
      }
    }

    template {
      metadata {
        labels = {
          app  = "cns-adversarial-coordinator"
          type = "distributed-testing"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9094"
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
          name  = "adversarial-coordinator"
          image = "cns-distributed-adversarial:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "200m"
              memory = "512Mi"
            }
            limits = {
              cpu    = "1000m"
              memory = "1Gi"
            }
          }

          env {
            name  = "SERVICE_TYPE"
            value = "adversarial-coordinator"
          }
          
          env {
            name  = "COORDINATOR_PORT"
            value = "8084"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9094"
          }
          
          # Target services for distributed attacks
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
          
          # Attack configuration
          env {
            name  = "REQUIRED_SURVIVAL_RATE"
            value = "90"
          }
          
          env {
            name  = "DISTRIBUTED_ATTACK_VECTORS"
            value = "cross_service_flash_crash,service_mesh_partition,circuit_breaker_cascade,distributed_manipulation,network_congestion,service_discovery_poison"
          }
          
          env {
            name  = "ATTACK_PARALLELISM"
            value = "5"
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
            name           = "coordinator"
            container_port = 8084
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9094
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "coordinator"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "coordinator"
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
            name       = "attack-config"
            mount_path = "/app/config/attacks"
            read_only  = true
          }
        }

        volume {
          name = "attack-config"
          
          config_map {
            name = kubernetes_config_map.distributed_attack_config.metadata[0].name
          }
        }
      }
    }
  }
}

# Distributed Attack Agents (multiple pods for parallel attacks)
resource "kubernetes_deployment" "distributed_attack_agents" {
  metadata {
    name      = "distributed-attack-agents"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-attack-agents"
      type        = "attack-simulation"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "chaos.engineering/enabled" = "true"
    }
  }

  spec {
    replicas = 5  # Multiple attack agents for parallel testing

    selector {
      match_labels = {
        app  = "cns-attack-agents"
        type = "attack-simulation"
      }
    }

    template {
      metadata {
        labels = {
          app  = "cns-attack-agents"
          type = "attack-simulation"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9095"
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
          name  = "attack-agent"
          image = "cns-attack-agent:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "100m"
              memory = "256Mi"
            }
            limits = {
              cpu    = "500m"
              memory = "512Mi"
            }
          }

          env {
            name  = "SERVICE_TYPE"
            value = "attack-agent"
          }
          
          env {
            name  = "AGENT_PORT"
            value = "8085"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9095"
          }
          
          env {
            name  = "COORDINATOR_ENDPOINT"
            value = "distributed-adversarial-coordinator-service:8084"
          }
          
          # Unique agent ID for distributed coordination
          env {
            name = "AGENT_ID"
            value_from {
              field_ref {
                field_path = "metadata.name"
              }
            }
          }
          
          # Pod IP for direct communication during mesh partitioning tests
          env {
            name = "POD_IP"
            value_from {
              field_ref {
                field_path = "status.podIP"
              }
            }
          }

          port {
            name           = "agent"
            container_port = 8085
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9095
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "agent"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "agent"
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
            name       = "attack-config"
            mount_path = "/app/config/attacks"
            read_only  = true
          }
        }

        volume {
          name = "attack-config"
          
          config_map {
            name = kubernetes_config_map.distributed_attack_config.metadata[0].name
          }
        }
      }
    }
  }
}

# Configuration for distributed attack scenarios
resource "kubernetes_config_map" "distributed_attack_config" {
  metadata {
    name      = "distributed-attack-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-adversarial-test"
      type = "attack-configuration"
    }
  }

  data = {
    "distributed_attacks.yaml" = <<-EOT
      distributed_attack_scenarios:
        cross_service_flash_crash:
          description: "Simulate flash crash across multiple services"
          target_services: ["protection", "gateway", "analytics"]
          attack_type: "coordinated_price_manipulation"
          parameters:
            price_drop_percent: 8.0
            attack_duration_seconds: 45
            coordination_delay_ms: 100
            expected_survival_rate: 90
            
        service_mesh_partition:
          description: "Test resilience during service mesh network partition"
          target_services: ["protection", "gateway"]
          attack_type: "network_partition_simulation"
          parameters:
            partition_duration_seconds: 30
            affected_percentage: 50
            reconnection_jitter_ms: 200
            expected_circuit_breaker_activation: true
            
        circuit_breaker_cascade:
          description: "Test circuit breaker coordination under cascade failure"
          target_services: ["protection", "analytics", "monitor"]
          attack_type: "cascade_failure_simulation"
          parameters:
            initial_failure_service: "analytics"
            cascade_propagation_delay_ms: 500
            failure_recovery_time_seconds: 60
            expected_isolation: true
            
        distributed_manipulation:
          description: "Coordinated position manipulation across services"
          target_services: ["protection", "gateway"]
          attack_type: "multi_service_manipulation"
          parameters:
            attack_vectors: ["position_flooding", "correlation_exploit", "timestamp_skew"]
            attack_intensity: "high"
            coordination_pattern: "round_robin"
            expected_detection_rate: 95
            
        network_congestion:
          description: "Simulate network congestion between services"
          target_services: ["gateway", "protection", "analytics"]
          attack_type: "network_stress_test"
          parameters:
            bandwidth_limit_mbps: 10
            latency_injection_ms: 100
            packet_loss_percentage: 5
            duration_seconds: 120
            expected_timeout_handling: true
            
        service_discovery_poison:
          description: "Test service discovery under DNS/endpoint poisoning"
          target_services: ["gateway"]
          attack_type: "service_discovery_attack"
          parameters:
            poison_endpoints: ["cns-protection-service", "cns-analytics-service"]
            redirect_percentage: 25
            attack_duration_seconds: 60
            expected_fallback_behavior: true
    EOT
    
    "attack_coordination.yaml" = <<-EOT
      coordination:
        attack_orchestration:
          parallel_attacks: 3
          attack_interval_seconds: 30
          coordination_timeout_seconds: 300
          failure_threshold_percentage: 10
          
        service_targeting:
          protection_service:
            weight: 40  # Most critical service
            attack_vectors: ["flash_crash", "manipulation", "circuit_breaker"]
            
          gateway_service:
            weight: 30  # Entry point attacks
            attack_vectors: ["rate_limit_bypass", "network_partition", "service_discovery"]
            
          analytics_service:
            weight: 20  # Data integrity attacks
            attack_vectors: ["data_corruption", "cascade_failure", "resource_exhaustion"]
            
          monitor_service:
            weight: 10  # Observability attacks
            attack_vectors: ["alert_suppression", "metric_manipulation"]
            
        success_criteria:
          minimum_survival_rate: 90
          maximum_recovery_time_seconds: 60
          required_circuit_breaker_activation: true
          expected_service_degradation: "graceful"
          otel_trace_continuity: true
    EOT
    
    "service_mesh_attacks.yaml" = <<-EOT
      service_mesh_specific_attacks:
        mtls_bypass_attempt:
          description: "Attempt to bypass mTLS encryption"
          target: "service_mesh_proxy"
          method: "certificate_spoofing"
          expected_outcome: "blocked"
          
        proxy_resource_exhaustion:
          description: "Exhaust service mesh proxy resources"
          target: "linkerd_proxy"
          method: "connection_flooding"
          parameters:
            concurrent_connections: 10000
            connection_rate_per_second: 1000
          expected_outcome: "rate_limited"
          
        sidecar_injection_bypass:
          description: "Test security without sidecar injection"
          target: "pod_without_sidecar"
          method: "direct_service_access"
          expected_outcome: "network_policy_blocked"
          
        traffic_split_manipulation:
          description: "Manipulate traffic split configuration"
          target: "traffic_split_config"
          method: "config_injection"
          expected_outcome: "admission_controller_blocked"
    EOT
  }
}

# Service for distributed adversarial coordinator
resource "kubernetes_service" "distributed_adversarial_coordinator_service" {
  metadata {
    name      = "distributed-adversarial-coordinator-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-adversarial-coordinator"
      type = "distributed-testing"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app  = "cns-adversarial-coordinator"
      type = "distributed-testing"
    }

    port {
      name        = "coordinator"
      port        = 8084
      target_port = 8084
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9094
      target_port = 9094
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Service for attack agents
resource "kubernetes_service" "distributed_attack_agents_service" {
  metadata {
    name      = "distributed-attack-agents-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-attack-agents"
      type = "attack-simulation"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app  = "cns-attack-agents"
      type = "attack-simulation"
    }

    port {
      name        = "agent"
      port        = 8085
      target_port = 8085
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9095
      target_port = 9095
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# CronJob for continuous distributed adversarial testing
resource "kubernetes_cron_job_v1" "continuous_distributed_testing" {
  metadata {
    name      = "continuous-distributed-testing"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-continuous-adversarial"
      type = "continuous-validation"
    }
    
    annotations = {
      "chaos.engineering/enabled" = "true"
      "distributed.testing/enabled" = "true"
    }
  }

  spec {
    schedule = "*/15 * * * *"  # Run every 15 minutes for continuous validation
    
    job_template {
      metadata {
        labels = {
          app  = "cns-continuous-adversarial"
          type = "continuous-validation"
        }
      }
      
      spec {
        template {
          metadata {
            labels = {
              app  = "cns-continuous-adversarial"
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
              name  = "continuous-adversarial-test"
              image = "cns-distributed-adversarial:${var.environment}-latest"
              
              command = ["/app/continuous_distributed_test"]
              args    = ["--coordinator", "distributed-adversarial-coordinator-service:8084", "--survival-rate", "90"]
              
              resources {
                requests = {
                  cpu    = "100m"
                  memory = "256Mi"
                }
                limits = {
                  cpu    = "500m"
                  memory = "512Mi"
                }
              }

              env {
                name  = "TEST_MODE"
                value = "continuous"
              }
              
              env {
                name  = "COORDINATOR_ENDPOINT"
                value = "distributed-adversarial-coordinator-service:8084"
              }
              
              env {
                name  = "REQUIRED_SURVIVAL_RATE"
                value = "90"
              }
              
              env {
                name  = "SERVICE_MESH_ENABLED"
                value = var.enable_service_mesh ? "true" : "false"
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
                name       = "attack-config"
                mount_path = "/app/config/attacks"
                read_only  = true
              }
            }

            volume {
              name = "attack-config"
              
              config_map {
                name = kubernetes_config_map.distributed_attack_config.metadata[0].name
              }
            }
          }
        }
      }
    }
  }
}

# Network Policy for adversarial testing (allow attacks but monitor)
resource "kubernetes_network_policy" "adversarial_testing_policy" {
  metadata {
    name      = "adversarial-testing-policy"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      "network-policy" = "adversarial-testing"
      "security"       = "testing"
    }
  }

  spec {
    pod_selector {
      match_labels = {
        type = "distributed-testing"
      }
    }

    # Allow ingress from attack agents
    ingress {
      from {
        pod_selector {
          match_labels = {
            type = "attack-simulation"
          }
        }
      }
      
      ports {
        port     = "8084"
        protocol = "TCP"
      }
    }

    # Allow egress to all CNS services for testing
    egress {
      to {
        pod_selector {
          match_labels = {
            app = "cns-protection"
          }
        }
      }
      to {
        pod_selector {
          match_labels = {
            app = "cns-gateway"
          }
        }
      }
      to {
        pod_selector {
          match_labels = {
            app = "cns-analytics"
          }
        }
      }
      to {
        pod_selector {
          match_labels = {
            app = "cns-monitor"
          }
        }
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      ports {
        port     = "8081"
        protocol = "TCP"
      }
      ports {
        port     = "8082"
        protocol = "TCP"
      }
      ports {
        port     = "8083"
        protocol = "TCP"
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}