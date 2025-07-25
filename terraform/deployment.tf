# CNS Deployment with security hardening and securityContext
resource "kubernetes_deployment" "cns_deployment" {
  metadata {
    name      = "cns-deployment"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      version     = "1.0.0"
      environment = var.environment
      security    = "hardened"
      securityContext = "enabled"
    }
    
    annotations = {
      "security.terraform.io/securityContext" = "enforced"
      "cis.compliance/level" = "1"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app = "cns"
      }
    }

    template {
      metadata {
        labels = {
          app     = "cns"
          version = "1.0.0"
        }
        
        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9090"
          "prometheus.io/path"   = "/metrics"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
        
        # Security context for pod
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          fs_group        = 2000
          
          seccomp_profile {
            type = "RuntimeDefault"
          }
        }

        # Init container to apply security patches
        init_container {
          name  = "security-patches"
          image = "python:3.11-slim"
          
          command = ["python", "/config/security_patches.py"]
          
          volume_mount {
            name       = "config"
            mount_path = "/config"
          }
          
          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            
            capabilities {
              drop = ["ALL"]
            }
          }
        }

        container {
          name  = "cns"
          image = "cns:${var.environment}-latest"
          
          # Resource limits enforcing 80/20 security patches
          resources {
            requests = {
              cpu    = "500m"
              memory = "1Gi"
            }
            limits = {
              cpu    = "2000m"      # Max 2 CPU cores (80% of typical node)
              memory = "2048Mi"     # Max 2GB RAM as per security patches
            }
          }

          # Environment variables
          env {
            name  = "ENVIRONMENT"
            value = var.environment
          }
          
          env {
            name  = "MAX_THREADS"
            value = "10"
          }
          
          env {
            name  = "MAX_PROCESSES"
            value = "5"
          }
          
          # Enhanced Protection Configuration
          env {
            name  = "ENHANCED_PROTECTION_ENABLED"
            value = "true"
          }
          
          env {
            name  = "VOLATILITY_THRESHOLD_PERCENT"
            value = "0.02"
          }
          
          env {
            name  = "VOLATILITY_WINDOW_MS"
            value = "60000"
          }
          
          env {
            name  = "CIRCUIT_BREAKER_ENABLED"
            value = "true"
          }
          
          env {
            name  = "POSITION_CORRELATION_TRACKING"
            value = "true"
          }
          
          env {
            name  = "MANIPULATION_DETECTION"
            value = "true"
          }
          
          # OTEL Configuration for Enhanced Metrics
          env {
            name  = "OTEL_EXPORTER_OTLP_ENDPOINT"
            value = "http://jaeger-collector:14268/api/traces"
          }
          
          env {
            name  = "OTEL_RESOURCE_ATTRIBUTES"
            value = "service.name=cns-enhanced-protection,service.version=1.0.0"
          }
          
          env {
            name  = "ENHANCED_METRICS_ENABLED"
            value = "true"
          }
          
          env {
            name = "API_KEY"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.cns_secret.metadata[0].name
                key  = "api_key"
              }
            }
          }

          # Ports
          port {
            name           = "http"
            container_port = 8080
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9090
            protocol       = "TCP"
          }

          # Health checks
          liveness_probe {
            http_get {
              path = "/health"
              port = "http"
            }
            
            initial_delay_seconds = 30
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "http"
            }
            
            initial_delay_seconds = 10
            period_seconds        = 5
            timeout_seconds       = 3
            failure_threshold     = 3
          }

          # Security context for container
          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            run_as_user               = 1000
            
            capabilities {
              drop = ["ALL"]
              add  = ["NET_BIND_SERVICE"]
            }
          }

          # Volume mounts
          volume_mount {
            name       = "config"
            mount_path = "/app/config"
            read_only  = true
          }
          
          volume_mount {
            name       = "data"
            mount_path = "/app/data"
          }
          
          volume_mount {
            name       = "tmp"
            mount_path = "/tmp"
          }
        }

        # Volumes
        volume {
          name = "config"
          
          config_map {
            name = kubernetes_config_map.cns_config.metadata[0].name
          }
        }
        
        volume {
          name = "data"
          
          persistent_volume_claim {
            claim_name = kubernetes_persistent_volume_claim.cns_data.metadata[0].name
          }
        }
        
        volume {
          name = "tmp"
          
          empty_dir {
            medium     = "Memory"
            size_limit = "1Gi"
          }
        }

        # Pod affinity for high availability
        affinity {
          pod_anti_affinity {
            preferred_during_scheduling_ignored_during_execution {
              weight = 100
              
              pod_affinity_term {
                label_selector {
                  match_expressions {
                    key      = "app"
                    operator = "In"
                    values   = ["cns"]
                  }
                }
                
                topology_key = "kubernetes.io/hostname"
              }
            }
          }
        }

        # Topology spread constraints for even distribution
        topology_spread_constraint {
          max_skew           = 1
          topology_key       = "topology.kubernetes.io/zone"
          when_unsatisfiable = "DoNotSchedule"
          
          label_selector {
            match_labels = {
              app = "cns"
            }
          }
        }
      }
    }
  }
}

# Adversarial Testing CronJob for continuous validation
resource "kubernetes_cron_job_v1" "cns_adversarial_testing" {
  metadata {
    name      = "cns-adversarial-testing"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app     = "cns-adversarial-test"
      type    = "security-validation"
      version = "1.0.0"
    }
    
    annotations = {
      "security.terraform.io/adversarial-testing" = "enabled"
      "cis.compliance/testing" = "continuous"
    }
  }

  spec {
    schedule = "*/30 * * * *"  # Run every 30 minutes
    
    job_template {
      metadata {
        labels = {
          app  = "cns-adversarial-test"
          type = "security-validation"
        }
      }
      
      spec {
        template {
          metadata {
            labels = {
              app  = "cns-adversarial-test"
              type = "security-validation"
            }
          }
          
          spec {
            service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
            restart_policy       = "OnFailure"
            
            # Security context for adversarial testing pod
            security_context {
              run_as_non_root = true
              run_as_user     = 1000
              fs_group        = 2000
              
              seccomp_profile {
                type = "RuntimeDefault"
              }
            }

            container {
              name  = "adversarial-test"
              image = "cns-adversarial:${var.environment}-latest"
              
              # Resource limits for testing container
              resources {
                requests = {
                  cpu    = "100m"
                  memory = "256Mi"
                }
                limits = {
                  cpu    = "1000m"
                  memory = "1Gi"
                }
              }

              # Environment variables for adversarial testing
              env {
                name  = "TARGET_SERVICE"
                value = "cns-service:8080"
              }
              
              env {
                name  = "TEST_SUITE"
                value = "enhanced_adversarial"
              }
              
              env {
                name  = "REQUIRED_SURVIVAL_RATE"
                value = "90"
              }
              
              env {
                name  = "OTEL_EXPORTER_OTLP_ENDPOINT"
                value = "http://jaeger-collector:14268/api/traces"
              }
              
              env {
                name  = "OTEL_RESOURCE_ATTRIBUTES"
                value = "service.name=cns-adversarial-test,service.version=1.0.0"
              }

              # Command to run the adversarial test suite
              command = ["/app/test_enhanced_adversarial"]
              args    = ["--target", "$(TARGET_SERVICE)", "--survival-rate", "$(REQUIRED_SURVIVAL_RATE)"]

              # Security context for container
              security_context {
                allow_privilege_escalation = false
                read_only_root_filesystem  = true
                run_as_non_root           = true
                run_as_user               = 1000
                
                capabilities {
                  drop = ["ALL"]
                }
              }

              # Volume mounts for test data
              volume_mount {
                name       = "test-data"
                mount_path = "/app/test-data"
                read_only  = true
              }
              
              volume_mount {
                name       = "tmp"
                mount_path = "/tmp"
              }
            }

            # Volumes for adversarial testing
            volume {
              name = "test-data"
              
              config_map {
                name = kubernetes_config_map.cns_adversarial_config.metadata[0].name
              }
            }
            
            volume {
              name = "tmp"
              
              empty_dir {
                medium     = "Memory"
                size_limit = "512Mi"
              }
            }
          }
        }
      }
    }
  }
}

# ConfigMap for adversarial test configuration
resource "kubernetes_config_map" "cns_adversarial_config" {
  metadata {
    name      = "cns-adversarial-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns-adversarial-test"
      type = "configuration"
    }
  }

  data = {
    "test_config.yaml" = <<-EOT
      adversarial_tests:
        flash_crash_protection: true
        position_manipulation_protection: true
        circuit_breaker_race_protection: true
        rapid_fire_protection: true
        correlation_manipulation_protection: true
        timestamp_manipulation_protection: true
        volatility_cascade_protection: true
        memory_exhaustion_protection: true
        performance_degradation_protection: true
        combined_attack_protection: true
      
      test_parameters:
        volatility_threshold: 0.02
        correlation_limit: 0.25
        circuit_breaker_timeout: 30
        rapid_fire_interval: 1000
        survival_rate_target: 90.0
        
      otel_config:
        metrics_enabled: true
        traces_enabled: true
        logs_enabled: true
        export_interval: 10
    EOT
    
    "attack_vectors.json" = <<-EOT
      {
        "flash_crash": {
          "price_drop_percent": 6.0,
          "duration_seconds": 30,
          "expected_block": true
        },
        "position_manipulation": {
          "max_correlated_positions": 20,
          "position_size": 25.0,
          "expected_block_threshold": 10
        },
        "circuit_breaker_race": {
          "concurrent_requests": 100,
          "timing_window_ms": 1,
          "expected_atomic_behavior": true
        },
        "rapid_fire_attack": {
          "requests_per_second": 10000,
          "duration_seconds": 10,
          "expected_throttling": true
        }
      }
    EOT
  }
}