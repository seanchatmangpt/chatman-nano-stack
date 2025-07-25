# CNS Multi-Service Deployment with Service Mesh Integration
# Implements distributed architecture with inter-service communication

# CNS Protection Service - Core enhanced protection system
resource "kubernetes_deployment" "cns_protection_service" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-protection"
      service     = "protection"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "config.linkerd.io/proxy-cpu-limit" = "1"
      "config.linkerd.io/proxy-memory-limit" = "250Mi"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app     = "cns-protection"
        service = "protection"
      }
    }

    template {
      metadata {
        labels = {
          app     = "cns-protection"
          service = "protection"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9090"
          "prometheus.io/path"   = "/metrics"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
        
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          fs_group        = 2000
          
          seccomp_profile {
            type = "RuntimeDefault"
          }
        }

        container {
          name  = "cns-protection"
          image = "cns-protection:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "500m"
              memory = "1Gi"
            }
            limits = {
              cpu    = "2000m"
              memory = "2048Mi"
            }
          }

          # Enhanced Protection Environment Variables
          env {
            name  = "SERVICE_TYPE"
            value = "protection"
          }
          
          env {
            name  = "GRPC_PORT"
            value = "8080"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9090"
          }
          
          env {
            name  = "ENHANCED_PROTECTION_ENABLED"
            value = "true"
          }
          
          env {
            name  = "VOLATILITY_THRESHOLD_PERCENT"
            value = "0.02"
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
            name  = "SERVICE_MESH_ENABLED"
            value = var.enable_service_mesh ? "true" : "false"
          }
          
          env {
            name  = "OTEL_EXPORTER_OTLP_ENDPOINT"
            value = "http://jaeger-collector:14268/api/traces"
          }

          port {
            name           = "grpc"
            container_port = 8080
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9090
            protocol       = "TCP"
          }

          liveness_probe {
            grpc {
              port    = 8080
              service = "health"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            grpc {
              port    = 8080
              service = "ready"
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
            name       = "mesh-config"
            mount_path = "/app/config/mesh"
            read_only  = true
          }
        }

        volume {
          name = "mesh-config"
          
          config_map {
            name = var.enable_service_mesh ? kubernetes_config_map.cns_service_mesh_config[0].metadata[0].name : kubernetes_config_map.cns_config.metadata[0].name
          }
        }
      }
    }
  }
}

# CNS Gateway Service - API Gateway with rate limiting
resource "kubernetes_deployment" "cns_gateway_service" {
  metadata {
    name      = "cns-gateway-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-gateway"
      service     = "gateway"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app     = "cns-gateway"
        service = "gateway"
      }
    }

    template {
      metadata {
        labels = {
          app     = "cns-gateway"
          service = "gateway" 
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9091"
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
          name  = "cns-gateway"
          image = "cns-gateway:${var.environment}-latest"
          
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
            value = "gateway"
          }
          
          env {
            name  = "HTTP_PORT"
            value = "8081"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9091"
          }
          
          env {
            name  = "RATE_LIMIT_ENABLED"
            value = "true"
          }
          
          env {
            name  = "RATE_LIMIT_RPS"
            value = "1000"
          }
          
          env {
            name  = "PROTECTION_SERVICE_ENDPOINT"
            value = "cns-protection-service:8080"
          }
          
          env {
            name  = "ANALYTICS_SERVICE_ENDPOINT"
            value = "cns-analytics-service:8082"
          }

          port {
            name           = "http"
            container_port = 8081
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9091
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "http"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = "http"
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
        }
      }
    }
  }
}

# CNS Analytics Service - Metrics aggregation and analysis
resource "kubernetes_deployment" "cns_analytics_service" {
  metadata {
    name      = "cns-analytics-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-analytics"
      service     = "analytics"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    replicas = 2  # Analytics service can run with fewer replicas

    selector {
      match_labels = {
        app     = "cns-analytics"
        service = "analytics"
      }
    }

    template {
      metadata {
        labels = {
          app     = "cns-analytics"
          service = "analytics"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9092"
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
          name  = "cns-analytics"
          image = "cns-analytics:${var.environment}-latest"
          
          resources {
            requests = {
              cpu    = "300m"
              memory = "1Gi"
            }
            limits = {
              cpu    = "1500m"
              memory = "2Gi"
            }
          }

          env {
            name  = "SERVICE_TYPE"
            value = "analytics"
          }
          
          env {
            name  = "GRPC_PORT"
            value = "8082"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9092"
          }
          
          env {
            name  = "PROTECTION_SERVICE_ENDPOINT"
            value = "cns-protection-service:8080"
          }
          
          env {
            name  = "GATEWAY_SERVICE_ENDPOINT"
            value = "cns-gateway-service:8081"
          }

          port {
            name           = "grpc"
            container_port = 8082
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9092
            protocol       = "TCP"
          }

          liveness_probe {
            grpc {
              port    = 8082
              service = "health"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            grpc {
              port    = 8082
              service = "ready"
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
        }
      }
    }
  }
}

# CNS Monitor Service - Health checks and alerting
resource "kubernetes_deployment" "cns_monitor_service" {
  metadata {
    name      = "cns-monitor-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-monitor"
      service     = "monitor"
      version     = "1.0.0"
      environment = var.environment
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    replicas = 1  # Monitor service runs as singleton

    selector {
      match_labels = {
        app     = "cns-monitor"
        service = "monitor"
      }
    }

    template {
      metadata {
        labels = {
          app     = "cns-monitor"
          service = "monitor"
          version = "1.0.0"
        }
        
        annotations = {
          "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9093"
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
          name  = "cns-monitor"
          image = "cns-monitor:${var.environment}-latest"
          
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
            value = "monitor"
          }
          
          env {
            name  = "HTTP_PORT"
            value = "8083"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9093"
          }
          
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

          port {
            name           = "http"
            container_port = 8083
            protocol       = "TCP"
          }
          
          port {
            name           = "metrics"
            container_port = 9093
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = "http"
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/status"
              port = "http"
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
        }
      }
    }
  }
}

# Services for inter-service communication
resource "kubernetes_service" "cns_protection_service" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app     = "cns-protection"
      service = "protection"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app     = "cns-protection"
      service = "protection"
    }

    port {
      name        = "grpc"
      port        = 8080
      target_port = 8080
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9090
      target_port = 9090
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

resource "kubernetes_service" "cns_gateway_service" {
  metadata {
    name      = "cns-gateway-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app     = "cns-gateway"
      service = "gateway"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app     = "cns-gateway"
      service = "gateway"
    }

    port {
      name        = "http"
      port        = 8081
      target_port = 8081
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9091
      target_port = 9091
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

resource "kubernetes_service" "cns_analytics_service" {
  metadata {
    name      = "cns-analytics-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app     = "cns-analytics"
      service = "analytics"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app     = "cns-analytics"
      service = "analytics"
    }

    port {
      name        = "grpc"
      port        = 8082
      target_port = 8082
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9092
      target_port = 9092
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

resource "kubernetes_service" "cns_monitor_service" {
  metadata {
    name      = "cns-monitor-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app     = "cns-monitor"
      service = "monitor"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
    }
  }

  spec {
    selector = {
      app     = "cns-monitor"
      service = "monitor"
    }

    port {
      name        = "http"
      port        = 8083
      target_port = 8083
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 9093
      target_port = 9093
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Network Policies for inter-service communication security
resource "kubernetes_network_policy" "cns_inter_service_policy" {
  metadata {
    name      = "cns-inter-service-policy"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      "network-policy" = "inter-service"
      "security"       = "enabled"
    }
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns-protection"
      }
    }

    ingress {
      from {
        pod_selector {
          match_labels = {
            service = "gateway"
          }
        }
      }
      from {
        pod_selector {
          match_labels = {
            service = "analytics"
          }
        }
      }
      from {
        pod_selector {
          match_labels = {
            service = "monitor"
          }
        }
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
    }

    egress {
      to {
        pod_selector {
          match_labels = {
            service = "analytics"
          }
        }
      }
      to {
        pod_selector {
          match_labels = {
            service = "monitor"
          }
        }
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

# Service Mesh Traffic Split for Canary Deployments
resource "kubernetes_manifest" "cns_traffic_split" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "split.smi-spec.io/v1alpha1"
    kind       = "TrafficSplit"
    metadata = {
      name      = "cns-protection-split"
      namespace = kubernetes_namespace.cns.metadata[0].name
    }
    spec = {
      service = "cns-protection-service"
      backends = [
        {
          service = "cns-protection-service"
          weight  = 100
        }
      ]
    }
  }
}