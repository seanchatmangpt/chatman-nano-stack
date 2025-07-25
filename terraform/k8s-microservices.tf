# CNS Microservices for Enhanced Inter-Service Communication
# 80/20 architecture with specialized services for protection, gateway, analytics, and monitoring

# CNS Protection Service (Core Protection Logic)
resource "kubernetes_deployment" "cns_protection_service" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns-protection"
      component   = "protection"
      version     = "v1.0.0"
      service-tier = "critical"
    }
  }

  spec {
    replicas = 3

    selector {
      match_labels = {
        app = "cns-protection"
      }
    }

    template {
      metadata {
        labels = {
          app = "cns-protection"
          version = "v1.0.0"
        }
        
        annotations = {
          "sidecar.istio.io/inject" = "true"
          "sidecar.istio.io/proxyCPU" = "100m"
          "sidecar.istio.io/proxyMemory" = "128Mi"
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
          name  = "protection"
          image = "cns-protection:v1.0.0"
          
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
            name  = "SERVICE_NAME"
            value = "cns-protection"
          }
          
          env {
            name  = "GRPC_PORT"
            value = "8080"
          }
          
          env {
            name  = "METRICS_PORT"
            value = "9090"
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
              port = 8080
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            grpc {
              port = 8080
            }
            initial_delay_seconds = 5
            period_seconds        = 5
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "tmp"
            mount_path = "/tmp"
          }
        }

        volume {
          name = "tmp"
          empty_dir {
            medium     = "Memory"
            size_limit = "256Mi"
          }
        }
      }
    }
  }
}

# CNS Gateway Service (API Gateway)
resource "kubernetes_deployment" "cns_gateway_service" {
  metadata {
    name      = "cns-gateway-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app       = "cns-gateway"
      component = "gateway"
      version   = "v1.0.0"
      service-tier = "critical"
    }
  }

  spec {
    replicas = 2

    selector {
      match_labels = {
        app = "cns-gateway"
      }
    }

    template {
      metadata {
        labels = {
          app = "cns-gateway"
          version = "v1.0.0"
        }
        
        annotations = {
          "sidecar.istio.io/inject" = "true"
          "sidecar.istio.io/proxyCPU" = "100m"
          "sidecar.istio.io/proxyMemory" = "128Mi"
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
          name  = "gateway"
          image = "cns-gateway:v1.0.0"
          
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
            name  = "SERVICE_NAME"
            value = "cns-gateway"
          }
          
          env {
            name  = "HTTP_PORT"
            value = "8081"
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

          liveness_probe {
            http_get {
              path = "/health"
              port = 8081
            }
            initial_delay_seconds = 15
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = 8081
            }
            initial_delay_seconds = 5
            period_seconds        = 5
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "tmp"
            mount_path = "/tmp"
          }
        }

        volume {
          name = "tmp"
          empty_dir {
            medium     = "Memory"
            size_limit = "256Mi"
          }
        }
      }
    }
  }
}

# CNS Analytics Service
resource "kubernetes_deployment" "cns_analytics_service" {
  metadata {
    name      = "cns-analytics-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app       = "cns-analytics"
      component = "analytics"
      version   = "v1.0.0"
      service-tier = "standard"
    }
  }

  spec {
    replicas = 2

    selector {
      match_labels = {
        app = "cns-analytics"
      }
    }

    template {
      metadata {
        labels = {
          app = "cns-analytics"
          version = "v1.0.0"
        }
        
        annotations = {
          "sidecar.istio.io/inject" = "true"
          "sidecar.istio.io/proxyCPU" = "50m"
          "sidecar.istio.io/proxyMemory" = "64Mi"
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
          name  = "analytics"
          image = "cns-analytics:v1.0.0"
          
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
            name  = "SERVICE_NAME"
            value = "cns-analytics"
          }
          
          env {
            name  = "GRPC_PORT"
            value = "8082"
          }

          port {
            name           = "grpc"
            container_port = 8082
            protocol       = "TCP"
          }

          liveness_probe {
            grpc {
              port = 8082
            }
            initial_delay_seconds = 20
            period_seconds        = 15
          }
          
          readiness_probe {
            grpc {
              port = 8082
            }
            initial_delay_seconds = 10
            period_seconds        = 10
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "tmp"
            mount_path = "/tmp"
          }
        }

        volume {
          name = "tmp"
          empty_dir {
            medium     = "Memory"
            size_limit = "128Mi"
          }
        }
      }
    }
  }
}

# CNS Monitor Service
resource "kubernetes_deployment" "cns_monitor_service" {
  metadata {
    name      = "cns-monitor-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app       = "cns-monitor"
      component = "monitoring"
      version   = "v1.0.0"
      service-tier = "standard"
    }
  }

  spec {
    replicas = 1

    selector {
      match_labels = {
        app = "cns-monitor"
      }
    }

    template {
      metadata {
        labels = {
          app = "cns-monitor"
          version = "v1.0.0"
        }
        
        annotations = {
          "sidecar.istio.io/inject" = "true"
          "sidecar.istio.io/proxyCPU" = "50m"
          "sidecar.istio.io/proxyMemory" = "64Mi"
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
          name  = "monitor"
          image = "cns-monitor:v1.0.0"
          
          resources {
            requests = {
              cpu    = "50m"
              memory = "128Mi"
            }
            limits = {
              cpu    = "200m"
              memory = "256Mi"
            }
          }

          env {
            name  = "SERVICE_NAME"
            value = "cns-monitor"
          }
          
          env {
            name  = "HTTP_PORT"
            value = "8083"
          }

          port {
            name           = "http"
            container_port = 8083
            protocol       = "TCP"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = 8083
            }
            initial_delay_seconds = 10
            period_seconds        = 30
          }
          
          readiness_probe {
            http_get {
              path = "/status"
              port = 8083
            }
            initial_delay_seconds = 5
            period_seconds        = 10
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            run_as_non_root           = true
            
            capabilities {
              drop = ["ALL"]
            }
          }

          volume_mount {
            name       = "tmp"
            mount_path = "/tmp"
          }
        }

        volume {
          name = "tmp"
          empty_dir {
            medium     = "Memory"
            size_limit = "64Mi"
          }
        }
      }
    }
  }
}

# Services for each microservice
resource "kubernetes_service" "cns_protection_service" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "cns-protection"
    }
  }

  spec {
    selector = {
      app = "cns-protection"
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
      app = "cns-gateway"
    }
  }

  spec {
    selector = {
      app = "cns-gateway"
    }

    port {
      name        = "http"
      port        = 8081
      target_port = 8081
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
      app = "cns-analytics"
    }
  }

  spec {
    selector = {
      app = "cns-analytics"
    }

    port {
      name        = "grpc"
      port        = 8082
      target_port = 8082
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
      app = "cns-monitor"
    }
  }

  spec {
    selector = {
      app = "cns-monitor"
    }

    port {
      name        = "http"
      port        = 8083
      target_port = 8083
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Service entries for external services
resource "kubernetes_manifest" "external_services_entry" {
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "ServiceEntry"
    metadata = {
      name      = "external-services"
      namespace = var.namespace
    }
    spec = {
      hosts = [
        "jaeger-collector.istio-system.svc.cluster.local",
        "prometheus.istio-system.svc.cluster.local"
      ]
      ports = [
        {
          number = 14268
          name = "jaeger-collector"
          protocol = "HTTP"
        },
        {
          number = 9090
          name = "prometheus"
          protocol = "HTTP"
        }
      ]
      location = "MESH_EXTERNAL"
      resolution = "DNS"
    }
  }
}

# Output microservices information
output "microservices_deployed" {
  value = {
    protection = "cns-protection-service:8080"
    gateway    = "cns-gateway-service:8081"
    analytics  = "cns-analytics-service:8082"
    monitor    = "cns-monitor-service:8083"
  }
  description = "CNS microservices endpoints for inter-service communication"
}

output "service_mesh_ready" {
  value = true
  description = "All microservices configured for service mesh communication"
}