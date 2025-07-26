terraform {
  required_version = ">= 1.0"
  
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.11"
    }
  }
}

# Variables
variable "namespace" {
  description = "Kubernetes namespace for Ash.Reactor deployment"
  type        = string
  default     = "ash-reactor-80-20"
}

variable "image_tag" {
  description = "Docker image tag for Ash.Reactor"
  type        = string
  default     = "latest"
}

variable "replicas" {
  description = "Number of Ash.Reactor replicas"
  type        = number
  default     = 3
}

variable "otel_endpoint" {
  description = "OpenTelemetry collector endpoint"
  type        = string
  default     = "http://otel-collector:4317"
}

# Create namespace
resource "kubernetes_namespace" "ash_reactor" {
  metadata {
    name = var.namespace
    
    labels = {
      "app"         = "ash-reactor"
      "environment" = "production"
      "managed-by"  = "terraform"
      "approach"    = "80-20"
    }
  }
}

# ConfigMap for Ash.Reactor configuration
resource "kubernetes_config_map" "ash_reactor_config" {
  metadata {
    name      = "ash-reactor-config"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
  }
  
  data = {
    "config.exs" = file("${path.module}/config/config.exs")
    
    # Environment variables
    OTEL_EXPORTER_OTLP_ENDPOINT = var.otel_endpoint
    OTEL_SERVICE_NAME           = "ash-reactor-80-20"
    LOG_LEVEL                   = "info"
    TTL_PROCESSING_ENABLED      = "true"
    REACTOR_MAX_CONCURRENCY     = "10"
  }
}

# Secret for sensitive data
resource "kubernetes_secret" "ash_reactor_secrets" {
  metadata {
    name      = "ash-reactor-secrets"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
  }
  
  data = {
    SECRET_KEY_BASE = base64encode(random_password.secret_key_base.result)
  }
}

resource "random_password" "secret_key_base" {
  length  = 64
  special = true
}

# Service Account
resource "kubernetes_service_account" "ash_reactor" {
  metadata {
    name      = "ash-reactor"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
  }
}

# Deployment
resource "kubernetes_deployment" "ash_reactor" {
  metadata {
    name      = "ash-reactor"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
    
    labels = {
      app     = "ash-reactor"
      version = var.image_tag
    }
  }
  
  spec {
    replicas = var.replicas
    
    selector {
      match_labels = {
        app = "ash-reactor"
      }
    }
    
    template {
      metadata {
        labels = {
          app     = "ash-reactor"
          version = var.image_tag
        }
        
        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9090"
          "prometheus.io/path"   = "/metrics"
        }
      }
      
      spec {
        service_account_name = kubernetes_service_account.ash_reactor.metadata[0].name
        
        container {
          name  = "ash-reactor"
          image = "ash-reactor-80-20:${var.image_tag}"
          
          port {
            container_port = 4000
            name          = "http"
          }
          
          port {
            container_port = 9090
            name          = "metrics"
          }
          
          env_from {
            config_map_ref {
              name = kubernetes_config_map.ash_reactor_config.metadata[0].name
            }
          }
          
          env_from {
            secret_ref {
              name = kubernetes_secret.ash_reactor_secrets.metadata[0].name
            }
          }
          
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
          
          liveness_probe {
            http_get {
              path = "/health"
              port = 4000
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }
          
          readiness_probe {
            http_get {
              path = "/ready"
              port = 4000
            }
            initial_delay_seconds = 5
            period_seconds        = 5
          }
        }
      }
    }
  }
}

# Service
resource "kubernetes_service" "ash_reactor" {
  metadata {
    name      = "ash-reactor"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
    
    labels = {
      app = "ash-reactor"
    }
  }
  
  spec {
    selector = {
      app = "ash-reactor"
    }
    
    port {
      name        = "http"
      port        = 80
      target_port = 4000
    }
    
    port {
      name        = "metrics"
      port        = 9090
      target_port = 9090
    }
    
    type = "ClusterIP"
  }
}

# HorizontalPodAutoscaler
resource "kubernetes_horizontal_pod_autoscaler_v2" "ash_reactor" {
  metadata {
    name      = "ash-reactor"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
  }
  
  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = kubernetes_deployment.ash_reactor.metadata[0].name
    }
    
    min_replicas = 2
    max_replicas = 10
    
    metric {
      type = "Resource"
      
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = 70
        }
      }
    }
    
    metric {
      type = "Resource"
      
      resource {
        name = "memory"
        target {
          type                = "Utilization"
          average_utilization = 80
        }
      }
    }
  }
}

# NetworkPolicy for security
resource "kubernetes_network_policy" "ash_reactor" {
  metadata {
    name      = "ash-reactor-network-policy"
    namespace = kubernetes_namespace.ash_reactor.metadata[0].name
  }
  
  spec {
    pod_selector {
      match_labels = {
        app = "ash-reactor"
      }
    }
    
    policy_types = ["Ingress", "Egress"]
    
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "ingress-nginx"
          }
        }
      }
      
      ports {
        port     = "4000"
        protocol = "TCP"
      }
    }
    
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "prometheus"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
    }
    
    egress {
      to {
        namespace_selector {
          match_labels = {
            name = "opentelemetry"
          }
        }
      }
      
      ports {
        port     = "4317"
        protocol = "TCP"
      }
    }
    
    # Allow DNS
    egress {
      ports {
        port     = "53"
        protocol = "UDP"
      }
    }
  }
}

# Output values
output "namespace" {
  value = kubernetes_namespace.ash_reactor.metadata[0].name
}

output "service_name" {
  value = kubernetes_service.ash_reactor.metadata[0].name
}

output "deployment_name" {
  value = kubernetes_deployment.ash_reactor.metadata[0].name
}