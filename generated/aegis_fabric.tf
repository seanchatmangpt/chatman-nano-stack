# CNS Aegis Fabric - Terraform Configuration
# Generated from TTL specifications

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
  }
}

variable "namespace" {
  description = "Kubernetes namespace"
  type        = string
  default     = "aegis-fabric"
}

variable "replicas" {
  description = "Number of replicas"
  type        = number
  default     = 5
}

resource "kubernetes_namespace" "aegis" {
  metadata {
    name = var.namespace
    
    labels = {
      "app"         = "aegis-fabric"
      "security"    = "enabled"
      "managed-by"  = "terraform"
    }
  }
}

resource "kubernetes_deployment" "aegis_bitactor" {
  metadata {
    name      = "aegis-bitactor"
    namespace = kubernetes_namespace.aegis.metadata[0].name
  }
  
  spec {
    replicas = var.replicas
    
    selector {
      match_labels = {
        app = "aegis-bitactor"
      }
    }
    
    template {
      metadata {
        labels = {
          app = "aegis-bitactor"
        }
      }
      
      spec {
        container {
          name  = "bitactor"
          image = "aegis-fabric/bitactor:latest"
          
          port {
            container_port = 8080
            name          = "enforcement"
          }
          
          port {
            container_port = 9090
            name          = "metrics"
          }
          
          resources {
            requests = {
              memory = "512Mi"
              cpu    = "500m"
            }
            limits = {
              memory = "2048Mi"
              cpu    = "2000m"
            }
          }
          
          security_context {
            run_as_non_root            = true
            run_as_user                = 1000
            allow_privilege_escalation = false
            read_only_root_filesystem  = true
            
            capabilities {
              drop = ["ALL"]
            }
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "aegis_bitactor" {
  metadata {
    name      = "aegis-bitactor-service"
    namespace = kubernetes_namespace.aegis.metadata[0].name
  }
  
  spec {
    selector = {
      app = "aegis-bitactor"
    }
    
    port {
      name        = "enforcement"
      port        = 8080
      target_port = 8080
    }
    
    port {
      name        = "metrics"
      port        = 9090
      target_port = 9090
    }
    
    type = "ClusterIP"
  }
}

output "namespace" {
  value = kubernetes_namespace.aegis.metadata[0].name
}

output "service_endpoint" {
  value = "${kubernetes_service.aegis_bitactor.metadata[0].name}.${var.namespace}.svc.cluster.local"
}