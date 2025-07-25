# CNS Forge 80/20 Terraform Configuration
# Leverages existing infrastructure patterns

terraform {
  required_version = ">= 1.0"
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
  }
}

provider "kubernetes" {
  config_path = var.kube_config_path
}

variable "kube_config_path" {
  description = "Path to kubeconfig file"
  type        = string
  default     = "~/.kube/config"
}

variable "namespace" {
  description = "Kubernetes namespace"
  type        = string
  default     = "cns-system"
}

# CNS Forge 80/20 Deployment
resource "kubernetes_manifest" "cns_forge_8020_deployment" {
  manifest = yamldecode(file("${path.module}/cns-forge-8020-deployment.yaml"))
}

# ConfigMap for CNS Forge configuration
resource "kubernetes_config_map" "cns_forge_8020_config" {
  metadata {
    name      = "cns-forge-8020-config"
    namespace = var.namespace
  }

  data = {
    "config.yaml" = <<-EOT
      cns_forge:
        ttl_hops: 8
        tick_budget: 8
        telemetry_enabled: true
        saga_mode: true
        universal_observability: true
      
      integration:
        existing_bitactor: true
        ash_reactor_bridge: true
        otel_metrics: true
        production_ready: true
    EOT
  }
}

# Network Policy for enhanced security
resource "kubernetes_network_policy" "cns_forge_8020_netpol" {
  metadata {
    name      = "cns-forge-8020-netpol"
    namespace = var.namespace
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns-forge-8020"
      }
    }

    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "cns-forge-8020"
          }
        }
      }
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      ports {
        port     = "9090"
        protocol = "TCP"
      }
    }

    egress {
      # Allow DNS
      ports {
        port     = "53"
        protocol = "UDP"
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

output "deployment_name" {
  value = "cns-forge-8020"
}

output "service_name" {
  value = "cns-forge-8020-service"
}
