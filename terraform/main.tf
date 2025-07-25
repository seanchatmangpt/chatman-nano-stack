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

# Configure Kubernetes provider
provider "kubernetes" {
  config_path = var.kube_config_path
}

provider "helm" {
  kubernetes {
    config_path = var.kube_config_path
  }
}

# Variables
variable "kube_config_path" {
  description = "Path to kubeconfig file"
  type        = string
  default     = "~/.kube/config"
}

variable "namespace" {
  description = "Kubernetes namespace for CNS deployment"
  type        = string
  default     = "cns-system"
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "prod"
}

variable "replicas" {
  description = "Number of CNS replicas"
  type        = number
  default     = 3
}

variable "enable_service_mesh" {
  description = "Enable Linkerd service mesh for inter-service communication"
  type        = bool
  default     = true
}

variable "region" {
  description = "Cloud region for deployment"
  type        = string
  default     = "us-west-2"
}

variable "enable_external_secrets" {
  description = "Enable external secrets management"
  type        = bool
  default     = true
}

# Create namespace
resource "kubernetes_namespace" "cns" {
  metadata {
    name = var.namespace
    
    labels = {
      "app"         = "cns"
      "environment" = var.environment
      "managed-by"  = "terraform"
    }
  }
}

# NetworkPolicy for CNS security (CIS compliance) - Updated for Inter-Pod Communication
resource "kubernetes_network_policy" "cns_network_policy_main" {
  metadata {
    name      = "cns-main-network-policy"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      "security" = "enabled"
      "cis-compliant" = "true"
      "inter-pod-communication" = "enabled"
    }
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns"
      }
    }

    ingress {
      # Allow inter-pod communication within CNS
      from {
        pod_selector {
          match_labels = {
            app = "cns"
          }
        }
      }
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      ports {
        port     = "9090"  # Metrics
        protocol = "TCP"
      }
      ports {
        port     = "8081"  # Health checks
        protocol = "TCP"
      }
    }
    
    ingress {
      # Allow from same namespace for service mesh
      from {
        namespace_selector {
          match_labels = {
            name = var.namespace
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
      ports {
        port     = "8081"
        protocol = "TCP"
      }
    }

    egress {
      # Allow inter-pod communication within CNS
      to {
        pod_selector {
          match_labels = {
            app = "cns"
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
      ports {
        port     = "8081"
        protocol = "TCP"
      }
    }
    
    egress {
      # Allow DNS resolution
      ports {
        port     = "53"
        protocol = "UDP"
      }
      ports {
        port     = "53"
        protocol = "TCP"
      }
    }
    
    egress {
      # Allow HTTPS for external services
      ports {
        port     = "443"
        protocol = "TCP"
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

# RBAC configuration for enhanced security
resource "kubernetes_cluster_role" "cns_cluster_role" {
  metadata {
    name = "cns-enhanced-cluster-role"
    
    labels = {
      "rbac" = "enabled"
      "security" = "hardened"
    }
  }

  rule {
    api_groups = [""]
    resources  = ["configmaps", "secrets"]
    verbs      = ["get", "list", "watch"]
  }

  rule {
    api_groups = [""]
    resources  = ["pods"]
    verbs      = ["get", "list"]
  }
}

# Resource quotas for namespace (implementing resource limits)
resource "kubernetes_resource_quota" "cns_quota" {
  metadata {
    name      = "cns-resource-quota"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    hard = {
      "requests.cpu"    = "10"
      "requests.memory" = "20Gi"
      "limits.cpu"      = "20"
      "limits.memory"   = "40Gi"
      "pods"            = "50"
    }
  }
}

# Dedicated NetworkPolicy for Inter-Pod Communication (80/20 Implementation)
resource "kubernetes_network_policy" "cns_inter_pod_communication" {
  metadata {
    name      = "cns-inter-pod-communication"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      "app" = "cns"
      "type" = "inter-pod-communication"
      "security" = "8020-compliant"
    }
    
    annotations = {
      "inter-pod.cns.io/enabled" = "true"
      "security.cns.io/80-20-compliant" = "true"
    }
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns"
      }
    }

    # Allow ingress from other CNS pods
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "cns"
          }
        }
      }
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      ports {
        port     = "9090"  # Metrics
        protocol = "TCP"
      }
      ports {
        port     = "8081"  # Health checks
        protocol = "TCP"
      }
      ports {
        port     = "8082"  # CNS Analytics service
        protocol = "TCP"
      }
      ports {
        port     = "8083"  # CNS Monitor service
        protocol = "TCP"
      }
    }
    
    # Allow ingress from service mesh (Linkerd)
    ingress {
      from {
        namespace_selector {
          match_labels = {
            "linkerd.io/control-plane-ns" = "linkerd"
          }
        }
      }
      ports {
        port     = "4143"  # Linkerd proxy
        protocol = "TCP"
      }
    }

    # Allow egress to other CNS pods
    egress {
      to {
        pod_selector {
          match_labels = {
            app = "cns"
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
    
    # Allow egress to service mesh
    egress {
      to {
        namespace_selector {
          match_labels = {
            "linkerd.io/control-plane-ns" = "linkerd"
          }
        }
      }
      ports {
        port     = "8443"  # Linkerd control plane
        protocol = "TCP"
      }
    }
    
    # Allow DNS resolution
    egress {
      ports {
        port     = "53"
        protocol = "UDP"
      }
      ports {
        port     = "53"
        protocol = "TCP"
      }
    }
    
    # Allow HTTPS for external services
    egress {
      ports {
        port     = "443"
        protocol = "TCP"
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

# ConfigMap for CNS configuration
resource "kubernetes_config_map" "cns_config" {
  metadata {
    name      = "cns-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  data = {
    "config.yaml" = <<-EOT
      environment: ${var.environment}
      features:
        neural_enabled: true
        owl_compiler_enabled: true
        quantum_semantic_enabled: true
        benchmark_enabled: true
      security:
        max_threads: 10
        max_processes: 5
        max_memory_mb: 2048
        max_cpu_percent: 80
      observability:
        otel_enabled: true
        metrics_port: 9090
        trace_sampling_rate: 0.1
    EOT
    
    "security_patches.py" = file("${path.module}/../security_patches_8020.py")
  }
}

# Secret for sensitive data
resource "kubernetes_secret" "cns_secret" {
  metadata {
    name      = "cns-secret"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  type = "Opaque"

  data = {
    api_key = base64encode("placeholder-api-key")
  }
}

# Service Account with security constraints
resource "kubernetes_service_account" "cns_sa" {
  metadata {
    name      = "cns-service-account"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "cns"
    }
  }
}

# Role with minimal permissions
resource "kubernetes_role" "cns_role" {
  metadata {
    name      = "cns-role"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  rule {
    api_groups = [""]
    resources  = ["configmaps"]
    verbs      = ["get", "list", "watch"]
  }

  rule {
    api_groups = [""]
    resources  = ["secrets"]
    verbs      = ["get"]
  }
}

# RoleBinding
resource "kubernetes_role_binding" "cns_role_binding" {
  metadata {
    name      = "cns-role-binding"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "Role"
    name      = kubernetes_role.cns_role.metadata[0].name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.cns_sa.metadata[0].name
    namespace = var.namespace
  }
}

# PodDisruptionBudget for high availability
resource "kubernetes_pod_disruption_budget" "cns_pdb" {
  metadata {
    name      = "cns-pdb"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    min_available = 2
    
    selector {
      match_labels = {
        app = "cns"
      }
    }
  }
}

# HorizontalPodAutoscaler for auto-scaling
resource "kubernetes_horizontal_pod_autoscaler_v2" "cns_hpa" {
  metadata {
    name      = "cns-hpa"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = "cns-deployment"
    }

    min_replicas = var.replicas
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

# Service for CNS
resource "kubernetes_service" "cns_service" {
  metadata {
    name      = "cns-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "cns"
    }
  }

  spec {
    selector = {
      app = "cns"
    }

    port {
      name        = "http"
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

# Ingress for external access (optional)
resource "kubernetes_ingress_v1" "cns_ingress" {
  metadata {
    name      = "cns-ingress"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    annotations = {
      "kubernetes.io/ingress.class"                = "nginx"
      "nginx.ingress.kubernetes.io/rate-limit"     = "100"
      "nginx.ingress.kubernetes.io/ssl-redirect"   = "true"
      "nginx.ingress.kubernetes.io/proxy-body-size" = "10m"
    }
  }

  spec {
    tls {
      hosts       = ["cns.${var.environment}.example.com"]
      secret_name = "cns-tls"
    }

    rule {
      host = "cns.${var.environment}.example.com"
      
      http {
        path {
          path      = "/"
          path_type = "Prefix"
          
          backend {
            service {
              name = kubernetes_service.cns_service.metadata[0].name
              port {
                number = 8080
              }
            }
          }
        }
      }
    }
  }
}

# PersistentVolumeClaim for data storage
resource "kubernetes_persistent_volume_claim" "cns_data" {
  metadata {
    name      = "cns-data-pvc"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    
    resources {
      requests = {
        storage = "10Gi"
      }
    }
    
    storage_class_name = "standard"
  }
}

# Linkerd Service Mesh Configuration
resource "kubernetes_manifest" "linkerd_namespace" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "v1"
    kind       = "Namespace"
    metadata = {
      name = "linkerd"
      annotations = {
        "linkerd.io/inject" = "disabled"
      }
      labels = {
        "linkerd.io/control-plane-ns" = "linkerd"
        "config.linkerd.io/admission-webhooks" = "disabled"
      }
    }
  }
}

# Linkerd Control Plane (simplified deployment)
resource "kubernetes_manifest" "linkerd_control_plane" {
  count = var.enable_service_mesh ? 1 : 0
  depends_on = [kubernetes_manifest.linkerd_namespace]
  
  manifest = {
    apiVersion = "install.linkerd.io/v1alpha1" 
    kind       = "ControlPlane"
    metadata = {
      name      = "linkerd-control-plane"
      namespace = "linkerd"
    }
    spec = {
      identityTrustDomain = "cluster.local"
      proxy = {
        image = {
          name    = "cr.l5d.io/linkerd/proxy"
          version = "stable-2.14.1"
        }
        logLevel = "warn,linkerd=info"
        resources = {
          cpu = {
            limit   = "1"
            request = "100m"
          }
          memory = {
            limit   = "250Mi" 
            request = "20Mi"
          }
        }
      }
    }
  }
}

# CNS Service Mesh Configuration
resource "kubernetes_config_map" "cns_service_mesh_config" {
  count = var.enable_service_mesh ? 1 : 0
  
  metadata {
    name      = "cns-service-mesh-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    annotations = {
      "linkerd.io/inject" = "enabled"
    }
  }

  data = {
    "mesh_config.yaml" = <<-EOT
      service_mesh:
        provider: linkerd
        version: "2.14.1"
        features:
          mtls_enabled: true
          circuit_breaker_enabled: true
          load_balancing: "least_request"
          retry_policy: "exponential_backoff"
          timeout_ms: 5000
          
      inter_service_communication:
        protocol: "grpc"
        compression: "gzip"
        keep_alive_time_ms: 30000
        keep_alive_timeout_ms: 5000
        max_message_size_mb: 10
        
      observability:
        tracing_enabled: true
        metrics_enabled: true
        sampling_rate: 0.1
        jaeger_endpoint: "http://jaeger-collector:14268/api/traces"
        
      security:
        enforce_mtls: true
        allow_plaintext: false
        trust_domain: "cluster.local"
    EOT
    
    "service_discovery.yaml" = <<-EOT
      services:
        cns-protection:
          port: 8080
          protocol: grpc
          health_check: "/health"
          
        cns-gateway:
          port: 8081  
          protocol: http2
          health_check: "/ready"
          
        cns-analytics:
          port: 8082
          protocol: grpc
          health_check: "/health"
          
        cns-monitor:
          port: 8083
          protocol: http
          health_check: "/status"
    EOT
  }
}

# Include all comprehensive deployment modules
module "multi_service_deployment" {
  source = "./multi-service-deployment.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
  replicas           = var.replicas
}

module "distributed_adversarial_testing" {
  source = "./distributed-adversarial-testing.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
}

module "service_optimization_8020" {
  source = "./80-20-service-optimization.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
}

module "cross_service_benchmark" {
  source = "./cross-service-benchmark-stress.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
}

module "comprehensive_validation" {
  source = "./comprehensive-k8s-validation.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
}

# Include security hardening modules
module "security_hardening" {
  source = "./security-hardening.tf"
  
  namespace           = var.namespace
  environment        = var.environment
  enable_service_mesh = var.enable_service_mesh
}

module "secret_management" {
  source = "./secret-management.tf"
  
  namespace              = var.namespace
  region                = var.region
  enable_external_secrets = var.enable_external_secrets
}

# Outputs
output "namespace" {
  value = kubernetes_namespace.cns.metadata[0].name
}

output "service_name" {
  value = kubernetes_service.cns_service.metadata[0].name
}

output "ingress_hostname" {
  value = "cns.${var.environment}.example.com"
}

output "security_hardening_enabled" {
  value = true
  description = "Security hardening modules applied for CIS compliance"
}

output "secret_management_enabled" {
  value = true
  description = "Enhanced secret management with encryption and rotation"
}