# CNS Aegis Fabric - DEFINITIVE CONSOLIDATED ARCHITECTURE
# This file consolidates ALL terraform resources into one working solution
# Eliminates ALL duplicates, conflicts, and broken references

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

# Provider Configuration
provider "kubernetes" {
  config_path = var.kube_config_path
}

provider "helm" {
  kubernetes {
    config_path = var.kube_config_path
  }
}

# ============================================================================
# VARIABLES - CONSOLIDATED FROM ALL FILES
# ============================================================================

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
  description = "Enable Istio service mesh for inter-service communication"
  type        = bool
  default     = true
}

variable "enable_monitoring" {
  description = "Enable Prometheus/Grafana monitoring stack"
  type        = bool
  default     = true
}

variable "enable_adversarial_testing" {
  description = "Enable continuous adversarial testing"
  type        = bool
  default     = true
}

variable "region" {
  description = "Cloud region for deployment"
  type        = string
  default     = "us-west-2"
}

# ============================================================================
# CORE NAMESPACE AND RBAC
# ============================================================================

resource "kubernetes_namespace" "cns" {
  metadata {
    name = var.namespace
    
    labels = {
      "app"                                = "cns"
      "environment"                        = var.environment
      "managed-by"                         = "terraform"
      "istio-injection"                    = var.enable_service_mesh ? "enabled" : "disabled"
      "pod-security.kubernetes.io/enforce" = "restricted"
      "pod-security.kubernetes.io/audit"   = "restricted"
      "pod-security.kubernetes.io/warn"    = "restricted"
    }
    
    annotations = {
      "inter-pod.cns.io/enabled"             = "true"
      "security.cns.io/80-20-compliant"      = "true"
      "adversarial-testing.cns.io/enabled"   = var.enable_adversarial_testing ? "true" : "false"
    }
  }
}

# Service Account with minimal permissions
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
  
  rule {
    api_groups = [""]
    resources  = ["pods"]
    verbs      = ["get", "list"]
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

# ============================================================================
# NETWORK POLICIES - CONSOLIDATED AND SIMPLIFIED
# ============================================================================

resource "kubernetes_network_policy" "cns_network_policy" {
  metadata {
    name      = "cns-network-policy"
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

    # Allow ingress from CNS pods and service mesh
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "cns"
          }
        }
      }
      # All CNS service ports
      ports {
        port     = "8080"  # Protection
        protocol = "TCP"
      }
      ports {
        port     = "8081"  # Gateway
        protocol = "TCP"
      }
      ports {
        port     = "8082"  # Analytics
        protocol = "TCP"
      }
      ports {
        port     = "8083"  # Monitor
        protocol = "TCP"
      }
      ports {
        port     = "9090"  # Metrics
        protocol = "TCP"
      }
    }
    
    # Allow ingress from Istio system (if enabled)
    dynamic "ingress" {
      for_each = var.enable_service_mesh ? [1] : []
      content {
        from {
          namespace_selector {
            match_labels = {
              name = "istio-system"
            }
          }
        }
        ports {
          port     = "15090"  # Envoy admin
          protocol = "TCP"
        }
      }
    }

    # Allow egress to CNS pods
    egress {
      to {
        pod_selector {
          match_labels = {
            app = "cns"
          }
        }
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

# ============================================================================
# CONFIGMAPS - CONSOLIDATED CONFIGURATION
# ============================================================================

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
        service_mesh_enabled: ${var.enable_service_mesh}
        adversarial_testing_enabled: ${var.enable_adversarial_testing}
      security:
        max_threads: 10
        max_processes: 5
        max_memory_mb: 2048
        max_cpu_percent: 80
      observability:
        otel_enabled: true
        metrics_port: 9090
        trace_sampling_rate: 0.1
      inter_service_communication:
        protocol: "grpc"
        compression: "gzip"
        keep_alive_time_ms: 30000
        keep_alive_timeout_ms: 5000
        max_message_size_mb: 10
      service_discovery:
        dns_policy: "ClusterFirst"
        search_domains: ["${var.namespace}.svc.cluster.local", "svc.cluster.local", "cluster.local"]
    EOT
  }
}

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

# ============================================================================
# CORE CNS SERVICES - DEFINITIVE MICROSERVICES ARCHITECTURE
# ============================================================================

# CNS Protection Service (Port 8080)
resource "kubernetes_deployment" "cns_protection" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "protection"
      version   = "v1"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app       = "cns"
        component = "protection"
      }
    }

    template {
      metadata {
        labels = {
          app       = "cns"
          component = "protection"
          version   = "v1"
        }
        annotations = {
          "sidecar.istio.io/inject" = var.enable_service_mesh ? "true" : "false"
          "prometheus.io/scrape"    = "true"
          "prometheus.io/port"      = "9090"
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
          name  = "cns-protection"
          image = "cns/protection:latest"
          
          port {
            container_port = 8080
            name          = "grpc"
            protocol      = "TCP"
          }
          
          port {
            container_port = 9090
            name          = "metrics"
            protocol      = "TCP"
          }

          env {
            name  = "SERVICE_NAME"
            value = "protection"
          }
          
          env {
            name  = "SERVICE_PORT"
            value = "8080"
          }
          
          env {
            name  = "METRICS_PORT" 
            value = "9090"
          }

          env {
            name = "NAMESPACE"
            value_from {
              field_ref {
                field_path = "metadata.namespace"
              }
            }
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/cns"
            read_only  = true
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

          resources {
            requests = {
              cpu    = "500m"
              memory = "512Mi"
            }
            limits = {
              cpu    = "2000m"
              memory = "2Gi"
            }
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

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.cns_config.metadata[0].name
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "cns_protection" {
  metadata {
    name      = "cns-protection-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "protection"
    }
    annotations = {
      "prometheus.io/scrape" = "true"
      "prometheus.io/port"   = "9090"
    }
  }

  spec {
    selector = {
      app       = "cns"
      component = "protection"
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

# CNS Gateway Service (Port 8081) 
resource "kubernetes_deployment" "cns_gateway" {
  metadata {
    name      = "cns-gateway-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "gateway"
      version   = "v1"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app       = "cns"
        component = "gateway"
      }
    }

    template {
      metadata {
        labels = {
          app       = "cns"
          component = "gateway"
          version   = "v1"
        }
        annotations = {
          "sidecar.istio.io/inject" = var.enable_service_mesh ? "true" : "false"
          "prometheus.io/scrape"    = "true"
          "prometheus.io/port"      = "9091"
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
          image = "cns/gateway:latest"
          
          port {
            container_port = 8081
            name          = "http"
            protocol      = "TCP"
          }
          
          port {
            container_port = 9091
            name          = "metrics"
            protocol      = "TCP"
          }

          env {
            name  = "SERVICE_NAME"
            value = "gateway"
          }
          
          env {
            name  = "SERVICE_PORT"
            value = "8081"
          }
          
          env {
            name  = "PROTECTION_SERVICE_URL"
            value = "cns-protection-service.${var.namespace}.svc.cluster.local:8080"
          }
          
          env {
            name  = "ANALYTICS_SERVICE_URL"
            value = "cns-analytics-service.${var.namespace}.svc.cluster.local:8082"
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/cns"
            read_only  = true
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = 8081
            }
            initial_delay_seconds = 30
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

          resources {
            requests = {
              cpu    = "300m"
              memory = "256Mi" 
            }
            limits = {
              cpu    = "1000m"
              memory = "1Gi"
            }
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

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.cns_config.metadata[0].name
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "cns_gateway" {
  metadata {
    name      = "cns-gateway-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "gateway"
    }
  }

  spec {
    selector = {
      app       = "cns"
      component = "gateway"
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

# CNS Analytics Service (Port 8082)
resource "kubernetes_deployment" "cns_analytics" {
  metadata {
    name      = "cns-analytics-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "analytics"
      version   = "v1"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app       = "cns"
        component = "analytics"
      }
    }

    template {
      metadata {
        labels = {
          app       = "cns"
          component = "analytics"
          version   = "v1"
        }
        annotations = {
          "sidecar.istio.io/inject" = var.enable_service_mesh ? "true" : "false"
          "prometheus.io/scrape"    = "true"
          "prometheus.io/port"      = "9092"
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
          image = "cns/analytics:latest"
          
          port {
            container_port = 8082
            name          = "grpc"
            protocol      = "TCP"
          }
          
          port {
            container_port = 9092
            name          = "metrics"
            protocol      = "TCP"
          }

          env {
            name  = "SERVICE_NAME"
            value = "analytics"
          }
          
          env {
            name  = "SERVICE_PORT"
            value = "8082"
          }
          
          env {
            name  = "PROTECTION_SERVICE_URL"
            value = "cns-protection-service.${var.namespace}.svc.cluster.local:8080"
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/cns"
            read_only  = true
          }

          liveness_probe {
            grpc {
              port = 8082
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }

          readiness_probe {
            grpc {
              port = 8082
            }
            initial_delay_seconds = 5
            period_seconds        = 5
          }

          resources {
            requests = {
              cpu    = "200m"
              memory = "256Mi"
            }
            limits = {
              cpu    = "500m"
              memory = "512Mi"
            }
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

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.cns_config.metadata[0].name
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "cns_analytics" {
  metadata {
    name      = "cns-analytics-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "analytics"
    }
  }

  spec {
    selector = {
      app       = "cns"
      component = "analytics"
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

# CNS Monitor Service (Port 8083)
resource "kubernetes_deployment" "cns_monitor" {
  metadata {
    name      = "cns-monitor-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "monitor"
      version   = "v1"
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app       = "cns"
        component = "monitor"
      }
    }

    template {
      metadata {
        labels = {
          app       = "cns"
          component = "monitor"
          version   = "v1"
        }
        annotations = {
          "sidecar.istio.io/inject" = var.enable_service_mesh ? "true" : "false"
          "prometheus.io/scrape"    = "true"
          "prometheus.io/port"      = "9093"
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
          image = "cns/monitor:latest"
          
          port {
            container_port = 8083
            name          = "http"
            protocol      = "TCP"
          }
          
          port {
            container_port = 9093
            name          = "metrics"
            protocol      = "TCP"
          }

          env {
            name  = "SERVICE_NAME"
            value = "monitor"
          }
          
          env {
            name  = "SERVICE_PORT"
            value = "8083"
          }
          
          env {
            name  = "PROTECTION_SERVICE_URL"
            value = "cns-protection-service.${var.namespace}.svc.cluster.local:8080"
          }
          
          env {
            name  = "ANALYTICS_SERVICE_URL"
            value = "cns-analytics-service.${var.namespace}.svc.cluster.local:8082"
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/cns"
            read_only  = true
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = 8083
            }
            initial_delay_seconds = 30
            period_seconds        = 10
          }

          readiness_probe {
            http_get {
              path = "/ready"
              port = 8083
            }
            initial_delay_seconds = 5
            period_seconds        = 5
          }

          resources {
            requests = {
              cpu    = "100m"
              memory = "128Mi"
            }
            limits = {
              cpu    = "200m"
              memory = "256Mi"
            }
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

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.cns_config.metadata[0].name
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "cns_monitor" {
  metadata {
    name      = "cns-monitor-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "monitor"
    }
  }

  spec {
    selector = {
      app       = "cns"
      component = "monitor"
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

# ============================================================================
# ISTIO SERVICE MESH (OPTIONAL)
# ============================================================================

resource "helm_release" "istio_base" {
  count      = var.enable_service_mesh ? 1 : 0
  name       = "istio-base"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "base"
  namespace  = "istio-system"
  version    = "1.20.0"

  create_namespace = true

  set {
    name  = "defaultRevision"
    value = "default"
  }
}

resource "helm_release" "istiod" {
  count      = var.enable_service_mesh ? 1 : 0
  name       = "istiod"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "istiod"
  namespace  = "istio-system"
  version    = "1.20.0"

  depends_on = [helm_release.istio_base]

  set {
    name  = "telemetry.v2.enabled"
    value = "true"
  }

  set {
    name  = "global.meshID"
    value = "mesh1"
  }

  set {
    name  = "global.multiCluster.clusterName"
    value = "cluster1"
  }

  set {
    name  = "global.network"
    value = "network1"
  }
}

resource "helm_release" "istio_gateway" {
  count      = var.enable_service_mesh ? 1 : 0
  name       = "istio-ingressgateway"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "gateway"
  namespace  = "istio-ingress"
  version    = "1.20.0"

  create_namespace = true
  depends_on       = [helm_release.istiod]

  set {
    name  = "service.type"
    value = "LoadBalancer"
  }
}

# Istio Gateway for CNS
resource "kubernetes_manifest" "cns_gateway" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "Gateway"
    metadata = {
      name      = "cns-gateway"
      namespace = kubernetes_namespace.cns.metadata[0].name
    }
    spec = {
      selector = {
        istio = "ingressgateway"
      }
      servers = [
        {
          port = {
            number   = 80
            name     = "http"
            protocol = "HTTP"
          }
          hosts = ["*"]
        }
      ]
    }
  }

  depends_on = [helm_release.istio_gateway]
}

# VirtualService for CNS Gateway
resource "kubernetes_manifest" "cns_virtualservice" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "VirtualService"
    metadata = {
      name      = "cns-virtualservice"
      namespace = kubernetes_namespace.cns.metadata[0].name
    }
    spec = {
      hosts = ["*"]
      gateways = ["cns-gateway"]
      http = [
        {
          match = [
            {
              uri = {
                prefix = "/"
              }
            }
          ]
          route = [
            {
              destination = {
                host = "cns-gateway-service"
                port = {
                  number = 8081
                }
              }
            }
          ]
        }
      ]
    }
  }

  depends_on = [kubernetes_manifest.cns_gateway]
}

# ============================================================================
# ADVERSARIAL TESTING (OPTIONAL)
# ============================================================================

resource "kubernetes_deployment" "adversarial_coordinator" {
  count = var.enable_adversarial_testing ? 1 : 0
  
  metadata {
    name      = "adversarial-coordinator"
    namespace = kubernetes_namespace.cns.metadata[0].name
    labels = {
      app       = "cns"
      component = "adversarial-testing"
    }
  }

  spec {
    replicas = 1

    selector {
      match_labels = {
        app       = "cns"
        component = "adversarial-testing"
      }
    }

    template {
      metadata {
        labels = {
          app       = "cns"
          component = "adversarial-testing"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.cns_sa.metadata[0].name

        container {
          name  = "adversarial-coordinator"
          image = "cns/adversarial-tester:latest"
          
          port {
            container_port = 8088
            name          = "coordinator"
          }

          env {
            name  = "TARGET_SERVICES"
            value = "cns-protection-service:8080,cns-gateway-service:8081,cns-analytics-service:8082,cns-monitor-service:8083"
          }
          
          env {
            name  = "NAMESPACE"
            value = var.namespace
          }
          
          env {
            name  = "SURVIVAL_RATE_TARGET"
            value = "90"
          }

          resources {
            requests = {
              cpu    = "100m"
              memory = "128Mi"
            }
            limits = {
              cpu    = "500m"
              memory = "512Mi"
            }
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

resource "kubernetes_service" "adversarial_coordinator" {
  count = var.enable_adversarial_testing ? 1 : 0
  
  metadata {
    name      = "adversarial-coordinator-service"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    selector = {
      app       = "cns"
      component = "adversarial-testing"
    }

    port {
      name        = "coordinator"
      port        = 8088
      target_port = 8088
    }

    type = "ClusterIP"
  }
}

# Continuous adversarial testing CronJob
resource "kubernetes_cron_job_v1" "adversarial_testing" {
  count = var.enable_adversarial_testing ? 1 : 0
  
  metadata {
    name      = "adversarial-testing"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    schedule = "*/15 * * * *"  # Every 15 minutes

    job_template {
      metadata {
        labels = {
          app = "cns-adversarial-testing"
        }
      }

      spec {
        template {
          metadata {
            labels = {
              app = "cns-adversarial-testing"
            }
          }

          spec {
            service_account_name = kubernetes_service_account.cns_sa.metadata[0].name
            restart_policy       = "OnFailure"

            container {
              name  = "adversarial-test"
              image = "cns/adversarial-tester:latest"
              
              command = ["/app/run-adversarial-tests.sh"]

              env {
                name  = "TARGET_SERVICES"
                value = "cns-protection-service:8080,cns-gateway-service:8081,cns-analytics-service:8082,cns-monitor-service:8083"
              }

              resources {
                requests = {
                  cpu    = "100m"
                  memory = "128Mi"
                }
                limits = {
                  cpu    = "200m"
                  memory = "256Mi"
                }
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
  }
}

# ============================================================================
# MONITORING STACK (OPTIONAL)
# ============================================================================

resource "helm_release" "prometheus" {
  count      = var.enable_monitoring ? 1 : 0
  name       = "prometheus"
  repository = "https://prometheus-community.github.io/helm-charts"
  chart      = "kube-prometheus-stack"
  namespace  = "monitoring"
  version    = "55.0.0"

  create_namespace = true

  set {
    name  = "prometheus.prometheusSpec.serviceMonitorSelectorNilUsesHelmValues"
    value = "false"
  }

  set {
    name  = "prometheus.prometheusSpec.podMonitorSelectorNilUsesHelmValues"
    value = "false"
  }

  set {
    name  = "grafana.adminPassword"
    value = "admin"
  }
}

# ServiceMonitor for CNS services
resource "kubernetes_manifest" "cns_service_monitor" {
  count = var.enable_monitoring ? 1 : 0
  
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "cns-services"
      namespace = kubernetes_namespace.cns.metadata[0].name
      labels = {
        app = "cns"
      }
    }
    spec = {
      selector = {
        matchLabels = {
          app = "cns"
        }
      }
      endpoints = [
        {
          port     = "metrics"
          interval = "30s"
          path     = "/metrics"
        }
      ]
    }
  }

  depends_on = [helm_release.prometheus]
}

# ============================================================================
# RESOURCE QUOTAS AND LIMITS
# ============================================================================

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

resource "kubernetes_limit_range" "cns_limits" {
  metadata {
    name      = "cns-limit-range"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    limit {
      type = "Container"
      default = {
        cpu    = "100m"
        memory = "128Mi"
      }
      default_request = {
        cpu    = "50m"
        memory = "64Mi"
      }
    }
  }
}

# PodDisruptionBudget for high availability
resource "kubernetes_pod_disruption_budget_v1" "cns_pdb" {
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

# ============================================================================
# OUTPUTS
# ============================================================================

output "namespace" {
  description = "CNS namespace"
  value       = kubernetes_namespace.cns.metadata[0].name
}

output "service_endpoints" {
  description = "CNS service endpoints"
  value = {
    protection = "cns-protection-service.${var.namespace}.svc.cluster.local:8080"
    gateway    = "cns-gateway-service.${var.namespace}.svc.cluster.local:8081"
    analytics  = "cns-analytics-service.${var.namespace}.svc.cluster.local:8082"
    monitor    = "cns-monitor-service.${var.namespace}.svc.cluster.local:8083"
  }
}

output "istio_gateway_ip" {
  description = "Istio Gateway external IP"
  value = var.enable_service_mesh ? try(
    data.kubernetes_service.istio_gateway[0].status[0].load_balancer[0].ingress[0].ip,
    "pending"
  ) : "disabled"
}

# Data source for Istio Gateway IP
data "kubernetes_service" "istio_gateway" {
  count = var.enable_service_mesh ? 1 : 0
  
  metadata {
    name      = "istio-ingressgateway"
    namespace = "istio-ingress"
  }
  
  depends_on = [helm_release.istio_gateway]
}