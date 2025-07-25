# BitActor Kubernetes Deployment - Terraform Configuration

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = ">= 2.20.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = ">= 2.9.0"
    }
  }
}

# Configure Kubernetes provider
provider "kubernetes" {
  config_path = var.kubeconfig_path
}

# Configure Helm provider
provider "helm" {
  kubernetes {
    config_path = var.kubeconfig_path
  }
}

# Create namespace
resource "kubernetes_namespace" "bitactor" {
  metadata {
    name = var.namespace
    
    labels = {
      app        = "bitactor"
      managed-by = "terraform"
      environment = var.environment
    }
  }
}

# Create ConfigMap for BitActor configuration
resource "kubernetes_config_map" "bitactor_config" {
  metadata {
    name      = "bitactor-config"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  data = {
    "bitactor.yaml" = yamlencode({
      server = {
        port = 9090
        metrics_enabled = true
        tick_budget = var.tick_budget
        ring_size = var.ring_size
      }
      performance = {
        thread_count = var.thread_count
        batch_size = 1000
      }
      security = {
        enable_tls = var.enable_tls
        atomic_operations = true
        endianness_check = true
      }
    })
  }
}

# Create service account
resource "kubernetes_service_account" "bitactor" {
  metadata {
    name      = "bitactor-sa"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  automount_service_account_token = false
}

# Create RBAC role
resource "kubernetes_role" "bitactor" {
  metadata {
    name      = "bitactor-role"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  rule {
    api_groups = [""]
    resources  = ["pods", "services"]
    verbs      = ["get", "list", "watch"]
  }
  
  rule {
    api_groups = ["metrics.k8s.io"]
    resources  = ["pods"]
    verbs      = ["get", "list"]
  }
}

# Create role binding
resource "kubernetes_role_binding" "bitactor" {
  metadata {
    name      = "bitactor-rolebinding"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "Role"
    name      = kubernetes_role.bitactor.metadata[0].name
  }
  
  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.bitactor.metadata[0].name
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
}

# Deploy BitActor using Helm
resource "helm_release" "bitactor" {
  name       = "bitactor"
  namespace  = kubernetes_namespace.bitactor.metadata[0].name
  chart      = "../helm/bitactor"
  
  values = [
    templatefile("../helm/bitactor/values.yaml", {
      image_repository = var.image_repository
      image_tag       = var.image_tag
      replicas        = var.replicas
      resources       = var.resources
      autoscaling     = var.autoscaling
      environment     = var.environment
    })
  ]
  
  set {
    name  = "serviceAccount.name"
    value = kubernetes_service_account.bitactor.metadata[0].name
  }
  
  set {
    name  = "configMap.name"
    value = kubernetes_config_map.bitactor_config.metadata[0].name
  }
  
  depends_on = [
    kubernetes_namespace.bitactor,
    kubernetes_service_account.bitactor,
    kubernetes_config_map.bitactor_config
  ]
}

# Create horizontal pod autoscaler
resource "kubernetes_horizontal_pod_autoscaler_v2" "bitactor" {
  metadata {
    name      = "bitactor-hpa"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = "bitactor"
    }
    
    min_replicas = var.autoscaling.min_replicas
    max_replicas = var.autoscaling.max_replicas
    
    metric {
      type = "Resource"
      
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = var.autoscaling.target_cpu_utilization
        }
      }
    }
    
    metric {
      type = "Resource"
      
      resource {
        name = "memory"
        target {
          type                = "Utilization"
          average_utilization = var.autoscaling.target_memory_utilization
        }
      }
    }
    
    # Custom metric for signal processing rate
    metric {
      type = "Pods"
      
      pods {
        metric {
          name = "bitactor_signals_per_second"
        }
        target {
          type          = "AverageValue"
          average_value = "1000000" # 1M signals/sec per pod
        }
      }
    }
  }
}

# Create PodDisruptionBudget
resource "kubernetes_pod_disruption_budget" "bitactor" {
  metadata {
    name      = "bitactor-pdb"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  spec {
    min_available = var.pod_disruption_budget.min_available
    
    selector {
      match_labels = {
        app = "bitactor"
      }
    }
  }
}

# Create NetworkPolicy for secure inter-pod communication
resource "kubernetes_network_policy" "bitactor" {
  metadata {
    name      = "bitactor-netpol"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
  
  spec {
    pod_selector {
      match_labels = {
        app = "bitactor"
      }
    }
    
    policy_types = ["Ingress", "Egress"]
    
    # Allow monitoring and observability
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "monitoring"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
    }
    
    # Allow inter-pod communication within BitActor
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "bitactor"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      
      ports {
        port     = "8081"
        protocol = "TCP"
      }
    }
    
    # Allow Kubernetes system components
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "kube-system"
          }
        }
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
    }
    
    # Allow ingress controllers
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "ingress-nginx"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
    }
    
    ingress {
      from {
        namespace_selector {
          match_labels = {
            name = "istio-system"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
    }
    
    # Allow inter-pod egress communication
    egress {
      to {
        pod_selector {
          match_labels = {
            app = "bitactor"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
      
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      
      ports {
        port     = "8081"
        protocol = "TCP"
      }
    }
    
    # Allow DNS resolution
    egress {
      to {
        namespace_selector {
          match_labels = {
            name = "kube-system"
          }
        }
      }
      
      ports {
        port     = "53"
        protocol = "UDP"
      }
      
      ports {
        port     = "53"
        protocol = "TCP"
      }
    }
    
    # Allow Kubernetes API server access for service discovery
    egress {
      to {}
      
      ports {
        port     = "443"
        protocol = "TCP"
      }
      
      ports {
        port     = "6443"
        protocol = "TCP"
      }
    }
    
    # Allow metrics export to monitoring
    egress {
      to {
        namespace_selector {
          match_labels = {
            name = "monitoring"
          }
        }
      }
      
      ports {
        port     = "9090"
        protocol = "TCP"
      }
      
      ports {
        port     = "9091"
        protocol = "TCP"
      }
    }
    
    # Allow external service discovery
    egress {
      to {}
      
      ports {
        port     = "80"
        protocol = "TCP"
      }
      
      ports {
        port     = "8500"
        protocol = "TCP"
      }
      
      ports {
        port     = "2379"
        protocol = "TCP"
      }
      
      ports {
        port     = "2380"
        protocol = "TCP"
      }
    }
  }
}

# Create Falco runtime security monitoring
resource "kubernetes_daemonset" "falco" {
  metadata {
    name      = "falco"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
    labels = {
      app = "falco"
    }
  }

  spec {
    selector {
      match_labels = {
        app = "falco"
      }
    }

    template {
      metadata {
        labels = {
          app = "falco"
        }
      }

      spec {
        service_account_name = "falco-sa"
        host_network         = true
        host_pid            = true

        container {
          name  = "falco"
          image = "falcosecurity/falco:0.37.1"

          security_context {
            privileged = true
          }

          args = [
            "/usr/bin/falco",
            "--cri=/run/containerd/containerd.sock",
            "--cri=/run/crio/crio.sock",
            "-K", "/var/run/secrets/kubernetes.io/serviceaccount/token",
            "-k", "https://kubernetes.default",
            "-pk"
          ]

          volume_mount {
            mount_path = "/host/var/run/docker.sock"
            name       = "docker-socket"
          }

          volume_mount {
            mount_path = "/host/dev"
            name       = "dev-fs"
          }

          volume_mount {
            mount_path = "/host/proc"
            name       = "proc-fs"
            read_only  = true
          }

          volume_mount {
            mount_path = "/host/boot"
            name       = "boot-fs"
            read_only  = true
          }

          volume_mount {
            mount_path = "/host/lib/modules"
            name       = "lib-modules"
            read_only  = true
          }

          volume_mount {
            mount_path = "/host/usr"
            name       = "usr-fs"
            read_only  = true
          }

          volume_mount {
            mount_path = "/host/etc"
            name       = "etc-fs"
            read_only  = true
          }

          resources {
            limits = {
              cpu    = "200m"
              memory = "512Mi"
            }
            requests = {
              cpu    = "100m"
              memory = "256Mi"
            }
          }
        }

        volume {
          name = "docker-socket"
          host_path {
            path = "/var/run/docker.sock"
          }
        }

        volume {
          name = "dev-fs"
          host_path {
            path = "/dev"
          }
        }

        volume {
          name = "proc-fs"
          host_path {
            path = "/proc"
          }
        }

        volume {
          name = "boot-fs"
          host_path {
            path = "/boot"
          }
        }

        volume {
          name = "lib-modules"
          host_path {
            path = "/lib/modules"
          }
        }

        volume {
          name = "usr-fs"
          host_path {
            path = "/usr"
          }
        }

        volume {
          name = "etc-fs"
          host_path {
            path = "/etc"
          }
        }

        toleration {
          effect = "NoSchedule"
          key    = "node-role.kubernetes.io/master"
        }

        toleration {
          effect = "NoSchedule"
          key    = "node-role.kubernetes.io/control-plane"
        }
      }
    }
  }
}

# Create Falco service account
resource "kubernetes_service_account" "falco" {
  metadata {
    name      = "falco-sa"
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
}

# Create Falco cluster role
resource "kubernetes_cluster_role" "falco" {
  metadata {
    name = "falco"
  }

  rule {
    api_groups = [""]
    resources  = ["events", "pods", "replicasets", "services", "daemonsets", "deployments", "nodes", "namespaces"]
    verbs      = ["get", "list", "watch", "create", "update", "patch"]
  }

  rule {
    api_groups = ["apps"]
    resources  = ["daemonsets", "deployments", "replicasets"]
    verbs      = ["get", "list", "watch"]
  }

  rule {
    api_groups = ["extensions"]
    resources  = ["daemonsets", "deployments", "replicasets"]
    verbs      = ["get", "list", "watch"]
  }
}

# Create Falco cluster role binding
resource "kubernetes_cluster_role_binding" "falco" {
  metadata {
    name = "falco"
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "ClusterRole"
    name      = kubernetes_cluster_role.falco.metadata[0].name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.falco.metadata[0].name
    namespace = kubernetes_namespace.bitactor.metadata[0].name
  }
}