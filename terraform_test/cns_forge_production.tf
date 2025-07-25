# CNS Aegis Fabric - Terraform Infrastructure
# Generated from TTL: cybersecurity_core.ttl
# Template: templates/terraform_aegis.tf.j2
# Generator: cns_forge_implementation.py
# Timestamp: 2025-07-25T10:50:29Z
# NO HANDCODING - This file is auto-generated

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

# Aegis Fabric namespace
resource "kubernetes_namespace" "aegis_fabric" {
  metadata {
    name = "cns-system"
    
    labels = {
      "app"                        = "aegis-fabric"
      "version"                    = "2025-07-25-10-50-29Z"
      "linkerd.io/inject"         = "enabled"
      "pod-security.kubernetes.io/enforce" = "restricted"
      "pod-security.kubernetes.io/audit"   = "restricted"
      "pod-security.kubernetes.io/warn"    = "restricted"
    }
    
    annotations = {
      "aegis.cns.io/threat-count"      = "20"
      "aegis.cns.io/gossip-enabled"    = "true"
      "aegis.cns.io/service-mesh"      = "linkerd"
      "generated-from"                 = "cybersecurity_core.ttl"
    }
  }
}

# Network policy for Aegis Fabric components
resource "kubernetes_network_policy" "aegis_fabric_network_policy" {
  metadata {
    name      = "aegis-fabric-network-policy"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }

  spec {
    pod_selector {
      match_labels = {
        app = "aegis-fabric"
      }
    }

    # Allow ingress from service mesh
    ingress {
      from {
        namespace_selector {
          match_labels = {
            "linkerd.io/control-plane-ns" = "linkerd"
          }
        }
      }
      ports {
        port     = "4143"
        protocol = "TCP"
      }
    }

    # Allow ingress for gRPC
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "aegis-fabric"
          }
        }
      }
      ports {
        port     = "8080"
        protocol = "TCP"
      }
    }

    # Allow ingress for gossip protocol
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "aegis-fabric"
          }
        }
      }
      ports {
        port     = "7946"
        protocol = "TCP"
      }
      ports {
        port     = "7946"
        protocol = "UDP"
      }
    }

    # Allow ingress for Erlang distribution
    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "aegis-fabric"
          }
        }
      }
      ports {
        port     = "4369"
        protocol = "TCP"
      }
      ports {
        port     = "9100-9200"
        protocol = "TCP"
      }
    }

    # Allow metrics scraping
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

    # Egress rules
    egress {
      # Allow DNS
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
      # Allow internal communication
      to {
        pod_selector {
          match_labels = {
            app = "aegis-fabric"
          }
        }
      }
    }

    egress {
      # Allow HTTPS for external threat feeds
      ports {
        port     = "443"
        protocol = "TCP"
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

# Resource quota for Aegis Fabric
resource "kubernetes_resource_quota" "aegis_fabric_quota" {
  metadata {
    name      = "aegis-fabric-quota"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }

  spec {
    hard = {
      "requests.cpu"    = "3000m"
      "requests.memory" = "3072Mi"
      "limits.cpu"      = "6000m"
      "limits.memory"   = "6144Mi"
      "pods"            = "30"
      "persistentvolumeclaims" = "10"
    }
  }
}

# Priority class for critical Aegis components
resource "kubernetes_priority_class" "aegis_critical" {
  metadata {
    name = "aegis-critical"
  }
  
  value          = 1000000
  global_default = false
  description    = "Priority class for critical Aegis Fabric components"
}

# Storage class for Aegis persistent data
resource "kubernetes_storage_class" "aegis_fast_ssd" {
  metadata {
    name = "aegis-fast-ssd"
    
    annotations = {
      "storageclass.kubernetes.io/is-default-class" = "false"
    }
  }
  
  storage_provisioner    = "kubernetes.io/aws-ebs"
  reclaim_policy        = "Retain"
  allow_volume_expansion = true
  volume_binding_mode   = "WaitForFirstConsumer"
  
  parameters = {
    type      = "gp3"
    iops      = "16000"
    throughput = "1000"
    encrypted = "true"
  }
}

# Persistent volume claim for threat signature cache
resource "kubernetes_persistent_volume_claim" "aegis_threat_cache" {
  metadata {
    name      = "aegis-threat-cache"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }

  spec {
    access_modes       = ["ReadWriteMany"]
    storage_class_name = kubernetes_storage_class.aegis_fast_ssd.metadata[0].name

    resources {
      requests = {
        storage = "100Gi"
      }
    }
  }
}

# Secret for Aegis encryption keys (placeholder - use external secret manager in production)
resource "kubernetes_secret" "aegis_encryption_keys" {
  metadata {
    name      = "aegis-encryption-keys"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }

  type = "Opaque"

  data = {
    gossip_key = base64encode("aegis-gossip-${substr(sha256("2025-07-25T10:50:29Z"), 0, 16)}")
    tls_cert   = base64encode("placeholder-cert")
    tls_key    = base64encode("placeholder-key")
  }
}

# ConfigMap for dynamic threat updates
resource "kubernetes_config_map" "aegis_dynamic_threats" {
  metadata {
    name      = "aegis-dynamic-threats"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
    
    annotations = {
      "aegis.cns.io/auto-reload" = "true"
      "aegis.cns.io/reload-interval" = "60s"
    }
  }

  data = {
    "dynamic_threats.yaml" = <<-EOT
      # Dynamic threat updates
      # These can be updated without regenerating from TTL
      dynamic_threats:
        - id: "zero-day-placeholder"
          type: "unknown"
          priority: "critical"
          pattern: "placeholder"
          enabled: false
    EOT
  }
}

# Monitoring and alerting rules
resource "kubernetes_config_map" "aegis_prometheus_rules" {
  metadata {
    name      = "aegis-prometheus-rules"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
    
    labels = {
      "prometheus" = "kube-prometheus"
      "role"       = "alert-rules"
    }
  }

  data = {
    "aegis.rules" = <<-EOT
      groups:
      - name: aegis.rules
        interval: 30s
        rules:
        - alert: AegisThreatDetectionRateLow
          expr: aegis_threat_detection_rate < 0.95
          for: 5m
          labels:
            severity: critical
            component: aegis
          annotations:
            summary: "Aegis threat detection rate below target"
            description: "Detection rate {{ $value }} is below target 0.95"
        
        - alert: AegisFalsePositiveRateHigh
          expr: aegis_false_positive_rate > 0.05
          for: 5m
          labels:
            severity: warning
            component: aegis
          annotations:
            summary: "Aegis false positive rate above threshold"
            description: "False positive rate {{ $value }} exceeds threshold 0.05"
        
        - alert: AegisGossipConvergenceSlow
          expr: aegis_gossip_convergence_ms > 500
          for: 2m
          labels:
            severity: warning
            component: gossip
          annotations:
            summary: "Gossip protocol convergence slow"
            description: "Convergence time {{ $value }}ms exceeds target 500ms"
        
        - alert: AegisHighLatency
          expr: histogram_quantile(0.95, aegis_request_duration_seconds_bucket) > 0.01
          for: 5m
          labels:
            severity: warning
            component: aegis
          annotations:
            summary: "Aegis P95 latency high"
            description: "P95 latency {{ $value }}s exceeds target"
    EOT
  }
}

# Outputs
output "aegis_namespace" {
  value = kubernetes_namespace.aegis_fabric.metadata[0].name
  description = "Aegis Fabric namespace"
}

output "threat_count" {
  value = 20
  description = "Number of threat signatures deployed"
}

output "critical_threats" {
  value = 5
  description = "Number of critical threat signatures"
}

output "gossip_config" {
  value = {
    fanout = 3
    interval_ms = 100
    max_hops = 5
    convergence_target_ms = 500
  }
  description = "Gossip protocol configuration"
}

output "service_mesh_enabled" {
  value = true
  description = "Service mesh mTLS status"
}

output "performance_targets" {
  value = {
    detection_rate = 0.95
    false_positive_rate = 0.05
    latency_ms = 10
    throughput_rps = 50000
  }
  description = "Performance target metrics"
}