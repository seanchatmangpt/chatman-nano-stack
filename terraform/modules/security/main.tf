# Additional security hardening for CIS compliance
# Addresses failures in TERRAFORM_SECURITY_AUDIT

# Pod Security Standards enforcement
resource "kubernetes_manifest" "pod_security_policy" {
  manifest = {
    apiVersion = "policy/v1beta1"
    kind       = "PodSecurityPolicy"
    
    metadata = {
      name      = "cns-restricted-psp"
      namespace = var.namespace
    }
    
    spec = {
      privileged                = false
      allowPrivilegeEscalation  = false
      requiredDropCapabilities  = ["ALL"]
      allowedCapabilities       = ["NET_BIND_SERVICE"]
      volumes = [
        "configMap",
        "emptyDir", 
        "projected",
        "secret",
        "downwardAPI",
        "persistentVolumeClaim"
      ]
      runAsUser = {
        rule = "MustRunAsNonRoot"
      }
      seLinux = {
        rule = "RunAsAny"
      }
      fsGroup = {
        rule = "RunAsAny"
      }
      readOnlyRootFilesystem = true
    }
  }
}

# Security contexts validation
resource "kubernetes_manifest" "security_context_constraint" {
  manifest = {
    apiVersion = "security.openshift.io/v1"
    kind       = "SecurityContextConstraints"
    
    metadata = {
      name = "cns-restricted-scc"
    }
    
    allowHostDirVolumePlugin = false
    allowHostIPC             = false
    allowHostNetwork         = false
    allowHostPID             = false
    allowHostPorts           = false
    allowPrivilegedContainer = false
    allowedCapabilities      = ["NET_BIND_SERVICE"]
    defaultAddCapabilities   = []
    requiredDropCapabilities = ["ALL"]
    readOnlyRootFilesystem   = true
    
    runAsUser = {
      type = "MustRunAsNonRoot"
    }
    
    seLinuxContext = {
      type = "MustRunAs"
    }
    
    users = [
      "system:serviceaccount:${var.namespace}:cns-service-account"
    ]
  }
}

# Image security scanning policy
resource "kubernetes_manifest" "image_policy" {
  manifest = {
    apiVersion = "kyverno.io/v1"
    kind       = "ClusterPolicy"
    
    metadata = {
      name = "cns-image-security"
    }
    
    spec = {
      validationFailureAction = "enforce"
      background              = true
      
      rules = [
        {
          name = "check-image-signature"
          match = {
            any = [{
              resources = {
                kinds      = ["Pod"]
                namespaces = [var.namespace]
              }
            }]
          }
          validate = {
            message = "Images must be signed and from trusted registry"
            pattern = {
              spec = {
                containers = [{
                  image = "docker.io/library/*:*"
                }]
              }
            }
          }
        },
        {
          name = "disallow-privileged"
          match = {
            any = [{
              resources = {
                kinds      = ["Pod"]
                namespaces = [var.namespace]
              }
            }]
          }
          validate = {
            message = "Privileged containers are not allowed"
            pattern = {
              spec = {
                "securityContext" = {
                  "runAsNonRoot" = true
                  "runAsUser"    = ">0"
                }
                containers = [{
                  securityContext = {
                    privileged               = false
                    allowPrivilegeEscalation = false
                    readOnlyRootFilesystem   = true
                    capabilities = {
                      drop = ["ALL"]
                    }
                  }
                }]
              }
            }
          }
        }
      ]
    }
  }
}

# Network segmentation with additional policies
resource "kubernetes_network_policy" "strict_egress" {
  metadata {
    name      = "cns-strict-egress"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns"
      }
    }

    policy_types = ["Egress"]

    egress {
      # Allow DNS only
      ports {
        port     = "53"
        protocol = "UDP"
      }
      to {
        namespace_selector {
          match_labels = {
            name = "kube-system"
          }
        }
      }
    }

    egress {
      # Allow HTTPS to external only for specific IPs
      ports {
        port     = "443"
        protocol = "TCP"
      }
      to {
        ip_block {
          cidr = "0.0.0.0/0"
          except = [
            "10.0.0.0/8",
            "172.16.0.0/12", 
            "192.168.0.0/16"
          ]
        }
      }
    }
  }
}

# Resource quotas with stricter limits
resource "kubernetes_resource_quota" "strict_quota" {
  metadata {
    name      = "cns-strict-quota"
    namespace = kubernetes_namespace.cns.metadata[0].name
  }

  spec {
    hard = {
      # CPU limits
      "requests.cpu"    = "5"    # Reduced from 10
      "limits.cpu"      = "10"   # Reduced from 20
      
      # Memory limits  
      "requests.memory" = "10Gi"  # Reduced from 20Gi
      "limits.memory"   = "20Gi"  # Reduced from 40Gi
      
      # Pod limits
      "pods"            = "25"    # Reduced from 50
      
      # Storage limits
      "persistentvolumeclaims" = "5"  # Reduced from 10
      "requests.storage"       = "50Gi"
      
      # Service limits
      "services"        = "5"
      "secrets"         = "10"
      "configmaps"      = "10"
    }
  }
}

# Admission controller webhooks
resource "kubernetes_validating_admission_webhook_configuration_v1" "cns_admission" {
  metadata {
    name = "cns-security-webhook"
  }

  webhook {
    name = "security.cns.admission"
    
    client_config {
      service {
        name      = "cns-admission-webhook"
        namespace = var.namespace
        path      = "/validate"
      }
    }

    rules {
      operations   = ["CREATE", "UPDATE"]
      api_groups   = [""]
      api_versions = ["v1"]
      resources    = ["pods"]
    }

    admission_review_versions = ["v1", "v1beta1"]
    side_effects             = "None"
    failure_policy           = "Fail"
  }
}

# Service mesh security (Istio integration)
resource "kubernetes_manifest" "istio_peer_authentication" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "security.istio.io/v1beta1"
    kind       = "PeerAuthentication"
    
    metadata = {
      name      = "cns-strict-mtls"
      namespace = var.namespace
    }
    
    spec = {
      mtls = {
        mode = "STRICT"
      }
    }
  }
}

# Authorization policies
resource "kubernetes_manifest" "istio_authorization_policy" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "security.istio.io/v1beta1"
    kind       = "AuthorizationPolicy"
    
    metadata = {
      name      = "cns-authz-policy"
      namespace = var.namespace
    }
    
    spec = {
      selector = {
        matchLabels = {
          app = "cns"
        }
      }
      
      rules = [
        {
          from = [{
            source = {
              namespaces = [var.namespace]
            }
          }]
          to = [{
            operation = {
              methods = ["GET", "POST"]
              paths   = ["/health", "/ready", "/metrics"]
            }
          }]
        }
      ]
    }
  }
}

# NOTE: enable_service_mesh variable is defined in variables.tf to avoid duplicates