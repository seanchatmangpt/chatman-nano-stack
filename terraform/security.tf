# Security-hardened Kubernetes resources for CNS deployment
# Implements 80/20 security best practices

# Pod Security Policy for strict security controls
resource "kubernetes_pod_security_policy" "cns_security" {
  metadata {
    name = "${var.namespace}-psp"
  }

  spec {
    privileged                 = false
    allow_privilege_escalation = false
    
    # Drop all capabilities
    required_drop_capabilities = ["ALL"]
    
    # Allowed volume types
    volumes = [
      "configMap",
      "emptyDir",
      "projected",
      "secret",
      "downwardAPI",
      "persistentVolumeClaim"
    ]
    
    # Must run as non-root
    run_as_user {
      rule = "MustRunAsNonRoot"
    }
    
    se_linux {
      rule = "RunAsAny"
    }
    
    fs_group {
      rule = "RunAsAny"
    }
    
    # Read-only root filesystem
    read_only_root_filesystem = true
  }
}

# Network Policy - Zero trust network security
resource "kubernetes_network_policy" "cns_network" {
  metadata {
    name      = "${var.namespace}-netpol"
    namespace = var.namespace
  }

  spec {
    pod_selector {
      match_labels = {
        app = "cns-quantum-compiler"
      }
    }
    
    policy_types = ["Ingress", "Egress"]
    
    # Ingress rules - only from same namespace
    ingress {
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
    }
    
    # Egress rules - only DNS and specific services
    egress {
      # Allow DNS
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
    }
    
    egress {
      # Allow HTTPS for external services
      ports {
        port     = "443"
        protocol = "TCP"
      }
    }
  }
}

# ConfigMap with security configuration
resource "kubernetes_config_map" "cns_security_config" {
  metadata {
    name      = "${var.namespace}-security-config"
    namespace = var.namespace
  }

  data = {
    "security_config.yaml" = yamlencode({
      security = {
        # Input validation limits from security_utils.py
        max_file_size       = 104857600  # 100MB
        max_string_length   = 1048576    # 1MB  
        max_collection_size = 10000
        
        # Forbidden patterns for code injection prevention
        forbidden_patterns = [
          "system(", "exec(", "__import__", "subprocess",
          "eval(", "shell=True", "/bin/", "rm -rf",
          "chmod", "setuid", "execve(", "__asm__",
          "$(", "`", "\\x", "popen(", "fork("
        ]
        
        # Path security
        allowed_directories = [
          "/app/data",
          "/app/cache", 
          "/tmp"
        ]
        
        # Additional hardening
        enable_path_canonicalization = true
        enable_input_validation      = true
        enable_code_sanitization     = true
      }
    })
  }
}

# NOTE: Service Account, Role, and Resource Quota are defined in main.tf to avoid duplicates

# Additional RBAC Role for security operations
resource "kubernetes_role" "cns_security_reader" {
  metadata {
    name      = "${var.namespace}-security-reader"
    namespace = var.namespace
  }

  # Allow reading security configs only
  rule {
    api_groups = [""]
    resources  = ["configmaps"]
    verbs      = ["get", "list"]
    resource_names = ["${var.namespace}-security-config"]
  }
}

# Security Context Constraints (for OpenShift compatibility)
resource "kubernetes_manifest" "cns_scc" {
  count = var.enable_openshift_scc ? 1 : 0
  
  manifest = {
    apiVersion = "security.openshift.io/v1"
    kind       = "SecurityContextConstraints"
    metadata = {
      name = "${var.namespace}-scc"
    }
    
    # Strict security settings
    allowHostDirVolumePlugin = false
    allowHostIPC             = false
    allowHostNetwork         = false
    allowHostPID             = false
    allowHostPorts           = false
    allowPrivilegedContainer = false
    allowedCapabilities      = []
    defaultAddCapabilities   = []
    fsGroup = {
      type = "MustRunAs"
      ranges = [{
        min = 2000
        max = 2000
      }]
    }
    readOnlyRootFilesystem = true
    requiredDropCapabilities = ["ALL"]
    runAsUser = {
      type = "MustRunAsNonRoot"
    }
    seLinuxContext = {
      type = "MustRunAs"
    }
    volumes = [
      "configMap",
      "downwardAPI",
      "emptyDir",
      "persistentVolumeClaim",
      "projected",
      "secret"
    ]
  }
}

# Variables for security configuration
variable "enable_openshift_scc" {
  description = "Enable OpenShift Security Context Constraints"
  type        = bool
  default     = false
}