# Enhanced secret management for CIS compliance
# Addresses failures in SECRET_MANAGEMENT_AUDIT

# Sealed Secrets for encryption at rest
resource "kubernetes_manifest" "sealed_secrets_controller" {
  manifest = {
    apiVersion = "apps/v1"
    kind       = "Deployment"
    
    metadata = {
      name      = "sealed-secrets-controller"
      namespace = "kube-system"
      labels = {
        name = "sealed-secrets-controller"
      }
    }
    
    spec = {
      replicas = 1
      
      selector = {
        matchLabels = {
          name = "sealed-secrets-controller"
        }
      }
      
      template = {
        metadata = {
          labels = {
            name = "sealed-secrets-controller"
          }
        }
        
        spec = {
          serviceAccountName = "sealed-secrets-controller"
          
          containers = [{
            name  = "sealed-secrets-controller"
            image = "quay.io/bitnami/sealed-secrets-controller:v0.18.0"
            
            ports = [{
              containerPort = 8080
              name         = "http"
            }]
            
            livenessProbe = {
              httpGet = {
                path = "/healthz"
                port = "http"
              }
              initialDelaySeconds = 30
              timeoutSeconds      = 1
              periodSeconds       = 10
              successThreshold    = 1
              failureThreshold    = 3
            }
            
            readinessProbe = {
              httpGet = {
                path = "/healthz" 
                port = "http"
              }
              initialDelaySeconds = 5
              timeoutSeconds      = 1
              periodSeconds       = 10
              successThreshold    = 1
              failureThreshold    = 3
            }
            
            securityContext = {
              readOnlyRootFilesystem   = true
              runAsNonRoot            = true
              runAsUser               = 1001
              allowPrivilegeEscalation = false
              capabilities = {
                drop = ["ALL"]
              }
            }
          }]
        }
      }
    }
  }
}

# ServiceAccount for sealed secrets
resource "kubernetes_service_account" "sealed_secrets_controller" {
  metadata {
    name      = "sealed-secrets-controller"
    namespace = "kube-system"
    
    labels = {
      name = "sealed-secrets-controller"
    }
  }
}

# RBAC for sealed secrets controller
resource "kubernetes_cluster_role" "sealed_secrets_controller" {
  metadata {
    name = "secrets-unsealer"
    
    labels = {
      name = "sealed-secrets-controller"
    }
  }

  rule {
    api_groups = [""]
    resources  = ["secrets"]
    verbs      = ["get", "create", "update"]  # Removed "list" and "delete" for security
    resource_names = ["sealed-secrets-key"]  # Restrict to specific secret only
  }

  rule {
    api_groups = [""]
    resources  = ["events"]
    verbs      = ["create", "patch"]
  }

  rule {
    api_groups = ["bitnami.com"]
    resources  = ["sealedsecrets"]
    verbs      = ["get", "list", "watch"]
  }
}

resource "kubernetes_cluster_role_binding" "sealed_secrets_controller" {
  metadata {
    name = "sealed-secrets-controller"
    
    labels = {
      name = "sealed-secrets-controller"
    }
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "ClusterRole"
    name      = kubernetes_cluster_role.sealed_secrets_controller.metadata[0].name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.sealed_secrets_controller.metadata[0].name
    namespace = "kube-system"
  }
}

# Enhanced CNS secrets with proper encryption
resource "kubernetes_manifest" "cns_sealed_secret" {
  manifest = {
    apiVersion = "bitnami.com/v1alpha1"
    kind       = "SealedSecret"
    
    metadata = {
      name      = "cns-sealed-secret"
      namespace = var.namespace
    }
    
    spec = {
      # SECURITY FIX: Use external secret references instead of hardcoded values
      encryptedData = {
        # These should be generated using kubeseal from external secret sources
        api_key      = var.sealed_api_key      # From external secret management
        db_password  = var.sealed_db_password  # From external secret management  
        tls_cert     = var.sealed_tls_cert     # From external certificate authority
        tls_key      = var.sealed_tls_key      # From external certificate authority
      }
      
      template = {
        metadata = {
          name      = "cns-secret"
          namespace = var.namespace
        }
        type = "Opaque"
      }
    }
  }
}

# Secret scanning with Falco rules
resource "kubernetes_config_map" "falco_secrets_rules" {
  metadata {
    name      = "falco-secrets-rules"
    namespace = "falco-system"
  }

  data = {
    "secrets_rules.yaml" = <<-EOT
      - rule: Detect Secret Access
        desc: Detect when secrets are accessed by unauthorized processes
        condition: >
          k8s_audit and
          ka.verb in (get, list) and
          ka.target.resource=secrets and
          not ka.user.name in (system:serviceaccount:kube-system:generic-garbage-collector,
                               system:serviceaccount:kube-system:attachdetach-controller,
                               system:serviceaccount:${var.namespace}:cns-service-account)
        output: >
          Unauthorized secret access (user=%ka.user.name verb=%ka.verb 
          target=%ka.target.resource reason=%ka.reason.reason)
        priority: WARNING
        
      - rule: Detect Secret Mount
        desc: Detect when containers mount secrets in unexpected ways
        condition: >
          container and
          fd.name startswith /var/run/secrets and
          not proc.name in (pause, dumb-init) and
          not container.image.repository in (k8s.gcr.io/pause, gcr.io/google_containers/pause)
        output: >
          Container accessing secrets (command=%proc.cmdline image=%container.image.repository 
          file=%fd.name)
        priority: INFO
        
      - rule: Detect Base64 Encoded Secrets
        desc: Detect potential secrets being base64 encoded
        condition: >
          spawned_process and
          proc.cmdline contains "base64" and
          proc.cmdline contains "echo"
        output: >
          Potential secret encoding detected (command=%proc.cmdline user=%user.name 
          container=%container.name)
        priority: WARNING
    EOT
  }
}

# Secret rotation CronJob
resource "kubernetes_cron_job_v1" "secret_rotation" {
  metadata {
    name      = "cns-secret-rotation"
    namespace = var.namespace
  }

  spec {
    schedule                      = "0 2 1 * *"  # Monthly at 2 AM
    successful_jobs_history_limit = 3
    failed_jobs_history_limit     = 1

    job_template {
      metadata {
        labels = {
          app = "secret-rotation"
        }
      }

      spec {
        template {
          metadata {
            labels = {
              app = "secret-rotation"
            }
          }

          spec {
            service_account_name = "secret-rotator"
            restart_policy       = "OnFailure"

            container {
              name  = "secret-rotator"
              image = "bitnami/kubectl:latest"

              command = ["sh", "-c"]
              args = [
                <<-EOT
                  # Generate new API key
                  NEW_API_KEY=$(openssl rand -hex 32)
                  
                  # Update secret
                  kubectl patch secret cns-secret -p='{"data":{"api_key":"'$(echo -n $NEW_API_KEY | base64)'"}}'
                  
                  # Restart deployment to pick up new secret
                  kubectl rollout restart deployment/cns-deployment
                  
                  echo "Secret rotation completed successfully"
                EOT
              ]

              security_context {
                run_as_non_root            = true
                run_as_user                = 1000
                read_only_root_filesystem  = true
                allow_privilege_escalation = false
                
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

# ServiceAccount for secret rotation
resource "kubernetes_service_account" "secret_rotator" {
  metadata {
    name      = "secret-rotator"
    namespace = var.namespace
  }
}

# Role for secret rotation
resource "kubernetes_role" "secret_rotator" {
  metadata {
    name      = "secret-rotator"
    namespace = var.namespace
  }

  rule {
    api_groups = [""]
    resources  = ["secrets"]
    verbs      = ["get", "patch", "update"]
  }

  rule {
    api_groups = ["apps"]
    resources  = ["deployments"]
    verbs      = ["get", "patch"]
  }
}

# RoleBinding for secret rotation
resource "kubernetes_role_binding" "secret_rotator" {
  metadata {
    name      = "secret-rotator"
    namespace = var.namespace
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "Role"
    name      = kubernetes_role.secret_rotator.metadata[0].name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.secret_rotator.metadata[0].name
    namespace = var.namespace
  }
}

# External secrets operator integration
resource "kubernetes_manifest" "external_secrets_store" {
  count = var.enable_external_secrets ? 1 : 0
  
  manifest = {
    apiVersion = "external-secrets.io/v1beta1"
    kind       = "SecretStore"
    
    metadata = {
      name      = "cns-secret-store"
      namespace = var.namespace
    }
    
    spec = {
      provider = {
        aws = {
          service = "SecretsManager"
          region  = var.region
          auth = {
            secretRef = {
              accessKeyID = {
                name = "aws-secret"
                key  = "access-key"
              }
              secretAccessKey = {
                name = "aws-secret" 
                key  = "secret-key"
              }
            }
          }
        }
      }
    }
  }
}

# External secret definition
resource "kubernetes_manifest" "external_secret" {
  count = var.enable_external_secrets ? 1 : 0
  
  manifest = {
    apiVersion = "external-secrets.io/v1beta1"
    kind       = "ExternalSecret"
    
    metadata = {
      name      = "cns-external-secret"
      namespace = var.namespace
    }
    
    spec = {
      refreshInterval = "1h"
      
      secretStoreRef = {
        name = "cns-secret-store"
        kind = "SecretStore"
      }
      
      target = {
        name           = "cns-secret"
        creationPolicy = "Owner"
      }
      
      data = [
        {
          secretKey = "api_key"
          remoteRef = {
            key = "cns/api-key"
          }
        },
        {
          secretKey = "db_password"
          remoteRef = {
            key = "cns/db-password"
          }
        }
      ]
    }
  }
}

# NOTE: enable_external_secrets variable is defined in variables.tf to avoid duplicates

# Variables for external secret references (SECURITY FIX)
variable "sealed_api_key" {
  description = "Sealed API key from external secret management (use kubeseal to encrypt)"
  type        = string
  default     = ""
  sensitive   = true
}

variable "sealed_db_password" {
  description = "Sealed database password from external secret management (use kubeseal to encrypt)" 
  type        = string
  default     = ""
  sensitive   = true
}

variable "sealed_tls_cert" {
  description = "Sealed TLS certificate from external CA (use kubeseal to encrypt)"
  type        = string
  default     = ""
  sensitive   = true
}

variable "sealed_tls_key" {
  description = "Sealed TLS private key from external CA (use kubeseal to encrypt)"
  type        = string
  default     = ""  
  sensitive   = true
}