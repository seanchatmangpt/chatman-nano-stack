# K8s Service Mesh Configuration for Inter-Service Communication
# Enhanced 80/20 implementation with Istio service mesh for secure service-to-service communication

# Istio namespace
resource "kubernetes_namespace" "istio_system" {
  metadata {
    name = "istio-system"
    
    labels = {
      name = "istio-system"
      istio-injection = "disabled"
      security = "enhanced"
    }
  }
}

# Istio control plane installation via Helm
resource "helm_release" "istio_base" {
  name       = "istio-base"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "base"
  namespace  = kubernetes_namespace.istio_system.metadata[0].name
  
  create_namespace = false
  
  set {
    name  = "global.istioNamespace"
    value = "istio-system"
  }
}

resource "helm_release" "istiod" {
  name       = "istiod"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "istiod"
  namespace  = kubernetes_namespace.istio_system.metadata[0].name
  
  depends_on = [helm_release.istio_base]
  
  set {
    name  = "global.meshID"
    value = "mesh1"
  }
  
  set {
    name  = "global.meshConfig.defaultConfig.proxyStatsMatcher.inclusionRegexps"
    value = ".*_cx_.*|.*_rq_.*|.*_rbac_.*"
  }
  
  # Enhanced security settings
  set {
    name  = "pilot.env.PILOT_ENABLE_WORKLOAD_IDENTITY"
    value = "true"
  }
  
  set {
    name  = "pilot.env.PILOT_ENABLE_CROSS_CLUSTER_WORKLOAD_ENTRY"
    value = "true"
  }
  
  values = [yamlencode({
    meshConfig = {
      # Default mTLS policy
      defaultConfig = {
        holdApplicationUntilProxyStarts = true
        proxyMetadata = {
          PILOT_ENABLE_IP_AUTOALLOCATION = "true"
        }
      }
      # Global mesh policy for inter-service communication
      defaultProviders = {
        metrics = ["prometheus"]
        tracing = ["jaeger"]
        accessLogging = ["envoy"]
      }
    }
  })]
}

# Istio Gateway for ingress traffic
resource "helm_release" "istio_ingress" {
  name       = "istio-ingress"
  repository = "https://istio-release.storage.googleapis.com/charts"
  chart      = "gateway"
  namespace  = kubernetes_namespace.istio_system.metadata[0].name
  
  depends_on = [helm_release.istiod]
  
  set {
    name  = "service.type"
    value = "LoadBalancer"
  }
  
  set {
    name  = "autoscaling.enabled"
    value = "true"
  }
  
  set {
    name  = "autoscaling.minReplicas"
    value = "2"
  }
  
  set {
    name  = "autoscaling.maxReplicas"
    value = "5"
  }
}

# Enable Istio injection for CNS namespace
resource "kubernetes_labels" "cns_namespace_istio" {
  api_version = "v1"
  kind        = "Namespace"
  
  metadata {
    name = var.namespace
  }
  
  labels = {
    "istio-injection" = "enabled"
    "service-mesh" = "istio"
    "mtls" = "strict"
  }
  
  depends_on = [helm_release.istiod]
}

# Peer Authentication for strict mTLS
resource "kubernetes_manifest" "peer_authentication_default" {
  manifest = {
    apiVersion = "security.istio.io/v1beta1"
    kind       = "PeerAuthentication"
    metadata = {
      name      = "default"
      namespace = var.namespace
    }
    spec = {
      mtls = {
        mode = "STRICT"
      }
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Authorization Policy for CNS services
resource "kubernetes_manifest" "authorization_policy_cns" {
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
          # Allow traffic from services in the same namespace
          from = [
            {
              source = {
                namespaces = [var.namespace]
              }
            }
          ]
          to = [
            {
              operation = {
                methods = ["GET", "POST"]
                paths = ["/api/*", "/health", "/ready", "/metrics"]
              }
            }
          ]
        },
        {
          # Allow Prometheus scraping
          from = [
            {
              source = {
                principals = ["cluster.local/ns/istio-system/sa/prometheus"]
              }
            }
          ]
          to = [
            {
              operation = {
                methods = ["GET"]
                paths = ["/metrics"]
              }
            }
          ]
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Virtual Service for CNS traffic management
resource "kubernetes_manifest" "virtual_service_cns" {
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "VirtualService"
    metadata = {
      name      = "cns-virtual-service"
      namespace = var.namespace
    }
    spec = {
      http = [
        {
          match = [
            {
              uri = {
                prefix = "/api/v1/"
              }
            }
          ]
          route = [
            {
              destination = {
                host = "cns-service"
                port = {
                  number = 8080
                }
              }
              weight = 100
            }
          ]
          # Enhanced routing policies
          retries = {
            attempts = 3
            perTryTimeout = "5s"
            retryOn = "5xx,gateway-error,connect-failure,refused-stream"
          }
          timeout = "30s"
          fault = {
            delay = {
              percentage = {
                value = 0.01  # 0.01% of requests for chaos testing
              }
              fixedDelay = "100ms"
            }
          }
        },
        {
          match = [
            {
              uri = {
                exact = "/health"
              }
            }
          ]
          route = [
            {
              destination = {
                host = "cns-service"
                port = {
                  number = 8080
                }
              }
            }
          ]
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Destination Rule for CNS service
resource "kubernetes_manifest" "destination_rule_cns" {
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "DestinationRule"
    metadata = {
      name      = "cns-destination-rule"
      namespace = var.namespace
    }
    spec = {
      host = "cns-service"
      trafficPolicy = {
        # Load balancing policy
        loadBalancer = {
          simple = "LEAST_REQUEST"
        }
        # Connection pool settings
        connectionPool = {
          tcp = {
            maxConnections = 100
            connectTimeout = "10s"
            tcpKeepalive = {
              time = "7200s"
              interval = "75s"
            }
          }
          http = {
            http1MaxPendingRequests = 10
            http2MaxRequests = 100
            maxRequestsPerConnection = 10
            maxRetries = 3
            consecutiveGatewayFailure = 5
            interval = "30s"
            baseEjectionTime = "30s"
          }
        }
        # Circuit breaker
        outlierDetection = {
          consecutiveGatewayFailure = 5
          consecutive5xxErrors = 5
          interval = "30s"
          baseEjectionTime = "30s"
          maxEjectionPercent = 50
          minHealthPercent = 50
        }
      }
      # mTLS settings
      portLevelSettings = [
        {
          port = {
            number = 8080
          }
          tls = {
            mode = "ISTIO_MUTUAL"
          }
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Service Monitor for Prometheus scraping
resource "kubernetes_manifest" "service_monitor_cns" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "cns-service-monitor"
      namespace = var.namespace
      labels = {
        app = "cns"
        monitoring = "enabled"
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
          port = "metrics"
          path = "/metrics"
          interval = "30s"
          scrapeTimeout = "10s"
        },
        {
          port = "http"
          path = "/stats/prometheus"
          interval = "30s"
          scrapeTimeout = "10s"
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod]
}

# Telemetry v2 configuration for enhanced observability
resource "kubernetes_manifest" "telemetry_v2_cns" {
  manifest = {
    apiVersion = "telemetry.istio.io/v1alpha1"
    kind       = "Telemetry"
    metadata = {
      name      = "cns-telemetry"
      namespace = var.namespace
    }
    spec = {
      metrics = [
        {
          providers = [
            {
              name = "prometheus"
            }
          ]
          overrides = [
            {
              match = {
                metric = "ALL_METRICS"
              }
              tagOverrides = {
                "service_name" = {
                  value = "cns-enhanced"
                }
                "service_version" = {
                  value = "v1.0.0"
                }
              }
            }
          ]
        }
      ]
      tracing = [
        {
          providers = [
            {
              name = "jaeger"
            }
          ]
        }
      ]
      accessLogging = [
        {
          providers = [
            {
              name = "envoy"
            }
          ]
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Gateway for external access
resource "kubernetes_manifest" "gateway_cns" {
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "Gateway"
    metadata = {
      name      = "cns-gateway"
      namespace = var.namespace
    }
    spec = {
      selector = {
        istio = "ingress"
      }
      servers = [
        {
          port = {
            number = 443
            name = "https"
            protocol = "HTTPS"
          }
          tls = {
            mode = "SIMPLE"
            credentialName = "cns-tls-secret"
          }
          hosts = [
            "cns.${var.environment}.example.com"
          ]
        },
        {
          port = {
            number = 80
            name = "http"
            protocol = "HTTP"
          }
          hosts = [
            "cns.${var.environment}.example.com"
          ]
          # Redirect HTTP to HTTPS
          tls = {
            httpsRedirect = true
          }
        }
      ]
    }
  }
  
  depends_on = [helm_release.istio_ingress]
}

# Sidecar configuration for optimized resource usage
resource "kubernetes_manifest" "sidecar_cns" {
  manifest = {
    apiVersion = "networking.istio.io/v1beta1"
    kind       = "Sidecar"
    metadata = {
      name      = "cns-sidecar"
      namespace = var.namespace
    }
    spec = {
      workloadSelector = {
        labels = {
          app = "cns"
        }
      }
      ingress = [
        {
          port = {
            number = 8080
            protocol = "HTTP"
            name = "http"
          }
          defaultEndpoint = "127.0.0.1:8080"
        }
      ]
      egress = [
        {
          # Allow egress to services in the same namespace
          hosts = [
            "./${var.namespace}/*"
          ]
        },
        {
          # Allow egress to istio-system for telemetry
          hosts = [
            "./istio-system/*"
          ]
        },
        {
          # Allow egress to external services
          hosts = [
            "istio-system/*",
            "./*"
          ]
        }
      ]
    }
  }
  
  depends_on = [helm_release.istiod, kubernetes_labels.cns_namespace_istio]
}

# Output service mesh information
output "service_mesh_enabled" {
  value = true
  description = "Istio service mesh enabled for inter-service communication"
}

output "mtls_enabled" {
  value = true
  description = "Mutual TLS enabled for secure service-to-service communication"
}

output "istio_namespace" {
  value = kubernetes_namespace.istio_system.metadata[0].name
  description = "Istio system namespace"
}

output "gateway_external_ip" {
  value = helm_release.istio_ingress.status
  description = "Istio ingress gateway status"
}