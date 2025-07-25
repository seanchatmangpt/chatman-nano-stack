# Inter-Pod Communication Services Configuration
# Implementing 80/20 service discovery for CNS components

# CNS Protection Service (Main Application)
resource "kubernetes_service" "cns_protection_service" {
  metadata {
    name      = "cns-protection"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      component   = "protection"
      service     = "cns-protection"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "inter-pod.cns.io/discovery" = "enabled"
      "prometheus.io/scrape" = "true"
      "prometheus.io/port"   = "9090"
      "prometheus.io/path"   = "/metrics"
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
    
    port {
      name        = "health"
      port        = 8081
      target_port = 8081
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# CNS Gateway Service (API Gateway)
resource "kubernetes_service" "cns_gateway_service" {
  metadata {
    name      = "cns-gateway"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      component   = "gateway"
      service     = "cns-gateway"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "inter-pod.cns.io/discovery" = "enabled"
    }
  }

  spec {
    selector = {
      app       = "cns"
      component = "gateway"
    }

    port {
      name        = "http2"
      port        = 8081
      target_port = 8081
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# CNS Analytics Service 
resource "kubernetes_service" "cns_analytics_service" {
  metadata {
    name      = "cns-analytics"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      component   = "analytics"
      service     = "cns-analytics"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "inter-pod.cns.io/discovery" = "enabled"
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
      name        = "health"
      port        = 8081
      target_port = 8081
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# CNS Monitor Service
resource "kubernetes_service" "cns_monitor_service" {
  metadata {
    name      = "cns-monitor"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      component   = "monitor"
      service     = "cns-monitor"
    }
    
    annotations = {
      "linkerd.io/inject" = var.enable_service_mesh ? "enabled" : "disabled"
      "inter-pod.cns.io/discovery" = "enabled"
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
      name        = "status"
      port        = 8084
      target_port = 8084
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Headless Service for StatefulSet-like behavior
resource "kubernetes_service" "cns_headless_service" {
  metadata {
    name      = "cns-headless"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app         = "cns"
      type        = "headless"
      service     = "cns-headless"
    }
    
    annotations = {
      "inter-pod.cns.io/discovery" = "enabled"
      "service.alpha.kubernetes.io/tolerate-unready-endpoints" = "true"
    }
  }

  spec {
    selector = {
      app = "cns"
    }

    cluster_ip = "None"  # Makes it headless
    
    port {
      name        = "inter-pod"
      port        = 8080
      target_port = 8080
      protocol    = "TCP"
    }

    publish_not_ready_addresses = true
  }
}

# Service Monitor for Prometheus (if monitoring is enabled)
resource "kubernetes_manifest" "cns_service_monitor" {
  count = var.enable_service_mesh ? 1 : 0
  
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "cns-inter-pod-monitor"
      namespace = kubernetes_namespace.cns.metadata[0].name
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
        }
      ]
    }
  }
}

# EndpointSlice for fine-grained service discovery
resource "kubernetes_manifest" "cns_endpoint_slice" {
  manifest = {
    apiVersion = "discovery.k8s.io/v1"
    kind       = "EndpointSlice"
    metadata = {
      name      = "cns-inter-pod-endpoints"
      namespace = kubernetes_namespace.cns.metadata[0].name
      labels = {
        "kubernetes.io/service-name" = "cns-headless"
        app = "cns"
      }
    }
    addressType = "IPv4"
    ports = [
      {
        appProtocol = "grpc"
        name        = "grpc"
        port        = 8080
        protocol    = "TCP"
      },
      {
        appProtocol = "http"
        name        = "metrics"
        port        = 9090
        protocol    = "TCP"
      },
      {
        appProtocol = "http"
        name        = "health"
        port        = 8081
        protocol    = "TCP"
      }
    ]
  }
}

# ConfigMap for Service Discovery Configuration
resource "kubernetes_config_map" "cns_service_discovery_config" {
  metadata {
    name      = "cns-service-discovery-config"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app  = "cns"
      type = "service-discovery"
    }
    
    annotations = {
      "inter-pod.cns.io/config" = "enabled"
    }
  }

  data = {
    "discovery.yaml" = <<-EOT
      service_discovery:
        enabled: true
        dns_resolution: true
        health_checking: true
        load_balancing: "round_robin"
        
      services:
        cns-protection:
          endpoint: "cns-protection.${kubernetes_namespace.cns.metadata[0].name}.svc.cluster.local:8080"
          protocol: "grpc"
          health_check: "/health"
          timeout_ms: 5000
          
        cns-gateway:
          endpoint: "cns-gateway.${kubernetes_namespace.cns.metadata[0].name}.svc.cluster.local:8081"
          protocol: "http2" 
          health_check: "/ready"
          timeout_ms: 3000
          
        cns-analytics:
          endpoint: "cns-analytics.${kubernetes_namespace.cns.metadata[0].name}.svc.cluster.local:8082"
          protocol: "grpc"
          health_check: "/health"
          timeout_ms: 10000
          
        cns-monitor:
          endpoint: "cns-monitor.${kubernetes_namespace.cns.metadata[0].name}.svc.cluster.local:8083"
          protocol: "http"
          health_check: "/status"
          timeout_ms: 2000
          
      connectivity:
        max_connections_per_service: 100
        max_requests_per_second: 1000
        connection_timeout_seconds: 30
        keepalive_time_seconds: 60
        
      security:
        mutual_tls: true
        verify_certificates: true
        allow_insecure: false
        trust_domain: "cluster.local"
        
      observability:
        metrics_enabled: true
        tracing_enabled: true
        logging_enabled: true
        sampling_rate: 0.1
    EOT
    
    "endpoints.json" = jsonencode({
      "endpoints" = {
        "cns-protection" = {
          "host" = "cns-protection"
          "port" = 8080
          "scheme" = "grpc"
          "health_check_path" = "/health"
        }
        "cns-gateway" = {
          "host" = "cns-gateway"
          "port" = 8081
          "scheme" = "http2"
          "health_check_path" = "/ready"
        }
        "cns-analytics" = {
          "host" = "cns-analytics"
          "port" = 8082
          "scheme" = "grpc"
          "health_check_path" = "/health"
        }
        "cns-monitor" = {
          "host" = "cns-monitor"
          "port" = 8083
          "scheme" = "http"
          "health_check_path" = "/status"
        }
      }
      "connection_limits" = {
        "max_connections_per_pod" = 100
        "max_requests_per_second" = 1000
        "connection_timeout_seconds" = 30
      }
    })
  }
}

# Outputs for service discovery
output "inter_pod_services" {
  value = {
    cns_protection = kubernetes_service.cns_protection_service.metadata[0].name
    cns_gateway    = kubernetes_service.cns_gateway_service.metadata[0].name
    cns_analytics  = kubernetes_service.cns_analytics_service.metadata[0].name
    cns_monitor    = kubernetes_service.cns_monitor_service.metadata[0].name
    headless       = kubernetes_service.cns_headless_service.metadata[0].name
  }
  description = "Inter-pod communication services"
}

output "service_discovery_enabled" {
  value = true
  description = "Inter-pod service discovery is enabled and configured"
}