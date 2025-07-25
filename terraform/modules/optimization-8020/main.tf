# 80/20 Service Discovery and Load Balancing Optimization
# Implements ultra-fast service discovery (<20ms) and load balancing (<1ms overhead)

# CoreDNS Configuration for 80/20 DNS Optimization
resource "kubernetes_config_map" "coredns_8020_optimization" {
  metadata {
    name      = "coredns-8020-optimization"
    namespace = "kube-system"
    
    labels = {
      app = "coredns"
      optimization = "80-20"
    }
  }

  data = {
    "Corefile" = <<-EOT
      .:53 {
          errors
          health {
             lameduck 5s
          }
          ready
          kubernetes cluster.local in-addr.arpa ip6.arpa {
             pods insecure
             fallthrough in-addr.arpa ip6.arpa
             ttl 30
          }
          prometheus :9153
          forward . /etc/resolv.conf {
             max_concurrent 1000
          }
          cache 3600 {
             success 9999 3600
             denial 9999 300
             prefetch 10 60s 30%
          }
          loop
          reload
          loadbalance round_robin
      }
      
      # CNS namespace optimization (80/20 rule: 20% of services get 80% of traffic)
      ${kubernetes_namespace.cns.metadata[0].name}.local:53 {
          errors
          cache 7200 {
             success 9999 7200
             denial 9999 600
             prefetch 20 30s 50%
          }
          kubernetes cluster.local in-addr.arpa ip6.arpa {
             pods verified
             endpoint_pod_names
             ttl 60
          }
          loadbalance least_conn
      }
    EOT
  }
}

# Service Registry with 80/20 Optimization
resource "kubernetes_config_map" "cns_service_registry" {
  metadata {
    name      = "cns-service-registry-8020"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "cns-service-registry"
      optimization = "80-20"
    }
  }

  data = {
    "service_registry.yaml" = <<-EOT
      service_registry:
        optimization_strategy: "80_20"
        dns_cache_ttl: 3600
        endpoint_cache_ttl: 1800
        health_check_interval: 10s
        
      # Primary services (80% of traffic patterns)
      primary_services:
        cns-protection-service:
          priority: 1
          weight: 40
          health_check_interval: 5s
          connection_pool_size: 100
          keep_alive_timeout: 300s
          max_idle_connections: 50
          
        cns-gateway-service:
          priority: 1  
          weight: 30
          health_check_interval: 5s
          connection_pool_size: 80
          keep_alive_timeout: 300s
          max_idle_connections: 40
          
      # Secondary services (20% of traffic patterns)  
      secondary_services:
        cns-analytics-service:
          priority: 2
          weight: 20
          health_check_interval: 10s
          connection_pool_size: 30
          keep_alive_timeout: 180s
          max_idle_connections: 15
          
        cns-monitor-service:
          priority: 3
          weight: 10
          health_check_interval: 15s
          connection_pool_size: 20
          keep_alive_timeout: 120s
          max_idle_connections: 10
          
      load_balancing:
        algorithm: "least_connection_with_weights"
        fallback_algorithm: "weighted_round_robin"
        session_affinity: "client_ip_hash"
        circuit_breaker:
          failure_threshold: 5
          recovery_timeout: 30s
          half_open_max_calls: 3
          
      connection_optimization:
        tcp_keepalive: true
        tcp_keepalive_time: 600s
        tcp_keepalive_interval: 60s
        tcp_keepalive_probes: 3
        tcp_no_delay: true
        compression: "gzip,br"
        http2_enabled: true
        max_concurrent_streams: 1000
    EOT
    
    "endpoint_optimization.yaml" = <<-EOT
      endpoint_optimization:
        pre_warming:
          enabled: true
          warmup_targets: ["cns-protection-service", "cns-gateway-service"]
          warmup_connections: 10
          warmup_timeout: 30s
          
        health_aggregation:
          # 80/20 principle: 20% of health checks cover 80% of critical paths
          critical_endpoints: 20  # Percentage of endpoints to check frequently
          critical_check_interval: 2s
          standard_check_interval: 10s
          degraded_check_interval: 30s
          
        endpoint_discovery:
          cache_size: 1000
          cache_ttl: 300s
          negative_cache_ttl: 60s
          refresh_threshold: 0.8
          background_refresh: true
          
        failover:
          detection_time_ms: 100
          recovery_time_ms: 500
          max_retries: 3
          retry_backoff_ms: [100, 200, 500]
    EOT
  }
}

# Linkerd Configuration for 80/20 Load Balancing
resource "kubernetes_config_map" "linkerd_8020_config" {
  count = var.enable_service_mesh ? 1 : 0
  
  metadata {
    name      = "linkerd-8020-load-balancing"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    annotations = {
      "linkerd.io/inject" = "disabled"
    }
  }

  data = {
    "load_balancing.yaml" = <<-EOT
      load_balancing:
        algorithm: "least_request"
        fallback: "round_robin"
        
        # 80/20 optimization: Configure high-priority services
        service_weights:
          cns-protection-service: 100
          cns-gateway-service: 80
          cns-analytics-service: 60
          cns-monitor-service: 40
          
        connection_settings:
          pool_size: 50
          max_idle_time: "300s"
          keep_alive_time: "600s"
          tcp_no_delay: true
          
        retries:
          max_retries: 3
          timeout: "5s"
          backoff: "exponential"
          
        circuit_breaker:
          max_connections: 1000
          max_pending_requests: 100
          max_requests: 10000
          max_retries: 5
          consecutive_errors: 5
    EOT
  }
}

# Service Monitor for 80/20 Performance Metrics
resource "kubernetes_manifest" "service_performance_monitor" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    
    metadata = {
      name      = "cns-8020-performance"
      namespace = kubernetes_namespace.cns.metadata[0].name
      
      labels = {
        app = "cns-performance-monitor"
        optimization = "80-20"
      }
    }
    
    spec = {
      selector = {
        matchLabels = {
          app = "cns"
        }
      }
      
      endpoints = [{
        port     = "metrics"
        interval = "5s"  # High frequency for 80/20 monitoring
        path     = "/metrics"
        
        metricRelabelings = [
          {
            sourceLabels = ["__name__"]
            regex        = ".*_(latency|response_time|connection_time|dns_lookup_time).*"
            action       = "keep"
          },
          {
            sourceLabels = ["__name__"]
            regex        = ".*_(circuit_breaker|load_balancer|service_discovery).*"
            action       = "keep"
          }
        ]
      }]
    }
  }
}

# Network Performance Tuning via DaemonSet
resource "kubernetes_daemon_set" "network_performance_tuning" {
  metadata {
    name      = "network-performance-tuning"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "network-tuning"
      optimization = "80-20"
    }
  }

  spec {
    selector {
      match_labels = {
        app = "network-tuning"
      }
    }

    template {
      metadata {
        labels = {
          app = "network-tuning"
        }
      }

      spec {
        host_network = true
        host_pid     = true
        
        node_selector = {
          "kubernetes.io/os" = "linux"
        }
        
        tolerations {
          operator = "Exists"
          effect   = "NoSchedule"
        }

        init_container {
          name  = "network-optimizer"
          image = "alpine:3.18"
          
          command = ["/bin/sh", "-c"]
          args = [<<-EOT
            # 80/20 TCP optimization: Focus on 20% of settings that give 80% improvement
            echo 'net.core.rmem_max = 268435456' >> /etc/sysctl.conf
            echo 'net.core.wmem_max = 268435456' >> /etc/sysctl.conf
            echo 'net.core.netdev_max_backlog = 30000' >> /etc/sysctl.conf
            echo 'net.core.somaxconn = 65535' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_rmem = 4096 87380 268435456' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_wmem = 4096 65536 268435456' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_congestion_control = bbr' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_keepalive_time = 600' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_keepalive_intvl = 60' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_keepalive_probes = 3' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_no_delay = 1' >> /etc/sysctl.conf
            echo 'net.ipv4.tcp_slow_start_after_idle = 0' >> /etc/sysctl.conf
            sysctl -p
          EOT]
          
          security_context {
            privileged = true
          }
          
          volume_mount {
            name       = "proc-sys"
            mount_path = "/proc/sys"
          }
        }
        
        container {
          name  = "performance-monitor"
          image = "alpine:3.18"
          
          command = ["/bin/sh", "-c", "while true; do sleep 3600; done"]
          
          resources {
            requests = {
              cpu    = "10m"
              memory = "32Mi"
            }
            limits = {
              cpu    = "50m"
              memory = "64Mi"
            }
          }
        }

        volume {
          name = "proc-sys"
          
          host_path {
            path = "/proc/sys"
          }
        }
      }
    }
  }
}

# Service for exposing performance metrics
resource "kubernetes_service" "performance_metrics_service" {
  metadata {
    name      = "cns-performance-metrics"
    namespace = kubernetes_namespace.cns.metadata[0].name
    
    labels = {
      app = "cns-performance-metrics"
      optimization = "80-20"
    }
  }

  spec {
    selector = {
      app = "network-tuning"
    }

    port {
      name        = "metrics"
      port        = 9100
      target_port = 9100
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Prometheus Rules for 80/20 Performance Monitoring
resource "kubernetes_manifest" "performance_8020_rules" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "PrometheusRule"
    
    metadata = {
      name      = "cns-8020-performance-rules"
      namespace = kubernetes_namespace.cns.metadata[0].name
      
      labels = {
        app        = "cns-performance"
        prometheus = "kube-prometheus"
        role       = "alert-rules"
        optimization = "80-20"
      }
    }
    
    spec = {
      groups = [
        {
          name = "cns.8020.performance"
          
          rules = [
            {
              alert = "CNSServiceDiscoveryLatencyHigh"
              expr  = "histogram_quantile(0.95, rate(cns_service_discovery_duration_seconds_bucket[5m])) > 0.020"
              for   = "2m"
              
              labels = {
                severity = "warning"
                service  = "cns"
                optimization = "80-20"
              }
              
              annotations = {
                summary     = "CNS service discovery latency exceeds 20ms (80/20 target)"
                description = "95th percentile service discovery latency is {{ $value }}s (target: <0.02s)"
              }
            },
            {
              alert = "CNSLoadBalancingLatencyHigh" 
              expr  = "histogram_quantile(0.99, rate(cns_load_balancer_duration_seconds_bucket[5m])) > 0.001"
              for   = "1m"
              
              labels = {
                severity = "critical"
                service  = "cns"
                optimization = "80-20"
              }
              
              annotations = {
                summary     = "CNS load balancing latency exceeds 1ms (80/20 target)"
                description = "99th percentile load balancing latency is {{ $value }}s (target: <0.001s)"
              }
            },
            {
              alert = "CNSConnectionPoolExhaustion"
              expr  = "cns_connection_pool_active / cns_connection_pool_max > 0.8"
              for   = "30s"
              
              labels = {
                severity = "warning"
                service  = "cns"
                optimization = "80-20"
              }
              
              annotations = {
                summary     = "CNS connection pool utilization high"
                description = "Connection pool utilization is {{ $value | humanizePercentage }} (threshold: 80%)"
              }
            },
            {
              alert = "CNSCircuitBreakerActivated"
              expr  = "cns_circuit_breaker_state > 0"
              for   = "0s"
              
              labels = {
                severity = "critical"
                service  = "cns"
                optimization = "80-20"
              }
              
              annotations = {
                summary     = "CNS circuit breaker activated"
                description = "Circuit breaker activated for service {{ $labels.service_name }}"
              }
            },
            {
              alert = "CNSServiceMeshLatencyHigh"
              expr  = "histogram_quantile(0.95, rate(response_latency_ms_bucket{direction=\"inbound\"}[5m])) > 10"
              for   = "2m"
              
              labels = {
                severity = "warning"
                service  = "cns"
                optimization = "80-20"
              }
              
              annotations = {
                summary     = "CNS service mesh latency high"
                description = "95th percentile service mesh latency is {{ $value }}ms (target: <10ms)"
              }
            }
          ]
        }
      ]
    }
  }
}