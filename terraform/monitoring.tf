# ServiceMonitor for Prometheus scraping
resource "kubernetes_manifest" "cns_service_monitor" {
  depends_on = [kubernetes_namespace.cns]
  
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    
    metadata = {
      name      = "cns-metrics"
      namespace = var.namespace
      
      labels = {
        app     = "cns"
        release = "prometheus"
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
        interval = "30s"
        path     = "/metrics"
        
        metricRelabelings = [
          {
            sourceLabels = ["__name__"]
            regex        = "benchmark_duration_ms|performance_score|test_results_total|neural_inference_rate|thread_count|process_count|enhanced_.*|volatility_.*|manipulation_.*|correlation_.*|circuit_breaker_.*|adversarial_.*"
            action       = "keep"
          }
        ]
      }]
    }
  }
}

# PrometheusRule for alerting
resource "kubernetes_manifest" "cns_prometheus_rules" {
  depends_on = [kubernetes_namespace.cns]
  
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "PrometheusRule"
    
    metadata = {
      name      = "cns-alerts"
      namespace = var.namespace
      
      labels = {
        app        = "cns"
        prometheus = "kube-prometheus"
        role       = "alert-rules"
      }
    }
    
    spec = {
      groups = [
        {
          name = "cns.security"
          
          rules = [
            {
              alert = "CNSThreadLimitExceeded"
              expr  = "thread_count > 8"
              for   = "5m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS thread count approaching limit"
                description = "CNS instance {{ $labels.instance }} has {{ $value }} threads (limit: 10)"
              }
            },
            {
              alert = "CNSProcessLimitExceeded"
              expr  = "process_count > 4"
              for   = "5m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS process count approaching limit"
                description = "CNS instance {{ $labels.instance }} has {{ $value }} processes (limit: 5)"
              }
            },
            {
              alert = "CNSResourceExhaustion"
              expr  = "rate(resource_exhaustion_attempts_total[5m]) > 0"
              for   = "1m"
              
              labels = {
                severity = "critical"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS under resource exhaustion attack"
                description = "CNS instance {{ $labels.instance }} is experiencing resource exhaustion attempts"
              }
            }
          ]
        },
        {
          name = "cns.enhanced_protection"
          
          rules = [
            {
              alert = "CNSFlashCrashDetected"
              expr  = "rate(enhanced_volatility_detections_total[5m]) > 0"
              for   = "1m"
              
              labels = {
                severity = "critical"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS detected flash crash attack"
                description = "CNS instance {{ $labels.instance }} detected {{ $value }} volatility spikes per second"
              }
            },
            {
              alert = "CNSManipulationAttempt"
              expr  = "rate(enhanced_manipulation_blocks_total[5m]) > 0"
              for   = "30s"
              
              labels = {
                severity = "critical"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS blocked manipulation attempt"
                description = "CNS instance {{ $labels.instance }} blocked {{ $value }} manipulation attempts per second"
              }
            },
            {
              alert = "CNSCorrelationBreach"
              expr  = "rate(enhanced_correlation_blocks_total[5m]) > 0"
              for   = "1m"
              
              labels = {
                severity = "warning"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS correlation limit breached"
                description = "CNS instance {{ $labels.instance }} blocked {{ $value }} correlated positions per second"
              }
            },
            {
              alert = "CNSCircuitBreakerActive"
              expr  = "enhanced_circuit_breaker_active == 1"
              for   = "0s"
              
              labels = {
                severity = "critical"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS circuit breaker activated"
                description = "CNS instance {{ $labels.instance }} has activated circuit breaker protection"
              }
            },
            {
              alert = "CNSAdversarialTestFailure"
              expr  = "adversarial_survival_rate < 90"
              for   = "0s"
              
              labels = {
                severity = "critical"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS adversarial test failure"
                description = "CNS adversarial survival rate dropped to {{ $value }}% (required: 90%)"
              }
            },
            {
              alert = "CNSEnhancedProtectionDisabled"
              expr  = "enhanced_protection_enabled == 0"
              for   = "1m"
              
              labels = {
                severity = "critical"
                service  = "cns"
                type     = "security"
              }
              
              annotations = {
                summary     = "CNS enhanced protection disabled"
                description = "CNS instance {{ $labels.instance }} has enhanced protection disabled"
              }
            }
          ]
        },
        {
          name = "cns.performance"
          
          rules = [
            {
              alert = "CNSHighLatency"
              expr  = "benchmark_duration_ms > 50"
              for   = "10m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS benchmark latency high"
                description = "CNS instance {{ $labels.instance }} has benchmark latency of {{ $value }}ms"
              }
            },
            {
              alert = "CNSLowPerformanceScore"
              expr  = "performance_score < 90"
              for   = "10m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS performance score low"
                description = "CNS instance {{ $labels.instance }} has performance score of {{ $value }}"
              }
            },
            {
              alert = "CNSNeuralInferenceRateLow"
              expr  = "neural_inference_rate < 100000"
              for   = "5m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS neural inference rate low"
                description = "CNS instance {{ $labels.instance }} inference rate: {{ $value }}/sec"
              }
            }
          ]
        },
        {
          name = "cns.availability"
          
          rules = [
            {
              alert = "CNSDown"
              expr  = "up{job=\"cns\"} == 0"
              for   = "1m"
              
              labels = {
                severity = "critical"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS instance down"
                description = "CNS instance {{ $labels.instance }} has been down for more than 1 minute"
              }
            },
            {
              alert = "CNSHighErrorRate"
              expr  = "rate(test_results_total{status=\"failed\"}[5m]) > 0.1"
              for   = "5m"
              
              labels = {
                severity = "warning"
                service  = "cns"
              }
              
              annotations = {
                summary     = "CNS test failure rate high"
                description = "CNS instance {{ $labels.instance }} has {{ $value }} test failures per second"
              }
            }
          ]
        }
      ]
    }
  }
}

# Grafana Dashboard ConfigMap
resource "kubernetes_config_map" "cns_grafana_dashboard" {
  metadata {
    name      = "cns-grafana-dashboard"
    namespace = var.namespace
    
    labels = {
      grafana_dashboard = "1"
    }
  }

  data = {
    "cns-dashboard.json" = jsonencode({
      annotations = {
        list = [
          {
            builtIn    = 1
            datasource = "-- Grafana --"
            enable     = true
            hide       = true
            iconColor  = "rgba(0, 211, 255, 1)"
            name       = "Annotations & Alerts"
            type       = "dashboard"
          }
        ]
      }
      
      editable = true
      gnetId   = null
      graphTooltip = 0
      id       = null
      links    = []
      
      panels = [
        {
          datasource = "Prometheus"
          fieldConfig = {
            defaults = {
              color = {
                mode = "palette-classic"
              }
              custom = {
                axisLabel   = ""
                axisPlacement = "auto"
                barAlignment  = 0
                drawStyle     = "line"
                fillOpacity   = 10
                gradientMode  = "none"
                hideFrom = {
                  tooltip = false
                  viz     = false
                  legend  = false
                }
                lineInterpolation = "linear"
                lineWidth         = 1
                pointSize         = 5
                scaleDistribution = {
                  type = "linear"
                }
                showPoints = "never"
                spanNulls  = true
                stacking = {
                  group = "A"
                  mode  = "none"
                }
                thresholdsStyle = {
                  mode = "off"
                }
              }
              mappings  = []
              thresholds = {
                mode = "absolute"
                steps = [
                  {
                    color = "green"
                    value = null
                  },
                  {
                    color = "red"
                    value = 80
                  }
                ]
              }
              unit = "ms"
            }
            overrides = []
          }
          gridPos = {
            h = 8
            w = 12
            x = 0
            y = 0
          }
          id      = 1
          options = {
            tooltip = {
              mode = "single"
            }
            legend = {
              calcs       = []
              displayMode = "list"
              placement   = "bottom"
            }
          }
          pluginVersion = "8.0.0"
          targets = [
            {
              expr         = "benchmark_duration_ms"
              legendFormat = "{{binary}}"
              refId        = "A"
            }
          ]
          title = "Benchmark Duration"
          type  = "timeseries"
        },
        {
          datasource = "Prometheus"
          fieldConfig = {
            defaults = {
              color = {
                mode = "thresholds"
              }
              mappings = []
              thresholds = {
                mode = "absolute"
                steps = [
                  {
                    color = "red"
                    value = null
                  },
                  {
                    color = "yellow"
                    value = 90
                  },
                  {
                    color = "green"
                    value = 95
                  }
                ]
              }
              unit = "none"
              min  = 0
              max  = 100
            }
            overrides = []
          }
          gridPos = {
            h = 8
            w = 12
            x = 12
            y = 0
          }
          id      = 2
          options = {
            orientation   = "auto"
            reduceOptions = {
              values = false
              calcs  = ["lastNotNull"]
              fields = ""
            }
            showThresholdLabels  = false
            showThresholdMarkers = true
            text = {}
          }
          pluginVersion = "8.0.0"
          targets = [
            {
              expr  = "performance_score"
              refId = "A"
            }
          ]
          title = "Performance Score"
          type  = "gauge"
        },
        {
          datasource = "Prometheus"
          fieldConfig = {
            defaults = {
              color = {
                mode = "palette-classic"
              }
              custom = {
                hideFrom = {
                  tooltip = false
                  viz     = false
                  legend  = false
                }
              }
              mappings = []
              unit     = "short"
            }
            overrides = []
          }
          gridPos = {
            h = 8
            w = 12
            x = 0
            y = 8
          }
          id      = 3
          options = {
            legend = {
              displayMode = "list"
              placement   = "right"
              values      = ["value"]
            }
            pieType      = "pie"
            tooltip = {
              mode = "single"
            }
            displayLabels = ["name", "percent"]
          }
          targets = [
            {
              expr         = "sum by (status) (test_results_total)"
              legendFormat = "{{status}}"
              refId        = "A"
            }
          ]
          title = "Test Results"
          type  = "piechart"
        },
        {
          datasource = "Prometheus"
          fieldConfig = {
            defaults = {
              color = {
                mode = "palette-classic"
              }
              custom = {
                axisLabel   = ""
                axisPlacement = "auto"
                barAlignment  = 0
                drawStyle     = "line"
                fillOpacity   = 10
                gradientMode  = "none"
                hideFrom = {
                  tooltip = false
                  viz     = false
                  legend  = false
                }
                lineInterpolation = "linear"
                lineWidth         = 1
                pointSize         = 5
                scaleDistribution = {
                  type = "linear"
                }
                showPoints = "never"
                spanNulls  = true
                stacking = {
                  group = "A"
                  mode  = "none"
                }
                thresholdsStyle = {
                  mode = "off"
                }
              }
              mappings  = []
              thresholds = {
                mode = "absolute"
                steps = [
                  {
                    color = "green"
                    value = null
                  },
                  {
                    color = "red"
                    value = 10
                  }
                ]
              }
              unit = "short"
            }
            overrides = []
          }
          gridPos = {
            h = 8
            w = 12
            x = 12
            y = 8
          }
          id      = 4
          options = {
            tooltip = {
              mode = "single"
            }
            legend = {
              calcs       = ["last"]
              displayMode = "list"
              placement   = "bottom"
            }
          }
          pluginVersion = "8.0.0"
          targets = [
            {
              expr         = "thread_count"
              legendFormat = "Threads"
              refId        = "A"
            },
            {
              expr         = "process_count"
              legendFormat = "Processes"
              refId        = "B"
            }
          ]
          title = "Resource Usage (Security Limits)"
          type  = "timeseries"
        }
      ]
      
      refresh       = "10s"
      schemaVersion = 27
      style         = "dark"
      tags          = ["cns", "security", "performance"]
      templating    = {
        list = []
      }
      time = {
        from = "now-6h"
        to   = "now"
      }
      timepicker = {}
      timezone   = ""
      title      = "CNS Dashboard"
      uid        = "cns-main"
      version    = 1
    })
  }
}