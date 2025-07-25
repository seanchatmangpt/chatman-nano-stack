output "service_endpoints" {
  description = "Service endpoints for inter-service communication"
  value = {
    protection = "cns-protection-service.${var.namespace}.svc.cluster.local:8080"
    gateway    = "cns-gateway-service.${var.namespace}.svc.cluster.local:8081"
    analytics  = "cns-analytics-service.${var.namespace}.svc.cluster.local:8082"
    monitor    = "cns-monitor-service.${var.namespace}.svc.cluster.local:8083"
  }
}

output "deployment_names" {
  description = "Deployment names for testing"
  value = [
    "cns-protection-service",
    "cns-gateway-service", 
    "cns-analytics-service",
    "cns-monitor-service"
  ]
}