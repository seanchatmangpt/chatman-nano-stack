# Terraform outputs for BitActor deployment

output "namespace" {
  description = "The namespace where BitActor is deployed"
  value       = kubernetes_namespace.bitactor.metadata[0].name
}

output "service_name" {
  description = "The name of the BitActor service"
  value       = "${helm_release.bitactor.name}-bitactor"
}

output "service_endpoint" {
  description = "The internal service endpoint"
  value       = "${helm_release.bitactor.name}-bitactor.${kubernetes_namespace.bitactor.metadata[0].name}.svc.cluster.local:9090"
}

output "deployment_status" {
  description = "Status of the Helm deployment"
  value       = helm_release.bitactor.status
}

output "deployment_version" {
  description = "Version of the deployed chart"
  value       = helm_release.bitactor.version
}

output "hpa_status" {
  description = "Horizontal Pod Autoscaler configuration"
  value = {
    min_replicas = kubernetes_horizontal_pod_autoscaler_v2.bitactor.spec[0].min_replicas
    max_replicas = kubernetes_horizontal_pod_autoscaler_v2.bitactor.spec[0].max_replicas
    metrics      = kubernetes_horizontal_pod_autoscaler_v2.bitactor.spec[0].metric
  }
}

output "monitoring_enabled" {
  description = "Whether monitoring is enabled"
  value       = var.monitoring.prometheus_enabled
}

output "security_features" {
  description = "Enabled security features"
  value = {
    tls_enabled          = var.enable_tls
    atomic_operations    = true
    endianness_handling  = true
    network_policy       = true
    pod_security_context = true
  }
}