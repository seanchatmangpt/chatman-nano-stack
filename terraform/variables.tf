# Cluster Configuration
variable "cluster_name" {
  description = "Name of the Kubernetes cluster"
  type        = string
  default     = "cns-cluster"
}

variable "region" {
  description = "Cloud region for deployment"
  type        = string
  default     = "us-west-2"
}

# Resource Configuration
variable "node_pool_size" {
  description = "Number of nodes in the Kubernetes cluster"
  type        = number
  default     = 3
}

variable "node_machine_type" {
  description = "Machine type for cluster nodes"
  type        = string
  default     = "t3.large"
}

# CNS Application Configuration
variable "cns_image" {
  description = "Docker image for CNS application"
  type        = string
  default     = "cns:latest"
}

variable "cns_image_pull_policy" {
  description = "Image pull policy for CNS containers"
  type        = string
  default     = "IfNotPresent"
}

# Security Configuration
variable "enable_network_policies" {
  description = "Enable Kubernetes network policies"
  type        = bool
  default     = true
}

variable "enable_pod_security_policies" {
  description = "Enable pod security policies"
  type        = bool
  default     = true
}

variable "tls_enabled" {
  description = "Enable TLS for ingress"
  type        = bool
  default     = true
}

variable "tls_cert_issuer" {
  description = "Certificate issuer for TLS"
  type        = string
  default     = "letsencrypt-prod"
}

# Resource Limits (80/20 Security Patches)
variable "max_threads" {
  description = "Maximum threads per container"
  type        = number
  default     = 10
}

variable "max_processes" {
  description = "Maximum processes per container"
  type        = number
  default     = 5
}

variable "max_memory_mb" {
  description = "Maximum memory per container in MB"
  type        = number
  default     = 2048
}

variable "max_cpu_cores" {
  description = "Maximum CPU cores per container"
  type        = number
  default     = 2
}

# Monitoring Configuration
variable "enable_monitoring" {
  description = "Enable Prometheus monitoring"
  type        = bool
  default     = true
}

variable "enable_logging" {
  description = "Enable centralized logging"
  type        = bool
  default     = true
}

variable "metrics_retention_days" {
  description = "Days to retain metrics"
  type        = number
  default     = 30
}

# High Availability Configuration
variable "enable_ha" {
  description = "Enable high availability configuration"
  type        = bool
  default     = true
}

variable "min_replicas" {
  description = "Minimum number of replicas"
  type        = number
  default     = 3
}

variable "max_replicas" {
  description = "Maximum number of replicas for autoscaling"
  type        = number
  default     = 10
}

# Storage Configuration
variable "storage_class" {
  description = "Storage class for persistent volumes"
  type        = string
  default     = "standard"
}

variable "data_volume_size" {
  description = "Size of data volume"
  type        = string
  default     = "10Gi"
}

# Backup Configuration
variable "enable_backups" {
  description = "Enable automated backups"
  type        = bool
  default     = true
}

variable "backup_schedule" {
  description = "Backup schedule (cron format)"
  type        = string
  default     = "0 2 * * *"
}

variable "backup_retention_days" {
  description = "Days to retain backups"
  type        = number
  default     = 7
}

# Security Enhancement Variables
variable "enable_service_mesh" {
  description = "Enable Istio service mesh for enhanced security"
  type        = bool
  default     = false
}

variable "enable_external_secrets" {
  description = "Enable external secrets operator integration"
  type        = bool
  default     = false
}

variable "enable_sealed_secrets" {
  description = "Enable sealed secrets for secret encryption"
  type        = bool
  default     = true
}

variable "enable_falco_monitoring" {
  description = "Enable Falco runtime security monitoring"
  type        = bool
  default     = true
}

variable "secret_rotation_schedule" {
  description = "Cron schedule for automatic secret rotation"
  type        = string
  default     = "0 2 1 * *"  # Monthly at 2 AM
}

# Tags
variable "tags" {
  description = "Tags to apply to all resources"
  type        = map(string)
  default = {
    Project     = "CNS"
    ManagedBy   = "Terraform"
    Environment = "Production"
    Security    = "Hardened"
    Compliance  = "CIS"
  }
}