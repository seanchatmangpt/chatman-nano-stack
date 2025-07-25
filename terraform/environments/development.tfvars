# Development Environment Configuration

environment = "dev"
namespace   = "cns-development"

# Cluster Configuration - Minimal for dev
cluster_name      = "cns-dev-cluster"
region           = "us-west-2"
node_pool_size   = 1
node_machine_type = "t3.medium"

# Minimal replicas for dev
replicas     = 1
min_replicas = 1
max_replicas = 3
enable_ha    = false

# Security - Relaxed for development
enable_network_policies      = false
enable_pod_security_policies = false
tls_enabled                 = false
tls_cert_issuer            = "self-signed"

# Resource Limits (80/20 Security Patches) - Still enforced
max_threads    = 10
max_processes  = 5
max_memory_mb  = 2048
max_cpu_cores  = 2

# Monitoring - Basic
enable_monitoring      = true
enable_logging        = false
metrics_retention_days = 7

# Storage - Minimal
storage_class    = "standard"
data_volume_size = "5Gi"

# Backups - Disabled for dev
enable_backups        = false
backup_schedule       = ""
backup_retention_days = 0

# Tags
tags = {
  Project     = "CNS"
  Environment = "Development"
  ManagedBy   = "Terraform"
  CostCenter  = "Engineering"
}