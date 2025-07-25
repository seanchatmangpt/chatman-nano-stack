# Production Environment Configuration

environment = "prod"
namespace   = "cns-production"

# Cluster Configuration
cluster_name      = "cns-prod-cluster"
region           = "us-west-2"
node_pool_size   = 5
node_machine_type = "t3.xlarge"

# High Availability
replicas     = 3
min_replicas = 3
max_replicas = 10
enable_ha    = true

# Security - Production hardening
enable_network_policies      = true
enable_pod_security_policies = true
tls_enabled                 = true
tls_cert_issuer            = "letsencrypt-prod"

# Resource Limits (80/20 Security Patches)
max_threads    = 10
max_processes  = 5
max_memory_mb  = 2048
max_cpu_cores  = 2

# Monitoring - Enhanced for production
enable_monitoring      = true
enable_logging        = true
metrics_retention_days = 90

# Storage
storage_class    = "fast-ssd"
data_volume_size = "50Gi"

# Backups - Critical for production
enable_backups        = true
backup_schedule       = "0 2 * * *"  # 2 AM daily
backup_retention_days = 30

# Tags
tags = {
  Project     = "CNS"
  Environment = "Production"
  ManagedBy   = "Terraform"
  CostCenter  = "Engineering"
  Compliance  = "SOC2"
}