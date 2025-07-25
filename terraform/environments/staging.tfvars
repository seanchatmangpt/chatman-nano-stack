# Staging Environment Configuration

environment = "staging"
namespace   = "cns-staging"

# Cluster Configuration - Smaller than prod
cluster_name      = "cns-staging-cluster"
region           = "us-west-2"
node_pool_size   = 3
node_machine_type = "t3.large"

# Moderate availability
replicas     = 2
min_replicas = 2
max_replicas = 5
enable_ha    = true

# Security - Production-like
enable_network_policies      = true
enable_pod_security_policies = true
tls_enabled                 = true
tls_cert_issuer            = "letsencrypt-staging"

# Resource Limits (80/20 Security Patches)
max_threads    = 10
max_processes  = 5
max_memory_mb  = 2048
max_cpu_cores  = 2

# Monitoring
enable_monitoring      = true
enable_logging        = true
metrics_retention_days = 30

# Storage
storage_class    = "standard"
data_volume_size = "20Gi"

# Backups - Less frequent
enable_backups        = true
backup_schedule       = "0 3 * * 0"  # Weekly on Sunday
backup_retention_days = 14

# Tags
tags = {
  Project     = "CNS"
  Environment = "Staging"
  ManagedBy   = "Terraform"
  CostCenter  = "Engineering"
}