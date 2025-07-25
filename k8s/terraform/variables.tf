# BitActor Terraform Variables

variable "kubeconfig_path" {
  description = "Path to kubeconfig file"
  type        = string
  default     = "~/.kube/config"
}

variable "namespace" {
  description = "Kubernetes namespace for BitActor"
  type        = string
  default     = "bitactor"
}

variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "production"
  
  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be development, staging, or production."
  }
}

variable "image_repository" {
  description = "Docker image repository for BitActor"
  type        = string
  default     = "bitactor/bitactor-service"
}

variable "image_tag" {
  description = "Docker image tag"
  type        = string
  default     = "latest"
}

variable "replicas" {
  description = "Number of BitActor replicas"
  type        = number
  default     = 3
  
  validation {
    condition     = var.replicas >= 1 && var.replicas <= 10
    error_message = "Replicas must be between 1 and 10."
  }
}

variable "resources" {
  description = "Resource requirements for BitActor pods"
  type = object({
    requests = object({
      cpu    = string
      memory = string
    })
    limits = object({
      cpu    = string
      memory = string
    })
  })
  default = {
    requests = {
      cpu    = "500m"
      memory = "512Mi"
    }
    limits = {
      cpu    = "2000m"
      memory = "2Gi"
    }
  }
}

variable "autoscaling" {
  description = "Autoscaling configuration"
  type = object({
    enabled                     = bool
    min_replicas               = number
    max_replicas               = number
    target_cpu_utilization     = number
    target_memory_utilization  = number
  })
  default = {
    enabled                     = true
    min_replicas               = 3
    max_replicas               = 10
    target_cpu_utilization     = 70
    target_memory_utilization  = 80
  }
}

variable "pod_disruption_budget" {
  description = "Pod disruption budget configuration"
  type = object({
    min_available = string
  })
  default = {
    min_available = "50%"
  }
}

variable "tick_budget" {
  description = "CPU tick budget for signal processing"
  type        = number
  default     = 10000  # 10 microseconds for non-x86
}

variable "ring_size" {
  description = "Size of the signal ring buffer"
  type        = number
  default     = 4096
  
  validation {
    condition     = var.ring_size >= 1024 && var.ring_size <= 65536
    error_message = "Ring size must be between 1024 and 65536."
  }
}

variable "thread_count" {
  description = "Number of processing threads"
  type        = number
  default     = 4
}

variable "enable_tls" {
  description = "Enable TLS for BitActor service"
  type        = bool
  default     = true
}

variable "monitoring" {
  description = "Monitoring configuration"
  type = object({
    prometheus_enabled = bool
    grafana_enabled   = bool
    metrics_port      = number
  })
  default = {
    prometheus_enabled = true
    grafana_enabled   = true
    metrics_port      = 9090
  }
}