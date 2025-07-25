variable "namespace" {
  description = "Kubernetes namespace"
  type        = string
  default     = "cns-system"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "prod"
}

variable "enable_service_mesh" {
  description = "Enable service mesh"
  type        = bool
  default     = true
}

variable "replicas" {
  description = "Number of replicas"
  type        = number
  default     = 3
}