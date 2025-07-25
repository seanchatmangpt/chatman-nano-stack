variable "namespace" {
  description = "Kubernetes namespace"
  type        = string
  default     = "cns-system"
}

variable "region" {
  description = "Cloud region"
  type        = string
  default     = "us-west-2"
}

variable "enable_external_secrets" {
  description = "Enable external secrets"
  type        = bool
  default     = true
}