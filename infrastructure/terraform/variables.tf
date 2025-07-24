variable "aws_region" {
  description = "AWS region for UHFT deployment"
  type        = string
  default     = "us-east-1"
}

variable "bloomberg_api_key" {
  description = "Bloomberg B-PIPE API key"
  type        = string
  sensitive   = true
}

variable "reuters_endpoint" {
  description = "Reuters Elektron Real-Time endpoint"
  type        = string
  default     = "elektron.reuters.com"
}

variable "exchange_api_key" {
  description = "Exchange API key for trade execution"
  type        = string
  sensitive   = true
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"
}

variable "monitoring_enabled" {
  description = "Enable CloudWatch detailed monitoring"
  type        = bool
  default     = true
}

variable "news_sources" {
  description = "List of news sources to connect"
  type        = list(string)
  default     = ["bloomberg", "reuters", "dow-jones"]
}