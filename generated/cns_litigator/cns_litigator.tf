terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = "us-west-2"
}

module "cns_litigator" {
  source = "../../../terraform/modules/cns-forge-service"
  
  service_name = "cns_litigator"
  instance_type = "t3.large"
  min_size = 2
  max_size = 10
  
  environment_variables = {
    MAX_TTL_HOPS = "8"
    TICK_BUDGET = "8"
    DOMAIN = "legal"
  }
  
  enable_monitoring = true
  enable_logging = true
}