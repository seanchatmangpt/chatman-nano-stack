terraform {
  required_version = ">= 1.5.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.0"
    }
  }
  
  backend "s3" {
    bucket         = "bitactor-uhft-terraform-state"
    key            = "global/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "bitactor-terraform-locks"
  }
}

# Global variables
locals {
  project_name = "bitactor-uhft"
  environment  = var.environment
  
  # UHFT requires ultra-low latency regions near major exchanges
  regions = {
    # NYSE/NASDAQ - US East
    aws_us_east_1 = {
      provider = "aws"
      region   = "us-east-1"
      exchanges = ["NYSE", "NASDAQ", "ARCA"]
      availability_zones = ["us-east-1a", "us-east-1c", "us-east-1d"]
    }
    
    # CME - US Central  
    aws_us_east_2 = {
      provider = "aws"
      region   = "us-east-2"
      exchanges = ["CME", "CBOE"]
      availability_zones = ["us-east-2a", "us-east-2b", "us-east-2c"]
    }
    
    # LSE/Eurex - EU
    aws_eu_west_1 = {
      provider = "aws"
      region   = "eu-west-1"
      exchanges = ["LSE", "EUREX"]
      availability_zones = ["eu-west-1a", "eu-west-1b", "eu-west-1c"]
    }
    
    # Tokyo Stock Exchange
    aws_ap_northeast_1 = {
      provider = "aws"
      region   = "ap-northeast-1"
      exchanges = ["TSE", "OSE"]
      availability_zones = ["ap-northeast-1a", "ap-northeast-1c", "ap-northeast-1d"]
    }
    
    # Hong Kong Exchange
    gcp_asia_east2 = {
      provider = "gcp"
      region   = "asia-east2"
      exchanges = ["HKEX"]
      zones = ["asia-east2-a", "asia-east2-b", "asia-east2-c"]
    }
    
    # Singapore Exchange
    azure_southeast_asia = {
      provider = "azure"
      region   = "southeastasia"
      exchanges = ["SGX"]
    }
  }
  
  # Network optimization for UHFT
  network_config = {
    enable_sr_iov       = true
    enable_ena          = true  # AWS Enhanced Networking
    enable_accelerated  = true  # Azure Accelerated Networking
    jumbo_frames        = true  # 9000 MTU
  }
  
  # Compute optimization
  compute_config = {
    aws_instance_types = {
      market_data  = "c7gn.16xlarge"  # Network optimized
      order_engine = "m7i.metal-24xl" # Bare metal for lowest latency
      risk_engine  = "r7iz.metal-16xl" # Memory optimized bare metal
    }
    
    gcp_machine_types = {
      market_data  = "c3-standard-88"
      order_engine = "c3-highmem-88"
      risk_engine  = "m3-megamem-64"
    }
    
    azure_vm_sizes = {
      market_data  = "Standard_F72s_v2"
      order_engine = "Standard_M128ms"
      risk_engine  = "Standard_E104i_v5"
    }
  }
}

# Multi-region module deployment
module "aws_regions" {
  for_each = { for k, v in local.regions : k => v if v.provider == "aws" }
  source   = "./modules/aws-region"
  
  region              = each.value.region
  availability_zones  = each.value.availability_zones
  exchanges          = each.value.exchanges
  project_name       = local.project_name
  environment        = local.environment
  network_config     = local.network_config
  compute_config     = local.compute_config.aws_instance_types
  
  # Cross-region mesh
  peer_regions = [for k, v in local.regions : v.region if k != each.key && v.provider == "aws"]
}

module "gcp_regions" {
  for_each = { for k, v in local.regions : k => v if v.provider == "gcp" }
  source   = "./modules/gcp-region"
  
  region         = each.value.region
  zones          = each.value.zones
  exchanges      = each.value.exchanges
  project_name   = local.project_name
  environment    = local.environment
  network_config = local.network_config
  compute_config = local.compute_config.gcp_machine_types
}

module "azure_regions" {
  for_each = { for k, v in local.regions : k => v if v.provider == "azure" }
  source   = "./modules/azure-region"
  
  location       = each.value.region
  exchanges      = each.value.exchanges
  project_name   = local.project_name
  environment    = local.environment
  network_config = local.network_config
  compute_config = local.compute_config.azure_vm_sizes
}

# Global load balancing and failover
module "global_dns" {
  source = "./modules/global-dns"
  
  project_name = local.project_name
  environment  = local.environment
  
  endpoints = merge(
    { for k, v in module.aws_regions : k => v.endpoints },
    { for k, v in module.gcp_regions : k => v.endpoints },
    { for k, v in module.azure_regions : k => v.endpoints }
  )
}

# Cross-cloud VPN mesh for private connectivity
module "cloud_interconnect" {
  source = "./modules/cloud-interconnect"
  
  aws_vpcs = { for k, v in module.aws_regions : k => v.vpc_id }
  gcp_vpcs = { for k, v in module.gcp_regions : k => v.vpc_id }
  azure_vnets = { for k, v in module.azure_regions : k => v.vnet_id }
}

# Kubernetes clusters in each region
module "eks_clusters" {
  for_each = module.aws_regions
  source   = "./modules/eks-cluster"
  
  cluster_name = "${local.project_name}-${each.key}"
  region       = each.value.region
  vpc_id       = each.value.vpc_id
  subnet_ids   = each.value.private_subnet_ids
  
  node_groups = {
    market_data = {
      instance_types = [local.compute_config.aws_instance_types.market_data]
      min_size       = 3
      max_size       = 10
      desired_size   = 5
      
      taints = [{
        key    = "workload"
        value  = "market-data"
        effect = "NO_SCHEDULE"
      }]
    }
    
    order_engine = {
      instance_types = [local.compute_config.aws_instance_types.order_engine]
      min_size       = 2
      max_size       = 6
      desired_size   = 3
      
      taints = [{
        key    = "workload"
        value  = "order-engine"
        effect = "NO_SCHEDULE"
      }]
    }
    
    risk_engine = {
      instance_types = [local.compute_config.aws_instance_types.risk_engine]
      min_size       = 2
      max_size       = 4
      desired_size   = 2
      
      taints = [{
        key    = "workload"
        value  = "risk-engine"
        effect = "NO_SCHEDULE"
      }]
    }
  }
}

# Observability stack
module "observability" {
  source = "./modules/observability"
  
  project_name = local.project_name
  environment  = local.environment
  
  prometheus_regions = keys(local.regions)
  grafana_region     = "aws_us_east_1"
  
  # OpenTelemetry collectors in each region
  otel_endpoints = merge(
    { for k, v in module.aws_regions : k => v.otel_endpoint },
    { for k, v in module.gcp_regions : k => v.otel_endpoint },
    { for k, v in module.azure_regions : k => v.otel_endpoint }
  )
}

# Security and compliance
module "security" {
  source = "./modules/security"
  
  project_name = local.project_name
  environment  = local.environment
  
  # Enable security features
  enable_waf          = true
  enable_ddos_protection = true
  enable_network_firewall = true
  enable_secrets_rotation = true
  
  # Compliance requirements
  compliance_standards = ["SOC2", "ISO27001", "PCI-DSS"]
}

# Disaster recovery
module "disaster_recovery" {
  source = "./modules/disaster-recovery"
  
  project_name = local.project_name
  environment  = local.environment
  
  # Backup configuration
  backup_retention_days = 30
  enable_cross_region_backup = true
  
  # Define DR regions for each primary region
  dr_pairs = {
    "aws_us_east_1" = "aws_us_west_2"
    "aws_eu_west_1" = "aws_eu_central_1"
    "aws_ap_northeast_1" = "aws_ap_southeast_1"
  }
}

# Outputs
output "global_endpoints" {
  description = "Global BitActor UHFT endpoints"
  value       = module.global_dns.endpoints
}

output "kubernetes_clusters" {
  description = "Kubernetes cluster endpoints"
  value = {
    for k, v in module.eks_clusters : k => {
      endpoint = v.cluster_endpoint
      region   = v.region
    }
  }
}

output "monitoring_dashboards" {
  description = "Monitoring dashboard URLs"
  value = {
    grafana    = module.observability.grafana_url
    prometheus = module.observability.prometheus_urls
  }
}