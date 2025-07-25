# CNS Forge Production Infrastructure - 80/20 Implementation
# Universal Business Logic Compiler Infrastructure

terraform {
  required_version = ">= 1.5.0"
  
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.11"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
  
  backend "s3" {
    bucket = "cns-forge-terraform-state"
    key    = "production/terraform.tfstate"
    region = "us-east-1"
  }
}

# Variables
variable "environment" {
  default = "production"
}

variable "region" {
  default = "us-east-1"
}

variable "cluster_name" {
  default = "cns-forge-prod"
}

# VPC for CNS Forge
module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  version = "5.0.0"
  
  name = "cns-forge-vpc"
  cidr = "10.0.0.0/16"
  
  azs             = ["${var.region}a", "${var.region}b", "${var.region}c"]
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
  
  enable_nat_gateway = true
  enable_vpn_gateway = true
  enable_dns_hostnames = true
  
  tags = {
    Environment = var.environment
    Application = "cns-forge"
    Component   = "network"
  }
}

# EKS Cluster for BitActor Mesh
module "eks" {
  source = "terraform-aws-modules/eks/aws"
  version = "19.15.3"
  
  cluster_name    = var.cluster_name
  cluster_version = "1.28"
  
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets
  
  # Node groups for BitActor mesh
  eks_managed_node_groups = {
    bitactor_nodes = {
      desired_capacity = 5
      max_capacity     = 20
      min_capacity     = 3
      
      instance_types = ["c6i.2xlarge"] # CPU-optimized for BitActor
      
      k8s_labels = {
        Environment = var.environment
        Application = "bitactor-mesh"
      }
      
      taints = [{
        key    = "bitactor"
        value  = "true"
        effect = "NO_SCHEDULE"
      }]
    }
    
    reactor_nodes = {
      desired_capacity = 3
      max_capacity     = 10
      min_capacity     = 2
      
      instance_types = ["m6i.xlarge"]
      
      k8s_labels = {
        Environment = var.environment
        Application = "reactor-workflows"
      }
    }
  }
  
  # Enable IRSA
  enable_irsa = true
  
  # Cluster addons
  cluster_addons = {
    coredns = {
      resolve_conflicts = "OVERWRITE"
    }
    kube-proxy = {}
    vpc-cni = {
      resolve_conflicts = "OVERWRITE"
    }
  }
  
  tags = {
    Environment = var.environment
    Application = "cns-forge"
  }
}

# RDS for persistent storage
resource "aws_db_instance" "cns_forge_db" {
  identifier = "cns-forge-prod"
  
  engine         = "postgres"
  engine_version = "15.4"
  instance_class = "db.r6g.large"
  
  allocated_storage     = 100
  max_allocated_storage = 1000
  storage_encrypted     = true
  
  db_name  = "cns_forge"
  username = "cns_admin"
  password = random_password.db_password.result
  
  vpc_security_group_ids = [aws_security_group.rds.id]
  db_subnet_group_name   = aws_db_subnet_group.cns_forge.name
  
  backup_retention_period = 30
  backup_window          = "03:00-04:00"
  maintenance_window     = "sun:04:00-sun:05:00"
  
  enabled_cloudwatch_logs_exports = ["postgresql"]
  
  tags = {
    Environment = var.environment
    Application = "cns-forge"
  }
}

# ElastiCache for BitActor coordination
resource "aws_elasticache_replication_group" "bitactor_cache" {
  replication_group_id       = "cns-forge-bitactor-cache"
  replication_group_description = "Redis cache for BitActor mesh coordination"
  
  engine               = "redis"
  engine_version      = "7.0"
  node_type           = "cache.r6g.large"
  number_cache_clusters = 3
  
  multi_az_enabled = true
  automatic_failover_enabled = true
  
  subnet_group_name = aws_elasticache_subnet_group.cns_forge.name
  security_group_ids = [aws_security_group.redis.id]
  
  at_rest_encryption_enabled = true
  transit_encryption_enabled = true
  
  snapshot_retention_limit = 7
  snapshot_window = "03:00-05:00"
  
  tags = {
    Environment = var.environment
    Application = "cns-forge"
  }
}

# S3 buckets for artifacts and compiled BitActors
resource "aws_s3_bucket" "bitactor_artifacts" {
  bucket = "cns-forge-bitactor-artifacts-${var.environment}"
  
  tags = {
    Environment = var.environment
    Application = "cns-forge"
  }
}

resource "aws_s3_bucket_versioning" "bitactor_artifacts" {
  bucket = aws_s3_bucket.bitactor_artifacts.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

# Kubernetes namespace for CNS Forge
resource "kubernetes_namespace" "cns_forge" {
  depends_on = [module.eks]
  
  metadata {
    name = "cns-forge"
    
    labels = {
      environment = var.environment
      application = "cns-forge"
      "pod-security.kubernetes.io/enforce" = "restricted"
    }
    
    annotations = {
      "cns.io/bitactor-mesh" = "enabled"
      "cns.io/ttl-enforcement" = "strict"
    }
  }
}

# Service mesh (Linkerd) for BitActor communication
resource "helm_release" "linkerd_control_plane" {
  depends_on = [module.eks]
  
  name       = "linkerd"
  repository = "https://helm.linkerd.io/stable"
  chart      = "linkerd-control-plane"
  namespace  = "linkerd"
  create_namespace = true
  
  set {
    name  = "global.proxy.resources.cpu.limit"
    value = "200m"
  }
  
  set {
    name  = "global.proxy.resources.memory.limit"
    value = "256Mi"
  }
}

# Prometheus for metrics
resource "helm_release" "prometheus" {
  depends_on = [module.eks]
  
  name       = "prometheus"
  repository = "https://prometheus-community.github.io/helm-charts"
  chart      = "kube-prometheus-stack"
  namespace  = "monitoring"
  create_namespace = true
  
  values = [
    <<-EOT
    prometheus:
      prometheusSpec:
        retention: 30d
        storageSpec:
          volumeClaimTemplate:
            spec:
              storageClassName: gp3
              accessModes: ["ReadWriteOnce"]
              resources:
                requests:
                  storage: 100Gi
    EOT
  ]
}

# OpenTelemetry Collector
resource "helm_release" "otel_collector" {
  depends_on = [module.eks]
  
  name       = "opentelemetry-collector"
  repository = "https://open-telemetry.github.io/opentelemetry-helm-charts"
  chart      = "opentelemetry-collector"
  namespace  = "otel"
  create_namespace = true
  
  values = [
    <<-EOT
    mode: deployment
    config:
      receivers:
        otlp:
          protocols:
            grpc:
              endpoint: 0.0.0.0:4317
            http:
              endpoint: 0.0.0.0:4318
      exporters:
        prometheus:
          endpoint: "0.0.0.0:8889"
        logging:
          loglevel: info
      service:
        pipelines:
          traces:
            receivers: [otlp]
            exporters: [logging]
          metrics:
            receivers: [otlp]
            exporters: [prometheus, logging]
    EOT
  ]
}

# Security Groups
resource "aws_security_group" "rds" {
  name_prefix = "cns-forge-rds-"
  vpc_id      = module.vpc.vpc_id
  
  ingress {
    from_port   = 5432
    to_port     = 5432
    protocol    = "tcp"
    cidr_blocks = module.vpc.private_subnets_cidr_blocks
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = {
    Name = "cns-forge-rds"
  }
}

resource "aws_security_group" "redis" {
  name_prefix = "cns-forge-redis-"
  vpc_id      = module.vpc.vpc_id
  
  ingress {
    from_port   = 6379
    to_port     = 6379
    protocol    = "tcp"
    cidr_blocks = module.vpc.private_subnets_cidr_blocks
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = {
    Name = "cns-forge-redis"
  }
}

# Subnet groups
resource "aws_db_subnet_group" "cns_forge" {
  name       = "cns-forge-db-subnet"
  subnet_ids = module.vpc.private_subnets
  
  tags = {
    Name = "cns-forge-db-subnet"
  }
}

resource "aws_elasticache_subnet_group" "cns_forge" {
  name       = "cns-forge-cache-subnet"
  subnet_ids = module.vpc.private_subnets
}

# Random password for RDS
resource "random_password" "db_password" {
  length  = 32
  special = true
}

# Store secrets in AWS Secrets Manager
resource "aws_secretsmanager_secret" "db_credentials" {
  name = "cns-forge/db-credentials"
}

resource "aws_secretsmanager_secret_version" "db_credentials" {
  secret_id = aws_secretsmanager_secret.db_credentials.id
  secret_string = jsonencode({
    username = aws_db_instance.cns_forge_db.username
    password = random_password.db_password.result
    host     = aws_db_instance.cns_forge_db.endpoint
    port     = 5432
    database = aws_db_instance.cns_forge_db.db_name
  })
}

# Outputs
output "eks_cluster_endpoint" {
  value = module.eks.cluster_endpoint
}

output "eks_cluster_name" {
  value = module.eks.cluster_name
}

output "db_endpoint" {
  value     = aws_db_instance.cns_forge_db.endpoint
  sensitive = true
}

output "redis_endpoint" {
  value = aws_elasticache_replication_group.bitactor_cache.primary_endpoint_address
}

output "bitactor_bucket" {
  value = aws_s3_bucket.bitactor_artifacts.id
}