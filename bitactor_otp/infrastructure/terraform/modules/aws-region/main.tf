# AWS Region Module for BitActor UHFT
# Deploys ultra-low latency infrastructure in a single AWS region

variable "region" {}
variable "availability_zones" { type = list(string) }
variable "exchanges" { type = list(string) }
variable "project_name" {}
variable "environment" {}
variable "network_config" { type = map(any) }
variable "compute_config" { type = map(string) }
variable "peer_regions" { type = list(string) }

locals {
  vpc_cidr = cidrsubnet("10.0.0.0/8", 8, index(data.aws_regions.all.names, var.region))
}

data "aws_regions" "all" {}

# VPC with enhanced networking
resource "aws_vpc" "main" {
  cidr_block           = local.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true
  
  tags = {
    Name        = "${var.project_name}-${var.region}"
    Environment = var.environment
    Region      = var.region
  }
}

# Enable jumbo frames
resource "aws_ec2_network_insights_path" "jumbo" {
  count = var.network_config.jumbo_frames ? 1 : 0
  
  source      = aws_vpc.main.id
  destination = aws_vpc.main.id
  protocol    = "tcp"
}

# Private subnets for compute
resource "aws_subnet" "private" {
  count             = length(var.availability_zones)
  vpc_id            = aws_vpc.main.id
  cidr_block        = cidrsubnet(local.vpc_cidr, 4, count.index)
  availability_zone = var.availability_zones[count.index]
  
  tags = {
    Name        = "${var.project_name}-private-${var.availability_zones[count.index]}"
    Environment = var.environment
    Type        = "private"
    "kubernetes.io/role/internal-elb" = "1"
  }
}

# Dedicated subnet for market data (colocation connectivity)
resource "aws_subnet" "market_data" {
  count             = length(var.availability_zones)
  vpc_id            = aws_vpc.main.id
  cidr_block        = cidrsubnet(local.vpc_cidr, 4, count.index + 8)
  availability_zone = var.availability_zones[count.index]
  
  tags = {
    Name        = "${var.project_name}-market-data-${var.availability_zones[count.index]}"
    Environment = var.environment
    Type        = "market-data"
    Exchanges   = join(",", var.exchanges)
  }
}

# Direct Connect for exchange connectivity
resource "aws_dx_connection" "exchanges" {
  for_each = toset(var.exchanges)
  
  name      = "${var.project_name}-${each.value}-direct"
  bandwidth = "10Gbps"
  location  = data.aws_dx_locations.available.locations[0] # Nearest to exchange
  
  tags = {
    Name        = "${var.project_name}-${each.value}"
    Environment = var.environment
    Exchange    = each.value
  }
}

data "aws_dx_locations" "available" {}

# Virtual interfaces for Direct Connect
resource "aws_dx_private_virtual_interface" "exchanges" {
  for_each = aws_dx_connection.exchanges
  
  connection_id    = each.value.id
  name            = "${each.key}-vif"
  vlan            = 100 + index(keys(aws_dx_connection.exchanges), each.key)
  customer_address = cidrhost(cidrsubnet("169.254.0.0/16", 14, index(keys(aws_dx_connection.exchanges), each.key)), 1)
  amazon_address   = cidrhost(cidrsubnet("169.254.0.0/16", 14, index(keys(aws_dx_connection.exchanges), each.key)), 2)
  bgp_asn         = 65000
  
  tags = {
    Name = "${var.project_name}-${each.key}-vif"
  }
}

# Local Zones for ultra-low latency (if available)
data "aws_ec2_local_zones" "available" {
  state = "opted-in"
  
  filter {
    name   = "region-name"
    values = [var.region]
  }
}

resource "aws_subnet" "local_zone" {
  for_each = toset(data.aws_ec2_local_zones.available.names)
  
  vpc_id               = aws_vpc.main.id
  cidr_block          = cidrsubnet(local.vpc_cidr, 8, 200 + index(data.aws_ec2_local_zones.available.names, each.value))
  availability_zone_id = each.value
  
  tags = {
    Name = "${var.project_name}-local-zone-${each.value}"
    Type = "local-zone"
  }
}

# Placement groups for cluster networking
resource "aws_placement_group" "cluster" {
  for_each = toset(["market-data", "order-engine", "risk-engine"])
  
  name     = "${var.project_name}-${var.region}-${each.value}"
  strategy = "cluster"
  
  tags = {
    Name     = "${var.project_name}-${each.value}"
    Workload = each.value
  }
}

# Launch templates with SR-IOV and ENA
resource "aws_launch_template" "uhft" {
  for_each = var.compute_config
  
  name_prefix = "${var.project_name}-${each.key}-"
  
  block_device_mappings {
    device_name = "/dev/xvda"
    
    ebs {
      volume_size           = 100
      volume_type          = "io2"
      iops                 = 64000  # Max IOPS
      delete_on_termination = true
      encrypted            = true
    }
  }
  
  # CPU options for deterministic performance
  cpu_options {
    core_count       = data.aws_ec2_instance_type.selected[each.key].default_cores
    threads_per_core = 1  # Disable hyperthreading for consistency
  }
  
  # Enhanced networking
  network_interfaces {
    associate_public_ip_address = false
    delete_on_termination       = true
    device_index               = 0
    interface_type             = var.network_config.enable_ena ? "efa" : "standard"
  }
  
  # Nitro enclaves for secure processing
  enclave_options {
    enabled = true
  }
  
  placement {
    group_name = aws_placement_group.cluster[each.key].name
    tenancy    = "dedicated"  # Dedicated hosts for isolation
  }
  
  metadata_options {
    http_endpoint = "enabled"
    http_tokens   = "required"
  }
  
  user_data = base64encode(templatefile("${path.module}/user-data.sh", {
    workload     = each.key
    project_name = var.project_name
    region       = var.region
  }))
  
  tag_specifications {
    resource_type = "instance"
    tags = {
      Name     = "${var.project_name}-${each.key}"
      Workload = each.key
    }
  }
}

data "aws_ec2_instance_type" "selected" {
  for_each = var.compute_config
  
  instance_type = each.value
}

# Auto Scaling Groups
resource "aws_autoscaling_group" "uhft" {
  for_each = var.compute_config
  
  name                = "${var.project_name}-${each.key}-asg"
  vpc_zone_identifier = aws_subnet.private[*].id
  min_size            = 2
  max_size            = 10
  desired_capacity    = 3
  
  launch_template {
    id      = aws_launch_template.uhft[each.key].id
    version = "$Latest"
  }
  
  tag {
    key                 = "Name"
    value               = "${var.project_name}-${each.key}"
    propagate_at_launch = true
  }
  
  tag {
    key                 = "Workload"
    value               = each.key
    propagate_at_launch = true
  }
}

# Application Load Balancer with ultra-low latency
resource "aws_lb" "internal" {
  name               = "${var.project_name}-${var.region}-internal"
  internal           = true
  load_balancer_type = "network"  # NLB for lowest latency
  subnets            = aws_subnet.private[*].id
  
  enable_cross_zone_load_balancing = false  # Keep traffic local
  
  tags = {
    Name = "${var.project_name}-internal-nlb"
  }
}

# VPC Endpoints for AWS services
resource "aws_vpc_endpoint" "s3" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.${var.region}.s3"
  
  tags = {
    Name = "${var.project_name}-s3-endpoint"
  }
}

resource "aws_vpc_endpoint" "ecr" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${var.region}.ecr.dkr"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.endpoints.id]
  
  tags = {
    Name = "${var.project_name}-ecr-endpoint"
  }
}

# Security group for endpoints
resource "aws_security_group" "endpoints" {
  name_prefix = "${var.project_name}-endpoints-"
  vpc_id      = aws_vpc.main.id
  
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [aws_vpc.main.cidr_block]
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = {
    Name = "${var.project_name}-endpoints-sg"
  }
}

# CloudWatch for nanosecond precision metrics
resource "aws_cloudwatch_metric_stream" "uhft" {
  name          = "${var.project_name}-metrics"
  role_arn      = aws_iam_role.metric_stream.arn
  firehose_arn  = aws_kinesis_firehose_delivery_stream.metrics.arn
  output_format = "opentelemetry0.7"
  
  # Include only BitActor metrics
  include_filter {
    namespace = "BitActor/UHFT"
  }
}

resource "aws_kinesis_firehose_delivery_stream" "metrics" {
  name        = "${var.project_name}-metrics-stream"
  destination = "http_endpoint"
  
  http_endpoint_configuration {
    url                = var.otel_collector_endpoint
    name              = "OpenTelemetry"
    access_key        = var.otel_access_key
    buffering_size    = 1
    buffering_interval = 1  # 1 second for near real-time
  }
}

resource "aws_iam_role" "metric_stream" {
  name = "${var.project_name}-metric-stream-role"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "streams.metrics.cloudwatch.amazonaws.com"
      }
    }]
  })
}

# Outputs
output "vpc_id" {
  value = aws_vpc.main.id
}

output "private_subnet_ids" {
  value = aws_subnet.private[*].id
}

output "endpoints" {
  value = {
    nlb = aws_lb.internal.dns_name
    dx  = { for k, v in aws_dx_connection.exchanges : k => v.id }
  }
}

output "otel_endpoint" {
  value = aws_kinesis_firehose_delivery_stream.metrics.arn
}

output "region" {
  value = var.region
}