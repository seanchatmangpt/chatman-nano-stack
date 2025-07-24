terraform {
  required_version = ">= 1.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
  
  backend "s3" {
    bucket = "cns-uhft-terraform-state"
    key    = "prod/terraform.tfstate"
    region = "us-east-1"
  }
}

provider "aws" {
  region = var.aws_region
}

# High-performance compute instances for UHFT
resource "aws_instance" "cns_primary" {
  ami           = "ami-0c55b159cbfafe1f0"  # Amazon Linux 2023 with Nitro
  instance_type = "c7i.metal-24xl"         # 96 vCPUs, 192 GiB RAM, 37.5 Gbps network
  
  placement_group = aws_placement_group.uhft_cluster.id
  tenancy        = "dedicated"             # Dedicated hardware for consistent latency
  
  root_block_device {
    volume_type = "io2"
    volume_size = 1000
    iops        = 64000                    # Max IOPS for ultra-low latency
  }
  
  user_data = templatefile("${path.module}/scripts/setup_cns.sh", {
    news_api_key     = var.bloomberg_api_key
    reuters_endpoint = var.reuters_endpoint
  })
  
  network_interface {
    device_index         = 0
    network_interface_id = aws_network_interface.cns_primary_eni.id
  }
  
  tags = {
    Name        = "cns-uhft-primary"
    Environment = "production"
    Purpose     = "news-validation"
  }
}

# Placement group for ultra-low latency
resource "aws_placement_group" "uhft_cluster" {
  name     = "cns-uhft-cluster"
  strategy = "cluster"
}

# Enhanced networking interface with SR-IOV
resource "aws_network_interface" "cns_primary_eni" {
  subnet_id       = aws_subnet.uhft_subnet.id
  security_groups = [aws_security_group.uhft_sg.id]
  
  attachment {
    instance     = aws_instance.cns_primary.id
    device_index = 0
  }
}

# VPC with dedicated network performance
resource "aws_vpc" "uhft_vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true
  
  tags = {
    Name = "cns-uhft-vpc"
  }
}

# Subnet in same AZ for low latency
resource "aws_subnet" "uhft_subnet" {
  vpc_id            = aws_vpc.uhft_vpc.id
  cidr_block        = "10.0.1.0/24"
  availability_zone = "us-east-1a"  # Specific AZ for consistency
  
  tags = {
    Name = "cns-uhft-subnet"
  }
}

# Security group for news feeds and monitoring
resource "aws_security_group" "uhft_sg" {
  name_prefix = "cns-uhft-"
  vpc_id      = aws_vpc.uhft_vpc.id
  
  # Bloomberg B-PIPE
  ingress {
    from_port   = 8194
    to_port     = 8194
    protocol    = "tcp"
    cidr_blocks = ["159.220.0.0/16"]  # Bloomberg network
  }
  
  # Reuters Elektron
  ingress {
    from_port   = 14002
    to_port     = 14002
    protocol    = "tcp"
    cidr_blocks = ["195.35.0.0/16"]  # Reuters network
  }
  
  # Monitoring
  ingress {
    from_port   = 9090
    to_port     = 9090
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# EFS for shared state and logs
resource "aws_efs_file_system" "cns_storage" {
  performance_mode = "maxIO"
  throughput_mode  = "provisioned"
  provisioned_throughput_in_mibps = 1024
  
  tags = {
    Name = "cns-uhft-storage"
  }
}

# CloudWatch for real-time metrics
resource "aws_cloudwatch_dashboard" "uhft_dashboard" {
  dashboard_name = "cns-uhft-metrics"
  
  dashboard_body = jsonencode({
    widgets = [
      {
        type   = "metric"
        width  = 12
        height = 6
        properties = {
          metrics = [
            ["CWAgent", "cns_validation_latency_ns", { stat = "Average" }],
            [".", ".", { stat = "p99" }]
          ]
          period = 1
          stat   = "Average"
          region = var.aws_region
          title  = "CNS Validation Latency (nanoseconds)"
        }
      },
      {
        type   = "metric"
        width  = 12
        height = 6
        properties = {
          metrics = [
            ["CWAgent", "news_throughput_per_sec", { stat = "Sum" }]
          ]
          period = 1
          stat   = "Sum"
          region = var.aws_region
          title  = "News Throughput (events/sec)"
        }
      }
    ]
  })
}

# Auto Scaling for burst capacity
resource "aws_autoscaling_group" "cns_workers" {
  name                = "cns-uhft-workers"
  vpc_zone_identifier = [aws_subnet.uhft_subnet.id]
  
  min_size         = 2
  max_size         = 10
  desired_capacity = 4
  
  launch_template {
    id      = aws_launch_template.cns_worker.id
    version = "$Latest"
  }
  
  tag {
    key                 = "Name"
    value               = "cns-worker"
    propagate_at_launch = true
  }
}

resource "aws_launch_template" "cns_worker" {
  name_prefix   = "cns-worker-"
  image_id      = "ami-0c55b159cbfafe1f0"
  instance_type = "c7i.8xlarge"
  
  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_type = "gp3"
      volume_size = 200
      iops        = 16000
    }
  }
  
  network_interfaces {
    associate_public_ip_address = false
    security_groups            = [aws_security_group.uhft_sg.id]
    delete_on_termination      = true
  }
  
  user_data = base64encode(templatefile("${path.module}/scripts/setup_worker.sh", {
    primary_ip = aws_instance.cns_primary.private_ip
  }))
}

# DynamoDB for ultra-fast state storage
resource "aws_dynamodb_table" "cns_state" {
  name           = "cns-validation-state"
  billing_mode   = "PAY_PER_REQUEST"
  hash_key       = "news_id"
  
  attribute {
    name = "news_id"
    type = "S"
  }
  
  global_secondary_index {
    name            = "ticker-index"
    hash_key        = "ticker"
    projection_type = "ALL"
  }
  
  attribute {
    name = "ticker"
    type = "S"
  }
  
  stream_enabled   = true
  stream_view_type = "NEW_AND_OLD_IMAGES"
  
  tags = {
    Name = "cns-validation-state"
  }
}

# Kinesis for real-time data streaming
resource "aws_kinesis_stream" "news_stream" {
  name             = "cns-news-stream"
  shard_count      = 10
  retention_period = 24
  
  stream_mode_details {
    stream_mode = "ON_DEMAND"
  }
  
  tags = {
    Name = "cns-news-stream"
  }
}

# Lambda for trade execution (connected to exchange APIs)
resource "aws_lambda_function" "trade_executor" {
  filename         = "${path.module}/lambda/trade_executor.zip"
  function_name    = "cns-trade-executor"
  role            = aws_iam_role.lambda_role.arn
  handler         = "index.handler"
  runtime         = "provided.al2"  # Custom runtime for C
  memory_size     = 3008
  timeout         = 1
  reserved_concurrent_executions = 100
  
  environment {
    variables = {
      EXCHANGE_API_KEY = var.exchange_api_key
      DDB_TABLE       = aws_dynamodb_table.cns_state.name
    }
  }
  
  vpc_config {
    subnet_ids         = [aws_subnet.uhft_subnet.id]
    security_group_ids = [aws_security_group.uhft_sg.id]
  }
}

# IAM roles
resource "aws_iam_role" "lambda_role" {
  name = "cns-lambda-executor"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "lambda.amazonaws.com"
      }
    }]
  })
}

# Outputs
output "cns_primary_ip" {
  value = aws_instance.cns_primary.public_ip
}

output "dashboard_url" {
  value = "https://console.aws.amazon.com/cloudwatch/home?region=${var.aws_region}#dashboards:name=${aws_cloudwatch_dashboard.uhft_dashboard.dashboard_name}"
}

output "news_stream_arn" {
  value = aws_kinesis_stream.news_stream.arn
}