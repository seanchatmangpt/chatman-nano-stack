#!/bin/bash
set -e

echo "=== CNS UHFT Production Deployment on AWS ==="
echo "This will deploy real infrastructure and incur AWS costs"
echo

# Check prerequisites
check_prerequisites() {
    echo "Checking prerequisites..."
    
    command -v terraform >/dev/null 2>&1 || { echo "terraform is required but not installed."; exit 1; }
    command -v aws >/dev/null 2>&1 || { echo "aws cli is required but not installed."; exit 1; }
    command -v python3 >/dev/null 2>&1 || { echo "python3 is required but not installed."; exit 1; }
    
    # Check AWS credentials
    aws sts get-caller-identity >/dev/null 2>&1 || { echo "AWS credentials not configured."; exit 1; }
    
    # Check environment variables
    if [ -z "$BLOOMBERG_API_KEY" ]; then
        echo "ERROR: BLOOMBERG_API_KEY environment variable not set"
        echo "Please obtain from Bloomberg Terminal Services"
        exit 1
    fi
    
    if [ -z "$REUTERS_USERNAME" ] || [ -z "$REUTERS_PASSWORD" ]; then
        echo "ERROR: Reuters credentials not set"
        echo "Please set REUTERS_USERNAME and REUTERS_PASSWORD"
        exit 1
    fi
    
    if [ -z "$EXCHANGE_API_KEY" ]; then
        echo "ERROR: EXCHANGE_API_KEY not set for trade execution"
        exit 1
    fi
    
    echo "✓ All prerequisites met"
}

# Deploy infrastructure
deploy_infrastructure() {
    echo
    echo "=== Deploying AWS Infrastructure with Terraform ==="
    
    cd infrastructure/terraform
    
    # Initialize Terraform
    terraform init
    
    # Create tfvars file
    cat > production.tfvars << EOF
bloomberg_api_key = "${BLOOMBERG_API_KEY}"
exchange_api_key  = "${EXCHANGE_API_KEY}"
environment       = "production"
EOF
    
    # Plan deployment
    echo "Planning infrastructure..."
    terraform plan -var-file=production.tfvars -out=tfplan
    
    # Apply
    echo "Deploying infrastructure (this will take 5-10 minutes)..."
    terraform apply tfplan
    
    # Get outputs
    export CNS_PRIMARY_IP=$(terraform output -raw cns_primary_ip)
    export DASHBOARD_URL=$(terraform output -raw dashboard_url)
    export NEWS_STREAM_ARN=$(terraform output -raw news_stream_arn)
    
    cd ../..
    
    echo "✓ Infrastructure deployed"
    echo "  Primary CNS Node: $CNS_PRIMARY_IP"
    echo "  Dashboard: $DASHBOARD_URL"
}

# Wait for systems to be ready
wait_for_ready() {
    echo
    echo "=== Waiting for CNS systems to initialize ==="
    
    # Wait for CNS to be healthy
    max_attempts=60
    attempt=0
    
    while [ $attempt -lt $max_attempts ]; do
        if curl -s http://${CNS_PRIMARY_IP}:8080/health | grep -q "healthy"; then
            echo "✓ CNS is healthy"
            break
        fi
        
        echo -n "."
        sleep 5
        ((attempt++))
    done
    
    if [ $attempt -eq $max_attempts ]; then
        echo "ERROR: CNS failed to become healthy"
        exit 1
    fi
    
    # Verify news sources connected
    echo "Verifying news source connections..."
    
    # Check Bloomberg
    if curl -s http://${CNS_PRIMARY_IP}:8080/status | jq -r '.sources.bloomberg.connected' | grep -q "true"; then
        echo "✓ Bloomberg connected"
    else
        echo "✗ Bloomberg connection failed"
        exit 1
    fi
    
    # Check Reuters
    if curl -s http://${CNS_PRIMARY_IP}:8080/status | jq -r '.sources.reuters.connected' | grep -q "true"; then
        echo "✓ Reuters connected"
    else
        echo "✗ Reuters connection failed"
        exit 1
    fi
}

# Run production stress test
run_stress_test() {
    echo
    echo "=== Running Production Stress Test ==="
    echo "Duration: 5 minutes"
    echo "Real news feeds: Bloomberg B-PIPE, Reuters Elektron"
    echo
    
    # Install Python dependencies
    pip3 install -r test/requirements.txt
    
    # Set CNS endpoint
    export CNS_ENDPOINT="http://${CNS_PRIMARY_IP}:8080"
    
    # Run stress test
    python3 test/production_stress_test.py
}

# Monitor results
monitor_results() {
    echo
    echo "=== Monitoring Real-Time Results ==="
    echo "CloudWatch Dashboard: $DASHBOARD_URL"
    echo
    echo "Press Ctrl+C to stop monitoring"
    
    # Stream CloudWatch metrics
    while true; do
        # Get latest metrics
        VALIDATION_LATENCY=$(aws cloudwatch get-metric-statistics \
            --namespace "CNS/UHFT" \
            --metric-name "ValidationLatencyNS" \
            --dimensions Name=Instance,Value=${CNS_PRIMARY_IP} \
            --start-time $(date -u -d '1 minute ago' +%Y-%m-%dT%H:%M:%S) \
            --end-time $(date -u +%Y-%m-%dT%H:%M:%S) \
            --period 60 \
            --statistics Average \
            --query 'Datapoints[0].Average' \
            --output text)
            
        THROUGHPUT=$(aws cloudwatch get-metric-statistics \
            --namespace "CNS/UHFT" \
            --metric-name "NewsThroughput" \
            --dimensions Name=Instance,Value=${CNS_PRIMARY_IP} \
            --start-time $(date -u -d '1 minute ago' +%Y-%m-%dT%H:%M:%S) \
            --end-time $(date -u +%Y-%m-%dT%H:%M:%S) \
            --period 60 \
            --statistics Sum \
            --query 'Datapoints[0].Sum' \
            --output text)
        
        echo "[$(date +%H:%M:%S)] Validation: ${VALIDATION_LATENCY}ns | Throughput: ${THROUGHPUT}/sec"
        
        sleep 10
    done
}

# Cleanup function
cleanup() {
    echo
    echo "=== Cleanup ==="
    read -p "Destroy AWS infrastructure? (y/N) " -n 1 -r
    echo
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        cd infrastructure/terraform
        terraform destroy -var-file=production.tfvars -auto-approve
        cd ../..
        echo "✓ Infrastructure destroyed"
    fi
}

# Main execution
main() {
    check_prerequisites
    
    # Deploy
    deploy_infrastructure
    wait_for_ready
    
    # Test
    run_stress_test
    
    # Monitor
    monitor_results
}

# Set trap for cleanup
trap cleanup EXIT

# Run
main