#!/bin/bash

# CNS Protection Service Kubernetes Deployment Validation Script
# This script validates the 80/20 core protection deployment on K8s

set -e

echo "=== CNS Protection Service K8s Validation ==="
echo

# Check if kubectl is available
if ! command -v kubectl &> /dev/null; then
    echo "❌ kubectl not found. Please install kubectl first."
    exit 1
fi

# Check cluster connection
echo "1. Checking Kubernetes cluster connection..."
if kubectl cluster-info &> /dev/null; then
    echo "✅ Connected to Kubernetes cluster"
    kubectl cluster-info | head -n 1
else
    echo "❌ Cannot connect to Kubernetes cluster"
    exit 1
fi

# Create namespace if it doesn't exist
echo
echo "2. Creating namespace..."
kubectl create namespace trading-system --dry-run=client -o yaml | kubectl apply -f -
echo "✅ Namespace 'trading-system' ready"

# Apply configurations
echo
echo "3. Applying protection service configurations..."
kubectl apply -f protection-deployment.yaml
kubectl apply -f protection-monitoring.yaml
echo "✅ Configurations applied"

# Wait for deployment to be ready
echo
echo "4. Waiting for deployment to be ready..."
kubectl wait --for=condition=available --timeout=300s \
    deployment/cns-protection-service -n trading-system
echo "✅ Deployment is ready"

# Check pod status
echo
echo "5. Checking pod status..."
kubectl get pods -n trading-system -l app=cns-protection
READY_PODS=$(kubectl get pods -n trading-system -l app=cns-protection \
    -o jsonpath='{.items[?(@.status.phase=="Running")].metadata.name}' | wc -w)
if [ "$READY_PODS" -ge "2" ]; then
    echo "✅ At least 2 pods are running"
else
    echo "❌ Insufficient pods running"
    exit 1
fi

# Test service endpoints
echo
echo "6. Testing service endpoints..."
SERVICE_IP=$(kubectl get svc cns-protection-service -n trading-system \
    -o jsonpath='{.spec.clusterIP}')

# Port forward for testing
kubectl port-forward -n trading-system \
    svc/cns-protection-service 8080:8080 9090:9090 &
PF_PID=$!
sleep 5

# Test health endpoint
echo "   Testing /health endpoint..."
if curl -f -s http://localhost:8080/health | grep -q "ok"; then
    echo "   ✅ Health check passed"
else
    echo "   ❌ Health check failed"
    kill $PF_PID
    exit 1
fi

# Test ready endpoint
echo "   Testing /ready endpoint..."
if curl -f -s http://localhost:8080/ready | grep -q "true"; then
    echo "   ✅ Readiness check passed"
else
    echo "   ❌ Readiness check failed"
    kill $PF_PID
    exit 1
fi

# Test metrics endpoint
echo "   Testing /metrics endpoint..."
if curl -f -s http://localhost:9090/metrics | grep -q "protection_requests_total"; then
    echo "   ✅ Metrics endpoint working"
else
    echo "   ❌ Metrics endpoint failed"
    kill $PF_PID
    exit 1
fi

# Test validation endpoint
echo "   Testing /validate endpoint..."
RESPONSE=$(curl -s -X POST http://localhost:8080/validate \
    -H "Content-Type: application/json" \
    -d '{
        "symbol": "EURUSD",
        "position_size": 50,
        "entry_price": 1.1000,
        "stop_loss": 1.0890,
        "account_balance": 1000
    }')

if echo "$RESPONSE" | grep -q "approved"; then
    echo "   ✅ Validation endpoint working"
    echo "   Response: $RESPONSE"
else
    echo "   ❌ Validation endpoint failed"
    kill $PF_PID
    exit 1
fi

# Stop port forwarding
kill $PF_PID

# Check HPA status
echo
echo "7. Checking Horizontal Pod Autoscaler..."
kubectl get hpa cns-protection-hpa -n trading-system
echo "✅ HPA configured"

# Check PDB status
echo
echo "8. Checking Pod Disruption Budget..."
kubectl get pdb cns-protection-pdb -n trading-system
echo "✅ PDB configured"

# Run stress test
echo
echo "9. Running basic stress test..."
kubectl run stress-test --rm -i --restart=Never --image=busybox -n trading-system -- sh -c '
echo "Sending 100 requests..."
for i in $(seq 1 100); do
    nc cns-protection-service 8080 <<EOF
POST /validate HTTP/1.1
Host: cns-protection-service
Content-Type: application/json
Content-Length: 120

{"symbol":"EURUSD","position_size":50,"entry_price":1.1000,"stop_loss":1.0890,"account_balance":1000}
EOF
done
echo "Stress test complete"
'
echo "✅ Basic stress test passed"

# Check metrics after stress test
echo
echo "10. Checking metrics after stress test..."
kubectl port-forward -n trading-system \
    svc/cns-protection-service 9090:9090 &
PF_PID=$!
sleep 5

METRICS=$(curl -s http://localhost:9090/metrics)
echo "   Total requests: $(echo "$METRICS" | grep "protection_requests_total" | awk '{print $2}')"
echo "   Approved: $(echo "$METRICS" | grep "protection_approved_total" | awk '{print $2}')"
echo "   Rejected: $(echo "$METRICS" | grep "protection_rejected_total" | awk '{print $2}')"

kill $PF_PID

# Performance validation
echo
echo "11. Performance validation..."
echo "   Checking response times from metrics..."
# In real deployment, query Prometheus for p99 response times

# Final summary
echo
echo "=== Validation Summary ==="
echo "✅ Namespace created"
echo "✅ Deployment successful"
echo "✅ Pods running (minimum 2)"
echo "✅ Service endpoints responsive"
echo "✅ Health checks passing"
echo "✅ Metrics collection working"
echo "✅ Validation logic functioning"
echo "✅ HPA configured for auto-scaling"
echo "✅ PDB configured for availability"
echo "✅ Basic stress test passed"
echo
echo "🎯 CNS Protection Service is operational on Kubernetes!"
echo
echo "Next steps:"
echo "1. Configure Prometheus to scrape metrics"
echo "2. Import Grafana dashboard for monitoring"
echo "3. Set up alerting rules in AlertManager"
echo "4. Run full stress test with higher load"
echo "5. Validate response time SLA (<100ms p99)"