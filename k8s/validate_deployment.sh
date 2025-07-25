#!/bin/bash
# BitActor Kubernetes Deployment Validation Script

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
NAMESPACE="${NAMESPACE:-bitactor}"
RELEASE_NAME="${RELEASE_NAME:-bitactor}"
EXPECTED_REPLICAS="${EXPECTED_REPLICAS:-3}"
TIMEOUT="${TIMEOUT:-300}"

echo "🚀 BitActor Kubernetes Deployment Validation"
echo "==========================================="
echo "Namespace: $NAMESPACE"
echo "Release: $RELEASE_NAME"
echo "Expected Replicas: $EXPECTED_REPLICAS"
echo ""

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check prerequisites
echo "📋 Checking prerequisites..."
if ! command_exists kubectl; then
    echo -e "${RED}❌ kubectl not found. Please install kubectl.${NC}"
    exit 1
fi

if ! command_exists helm; then
    echo -e "${RED}❌ helm not found. Please install helm.${NC}"
    exit 1
fi

# Check cluster connectivity
echo "🔗 Checking cluster connectivity..."
if ! kubectl cluster-info >/dev/null 2>&1; then
    echo -e "${RED}❌ Cannot connect to Kubernetes cluster${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Connected to cluster${NC}"

# Check namespace
echo "📦 Checking namespace..."
if ! kubectl get namespace "$NAMESPACE" >/dev/null 2>&1; then
    echo -e "${RED}❌ Namespace $NAMESPACE not found${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Namespace exists${NC}"

# Check Helm release
echo "⚙️  Checking Helm release..."
if ! helm list -n "$NAMESPACE" | grep -q "$RELEASE_NAME"; then
    echo -e "${RED}❌ Helm release $RELEASE_NAME not found in namespace $NAMESPACE${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Helm release deployed${NC}"

# Check deployment
echo "🚢 Checking deployment..."
DEPLOYMENT_NAME="${RELEASE_NAME}-bitactor"
if ! kubectl get deployment "$DEPLOYMENT_NAME" -n "$NAMESPACE" >/dev/null 2>&1; then
    echo -e "${RED}❌ Deployment $DEPLOYMENT_NAME not found${NC}"
    exit 1
fi

# Wait for deployment to be ready
echo "⏳ Waiting for deployment to be ready (timeout: ${TIMEOUT}s)..."
if kubectl rollout status deployment/"$DEPLOYMENT_NAME" -n "$NAMESPACE" --timeout="${TIMEOUT}s"; then
    echo -e "${GREEN}✅ Deployment is ready${NC}"
else
    echo -e "${RED}❌ Deployment failed to become ready${NC}"
    exit 1
fi

# Check pod status
echo "🔍 Checking pod status..."
READY_PODS=$(kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor -o jsonpath='{.items[?(@.status.phase=="Running")].metadata.name}' | wc -w | tr -d ' ')
TOTAL_PODS=$(kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor -o jsonpath='{.items[*].metadata.name}' | wc -w | tr -d ' ')

echo "Pods ready: $READY_PODS/$TOTAL_PODS"
if [ "$READY_PODS" -lt "$EXPECTED_REPLICAS" ]; then
    echo -e "${YELLOW}⚠️  Warning: Only $READY_PODS pods ready, expected at least $EXPECTED_REPLICAS${NC}"
else
    echo -e "${GREEN}✅ All expected pods are running${NC}"
fi

# Check service
echo "🌐 Checking service..."
SERVICE_NAME="${RELEASE_NAME}-bitactor"
if ! kubectl get service "$SERVICE_NAME" -n "$NAMESPACE" >/dev/null 2>&1; then
    echo -e "${RED}❌ Service $SERVICE_NAME not found${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Service exists${NC}"

# Check HPA
echo "📊 Checking Horizontal Pod Autoscaler..."
if kubectl get hpa -n "$NAMESPACE" | grep -q "bitactor"; then
    HPA_STATUS=$(kubectl get hpa -n "$NAMESPACE" -o wide | grep bitactor)
    echo "HPA Status: $HPA_STATUS"
    echo -e "${GREEN}✅ HPA configured${NC}"
else
    echo -e "${YELLOW}⚠️  HPA not found${NC}"
fi

# Check Network Policy
echo "🔒 Checking Network Policy..."
if kubectl get networkpolicy -n "$NAMESPACE" | grep -q "bitactor"; then
    echo -e "${GREEN}✅ Network Policy configured${NC}"
else
    echo -e "${YELLOW}⚠️  Network Policy not found${NC}"
fi

# Check Pod Disruption Budget
echo "🛡️  Checking Pod Disruption Budget..."
if kubectl get pdb -n "$NAMESPACE" | grep -q "bitactor"; then
    echo -e "${GREEN}✅ Pod Disruption Budget configured${NC}"
else
    echo -e "${YELLOW}⚠️  Pod Disruption Budget not found${NC}"
fi

# Test service connectivity
echo "🧪 Testing service connectivity..."
POD_NAME=$(kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor -o jsonpath='{.items[0].metadata.name}')
if [ -n "$POD_NAME" ]; then
    echo "Using pod: $POD_NAME"
    
    # Check health endpoint
    if kubectl exec -n "$NAMESPACE" "$POD_NAME" -- wget -qO- http://localhost:9090/health >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Health check passed${NC}"
    else
        echo -e "${YELLOW}⚠️  Health check failed or not implemented${NC}"
    fi
    
    # Check metrics endpoint
    if kubectl exec -n "$NAMESPACE" "$POD_NAME" -- wget -qO- http://localhost:9090/metrics >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Metrics endpoint accessible${NC}"
    else
        echo -e "${YELLOW}⚠️  Metrics endpoint not accessible${NC}"
    fi
else
    echo -e "${YELLOW}⚠️  No running pods found for connectivity test${NC}"
fi

# Performance validation
echo "⚡ Checking performance metrics..."
if [ -n "$POD_NAME" ]; then
    # Get container resource usage
    METRICS=$(kubectl top pod "$POD_NAME" -n "$NAMESPACE" --containers 2>/dev/null || echo "Metrics not available")
    echo "Resource usage: $METRICS"
fi

# Security validation
echo "🔐 Validating security configuration..."
# Check if pods are running as non-root
NON_ROOT=$(kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor -o jsonpath='{.items[0].spec.containers[0].securityContext.runAsNonRoot}')
if [ "$NON_ROOT" = "true" ]; then
    echo -e "${GREEN}✅ Pods running as non-root${NC}"
else
    echo -e "${YELLOW}⚠️  Pods not configured to run as non-root${NC}"
fi

# Summary
echo ""
echo "📈 Deployment Validation Summary"
echo "================================"
echo -e "${GREEN}✅ Kubernetes connectivity${NC}"
echo -e "${GREEN}✅ Namespace exists${NC}"
echo -e "${GREEN}✅ Helm release deployed${NC}"
echo -e "${GREEN}✅ Deployment ready${NC}"
echo -e "${GREEN}✅ Service configured${NC}"

if [ "$READY_PODS" -ge "$EXPECTED_REPLICAS" ]; then
    echo -e "${GREEN}✅ Expected number of pods running${NC}"
else
    echo -e "${YELLOW}⚠️  Fewer pods than expected${NC}"
fi

# Check for any failing pods
FAILING_PODS=$(kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor --field-selector=status.phase!=Running -o name 2>/dev/null | wc -l | tr -d ' ')
if [ "$FAILING_PODS" -gt 0 ]; then
    echo -e "${RED}❌ $FAILING_PODS pods are not running${NC}"
    kubectl get pods -n "$NAMESPACE" -l app.kubernetes.io/name=bitactor --field-selector=status.phase!=Running
    exit 1
fi

echo ""
echo -e "${GREEN}🎉 BitActor deployment validation completed successfully!${NC}"
echo ""
echo "Next steps:"
echo "1. Monitor pod logs: kubectl logs -n $NAMESPACE -l app.kubernetes.io/name=bitactor -f"
echo "2. Check metrics: kubectl exec -n $NAMESPACE $POD_NAME -- wget -qO- http://localhost:9090/metrics"
echo "3. Run performance tests against the service"
echo "4. Configure monitoring dashboards in Grafana"

exit 0