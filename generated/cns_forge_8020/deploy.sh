#!/bin/bash
# CNS Forge 80/20 Deployment Script
# Leverages existing infrastructure for production deployment

set -e

echo "🚀 CNS Forge 80/20 Deployment Starting..."

# Build components
echo "🔨 Building components..."
make clean && make all

# Run tests
echo "🧪 Running tests..."
make test

# Build Erlang components
echo "🐿️ Building Erlang components..."
make erlang

# Create Docker image (using existing patterns)
echo "🐳 Building Docker image..."
cat > Dockerfile.cns-forge-8020 << 'EOF'
FROM alpine:latest

RUN apk add --no-cache libc6-compat

COPY cns_forge_8020_demo /usr/local/bin/
COPY cns-forge-8020-deployment.yaml /etc/cns-forge/

EXPOSE 8080 9090 8081

CMD ["/usr/local/bin/cns_forge_8020_demo"]
EOF

docker build -t cns-forge:8020 -f Dockerfile.cns-forge-8020 .

# Deploy to Kubernetes
echo "☸️ Deploying to Kubernetes..."
kubectl create namespace cns-system --dry-run=client -o yaml | kubectl apply -f -
kubectl apply -f cns-forge-8020-deployment.yaml

# Wait for deployment
echo "⏳ Waiting for deployment to be ready..."
kubectl rollout status deployment/cns-forge-8020 -n cns-system --timeout=300s

# Verify deployment
echo "✅ Verifying deployment..."
kubectl get pods -n cns-system -l app=cns-forge-8020

# Display service information
echo "📋 Service information:"
kubectl get svc cns-forge-8020-service -n cns-system

echo "🎉 CNS Forge 80/20 Deployment Complete!"
echo "📊 Monitor with: kubectl logs -f deployment/cns-forge-8020 -n cns-system"
