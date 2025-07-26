#!/bin/bash
# Ash.Reactor 80/20 Deployment Script

set -e

echo "🚀 Deploying Ash.Reactor 80/20 to Kubernetes"

# Check prerequisites
command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required but not installed."; exit 1; }
command -v kubectl >/dev/null 2>&1 || { echo "❌ kubectl is required but not installed."; exit 1; }
command -v terraform >/dev/null 2>&1 || { echo "❌ Terraform is required but not installed."; exit 1; }

# Build Docker image
echo "📦 Building Docker image..."
docker build -t ash-reactor-80-20:latest .

# Deploy with Terraform
echo "🏗️ Deploying infrastructure with Terraform..."
cd terraform
terraform init
terraform plan -out=tfplan
terraform apply tfplan
cd ..

# Alternative: Deploy with kubectl
# echo "☸️ Deploying to Kubernetes..."
# kubectl apply -f k8s/otel-collector.yaml
# kubectl apply -f k8s/deployment.yaml

# Wait for deployment
echo "⏳ Waiting for pods to be ready..."
kubectl wait --for=condition=ready pod -l app=ash-reactor -n ash-reactor-80-20 --timeout=300s

# Check deployment status
echo "✅ Deployment status:"
kubectl get pods -n ash-reactor-80-20
kubectl get svc -n ash-reactor-80-20

echo "🎉 Deployment complete!"