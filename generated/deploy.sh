#!/bin/bash
set -e

echo "🚀 Deploying CNS Forge Portfolio..."

# Initialize Terraform
cd terraform
terraform init
terraform plan -out=tfplan

# Apply Terraform (commented out for safety)
# terraform apply tfplan

# Configure kubectl
# aws eks update-kubeconfig --name cns-forge-cluster --region us-west-2

# Deploy to Kubernetes
cd ../k8s
kubectl apply -f namespace.yaml
kubectl apply -f otel-collector.yaml

for service in cns_litigator cns_quant cns_clinician cns_fabricator; do
    kubectl apply -f ${service}.yaml
done

echo "✅ Deployment complete!"

# Check deployment status
kubectl get pods -n cns-forge
kubectl get services -n cns-forge
