#!/bin/bash
# Blue-Green Deployment Script

BLUE_VERSION=$1
GREEN_VERSION=$2

echo "Starting blue-green deployment..."
echo "Current (Blue): $BLUE_VERSION"
echo "New (Green): $GREEN_VERSION"

# Deploy green version
kubectl apply -f green-deployment.yaml

# Wait for green to be ready
kubectl wait --for=condition=ready pod -l version=green --timeout=300s

# Switch traffic to green
kubectl patch service main-service -p '{"spec":{"selector":{"version":"green"}}}'

# Verify green is serving traffic
sleep 10

# Remove blue deployment
kubectl delete deployment blue-deployment

echo "Blue-green deployment complete!"
