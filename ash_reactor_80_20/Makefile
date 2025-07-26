.PHONY: build deploy test clean

# Build Docker image
build:
	docker build -t ash-reactor-80-20:latest .

# Deploy with Terraform
deploy-terraform:
	cd terraform && terraform init && terraform apply -auto-approve

# Deploy with kubectl
deploy-k8s:
	kubectl apply -f k8s/otel-collector.yaml
	kubectl apply -f k8s/deployment.yaml

# Run tests
test:
	mix test test/validation_test.exs

# Local development
dev:
	iex -S mix

# Check deployment status
status:
	kubectl get pods -n ash-reactor-80-20
	kubectl get svc -n ash-reactor-80-20
	kubectl get hpa -n ash-reactor-80-20

# View logs
logs:
	kubectl logs -n ash-reactor-80-20 -l app=ash-reactor --tail=100 -f

# Port forward for local access
port-forward:
	kubectl port-forward -n ash-reactor-80-20 svc/ash-reactor 4000:80

# Clean up
clean:
	kubectl delete -f k8s/deployment.yaml || true
	kubectl delete -f k8s/otel-collector.yaml || true
	cd terraform && terraform destroy -auto-approve || true

# Full deployment
all: build deploy-terraform status