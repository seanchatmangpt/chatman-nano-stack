# CNS Aegis Fabric - Makefile
# Generated from TTL specifications

.PHONY: all build test clean docker k8s

all: build test

build:
	@echo "Building BitActor..."
	gcc -O3 -march=native -mtune=native \
		-fstack-protector-strong \
		-D_FORTIFY_SOURCE=2 \
		-o bitactor bitactor_generated.c \
		-lpthread -lm

test: build
	@echo "Running validation..."
	python3 aegis_fabric_validator.py

docker:
	@echo "Building Docker image..."
	docker build -f Dockerfile.aegis -t aegis-fabric/bitactor:latest .

k8s:
	@echo "Deploying to Kubernetes..."
	kubectl apply -f aegis_fabric_deployment.yaml

terraform-init:
	@echo "Initializing Terraform..."
	terraform init

terraform-plan: terraform-init
	@echo "Planning Terraform deployment..."
	terraform plan

terraform-apply: terraform-plan
	@echo "Applying Terraform deployment..."
	terraform apply -auto-approve

clean:
	rm -f bitactor bitactor_test
	rm -f aegis_validation_results.json

help:
	@echo "Available targets:"
	@echo "  make build     - Build BitActor binary"
	@echo "  make test      - Run validation tests"
	@echo "  make docker    - Build Docker image"
	@echo "  make k8s       - Deploy to Kubernetes"
	@echo "  make clean     - Clean build artifacts"
