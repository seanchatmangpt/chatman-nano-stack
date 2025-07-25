# CNS Forge Quick Start Guide

Get up and running with CNS Forge in 15 minutes!

## Prerequisites

- macOS, Linux, or WSL2
- Docker Desktop installed and running
- `kubectl` configured
- AWS CLI configured (for production deployment)
- Git

## 1. Clone and Setup (2 minutes)

```bash
# Clone the CNS Forge repository
git clone https://github.com/your-org/cns-forge.git
cd cns-forge

# Install dependencies
brew install elixir gcc terraform kubectl
pip install -r requirements.txt
```

## 2. Generate Your First Service (5 minutes)

### Option A: Use Existing Ontology

```bash
# Generate a financial trading service
python cns_forge_generator.py \
  --ontology ontologies/production_forex_trading.ttl \
  --output generated/my_trading_app \
  --name forex_trader
```

### Option B: Create Custom Ontology

Create `my_service.ttl`:

```turtle
@prefix : <http://mycompany.com/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

:MyService rdf:type owl:Ontology .

:Customer rdf:type owl:Class .
:Order rdf:type owl:Class .
:Product rdf:type owl:Class .

:hasOrder rdf:type owl:ObjectProperty ;
    rdfs:domain :Customer ;
    rdfs:range :Order .
```

Generate service:

```bash
python cns_forge_generator.py \
  --ontology my_service.ttl \
  --output generated/my_service \
  --name my_service
```

## 3. Compile and Test (3 minutes)

```bash
cd generated/my_service

# Compile C implementation
make

# Run tests
python test_my_service.py

# Expected output:
# ✅ 8-tick compliance: PASS
# ✅ Adversarial tests: PASS
# ✅ Stress tests: PASS
```

## 4. Local Deployment with Docker (3 minutes)

```bash
# Build Docker image
docker build -t my_service:latest .

# Run locally
docker run -p 8080:8080 my_service:latest

# Test the service
curl http://localhost:8080/health
# {"status": "healthy", "version": "1.0.0"}
```

## 5. Deploy to Kubernetes (2 minutes)

```bash
# Create namespace
kubectl create namespace cns-forge

# Deploy service
kubectl apply -f k8s/ -n cns-forge

# Check deployment
kubectl get pods -n cns-forge
# NAME                          READY   STATUS    RESTARTS   AGE
# my-service-7d4b8c6f5-x2n4p   1/1     Running   0          30s

# Port forward to test
kubectl port-forward -n cns-forge svc/my-service 8080:80

# Access service
curl http://localhost:8080/api/v1/status
```

## Quick Examples

### Example 1: Legal Case Management

```bash
# Generate legal service
python cns_forge_generator.py \
  --ontology ontologies/legal_case.ttl \
  --output generated/legal_app \
  --features "case-tracking,billing,documents"

# Deploy
cd generated/legal_app
make && docker build -t legal-app .
docker run -p 8081:8080 legal-app
```

### Example 2: Healthcare System

```bash
# Generate healthcare service
python cns_forge_generator.py \
  --ontology ontologies/healthcare_core.ttl \
  --output generated/health_app \
  --compliance hipaa

# Test HIPAA compliance
python test_health_app.py --compliance-check
```

### Example 3: IoT Monitoring

```bash
# Generate IoT service
python cns_forge_generator.py \
  --ontology ontologies/industrial_iot_core.ttl \
  --output generated/iot_monitor \
  --edge-optimized

# Run edge simulation
cd generated/iot_monitor
./run_edge_simulation.sh
```

## Common Commands

### Generate Multiple Services

```bash
# Batch generation
for service in legal finance healthcare iot; do
  python cns_forge_generator.py \
    --ontology ontologies/${service}_core.ttl \
    --output generated/${service}_app
done
```

### Run All Tests

```bash
# Comprehensive testing
cd generated
for dir in */; do
  echo "Testing $dir..."
  cd "$dir" && make test && cd ..
done
```

### Deploy Everything

```bash
# Deploy all services
kubectl apply -f generated/*/k8s/ -n cns-forge

# Check all deployments
kubectl get all -n cns-forge
```

## Performance Verification

```bash
# Run performance benchmark
./benchmark.sh my_service

# Output:
# Service: my_service
# 8-tick compliance: 97.8%
# P99 latency: 0.8ms
# Throughput: 15,234 req/s
# Adversarial survival: 100%
```

## Next Steps

1. **Customize Templates**: Edit `templates/*.j2` for your needs
2. **Add Ontologies**: Create domain-specific ontologies
3. **Production Deploy**: Use `terraform apply` for AWS deployment
4. **Monitor**: Access metrics at `http://localhost:9090/metrics`

## Troubleshooting

### Service Won't Start
```bash
# Check logs
docker logs <container-id>
kubectl logs -n cns-forge <pod-name>

# Common fix: increase memory
docker run -m 2g my_service:latest
```

### Compilation Errors
```bash
# Ensure GCC is updated
gcc --version  # Should be 11+

# Clean and rebuild
make clean && make
```

### Performance Issues
```bash
# Profile the service
perf record -g ./my_service_test
perf report

# Check 8-tick compliance
./benchmark_8tick
```

## Getting Help

- 📖 [Full Documentation](./cns-forge-portfolio.md)
- 🔧 [Technical Guide](./cns-forge-technical-guide.md)
- 💬 [Community Forum](https://forum.cns-forge.io)
- 🐛 [Issue Tracker](https://github.com/your-org/cns-forge/issues)

---

**Ready to build production-grade services in minutes? Start with CNS Forge!** 🚀