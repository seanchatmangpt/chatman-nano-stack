# CNS Terraform Infrastructure

Infrastructure as Code for CNS (Cognitive Neural System) Kubernetes deployment with enterprise-grade security and high availability.

## 🏗️ Architecture

This Terraform configuration provisions:
- Kubernetes namespace with resource quotas
- Hardened deployment with security patches
- Network policies and RBAC
- Monitoring and alerting (Prometheus/Grafana)
- Auto-scaling and high availability
- Persistent storage

## 📁 Directory Structure

```
terraform/
├── main.tf                    # Main configuration
├── deployment.tf              # CNS deployment
├── monitoring.tf              # Observability setup
├── variables.tf               # Variable definitions
├── environments/              # Environment configs
│   ├── production.tfvars
│   ├── staging.tfvars
│   └── development.tfvars
└── README.md
```

## 🚀 Quick Start

### Prerequisites
- Terraform >= 1.0
- Kubernetes cluster
- kubectl configured
- Helm (for monitoring stack)

### Deploy to Production

```bash
# Initialize Terraform
terraform init

# Review plan
terraform plan -var-file=environments/production.tfvars

# Apply configuration
terraform apply -var-file=environments/production.tfvars

# Verify deployment
kubectl get all -n cns-production
```

## 🔧 Configuration

### Key Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `namespace` | K8s namespace | `cns-system` |
| `replicas` | Number of pods | `3` |
| `max_threads` | Thread limit | `10` |
| `max_processes` | Process limit | `5` |
| `max_memory_mb` | Memory limit | `2048` |
| `enable_monitoring` | Prometheus/Grafana | `true` |

### Environment-Specific Configs

**Production** (`environments/production.tfvars`):
- 3 replicas minimum
- Enhanced monitoring (90-day retention)
- Daily backups with 30-day retention
- TLS enabled with cert-manager

**Staging** (`environments/staging.tfvars`):
- 2 replicas
- Standard monitoring (30-day retention)
- Weekly backups
- TLS with staging certificates

**Development** (`environments/development.tfvars`):
- 1 replica
- Basic monitoring (7-day retention)
- No backups
- TLS disabled

## 🛡️ Security Features

Implements 80/20 security principle:

1. **Resource Limits**
   - CPU: Max 2 cores (80% of node)
   - Memory: Max 2GB
   - Threads: Max 10
   - Processes: Max 5

2. **Pod Security**
   - Non-root user (UID 1000)
   - Read-only root filesystem
   - Dropped all capabilities
   - Seccomp profile enabled

3. **Network Security**
   - Network policies enforced
   - Egress limited to DNS/HTTPS
   - Ingress from specific namespaces only

4. **RBAC**
   - Minimal permissions
   - Dedicated service account
   - Role-based access

## 📊 Monitoring

### Prometheus Metrics
- `benchmark_duration_ms`
- `performance_score`
- `thread_count`
- `process_count`
- `neural_inference_rate`

### Grafana Dashboards
Automatically provisioned dashboard includes:
- Performance metrics
- Resource usage
- Security limits
- Test results

### Alerts
Pre-configured alerts for:
- Thread/process limit violations
- Performance degradation
- Service availability
- Resource exhaustion attempts

## 🔄 Operations

### Scale Application
```bash
# Update replicas
terraform apply -var="replicas=5" -var-file=environments/production.tfvars
```

### Update Security Patches
```bash
# Edit security_patches_8020.py
# Then apply changes
terraform apply -var-file=environments/production.tfvars
```

### Backup State
```bash
# Configure backend
terraform {
  backend "s3" {
    bucket = "cns-terraform-state"
    key    = "prod/terraform.tfstate"
    region = "us-west-2"
  }
}
```

## 🧪 Validation

After deployment, validate with:

```bash
# Check resources
terraform output

# Run stress tests
kubectl apply -f ../kubernetes/stress-test-job.yaml
kubectl logs -n cns-production job/cns-stress-test

# Check metrics
kubectl port-forward -n cns-production svc/cns-service 9090:9090
curl http://localhost:9090/metrics
```

## 🚨 Troubleshooting

### Common Issues

1. **Resource Quota Exceeded**
   ```bash
   kubectl describe resourcequota -n cns-production
   ```

2. **Pod Not Starting**
   ```bash
   kubectl describe pod -n cns-production <pod-name>
   ```

3. **Terraform State Lock**
   ```bash
   terraform force-unlock <lock-id>
   ```

### Debug Commands

```bash
# Show all resources
terraform show

# Detailed plan
terraform plan -var-file=environments/production.tfvars -out=tfplan
terraform show -json tfplan | jq

# Import existing resources
terraform import kubernetes_namespace.cns cns-system
```

## 🔐 Best Practices

1. **State Management**
   - Use remote backend (S3/GCS)
   - Enable state locking
   - Regular state backups

2. **Secrets**
   - Use Kubernetes secrets
   - Never commit sensitive data
   - Consider external secret managers

3. **Monitoring**
   - Review metrics regularly
   - Set up alerting
   - Monitor security events

4. **Updates**
   - Test in dev/staging first
   - Use canary deployments
   - Have rollback plan

## 📈 Cost Optimization

- Use spot instances for dev/staging
- Right-size node pools
- Enable cluster autoscaler
- Use PVCs with appropriate storage classes

## 🏁 Production Checklist

- [ ] Remote state configured
- [ ] Backups enabled
- [ ] Monitoring configured
- [ ] Alerts set up
- [ ] TLS certificates valid
- [ ] Network policies tested
- [ ] Resource limits appropriate
- [ ] Security patches validated
- [ ] Disaster recovery tested
- [ ] Documentation updated

## 📝 Notes

- Always validate security patches after deployment
- Monitor thread/process counts to ensure limits work
- Regular security audits recommended
- Keep Terraform and provider versions updated