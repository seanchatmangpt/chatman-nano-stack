# CNS Kubernetes Deployment

This directory contains Kubernetes manifests for deploying the CNS (Cognitive Neural System) with hardened security following the 80/20 principle.

## 🛡️ Security Features

The deployment implements the critical 20% of security measures that provide 80% protection:

1. **Resource Limits** - Thread (10) and process (5) limits enforced
2. **Pod Security** - Non-root user, read-only filesystem, dropped capabilities
3. **Network Policies** - Strict ingress/egress rules
4. **RBAC** - Minimal permissions with dedicated service account

## 📁 Files Overview

- `namespace.yaml` - Namespace with resource quotas and limits
- `configmap.yaml` - Application configuration and security patches
- `deployment.yaml` - Main deployment with security hardening
- `service.yaml` - Service and RBAC configuration
- `network-policy.yaml` - Network segmentation policies
- `high-availability.yaml` - PDB and HPA for resilience
- `stress-test-job.yaml` - Validation tests

## 🚀 Quick Start

### Prerequisites
- Kubernetes cluster (1.24+)
- kubectl configured
- 3+ nodes for high availability

### Deploy with kubectl

```bash
# Deploy all resources
kubectl apply -f namespace.yaml
kubectl apply -f .

# Or use kustomize
kubectl apply -k .
```

### Deploy with Terraform

```bash
cd ../terraform
terraform init
terraform plan -var-file=environments/production.tfvars
terraform apply -var-file=environments/production.tfvars
```

## 🧪 Validation

Run the stress tests to validate the deployment:

```bash
# Run stress tests
kubectl apply -f stress-test-job.yaml

# Check results
kubectl logs -n cns-system job/cns-stress-test

# Expected output:
# ✅ Health check passed
# ✅ Readiness check passed  
# ✅ Concurrent request test passed
# ✅ Thread limit properly enforced
# ✅ ALL STRESS TESTS PASSED
```

## 📊 Monitoring

### View metrics
```bash
# Port-forward to access metrics
kubectl port-forward -n cns-system svc/cns-service 9090:9090

# Access metrics at http://localhost:9090/metrics
```

### Key metrics to monitor:
- `thread_count` - Should stay below 10
- `process_count` - Should stay below 5
- `benchmark_duration_ms` - Should be under 50ms
- `performance_score` - Should be above 90

## 🔒 Security Verification

### Check security patches are applied:
```bash
kubectl exec -n cns-system deploy/cns-deployment -- env | grep SECURITY_PATCHES_APPLIED
# Should output: SECURITY_PATCHES_APPLIED=true
```

### Verify resource limits:
```bash
kubectl describe pod -n cns-system -l app=cns | grep -A5 "Limits:"
# Should show: cpu: 2, memory: 2Gi
```

### Test network policies:
```bash
# This should fail (blocked by network policy)
kubectl run test-pod --image=busybox --rm -it -- wget cns-service.cns-system:8080
```

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Ingress Controller                    │
└────────────────────────┬────────────────────────────────┘
                         │
┌────────────────────────┼────────────────────────────────┐
│   CNS Namespace        │          Network Policies      │
│                        │                                 │
│  ┌─────────────┐  ┌────┴────┐  ┌─────────────┐        │
│  │   Pod 1     │  │  Pod 2  │  │   Pod 3     │        │
│  │  (Zone A)   │  │ (Zone B)│  │  (Zone C)   │        │
│  └─────────────┘  └─────────┘  └─────────────┘        │
│         │               │              │                 │
│  ┌──────┴───────────────┴──────────────┴──────┐        │
│  │            CNS Service (ClusterIP)          │        │
│  └─────────────────────────────────────────────┘        │
│                                                         │
│  Resource Quotas | RBAC | Security Policies            │
└─────────────────────────────────────────────────────────┘
```

## 📈 Scaling

The deployment includes:
- **HPA**: Auto-scales 3-10 replicas based on CPU/memory
- **PDB**: Ensures minimum 2 pods during disruptions
- **Anti-affinity**: Spreads pods across nodes/zones

## 🔧 Customization

### Adjust replicas:
```bash
kubectl scale deployment -n cns-system cns-deployment --replicas=5
```

### Update resource limits:
Edit `deployment.yaml` and adjust the resources section.

### Modify security policies:
Edit `network-policy.yaml` for network rules or update ConfigMap for application settings.

## 🚨 Troubleshooting

### Check pod status:
```bash
kubectl get pods -n cns-system
kubectl describe pod -n cns-system <pod-name>
kubectl logs -n cns-system <pod-name>
```

### Common issues:

1. **Pods not starting**: Check resource quotas
   ```bash
   kubectl describe resourcequota -n cns-system
   ```

2. **Network connectivity**: Verify network policies
   ```bash
   kubectl get networkpolicy -n cns-system
   ```

3. **Performance issues**: Check HPA status
   ```bash
   kubectl get hpa -n cns-system
   ```

## 🏁 Production Checklist

- [ ] TLS certificates configured for ingress
- [ ] Backup strategy implemented  
- [ ] Monitoring and alerting configured
- [ ] Security patches validated
- [ ] Resource limits appropriate for workload
- [ ] Network policies tested
- [ ] Disaster recovery plan documented
- [ ] Performance benchmarks meet SLA

## 📝 Notes

- The deployment enforces the 80/20 security patches:
  - Max 10 threads per container
  - Max 5 processes per container  
  - Max 2GB memory per container
  - Encoding validation enabled

- For development environments, some security features can be relaxed using the dev tfvars file.

- Always run the stress tests after deployment to ensure security patches are working correctly.