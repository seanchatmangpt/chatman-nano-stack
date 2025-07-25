# CNS Forge Deployment Guide

## Table of Contents

1. [Local Development](#local-development)
2. [Docker Deployment](#docker-deployment)
3. [Kubernetes Deployment](#kubernetes-deployment)
4. [AWS Production Deployment](#aws-production-deployment)
5. [Monitoring and Observability](#monitoring-and-observability)
6. [Security Hardening](#security-hardening)
7. [Backup and Recovery](#backup-and-recovery)
8. [Troubleshooting](#troubleshooting)

## Local Development

### Prerequisites

```bash
# Install Elixir 1.15+
brew install elixir

# Install PostgreSQL 14+
brew install postgresql@14
brew services start postgresql@14

# Install Redis
brew install redis
brew services start redis

# Install Node.js (for assets)
brew install node
```

### Setup

```bash
# Clone repository
git clone https://github.com/your-org/cns-forge.git
cd cns-forge

# Install dependencies
mix deps.get
mix deps.compile

# Setup database
mix ecto.create
mix ecto.migrate

# Install npm dependencies
cd assets && npm install && cd ..

# Start Phoenix server
mix phx.server
```

The application will be available at http://localhost:4000

### Development Configuration

Create a `.env` file for local development:

```bash
export DATABASE_URL=ecto://postgres:postgres@localhost/cns_forge_dev
export REDIS_URL=redis://localhost:6379
export SECRET_KEY_BASE=$(mix phx.gen.secret)
export PHX_HOST=localhost
export PORT=4000
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
```

## Docker Deployment

### Building the Image

Create a `Dockerfile`:

```dockerfile
# Build stage
FROM elixir:1.15-alpine AS build

RUN apk add --no-cache build-base git python3

WORKDIR /app

# Install hex + rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set build ENV
ENV MIX_ENV=prod

# Install mix dependencies
COPY mix.exs mix.lock ./
COPY config config
RUN mix deps.get --only $MIX_ENV
RUN mix deps.compile

# Copy application files
COPY priv priv
COPY lib lib
COPY assets assets

# Compile assets
RUN mix assets.deploy

# Compile application
RUN mix compile

# Build release
RUN mix release

# Runtime stage
FROM alpine:3.18

RUN apk add --no-cache openssl ncurses-libs libstdc++ libgcc

WORKDIR /app

RUN chown nobody:nobody /app

USER nobody:nobody

COPY --from=build --chown=nobody:nobody /app/_build/prod/rel/cns_forge ./

ENV HOME=/app

EXPOSE 4000

CMD ["bin/cns_forge", "start"]
```

### Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  postgres:
    image: postgres:14-alpine
    environment:
      POSTGRES_USER: cns_forge
      POSTGRES_PASSWORD: cns_forge_pass
      POSTGRES_DB: cns_forge_prod
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    command: redis-server --appendonly yes
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"

  app:
    build: .
    depends_on:
      - postgres
      - redis
    environment:
      DATABASE_URL: ecto://cns_forge:cns_forge_pass@postgres/cns_forge_prod
      REDIS_URL: redis://redis:6379
      SECRET_KEY_BASE: ${SECRET_KEY_BASE}
      PHX_HOST: ${PHX_HOST}
      PORT: 4000
    ports:
      - "4000:4000"

  otel-collector:
    image: otel/opentelemetry-collector:latest
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4317:4317"  # OTLP gRPC
      - "4318:4318"  # OTLP HTTP

volumes:
  postgres_data:
  redis_data:
```

### Running with Docker Compose

```bash
# Generate secret key
export SECRET_KEY_BASE=$(openssl rand -base64 64)
export PHX_HOST=localhost

# Start services
docker-compose up -d

# View logs
docker-compose logs -f app

# Stop services
docker-compose down
```

## Kubernetes Deployment

### Prerequisites

- Kubernetes cluster (1.25+)
- kubectl configured
- Helm 3.0+

### Namespace Setup

```bash
kubectl create namespace cns-forge
kubectl config set-context --current --namespace=cns-forge
```

### ConfigMap

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: cns-forge-config
  namespace: cns-forge
data:
  PHX_HOST: "cns-forge.example.com"
  PORT: "4000"
  POOL_SIZE: "10"
  BITACTOR_TTL_BUDGET: "8"
  BITACTOR_RING_SIZE: "1024"
  REACTOR_MAX_CONCURRENCY: "100"
  REACTOR_BATCH_SIZE: "50"
```

### Secret

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: cns-forge-secret
  namespace: cns-forge
type: Opaque
stringData:
  DATABASE_URL: "ecto://user:pass@postgres:5432/cns_forge"
  REDIS_URL: "redis://redis:6379"
  SECRET_KEY_BASE: "your-secret-key-base"
  OTEL_EXPORTER_OTLP_ENDPOINT: "http://otel-collector:4317"
```

### Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cns-forge
  namespace: cns-forge
spec:
  replicas: 3
  selector:
    matchLabels:
      app: cns-forge
  template:
    metadata:
      labels:
        app: cns-forge
    spec:
      containers:
      - name: cns-forge
        image: your-registry/cns-forge:latest
        ports:
        - containerPort: 4000
        envFrom:
        - configMapRef:
            name: cns-forge-config
        - secretRef:
            name: cns-forge-secret
        resources:
          requests:
            cpu: 250m
            memory: 512Mi
          limits:
            cpu: 1000m
            memory: 1Gi
        livenessProbe:
          httpGet:
            path: /health
            port: 4000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 4000
          initialDelaySeconds: 20
          periodSeconds: 5
```

### Service

```yaml
apiVersion: v1
kind: Service
metadata:
  name: cns-forge
  namespace: cns-forge
spec:
  selector:
    app: cns-forge
  ports:
  - port: 80
    targetPort: 4000
  type: ClusterIP
```

### Ingress

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: cns-forge
  namespace: cns-forge
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/websocket-services: cns-forge
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - cns-forge.example.com
    secretName: cns-forge-tls
  rules:
  - host: cns-forge.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: cns-forge
            port:
              number: 80
```

### Helm Chart Installation

```bash
# Add Helm repository
helm repo add cns-forge https://charts.cnsforge.io
helm repo update

# Install with custom values
helm install cns-forge cns-forge/cns-forge \
  --namespace cns-forge \
  --values values.yaml

# values.yaml example:
cat <<EOF > values.yaml
replicaCount: 3

image:
  repository: your-registry/cns-forge
  tag: latest
  pullPolicy: IfNotPresent

service:
  type: ClusterIP
  port: 80

ingress:
  enabled: true
  className: nginx
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
  hosts:
    - host: cns-forge.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: cns-forge-tls
      hosts:
        - cns-forge.example.com

resources:
  limits:
    cpu: 1000m
    memory: 1Gi
  requests:
    cpu: 250m
    memory: 512Mi

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
  targetMemoryUtilizationPercentage: 80

postgresql:
  enabled: true
  auth:
    database: cns_forge
    username: cns_forge
    password: changeme

redis:
  enabled: true
  auth:
    enabled: true
    password: changeme

otel:
  enabled: true
  endpoint: http://otel-collector:4317
EOF

# Upgrade deployment
helm upgrade cns-forge cns-forge/cns-forge \
  --namespace cns-forge \
  --values values.yaml
```

## AWS Production Deployment

### Terraform Infrastructure

```bash
cd terraform

# Initialize Terraform
terraform init

# Review plan
terraform plan -out=tfplan

# Apply infrastructure
terraform apply tfplan
```

This creates:

1. **VPC and Networking**
   - Multi-AZ VPC with public/private subnets
   - NAT Gateways for outbound traffic
   - Security groups with least privilege

2. **EKS Cluster**
   - Managed Kubernetes control plane
   - Auto-scaling node groups
   - OIDC provider for IRSA

3. **RDS PostgreSQL**
   - Multi-AZ deployment
   - Automated backups
   - Performance Insights enabled

4. **ElastiCache Redis**
   - Cluster mode enabled
   - Multi-AZ with automatic failover

5. **Application Load Balancer**
   - SSL/TLS termination
   - Health checks
   - Target group for EKS nodes

### Deploy to EKS

```bash
# Configure kubectl
aws eks update-kubeconfig --name cns-forge-cluster --region us-east-1

# Install AWS Load Balancer Controller
helm install aws-load-balancer-controller \
  eks/aws-load-balancer-controller \
  -n kube-system \
  --set clusterName=cns-forge-cluster

# Deploy application
kubectl apply -f k8s/
```

### Database Migration

```bash
# Port-forward to run migrations
kubectl port-forward deployment/cns-forge 4000:4000

# Run migrations
mix ecto.migrate

# Or use a Kubernetes Job:
kubectl apply -f - <<EOF
apiVersion: batch/v1
kind: Job
metadata:
  name: cns-forge-migrate
spec:
  template:
    spec:
      containers:
      - name: migrate
        image: your-registry/cns-forge:latest
        command: ["bin/cns_forge", "eval", "CNSForge.Release.migrate"]
        envFrom:
        - secretRef:
            name: cns-forge-secret
      restartPolicy: OnFailure
EOF
```

## Monitoring and Observability

### Prometheus Setup

```yaml
apiVersion: v1
kind: ServiceMonitor
metadata:
  name: cns-forge
  namespace: cns-forge
spec:
  selector:
    matchLabels:
      app: cns-forge
  endpoints:
  - port: metrics
    interval: 30s
    path: /metrics
```

### Grafana Dashboards

Import the provided dashboards:

1. **CNS Forge Overview** - System health, request rates, latency
2. **BitActor Metrics** - Actor creation, TTL usage, hop rates
3. **Workflow Performance** - Execution times, success rates
4. **Resource Usage** - CPU, memory, network metrics

### OpenTelemetry Configuration

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
      http:
        endpoint: 0.0.0.0:4318

processors:
  batch:
    timeout: 1s
    send_batch_size: 1024

  memory_limiter:
    check_interval: 1s
    limit_mib: 512

exporters:
  prometheus:
    endpoint: "0.0.0.0:8889"
    
  jaeger:
    endpoint: jaeger-collector:14250
    tls:
      insecure: true

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [jaeger]
    
    metrics:
      receivers: [otlp]
      processors: [memory_limiter, batch]
      exporters: [prometheus]
```

### Alerts

```yaml
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: cns-forge-alerts
spec:
  groups:
  - name: cns-forge
    rules:
    - alert: HighErrorRate
      expr: rate(phoenix_request_duration_count{status=~"5.."}[5m]) > 0.05
      for: 5m
      annotations:
        summary: High error rate detected
        
    - alert: BitActorTTLExhaustion
      expr: rate(cns_forge_bitactor_ttl_exhausted_total[5m]) > 10
      for: 5m
      annotations:
        summary: High rate of TTL exhaustion
        
    - alert: SlowCompilation
      expr: histogram_quantile(0.95, cns_forge_compilation_duration_seconds) > 1
      for: 10m
      annotations:
        summary: Compilation taking too long
```

## Security Hardening

### Network Policies

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: cns-forge-netpol
spec:
  podSelector:
    matchLabels:
      app: cns-forge
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: ingress-nginx
    ports:
    - protocol: TCP
      port: 4000
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: cns-forge
    ports:
    - protocol: TCP
      port: 5432  # PostgreSQL
    - protocol: TCP
      port: 6379  # Redis
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: TCP
      port: 53   # DNS
    - protocol: UDP
      port: 53
```

### Pod Security Policy

```yaml
apiVersion: policy/v1beta1
kind: PodSecurityPolicy
metadata:
  name: cns-forge-psp
spec:
  privileged: false
  allowPrivilegeEscalation: false
  requiredDropCapabilities:
    - ALL
  volumes:
    - 'configMap'
    - 'secret'
    - 'projected'
    - 'emptyDir'
    - 'persistentVolumeClaim'
  hostNetwork: false
  hostIPC: false
  hostPID: false
  runAsUser:
    rule: 'MustRunAsNonRoot'
  seLinux:
    rule: 'RunAsAny'
  fsGroup:
    rule: 'RunAsAny'
  readOnlyRootFilesystem: true
```

### RBAC

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: cns-forge-role
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list", "watch"]
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: cns-forge-rolebinding
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: cns-forge-role
subjects:
- kind: ServiceAccount
  name: cns-forge
```

## Backup and Recovery

### Database Backups

```bash
# Manual backup
kubectl exec -it postgres-0 -- pg_dump -U cns_forge cns_forge_prod > backup.sql

# Automated backup CronJob
kubectl apply -f - <<EOF
apiVersion: batch/v1
kind: CronJob
metadata:
  name: postgres-backup
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: postgres-backup
            image: postgres:14
            command:
            - /bin/sh
            - -c
            - |
              DATE=\$(date +%Y%m%d_%H%M%S)
              pg_dump -h postgres -U cns_forge cns_forge_prod | \
                aws s3 cp - s3://cns-forge-backups/postgres/\$DATE.sql
            env:
            - name: PGPASSWORD
              valueFrom:
                secretKeyRef:
                  name: postgres-secret
                  key: password
          restartPolicy: OnFailure
EOF
```

### Disaster Recovery

1. **Database Recovery**
   ```bash
   # Download latest backup
   aws s3 cp s3://cns-forge-backups/postgres/latest.sql backup.sql
   
   # Restore
   kubectl exec -i postgres-0 -- psql -U cns_forge cns_forge_prod < backup.sql
   ```

2. **Application State Recovery**
   ```bash
   # Scale down
   kubectl scale deployment cns-forge --replicas=0
   
   # Restore data
   # ... restore database, redis, etc.
   
   # Scale up
   kubectl scale deployment cns-forge --replicas=3
   ```

## Troubleshooting

### Common Issues

#### Pods Not Starting

```bash
# Check pod status
kubectl get pods -l app=cns-forge

# View pod logs
kubectl logs -l app=cns-forge --tail=100

# Describe pod
kubectl describe pod <pod-name>

# Check events
kubectl get events --sort-by='.lastTimestamp'
```

#### Database Connection Issues

```bash
# Test database connectivity
kubectl run -it --rm debug --image=postgres:14 --restart=Never -- \
  psql -h postgres -U cns_forge -d cns_forge_prod -c "SELECT 1"

# Check secrets
kubectl get secret cns-forge-secret -o yaml
```

#### Performance Issues

```bash
# Get resource usage
kubectl top pods -l app=cns-forge

# Check HPA status
kubectl get hpa

# View metrics
kubectl port-forward svc/prometheus 9090:9090
# Visit http://localhost:9090
```

#### Migration Issues

```bash
# Run migration manually
kubectl exec -it deployment/cns-forge -- bin/cns_forge eval "CNSForge.Release.migrate"

# Rollback migration
kubectl exec -it deployment/cns-forge -- bin/cns_forge eval "CNSForge.Release.rollback"
```

### Debug Mode

```bash
# Enable debug logging
kubectl set env deployment/cns-forge LOG_LEVEL=debug

# Interactive shell
kubectl exec -it deployment/cns-forge -- bin/cns_forge remote

# Run diagnostics
kubectl exec deployment/cns-forge -- bin/cns_forge eval "CNSForge.Diagnostics.run"
```

### Health Check Endpoints

- `/health` - Basic liveness check
- `/ready` - Readiness check (includes DB connectivity)
- `/metrics` - Prometheus metrics
- `/debug` - Debug information (dev only)

### Support

For production issues:
1. Check logs and metrics
2. Review alerts in Prometheus/Grafana
3. Check distributed traces in Jaeger
4. Contact on-call engineer if critical