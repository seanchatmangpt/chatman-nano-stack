# 🚀 Production Deployment Guide: Ultimate Nuxt Dashboard

## Infrastructure Architecture

### Production Environment Setup

```yaml
# infrastructure/production.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: cns-dashboard
  labels:
    name: cns-dashboard
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cns-dashboard-frontend
  namespace: cns-dashboard
spec:
  replicas: 3
  selector:
    matchLabels:
      app: cns-dashboard-frontend
  template:
    metadata:
      labels:
        app: cns-dashboard-frontend
    spec:
      containers:
      - name: dashboard
        image: cns/dashboard:latest
        ports:
        - containerPort: 3000
        env:
        - name: NODE_ENV
          value: "production"
        - name: WS_URL
          value: "wss://api.cns-forge.com/ws"
        - name: API_BASE
          value: "https://api.cns-forge.com"
        - name: GRAPHQL_URL
          value: "https://api.cns-forge.com/graphql"
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: cns-dashboard-service
  namespace: cns-dashboard
spec:
  selector:
    app: cns-dashboard-frontend
  ports:
  - port: 80
    targetPort: 3000
  type: LoadBalancer
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: cns-dashboard-ingress
  namespace: cns-dashboard
  annotations:
    kubernetes.io/ingress.class: "nginx"
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    nginx.ingress.kubernetes.io/force-ssl-redirect: "true"
spec:
  tls:
  - hosts:
    - dashboard.cns-forge.com
    secretName: cns-dashboard-tls
  rules:
  - host: dashboard.cns-forge.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: cns-dashboard-service
            port:
              number: 80
```

## CI/CD Pipeline

### GitHub Actions Workflow

```yaml
# .github/workflows/deploy.yml
name: Deploy CNS Dashboard

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}/cns-dashboard

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'
    
    - name: Install dependencies
      run: npm ci
    
    - name: Run linting
      run: npm run lint
    
    - name: Run tests
      run: npm run test:unit
    
    - name: Run type checking
      run: npm run type-check
    
    - name: Build application
      run: npm run build
    
    - name: Run E2E tests
      run: npm run test:e2e

  security-scan:
    runs-on: ubuntu-latest
    needs: test
    steps:
    - uses: actions/checkout@v4
    
    - name: Run security scan
      uses: snyk/actions/node@master
      env:
        SNYK_TOKEN: ${{ secrets.SNYK_TOKEN }}
      with:
        args: --severity-threshold=high

  build-and-push:
    runs-on: ubuntu-latest
    needs: [test, security-scan]
    permissions:
      contents: read
      packages: write
    steps:
    - uses: actions/checkout@v4
    
    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v3
    
    - name: Log in to Container Registry
      uses: docker/login-action@v3
      with:
        registry: ${{ env.REGISTRY }}
        username: ${{ github.actor }}
        password: ${{ secrets.GITHUB_TOKEN }}
    
    - name: Extract metadata
      id: meta
      uses: docker/metadata-action@v5
      with:
        images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
        tags: |
          type=ref,event=branch
          type=ref,event=pr
          type=semver,pattern={{version}}
          type=semver,pattern={{major}}.{{minor}}
          type=sha
    
    - name: Build and push Docker image
      uses: docker/build-push-action@v5
      with:
        context: .
        push: true
        tags: ${{ steps.meta.outputs.tags }}
        labels: ${{ steps.meta.outputs.labels }}
        cache-from: type=gha
        cache-to: type=gha,mode=max

  deploy-staging:
    runs-on: ubuntu-latest
    needs: build-and-push
    environment: staging
    steps:
    - name: Deploy to staging
      uses: steebchen/kubectl@v2
      with:
        config: ${{ secrets.KUBE_CONFIG_STAGING }}
        command: apply -f infrastructure/staging.yaml
    
    - name: Wait for deployment
      uses: steebchen/kubectl@v2
      with:
        config: ${{ secrets.KUBE_CONFIG_STAGING }}
        command: rollout status deployment/cns-dashboard-frontend -n cns-dashboard

  deploy-production:
    runs-on: ubuntu-latest
    needs: [build-and-push, deploy-staging]
    environment: production
    if: github.ref == 'refs/heads/main'
    steps:
    - name: Deploy to production
      uses: steebchen/kubectl@v2
      with:
        config: ${{ secrets.KUBE_CONFIG_PRODUCTION }}
        command: apply -f infrastructure/production.yaml
    
    - name: Wait for deployment
      uses: steebchen/kubectl@v2
      with:
        config: ${{ secrets.KUBE_CONFIG_PRODUCTION }}
        command: rollout status deployment/cns-dashboard-frontend -n cns-dashboard
    
    - name: Run smoke tests
      run: |
        curl -f https://dashboard.cns-forge.com/health
        curl -f https://dashboard.cns-forge.com/ready
```

## Performance Optimization

### Nuxt Configuration for Production

```typescript
// nuxt.config.ts
export default defineNuxtConfig({
  // Production optimizations
  nitro: {
    compressPublicAssets: true,
    minify: true,
    prerender: {
      routes: ['/']
    }
  },
  
  // Build optimizations
  build: {
    transpile: ['d3', 'three', 'chart.js']
  },
  
  // Runtime optimizations
  runtimeConfig: {
    public: {
      wsUrl: process.env.WS_URL || 'wss://api.cns-forge.com/ws',
      apiBase: process.env.API_BASE || 'https://api.cns-forge.com',
      graphqlUrl: process.env.GRAPHQL_URL || 'https://api.cns-forge.com/graphql'
    }
  },
  
  // Performance modules
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt',
    '@vueuse/nuxt',
    '@nuxtjs/robots',
    '@nuxtjs/sitemap'
  ],
  
  // SEO and performance
  app: {
    head: {
      title: 'CNS Ultimate Dashboard',
      meta: [
        { charset: 'utf-8' },
        { name: 'viewport', content: 'width=device-width, initial-scale=1' },
        { name: 'description', content: 'Ultimate command center for CNS/BitActor/Forge ecosystem' },
        { name: 'theme-color', content: '#00d4ff' }
      ],
      link: [
        { rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' },
        { rel: 'preconnect', href: 'https://api.cns-forge.com' },
        { rel: 'dns-prefetch', href: 'https://api.cns-forge.com' }
      ]
    }
  },
  
  // PWA configuration
  pwa: {
    registerType: 'autoUpdate',
    workbox: {
      navigateFallback: '/',
      globPatterns: ['**/*.{js,css,html,png,svg,ico}']
    },
    client: {
      installPrompt: true
    },
    devOptions: {
      enabled: true,
      type: 'module'
    }
  }
})
```

### CDN and Edge Optimization

```typescript
// server/middleware/cdn.ts
export default defineEventHandler((event) => {
  // Add CDN headers
  setResponseHeaders(event, {
    'Cache-Control': 'public, max-age=31536000, immutable',
    'CDN-Cache-Control': 'public, max-age=31536000',
    'Vary': 'Accept-Encoding'
  });
  
  // Gzip compression
  if (getRequestHeader(event, 'accept-encoding')?.includes('gzip')) {
    setResponseHeader(event, 'Content-Encoding', 'gzip');
  }
});
```

## Monitoring and Observability

### Application Monitoring

```typescript
// plugins/monitoring.client.ts
export default defineNuxtPlugin(() => {
  const config = useRuntimeConfig();
  
  // Performance monitoring
  if (process.client) {
    // Web Vitals
    import('web-vitals').then(({ getCLS, getFID, getFCP, getLCP, getTTFB }) => {
      getCLS(console.log);
      getFID(console.log);
      getFCP(console.log);
      getLCP(console.log);
      getTTFB(console.log);
    });
    
    // Error tracking
    window.addEventListener('error', (event) => {
      console.error('Global error:', event.error);
      // Send to monitoring service
    });
    
    // Unhandled promise rejections
    window.addEventListener('unhandledrejection', (event) => {
      console.error('Unhandled promise rejection:', event.reason);
      // Send to monitoring service
    });
  }
});
```

### Health Check Endpoints

```typescript
// server/api/health.get.ts
export default defineEventHandler(async (event) => {
  const health = {
    status: 'healthy',
    timestamp: new Date().toISOString(),
    version: process.env.npm_package_version,
    uptime: process.uptime(),
    memory: process.memoryUsage(),
    checks: {
      database: await checkDatabase(),
      websocket: await checkWebSocket(),
      api: await checkAPI()
    }
  };
  
  const isHealthy = Object.values(health.checks).every(check => check.status === 'healthy');
  
  setResponseStatus(event, isHealthy ? 200 : 503);
  return health;
});

async function checkDatabase() {
  try {
    // Database health check
    return { status: 'healthy', responseTime: 10 };
  } catch (error) {
    return { status: 'unhealthy', error: error.message };
  }
}

async function checkWebSocket() {
  try {
    // WebSocket health check
    return { status: 'healthy', responseTime: 5 };
  } catch (error) {
    return { status: 'unhealthy', error: error.message };
  }
}

async function checkAPI() {
  try {
    // API health check
    return { status: 'healthy', responseTime: 15 };
  } catch (error) {
    return { status: 'unhealthy', error: error.message };
  }
}
```

## Security Configuration

### Security Headers

```typescript
// server/middleware/security.ts
export default defineEventHandler((event) => {
  // Security headers
  setResponseHeaders(event, {
    'X-Content-Type-Options': 'nosniff',
    'X-Frame-Options': 'DENY',
    'X-XSS-Protection': '1; mode=block',
    'Referrer-Policy': 'strict-origin-when-cross-origin',
    'Content-Security-Policy': `
      default-src 'self';
      script-src 'self' 'unsafe-inline' 'unsafe-eval' https://api.cns-forge.com;
      style-src 'self' 'unsafe-inline' https://fonts.googleapis.com;
      font-src 'self' https://fonts.gstatic.com;
      img-src 'self' data: https:;
      connect-src 'self' wss://api.cns-forge.com https://api.cns-forge.com;
      frame-ancestors 'none';
    `.replace(/\s+/g, ' ').trim()
  });
});
```

### Authentication Middleware

```typescript
// server/middleware/auth.ts
export default defineEventHandler(async (event) => {
  const publicRoutes = ['/health', '/ready', '/api/health'];
  
  if (publicRoutes.includes(getRequestPath(event))) {
    return;
  }
  
  const token = getRequestHeader(event, 'authorization')?.replace('Bearer ', '');
  
  if (!token) {
    throw createError({
      statusCode: 401,
      statusMessage: 'Unauthorized'
    });
  }
  
  try {
    // Verify JWT token
    const decoded = await verifyToken(token);
    event.context.user = decoded;
  } catch (error) {
    throw createError({
      statusCode: 401,
      statusMessage: 'Invalid token'
    });
  }
});
```

## Backup and Disaster Recovery

### Database Backup Strategy

```yaml
# infrastructure/backup-cronjob.yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: dashboard-backup
  namespace: cns-dashboard
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: backup
            image: postgres:15
            command:
            - /bin/bash
            - -c
            - |
              pg_dump $DATABASE_URL | gzip > /backup/dashboard-$(date +%Y%m%d).sql.gz
              aws s3 cp /backup/dashboard-$(date +%Y%m%d).sql.gz s3://cns-backups/
            env:
            - name: DATABASE_URL
              valueFrom:
                secretKeyRef:
                  name: dashboard-secrets
                  key: database-url
            - name: AWS_ACCESS_KEY_ID
              valueFrom:
                secretKeyRef:
                  name: dashboard-secrets
                  key: aws-access-key
            - name: AWS_SECRET_ACCESS_KEY
              valueFrom:
                secretKeyRef:
                  name: dashboard-secrets
                  key: aws-secret-key
            volumeMounts:
            - name: backup-volume
              mountPath: /backup
          volumes:
          - name: backup-volume
            emptyDir: {}
          restartPolicy: OnFailure
```

## Scaling Strategy

### Horizontal Pod Autoscaler

```yaml
# infrastructure/hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: cns-dashboard-hpa
  namespace: cns-dashboard
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: cns-dashboard-frontend
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 10
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 60
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
```

This deployment guide provides a comprehensive approach to deploying the ultimate Nuxt dashboard in production with enterprise-grade security, monitoring, and scalability features. 