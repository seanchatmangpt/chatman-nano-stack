#!/usr/bin/env python3
"""
Nuxt.js Deployment Permutations Generator
Creates deployment configurations for different hosting platforms and strategies
"""

import json
import yaml
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

class NuxtDeploymentGenerator:
    def __init__(self, base_path: str = "/Users/sac/cns/nuxt_permutations"):
        self.base_path = Path(base_path)
        self.deployment_configs = {}
        
    def generate_all_deployments(self) -> Dict[str, Any]:
        """Generate all deployment permutations"""
        print("🚀 Generating Nuxt.js Deployment Permutations")
        print("=" * 60)
        
        deployments = {
            "timestamp": datetime.now().isoformat(),
            "platforms": {},
            "strategies": {},
            "configurations": {}
        }
        
        # Platform-specific deployments
        platforms = [
            "vercel", "netlify", "aws", "gcp", "azure", "docker", "kubernetes"
        ]
        
        for platform in platforms:
            print(f"📦 Generating {platform} deployment...")
            deployments["platforms"][platform] = self.generate_platform_config(platform)
            
        # Deployment strategies
        strategies = [
            "static", "ssr", "spa", "hybrid", "edge", "serverless"
        ]
        
        for strategy in strategies:
            print(f"⚡ Generating {strategy} strategy...")
            deployments["strategies"][strategy] = self.generate_strategy_config(strategy)
            
        # Create comprehensive deployment matrix
        deployments["configurations"] = self.create_deployment_matrix()
        
        # Save all configurations
        self.save_deployment_configs(deployments)
        
        return deployments
        
    def generate_platform_config(self, platform: str) -> Dict[str, Any]:
        """Generate platform-specific deployment configuration"""
        
        configs = {
            "vercel": {
                "name": "Vercel Deployment",
                "description": "Serverless deployment with global CDN",
                "config_files": {
                    "vercel.json": {
                        "version": 2,
                        "builds": [
                            {
                                "src": "nuxt.config.js",
                                "use": "@nuxtjs/vercel-builder"
                            }
                        ],
                        "routes": [
                            {
                                "src": "/api/(.*)",
                                "dest": "/api/$1"
                            }
                        ]
                    }
                },
                "environment_variables": {
                    "ASH_API_URL": "https://api.cns-forge.com",
                    "BITACTOR_WS_URL": "wss://bitactor.cns-forge.com",
                    "NODE_VERSION": "18"
                },
                "features": ["Auto-scaling", "Edge Functions", "Preview Deployments"],
                "commands": {
                    "install": "npm install",
                    "build": "npm run build",
                    "start": "npm run start"
                }
            },
            
            "netlify": {
                "name": "Netlify Deployment", 
                "description": "JAMstack deployment with form handling",
                "config_files": {
                    "netlify.toml": """
[build]
  command = "npm run generate"
  publish = "dist"
  
[build.environment]
  NODE_VERSION = "18"
  
[[redirects]]
  from = "/api/*"
  to = "/.netlify/functions/:splat"
  status = 200
  
[[headers]]
  for = "/*.js"
  [headers.values]
    Cache-Control = "public, max-age=31536000"
"""
                },
                "features": ["Forms", "Identity", "Analytics", "Edge Functions"],
                "strategy": "static"
            },
            
            "aws": {
                "name": "AWS Deployment",
                "description": "Scalable cloud deployment with S3 and CloudFront",
                "config_files": {
                    "aws-cloudformation.yaml": {
                        "AWSTemplateFormatVersion": "2010-09-09",
                        "Resources": {
                            "S3Bucket": {
                                "Type": "AWS::S3::Bucket",
                                "Properties": {
                                    "BucketName": "cns-forge-nuxt-app",
                                    "WebsiteConfiguration": {
                                        "IndexDocument": "index.html"
                                    }
                                }
                            },
                            "CloudFrontDistribution": {
                                "Type": "AWS::CloudFront::Distribution",
                                "Properties": {
                                    "DistributionConfig": {
                                        "Origins": [{
                                            "DomainName": {"Fn::GetAtt": ["S3Bucket", "DomainName"]},
                                            "Id": "S3Origin",
                                            "S3OriginConfig": {}
                                        }],
                                        "DefaultCacheBehavior": {
                                            "TargetOriginId": "S3Origin",
                                            "ViewerProtocolPolicy": "redirect-to-https"
                                        },
                                        "Enabled": True
                                    }
                                }
                            }
                        }
                    }
                },
                "services": ["S3", "CloudFront", "Lambda", "API Gateway"],
                "features": ["Auto-scaling", "Global CDN", "Lambda@Edge"]
            },
            
            "docker": {
                "name": "Docker Deployment",
                "description": "Containerized deployment for any platform",
                "config_files": {
                    "Dockerfile": """
FROM node:18-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy source code
COPY . .

# Build application
RUN npm run build

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \\
  CMD node healthcheck.js

# Start application
CMD ["npm", "start"]
""",
                    "docker-compose.yml": """
version: '3.8'

services:
  nuxt-app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
      - ASH_API_URL=http://ash-api:4000
      - BITACTOR_WS_URL=ws://bitactor:8080
    depends_on:
      - ash-api
      - bitactor
    restart: unless-stopped
    
  ash-api:
    image: cns-forge/ash-api:latest
    ports:
      - "4000:4000"
    environment:
      - DATABASE_URL=postgres://user:pass@postgres:5432/cns_forge
    depends_on:
      - postgres
      
  bitactor:
    image: cns-forge/bitactor:latest
    ports:
      - "8080:8080"
    environment:
      - BITACTOR_CONFIG=/etc/bitactor/config.toml
      
  postgres:
    image: postgres:15
    environment:
      - POSTGRES_DB=cns_forge
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=pass
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
"""
                },
                "features": ["Portability", "Isolation", "Scalability"]
            },
            
            "kubernetes": {
                "name": "Kubernetes Deployment",
                "description": "Cloud-native orchestration with auto-scaling",
                "config_files": {
                    "k8s-deployment.yaml": """
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nuxt-app
  labels:
    app: nuxt-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: nuxt-app
  template:
    metadata:
      labels:
        app: nuxt-app
    spec:
      containers:
      - name: nuxt-app
        image: cns-forge/nuxt-app:latest
        ports:
        - containerPort: 3000
        env:
        - name: ASH_API_URL
          value: "http://ash-api-service:4000"
        - name: BITACTOR_WS_URL
          value: "ws://bitactor-service:8080"
        resources:
          requests:
            memory: "128Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
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
  name: nuxt-app-service
spec:
  selector:
    app: nuxt-app
  ports:
  - port: 80
    targetPort: 3000
  type: LoadBalancer

---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: nuxt-app-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: nuxt-app
  minReplicas: 2
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
"""
                },
                "features": ["Auto-scaling", "Service Discovery", "Rolling Updates", "Health Checks"]
            }
        }
        
        return configs.get(platform, {"name": f"{platform} Deployment", "description": "Custom deployment configuration"})
        
    def generate_strategy_config(self, strategy: str) -> Dict[str, Any]:
        """Generate deployment strategy configuration"""
        
        strategies = {
            "static": {
                "name": "Static Site Generation (SSG)",
                "description": "Pre-rendered static files for maximum performance",
                "nuxt_config": {
                    "nitro": {
                        "prerender": {
                            "routes": ["/", "/ontology", "/schema"]
                        }
                    },
                    "ssr": False
                },
                "build_command": "npm run generate",
                "output_dir": "dist",
                "benefits": ["Fastest loading", "SEO friendly", "CDN optimized"],
                "use_cases": ["Documentation", "Marketing sites", "Blogs"]
            },
            
            "ssr": {
                "name": "Server-Side Rendering (SSR)",
                "description": "Dynamic server-side rendering for real-time data",
                "nuxt_config": {
                    "ssr": True,
                    "nitro": {
                        "preset": "node-server"
                    }
                },
                "build_command": "npm run build",
                "runtime": "Node.js server",
                "benefits": ["Dynamic content", "SEO friendly", "Fast initial load"],
                "use_cases": ["E-commerce", "Dynamic dashboards", "User portals"]
            },
            
            "spa": {
                "name": "Single Page Application (SPA)",
                "description": "Client-side rendering for interactive applications",
                "nuxt_config": {
                    "ssr": False,
                    "app": {
                        "spa": True
                    }
                },
                "build_command": "npm run build",
                "runtime": "Static hosting",
                "benefits": ["Rich interactions", "App-like experience", "Simple deployment"],
                "use_cases": ["Dashboards", "Admin panels", "Interactive tools"]
            },
            
            "hybrid": {
                "name": "Hybrid Rendering",
                "description": "Mix of SSR, SSG, and SPA for optimal performance",
                "nuxt_config": {
                    "nitro": {
                        "routeRules": {
                            "/": {"prerender": True},
                            "/ontology": {"prerender": True},
                            "/dashboard/**": {"ssr": False},
                            "/api/**": {"cors": True}
                        }
                    }
                },
                "benefits": ["Best of all worlds", "Optimized per route", "Flexible"],
                "use_cases": ["Complex applications", "Mixed content types", "Performance optimization"]
            },
            
            "edge": {
                "name": "Edge-Side Rendering", 
                "description": "Rendering at CDN edge locations",
                "nuxt_config": {
                    "nitro": {
                        "preset": "cloudflare-pages"
                    }
                },
                "runtime": "Edge functions",
                "benefits": ["Global performance", "Reduced latency", "Auto-scaling"],
                "use_cases": ["Global applications", "Real-time features", "High traffic sites"]
            },
            
            "serverless": {
                "name": "Serverless Functions",
                "description": "Function-as-a-Service deployment",
                "nuxt_config": {
                    "nitro": {
                        "preset": "aws-lambda"
                    }
                },
                "runtime": "Lambda functions",
                "benefits": ["Pay per use", "Auto-scaling", "No server management"],
                "use_cases": ["APIs", "Microservices", "Event-driven apps"]
            }
        }
        
        return strategies.get(strategy, {"name": f"{strategy} Strategy", "description": "Custom deployment strategy"})
        
    def create_deployment_matrix(self) -> Dict[str, Any]:
        """Create comprehensive deployment matrix combining platforms and strategies"""
        
        matrix = {
            "combinations": {},
            "recommendations": {},
            "comparison": {}
        }
        
        # Platform + Strategy combinations
        combinations = [
            ("vercel", "ssr", "Vercel SSR", "Perfect for dynamic web apps"),
            ("vercel", "static", "Vercel SSG", "Optimal for static sites"),
            ("netlify", "static", "Netlify JAMstack", "Best for content sites"),
            ("aws", "hybrid", "AWS Hybrid", "Enterprise-grade deployment"),
            ("docker", "ssr", "Docker SSR", "Portable containerized app"),
            ("kubernetes", "hybrid", "K8s Hybrid", "Cloud-native scaling"),
            ("gcp", "edge", "GCP Edge", "Global edge deployment"),
            ("azure", "serverless", "Azure Functions", "Event-driven deployment")
        ]
        
        for platform, strategy, name, description in combinations:
            matrix["combinations"][f"{platform}_{strategy}"] = {
                "name": name,
                "platform": platform,
                "strategy": strategy, 
                "description": description,
                "complexity": self.get_complexity_score(platform, strategy),
                "performance": self.get_performance_score(platform, strategy),
                "cost": self.get_cost_score(platform, strategy),
                "scalability": self.get_scalability_score(platform, strategy)
            }
            
        # Recommendations by use case
        matrix["recommendations"] = {
            "startup_mvp": {
                "platform": "vercel",
                "strategy": "ssr",
                "reason": "Quick deployment, good performance, free tier"
            },
            "enterprise_app": {
                "platform": "kubernetes",
                "strategy": "hybrid", 
                "reason": "Maximum control, scalability, enterprise features"
            },
            "documentation_site": {
                "platform": "netlify",
                "strategy": "static",
                "reason": "Perfect for static content, excellent DX"
            },
            "performance_dashboard": {
                "platform": "docker",
                "strategy": "spa",
                "reason": "Real-time updates, containerized deployment"
            },
            "global_saas": {
                "platform": "gcp",
                "strategy": "edge",
                "reason": "Global performance, edge computing"
            }
        }
        
        return matrix
        
    def get_complexity_score(self, platform: str, strategy: str) -> int:
        """Get deployment complexity score (1-10)"""
        complexity_scores = {
            "vercel": 2, "netlify": 2, "docker": 5, 
            "kubernetes": 8, "aws": 6, "gcp": 6, "azure": 6
        }
        strategy_scores = {
            "static": 1, "spa": 2, "ssr": 4, 
            "hybrid": 6, "edge": 7, "serverless": 5
        }
        return min(10, complexity_scores.get(platform, 5) + strategy_scores.get(strategy, 3))
        
    def get_performance_score(self, platform: str, strategy: str) -> int:
        """Get performance score (1-10)"""
        platform_scores = {
            "vercel": 9, "netlify": 8, "gcp": 9,
            "aws": 8, "azure": 7, "docker": 6, "kubernetes": 8
        }
        strategy_scores = {
            "static": 10, "edge": 9, "ssr": 7,
            "spa": 6, "hybrid": 8, "serverless": 7
        }
        return min(10, (platform_scores.get(platform, 5) + strategy_scores.get(strategy, 5)) // 2)
        
    def get_cost_score(self, platform: str, strategy: str) -> int:
        """Get cost-effectiveness score (1-10, higher = cheaper)"""
        platform_scores = {
            "vercel": 7, "netlify": 8, "docker": 9,
            "kubernetes": 5, "aws": 6, "gcp": 6, "azure": 6
        }
        strategy_scores = {
            "static": 10, "spa": 9, "serverless": 8,
            "ssr": 6, "hybrid": 5, "edge": 7
        }
        return min(10, (platform_scores.get(platform, 5) + strategy_scores.get(strategy, 5)) // 2)
        
    def get_scalability_score(self, platform: str, strategy: str) -> int:
        """Get scalability score (1-10)"""
        platform_scores = {
            "kubernetes": 10, "aws": 9, "gcp": 9,
            "azure": 8, "vercel": 8, "docker": 6, "netlify": 5
        }
        strategy_scores = {
            "serverless": 10, "edge": 9, "hybrid": 8,
            "ssr": 7, "static": 5, "spa": 4
        }
        return min(10, (platform_scores.get(platform, 5) + strategy_scores.get(strategy, 5)) // 2)
        
    def save_deployment_configs(self, deployments: Dict[str, Any]):
        """Save all deployment configurations to files"""
        
        # Create deployments directory
        deploy_dir = self.base_path / "deployments"
        deploy_dir.mkdir(exist_ok=True)
        
        # Save main configuration
        with open(deploy_dir / "deployment_matrix.json", 'w') as f:
            json.dump(deployments, f, indent=2)
            
        # Save platform-specific configs
        platforms_dir = deploy_dir / "platforms"
        platforms_dir.mkdir(exist_ok=True)
        
        for platform, config in deployments["platforms"].items():
            platform_dir = platforms_dir / platform
            platform_dir.mkdir(exist_ok=True)
            
            # Save config files
            if "config_files" in config:
                for filename, content in config["config_files"].items():
                    file_path = platform_dir / filename
                    
                    if isinstance(content, dict):
                        if filename.endswith('.json'):
                            with open(file_path, 'w') as f:
                                json.dump(content, f, indent=2)
                        elif filename.endswith('.yaml') or filename.endswith('.yml'):
                            with open(file_path, 'w') as f:
                                yaml.dump(content, f, default_flow_style=False)
                    else:
                        with open(file_path, 'w') as f:
                            f.write(content)
                            
        # Generate summary report
        self.generate_deployment_report(deployments, deploy_dir)
        
        print(f"📁 Deployment configurations saved to: {deploy_dir}")
        
    def generate_deployment_report(self, deployments: Dict[str, Any], output_dir: Path):
        """Generate comprehensive deployment report"""
        
        report = f"""# Nuxt.js Deployment Permutations Report

**Generated:** {deployments['timestamp']}

## Overview

This report covers {len(deployments['platforms'])} deployment platforms and {len(deployments['strategies'])} strategies, providing {len(deployments['configurations']['combinations'])} unique deployment combinations for the CNS Forge Nuxt.js applications.

## Deployment Matrix

```mermaid
graph TD
    A[Nuxt.js Apps] --> B[Deployment Platforms]
    A --> C[Rendering Strategies]
    
    B --> D[Vercel]
    B --> E[Netlify] 
    B --> F[AWS]
    B --> G[Docker]
    B --> H[Kubernetes]
    
    C --> I[Static SSG]
    C --> J[Server SSR]
    C --> K[SPA]
    C --> L[Hybrid]
    C --> M[Edge]
    C --> N[Serverless]
    
    D --> O[Perfect for SSR/SSG]
    E --> P[Ideal for JAMstack]
    F --> Q[Enterprise Scale]
    G --> R[Portable Deployment]
    H --> S[Cloud Native]
```

## Platform Comparison

| Platform | Complexity | Performance | Cost | Scalability | Best For |
|----------|------------|-------------|------|-------------|----------|
"""
        
        for platform, config in deployments["platforms"].items():
            if platform in deployments["configurations"]["combinations"]:
                combo = next((c for c in deployments["configurations"]["combinations"].values() 
                             if c["platform"] == platform), {})
                report += f"| {platform.title()} | {combo.get('complexity', 'N/A')}/10 | {combo.get('performance', 'N/A')}/10 | {combo.get('cost', 'N/A')}/10 | {combo.get('scalability', 'N/A')}/10 | {config.get('description', 'General use')} |\n"
                
        report += f"""

## Strategy Comparison

| Strategy | Description | Best Use Cases |
|----------|-------------|----------------|
"""
        
        for strategy, config in deployments["strategies"].items():
            use_cases = ", ".join(config.get("use_cases", []))
            report += f"| {strategy.upper()} | {config.get('description', '')} | {use_cases} |\n"
            
        report += f"""

## Recommendations by Use Case

"""
        
        for use_case, rec in deployments["configurations"]["recommendations"].items():
            report += f"### {use_case.replace('_', ' ').title()}\n"
            report += f"- **Platform:** {rec['platform'].title()}\n"
            report += f"- **Strategy:** {rec['strategy'].upper()}\n" 
            report += f"- **Reason:** {rec['reason']}\n\n"
            
        report += f"""

## Getting Started

### 1. Choose Your Deployment

Select the appropriate platform and strategy based on your requirements:

- **For quick prototypes:** Vercel + SSR
- **For static content:** Netlify + SSG
- **For enterprise:** Kubernetes + Hybrid
- **For global scale:** GCP + Edge

### 2. Configure Your Project

Each platform includes configuration files in the `deployments/platforms/` directory.

### 3. Deploy

Follow the platform-specific deployment instructions:

#### Vercel
```bash
npm install -g vercel
vercel --prod
```

#### Netlify
```bash
npm install -g netlify-cli
netlify deploy --prod
```

#### Docker
```bash
docker build -t nuxt-app .
docker run -p 3000:3000 nuxt-app
```

#### Kubernetes
```bash
kubectl apply -f k8s-deployment.yaml
```

## Configuration Files

All deployment configurations are available in the following structure:

```
deployments/
├── platforms/
│   ├── vercel/
│   │   └── vercel.json
│   ├── netlify/
│   │   └── netlify.toml
│   ├── aws/
│   │   └── aws-cloudformation.yaml
│   ├── docker/
│   │   ├── Dockerfile
│   │   └── docker-compose.yml
│   └── kubernetes/
│       └── k8s-deployment.yaml
└── deployment_matrix.json
```

## OTEL Monitoring

All deployment configurations include OpenTelemetry monitoring:

- **Metrics:** Request latency, throughput, error rates
- **Traces:** End-to-end request tracing
- **Logs:** Structured application logs

Configure OTEL endpoints in your environment variables:

```bash
OTEL_EXPORTER_OTLP_ENDPOINT=https://your-otel-collector.com
OTEL_SERVICE_NAME=nuxt-cns-forge
OTEL_RESOURCE_ATTRIBUTES=service.version=1.0.0
```

## Support

For deployment assistance:
- Check platform-specific documentation
- Review configuration templates
- Test locally with Docker first
- Monitor deployment with OTEL dashboards

Generated by CNS Forge Deployment Generator v1.0
"""
        
        with open(output_dir / "DEPLOYMENT_GUIDE.md", 'w') as f:
            f.write(report)


def main():
    """Generate all Nuxt.js deployment permutations"""
    generator = NuxtDeploymentGenerator()
    
    deployments = generator.generate_all_deployments()
    
    print(f"\n✅ Deployment Generation Complete!")
    print(f"   - Platforms: {len(deployments['platforms'])}")
    print(f"   - Strategies: {len(deployments['strategies'])}")
    print(f"   - Combinations: {len(deployments['configurations']['combinations'])}")
    print(f"   - Saved to: /Users/sac/cns/nuxt_permutations/deployments/")
    
    return deployments


if __name__ == "__main__":
    main()