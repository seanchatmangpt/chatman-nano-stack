#!/usr/bin/env python3
"""
BitActor Kubernetes Adversarial Security Validator

This script performs comprehensive adversarial testing of the BitActor K8s deployment
to identify security vulnerabilities, misconfigurations, and attack vectors.

CRITICAL: This tool is for defensive security testing only.
"""

import json
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import yaml

class Severity(Enum):
    CRITICAL = "CRITICAL"
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"
    INFO = "INFO"

@dataclass
class SecurityFinding:
    """Security finding with CVSS-like scoring"""
    id: str
    title: str
    description: str
    severity: Severity
    cvss_score: float
    attack_scenario: str
    remediation: str
    evidence: List[str]
    affected_components: List[str]

class AdversarialK8sValidator:
    """Comprehensive adversarial security validator for K8s deployments"""
    
    def __init__(self, k8s_path: str, namespace: str = "bitactor"):
        self.k8s_path = Path(k8s_path)
        self.namespace = namespace
        self.findings: List[SecurityFinding] = []
        self.terraform_path = self.k8s_path / "terraform"
        self.helm_path = self.k8s_path / "helm" / "bitactor"
        
    def run_comprehensive_audit(self) -> Dict:
        """Execute all adversarial security tests"""
        print("🎯 ADVERSARIAL K8S SECURITY VALIDATION - 80/20 CRITICAL AUDIT")
        print("=" * 70)
        
        # Phase 1: Terraform IaC Security Audit
        print("\n🔍 PHASE 1: TERRAFORM IAC ADVERSARIAL AUDIT")
        self._audit_terraform_security()
        
        # Phase 2: K8s Configuration Security
        print("\n🛡️  PHASE 2: K8S SECURITY CONFIGURATION AUDIT")
        self._audit_k8s_security()
        
        # Phase 3: Container Security Analysis
        print("\n📦 PHASE 3: CONTAINER SECURITY ANALYSIS")
        self._audit_container_security()
        
        # Phase 4: RBAC & Network Policy Testing
        print("\n🔐 PHASE 4: RBAC & NETWORK POLICY ADVERSARIAL TESTING")
        self._audit_rbac_network_security()
        
        # Phase 5: Privilege Escalation Testing
        print("\n⚡ PHASE 5: PRIVILEGE ESCALATION ATTACK VECTORS")
        self._audit_privilege_escalation()
        
        # Phase 6: Attack Surface Analysis
        print("\n🎯 PHASE 6: ATTACK SURFACE & LATERAL MOVEMENT")
        self._audit_attack_surface()
        
        return self._generate_report()
    
    def _audit_terraform_security(self):
        """Audit Terraform configurations for security issues"""
        
        # Check for hardcoded secrets
        self._check_hardcoded_secrets()
        
        # Analyze RBAC permissions
        self._analyze_rbac_permissions()
        
        # Validate network policies
        self._validate_network_policies()
        
        # Check resource limits and security contexts
        self._check_resource_security()
        
        # Analyze HPA and PDB configurations
        self._analyze_availability_configs()
    
    def _check_hardcoded_secrets(self):
        """Scan for hardcoded secrets and sensitive data"""
        sensitive_patterns = [
            (r'password\s*=\s*["\'][^"\']+["\']', "Hardcoded password"),
            (r'secret\s*=\s*["\'][^"\']+["\']', "Hardcoded secret"),
            (r'token\s*=\s*["\'][^"\']+["\']', "Hardcoded token"),
            (r'key\s*=\s*["\'][^"\']+["\']', "Hardcoded key"),
            (r'[A-Za-z0-9+/]{32,}={0,2}', "Base64 encoded data"),
        ]
        
        terraform_files = list(self.terraform_path.glob("*.tf"))
        helm_files = list(self.helm_path.rglob("*.yaml"))
        
        for file_path in terraform_files + helm_files:
            try:
                content = file_path.read_text()
                for pattern, desc in sensitive_patterns:
                    matches = re.findall(pattern, content, re.IGNORECASE)
                    if matches:
                        self.findings.append(SecurityFinding(
                            id=f"SECRET-{len(self.findings)+1}",
                            title=f"Potential {desc} in {file_path.name}",
                            description=f"Found potential sensitive data pattern in {file_path}",
                            severity=Severity.HIGH,
                            cvss_score=7.5,
                            attack_scenario="Attacker gains access to source code and extracts credentials",
                            remediation="Use Kubernetes secrets or external secret management",
                            evidence=[f"Pattern: {pattern}", f"File: {file_path}"],
                            affected_components=[str(file_path)]
                        ))
            except Exception as e:
                print(f"⚠️  Error scanning {file_path}: {e}")
    
    def _analyze_rbac_permissions(self):
        """Analyze RBAC for overprivileged access"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Check for cluster-admin or admin roles
            if "cluster-admin" in content or '"admin"' in content:
                self.findings.append(SecurityFinding(
                    id=f"RBAC-{len(self.findings)+1}",
                    title="Potentially overprivileged RBAC detected",
                    description="Cluster-admin or admin roles detected in RBAC configuration",
                    severity=Severity.HIGH,
                    cvss_score=8.1,
                    attack_scenario="Service account compromise leads to cluster-wide privilege escalation",
                    remediation="Use principle of least privilege with minimal required permissions",
                    evidence=["Found admin/cluster-admin references"],
                    affected_components=["RBAC", "ServiceAccount"]
                ))
            
            # Check for wildcard permissions
            wildcard_patterns = [r'resources\s*=\s*\[.*"\*".*\]', r'verbs\s*=\s*\[.*"\*".*\]']
            for pattern in wildcard_patterns:
                if re.search(pattern, content):
                    self.findings.append(SecurityFinding(
                        id=f"RBAC-{len(self.findings)+1}",
                        title="Wildcard permissions in RBAC",
                        description="RBAC configuration uses wildcard (*) permissions",
                        severity=Severity.MEDIUM,
                        cvss_score=6.5,
                        attack_scenario="Compromised service account gains excessive permissions",
                        remediation="Specify exact resources and verbs needed",
                        evidence=[f"Pattern: {pattern}"],
                        affected_components=["RBAC"]
                    ))
                    
        except Exception as e:
            print(f"⚠️  Error analyzing RBAC: {e}")
    
    def _validate_network_policies(self):
        """Validate network policy effectiveness"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Check if network policies exist
            if "kubernetes_network_policy" not in content:
                self.findings.append(SecurityFinding(
                    id=f"NETWORK-{len(self.findings)+1}",
                    title="No network policies defined",
                    description="Deployment lacks network segmentation controls",
                    severity=Severity.HIGH,
                    cvss_score=7.3,
                    attack_scenario="Pod compromise enables unrestricted lateral movement",
                    remediation="Implement least-privilege network policies",
                    evidence=["No NetworkPolicy resources found"],
                    affected_components=["Network"]
                ))
            else:
                # Check for overly permissive network policies
                if re.search(r'from\s*{\s*}', content):  # Empty from block = allow all
                    self.findings.append(SecurityFinding(
                        id=f"NETWORK-{len(self.findings)+1}",
                        title="Overly permissive network policy",
                        description="Network policy allows traffic from all sources",
                        severity=Severity.MEDIUM,
                        cvss_score=5.4,
                        attack_scenario="Compromised pod can receive traffic from any source",
                        remediation="Restrict ingress to specific sources only",
                        evidence=["Empty 'from' block in NetworkPolicy"],
                        affected_components=["NetworkPolicy"]
                    ))
                    
        except Exception as e:
            print(f"⚠️  Error validating network policies: {e}")
    
    def _check_resource_security(self):
        """Check resource limits and security contexts"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            # Check security context
            security_context = values.get('securityContext', {})
            pod_security_context = values.get('podSecurityContext', {})
            
            # Critical security checks
            security_issues = []
            
            if not security_context.get('runAsNonRoot', False):
                security_issues.append("Container not configured to run as non-root")
            
            if security_context.get('allowPrivilegeEscalation', True):
                security_issues.append("Privilege escalation not disabled")
            
            if not security_context.get('readOnlyRootFilesystem', False):
                security_issues.append("Root filesystem not read-only")
                
            if 'ALL' not in security_context.get('capabilities', {}).get('drop', []):
                security_issues.append("Not all capabilities dropped")
            
            if security_issues:
                self.findings.append(SecurityFinding(
                    id=f"SECURITY-{len(self.findings)+1}",
                    title="Insecure container security context",
                    description=f"Security context has {len(security_issues)} issues",
                    severity=Severity.HIGH,
                    cvss_score=7.8,
                    attack_scenario="Container escape or privilege escalation through insecure context",
                    remediation="Implement secure security context with least privilege",
                    evidence=security_issues,
                    affected_components=["SecurityContext", "Pod"]
                ))
            
            # Check resource limits
            resources = values.get('resources', {})
            if not resources.get('limits'):
                self.findings.append(SecurityFinding(
                    id=f"RESOURCE-{len(self.findings)+1}",
                    title="No resource limits defined",
                    description="Container has no CPU or memory limits",
                    severity=Severity.MEDIUM,
                    cvss_score=5.9,
                    attack_scenario="Resource exhaustion attack or noisy neighbor issues",
                    remediation="Set appropriate CPU and memory limits",
                    evidence=["No resource limits in values.yaml"],
                    affected_components=["Resources"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking resource security: {e}")
    
    def _analyze_availability_configs(self):
        """Analyze HPA and PDB for availability attacks"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Check HPA configuration
            hpa_match = re.search(r'min_replicas\s*=\s*(\d+)', content)
            if hpa_match:
                min_replicas = int(hpa_match.group(1))
                if min_replicas < 2:
                    self.findings.append(SecurityFinding(
                        id=f"AVAILABILITY-{len(self.findings)+1}",
                        title="Low minimum replica count in HPA",
                        description=f"HPA configured with only {min_replicas} minimum replicas",
                        severity=Severity.MEDIUM,
                        cvss_score=4.3,
                        attack_scenario="Single point of failure enables easy DoS attacks",
                        remediation="Set minimum replicas to at least 2 for high availability",
                        evidence=[f"min_replicas = {min_replicas}"],
                        affected_components=["HPA"]
                    ))
            
            # Check PDB configuration
            if "kubernetes_pod_disruption_budget" not in content:
                self.findings.append(SecurityFinding(
                    id=f"AVAILABILITY-{len(self.findings)+1}",
                    title="No Pod Disruption Budget configured",
                    description="No PDB to protect against voluntary disruptions",
                    severity=Severity.LOW,
                    cvss_score=3.1,
                    attack_scenario="Cluster maintenance causes complete service outage",
                    remediation="Configure appropriate Pod Disruption Budget",
                    evidence=["No PodDisruptionBudget resource found"],
                    affected_components=["PDB"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error analyzing availability configs: {e}")
    
    def _audit_k8s_security(self):
        """Audit Kubernetes-specific security configurations"""
        
        # Check admission controllers
        self._check_admission_controllers()
        
        # Validate pod security policies/standards
        self._validate_pod_security()
        
        # Check service mesh configuration
        self._check_service_mesh()
    
    def _check_admission_controllers(self):
        """Check for security admission controllers"""
        # This would typically query the cluster, but for static analysis
        # we check if security policies are referenced
        
        try:
            values_file = self.helm_path / "values.yaml"
            content = values_file.read_text()
            
            if "seccompProfile" not in content:
                self.findings.append(SecurityFinding(
                    id=f"ADMISSION-{len(self.findings)+1}",
                    title="No seccomp profile configured",
                    description="Container lacks seccomp security profile",
                    severity=Severity.MEDIUM,
                    cvss_score=5.4,
                    attack_scenario="Container can make unrestricted system calls",
                    remediation="Configure appropriate seccomp profile",
                    evidence=["No seccompProfile in pod spec"],
                    affected_components=["Pod", "SecurityProfile"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking admission controllers: {e}")
    
    def _validate_pod_security(self):
        """Validate pod security standards compliance"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            # Check for Pod Security Standards compliance
            pod_security = values.get('podSecurityContext', {})
            
            pss_violations = []
            
            # Restricted profile requirements
            if pod_security.get('runAsUser', 0) == 0:
                pss_violations.append("Running as root user (UID 0)")
            
            if not pod_security.get('runAsNonRoot', False):
                pss_violations.append("Not enforcing non-root execution")
            
            if pod_security.get('fsGroup', 0) == 0:
                pss_violations.append("Using root group (GID 0)")
            
            if pss_violations:
                self.findings.append(SecurityFinding(
                    id=f"PSS-{len(self.findings)+1}",
                    title="Pod Security Standards violations",
                    description=f"Pod violates {len(pss_violations)} PSS requirements",
                    severity=Severity.HIGH,
                    cvss_score=7.2,
                    attack_scenario="Pod security violations enable container escape",
                    remediation="Align with Pod Security Standards 'restricted' profile",
                    evidence=pss_violations,
                    affected_components=["PodSecurityContext"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error validating pod security: {e}")
    
    def _check_service_mesh(self):
        """Check service mesh security configuration"""
        # Look for Istio, Linkerd, or other service mesh configurations
        try:
            helm_files = list(self.helm_path.rglob("*.yaml"))
            has_service_mesh = False
            
            for file_path in helm_files:
                content = file_path.read_text()
                if any(mesh in content for mesh in ['istio', 'linkerd', 'consul', 'envoy']):
                    has_service_mesh = True
                    break
            
            if not has_service_mesh:
                self.findings.append(SecurityFinding(
                    id=f"MESH-{len(self.findings)+1}",
                    title="No service mesh detected",
                    description="No service mesh for enhanced security and observability",
                    severity=Severity.LOW,
                    cvss_score=3.7,
                    attack_scenario="Limited observability and control over service communication",
                    remediation="Consider implementing service mesh for mTLS and traffic control",
                    evidence=["No service mesh annotations or configurations found"],
                    affected_components=["ServiceMesh"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking service mesh: {e}")
    
    def _audit_container_security(self):
        """Audit container and image security"""
        
        # Analyze Dockerfile security
        self._analyze_dockerfile_security()
        
        # Check image security practices
        self._check_image_security()
        
        # Validate runtime security
        self._validate_runtime_security()
    
    def _analyze_dockerfile_security(self):
        """Analyze Dockerfile for security issues"""
        try:
            dockerfile = self.k8s_path / "Dockerfile"
            if not dockerfile.exists():
                return
                
            content = dockerfile.read_text()
            lines = content.split('\n')
            
            security_issues = []
            
            # Check for security anti-patterns
            for i, line in enumerate(lines, 1):
                line = line.strip()
                
                if line.startswith('RUN') and 'apt-get update' in line and '&&' not in line:
                    security_issues.append(f"Line {i}: Uncombined apt-get update (cache poisoning risk)")
                
                if line.startswith('RUN') and 'curl' in line and 'http://' in line:
                    security_issues.append(f"Line {i}: Insecure HTTP download")
                
                if line.startswith('USER') and ('root' in line or '0' in line):
                    security_issues.append(f"Line {i}: Explicitly running as root")
                
                if line.startswith('COPY') and '--chown=' not in line and 'USER' not in content:
                    security_issues.append(f"Line {i}: Files copied without proper ownership")
            
            # Check for non-root user
            if 'USER' not in content or content.count('USER root') > 0:
                security_issues.append("No non-root user specified")
            
            # Check for HEALTHCHECK
            if 'HEALTHCHECK' not in content:
                security_issues.append("No health check defined")
            
            if security_issues:
                self.findings.append(SecurityFinding(
                    id=f"DOCKERFILE-{len(self.findings)+1}",
                    title="Dockerfile security issues",
                    description=f"Dockerfile has {len(security_issues)} security concerns",
                    severity=Severity.MEDIUM,
                    cvss_score=6.1,
                    attack_scenario="Container vulnerabilities enable privilege escalation",
                    remediation="Follow Dockerfile security best practices",
                    evidence=security_issues,
                    affected_components=["Dockerfile", "Container"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error analyzing Dockerfile: {e}")
    
    def _check_image_security(self):
        """Check container image security practices"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            image_config = values.get('image', {})
            
            # Check image tag
            tag = image_config.get('tag', 'latest')
            if tag == 'latest':
                self.findings.append(SecurityFinding(
                    id=f"IMAGE-{len(self.findings)+1}",
                    title="Using 'latest' image tag",
                    description="Container image uses mutable 'latest' tag",
                    severity=Severity.MEDIUM,
                    cvss_score=5.3,
                    attack_scenario="Image tag mutation leads to deployment of malicious images",
                    remediation="Use specific version tags or SHA256 digests",
                    evidence=[f"Image tag: {tag}"],
                    affected_components=["Image"]
                ))
            
            # Check pull policy
            pull_policy = image_config.get('pullPolicy', 'Always')
            if pull_policy != 'Always' and tag == 'latest':
                self.findings.append(SecurityFinding(
                    id=f"IMAGE-{len(self.findings)+1}",
                    title="Inconsistent image pull policy",
                    description="Using 'latest' tag without 'Always' pull policy",
                    severity=Severity.LOW,
                    cvss_score=4.1,
                    attack_scenario="Stale vulnerable images may be used",
                    remediation="Use 'Always' pull policy with 'latest' tag",
                    evidence=[f"Pull policy: {pull_policy}, Tag: {tag}"],
                    affected_components=["Image"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking image security: {e}")
    
    def _validate_runtime_security(self):
        """Validate runtime security configurations"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            # Check for runtime security tools
            annotations = values.get('podAnnotations', {})
            
            runtime_security_tools = [
                'falco', 'twistlock', 'aqua', 'sysdig', 'nbs.runtimesecurity'
            ]
            
            has_runtime_security = any(
                tool in str(annotations).lower() 
                for tool in runtime_security_tools
            )
            
            if not has_runtime_security:
                self.findings.append(SecurityFinding(
                    id=f"RUNTIME-{len(self.findings)+1}",
                    title="No runtime security monitoring",
                    description="No runtime security tools detected",
                    severity=Severity.MEDIUM,
                    cvss_score=5.8,
                    attack_scenario="Runtime attacks go undetected",
                    remediation="Deploy runtime security monitoring (Falco, Twistlock, etc.)",
                    evidence=["No runtime security annotations found"],
                    affected_components=["Runtime"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error validating runtime security: {e}")
    
    def _audit_rbac_network_security(self):
        """Comprehensive RBAC and network security audit"""
        
        # Test service account token security
        self._test_service_account_tokens()
        
        # Test network policy bypasses
        self._test_network_policy_bypasses()
        
        # Check cluster role bindings
        self._check_cluster_role_bindings()
    
    def _test_service_account_tokens(self):
        """Test service account token security"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Check if service account auto-mounts tokens
            if "automountServiceAccountToken" not in content:
                self.findings.append(SecurityFinding(
                    id=f"SA-TOKEN-{len(self.findings)+1}",
                    title="Service account token auto-mount not disabled",
                    description="Service account tokens automatically mounted in pods",
                    severity=Severity.MEDIUM,
                    cvss_score=6.2,
                    attack_scenario="Compromised pod can use service account tokens for lateral movement",
                    remediation="Set automountServiceAccountToken: false if not needed",
                    evidence=["No automountServiceAccountToken configuration found"],
                    affected_components=["ServiceAccount"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error testing service account tokens: {e}")
    
    def _test_network_policy_bypasses(self):
        """Test for network policy bypass opportunities"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Check for DNS policy gaps
            dns_egress = re.search(r'ports\s*{\s*port\s*=\s*"53"', content)
            if dns_egress:
                # Check if DNS egress is overly permissive
                if re.search(r'to\s*{\s*}\s*ports\s*{\s*port\s*=\s*"53"', content):
                    self.findings.append(SecurityFinding(
                        id=f"NETPOL-{len(self.findings)+1}",
                        title="Overly permissive DNS egress",
                        description="Network policy allows DNS queries to any destination",
                        severity=Severity.LOW,
                        cvss_score=4.4,
                        attack_scenario="DNS tunneling for data exfiltration",
                        remediation="Restrict DNS egress to specific nameservers",
                        evidence=["Unrestricted DNS egress in NetworkPolicy"],
                        affected_components=["NetworkPolicy"]
                    ))
                    
        except Exception as e:
            print(f"⚠️  Error testing network policy bypasses: {e}")
    
    def _check_cluster_role_bindings(self):
        """Check for dangerous cluster role bindings"""
        try:
            terraform_main = self.terraform_path / "main.tf"
            content = terraform_main.read_text()
            
            # Look for cluster role bindings (should only be role bindings)
            if "kubernetes_cluster_role_binding" in content:
                self.findings.append(SecurityFinding(
                    id=f"CLUSTERRB-{len(self.findings)+1}",
                    title="Cluster role binding detected",
                    description="Service uses cluster-wide permissions",
                    severity=Severity.HIGH,
                    cvss_score=7.8,
                    attack_scenario="Service account compromise leads to cluster-wide access",
                    remediation="Use namespace-scoped role bindings instead",
                    evidence=["ClusterRoleBinding resource found"],
                    affected_components=["RBAC"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking cluster role bindings: {e}")
    
    def _audit_privilege_escalation(self):
        """Test for privilege escalation attack vectors"""
        
        # Check container capabilities
        self._check_container_capabilities()
        
        # Test volume mount security
        self._test_volume_mounts()
        
        # Check privileged contexts
        self._check_privileged_contexts()
    
    def _check_container_capabilities(self):
        """Check container Linux capabilities"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            security_context = values.get('securityContext', {})
            capabilities = security_context.get('capabilities', {})
            
            # Check for dangerous capabilities
            add_caps = capabilities.get('add', [])
            dangerous_caps = ['SYS_ADMIN', 'SYS_PTRACE', 'SYS_MODULE', 'DAC_OVERRIDE', 'SETUID', 'SETGID']
            
            found_dangerous = [cap for cap in add_caps if cap in dangerous_caps]
            
            if found_dangerous:
                self.findings.append(SecurityFinding(
                    id=f"CAPS-{len(self.findings)+1}",
                    title="Dangerous container capabilities",
                    description=f"Container has dangerous capabilities: {found_dangerous}",
                    severity=Severity.HIGH,
                    cvss_score=8.2,
                    attack_scenario="Dangerous capabilities enable container escape",
                    remediation="Remove unnecessary capabilities, use minimal set",
                    evidence=[f"Capabilities: {found_dangerous}"],
                    affected_components=["Capabilities"]
                ))
            
            # Check if ALL capabilities are dropped
            drop_caps = capabilities.get('drop', [])
            if 'ALL' not in drop_caps:
                self.findings.append(SecurityFinding(
                    id=f"CAPS-{len(self.findings)+1}",
                    title="Not all capabilities dropped",
                    description="Container retains unnecessary Linux capabilities",
                    severity=Severity.MEDIUM,
                    cvss_score=6.4,
                    attack_scenario="Retained capabilities may enable privilege escalation",
                    remediation="Drop ALL capabilities by default, add only what's needed",
                    evidence=[f"Drop capabilities: {drop_caps}"],
                    affected_components=["Capabilities"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking container capabilities: {e}")
    
    def _test_volume_mounts(self):
        """Test volume mount security"""
        try:
            deployment_file = self.helm_path / "templates" / "deployment.yaml"
            content = deployment_file.read_text()
            
            # Check for dangerous volume mounts
            dangerous_mounts = [
                ('/var/run/docker.sock', 'Docker socket mount'),
                ('/proc', 'Host proc filesystem'),
                ('/sys', 'Host sys filesystem'),
                ('/dev', 'Host device filesystem'),
                ('/host', 'Host root filesystem'),
                ('/etc/passwd', 'Host passwd file'),
                ('/etc/shadow', 'Host shadow file')
            ]
            
            for mount_path, description in dangerous_mounts:
                if mount_path in content:
                    self.findings.append(SecurityFinding(
                        id=f"VOLUME-{len(self.findings)+1}",
                        title=f"Dangerous volume mount: {description}",
                        description=f"Container mounts dangerous host path: {mount_path}",
                        severity=Severity.CRITICAL,
                        cvss_score=9.1,
                        attack_scenario="Volume mount enables container escape to host",
                        remediation="Remove dangerous volume mounts, use specific paths only",
                        evidence=[f"Mount path: {mount_path}"],
                        affected_components=["VolumeMount"]
                    ))
            
            # Check for writable host mounts
            if re.search(r'readOnly:\s*false', content) and '/host' in content:
                self.findings.append(SecurityFinding(
                    id=f"VOLUME-{len(self.findings)+1}",
                    title="Writable host filesystem mount",
                    description="Container has writable access to host filesystem",
                    severity=Severity.CRITICAL,
                    cvss_score=9.3,
                    attack_scenario="Writable host mount enables host compromise",
                    remediation="Make host mounts read-only",
                    evidence=["readOnly: false with host mount"],
                    affected_components=["VolumeMount"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error testing volume mounts: {e}")
    
    def _check_privileged_contexts(self):
        """Check for privileged security contexts"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            security_context = values.get('securityContext', {})
            pod_security_context = values.get('podSecurityContext', {})
            
            # Check for privileged execution
            if security_context.get('privileged', False):
                self.findings.append(SecurityFinding(
                    id=f"PRIV-{len(self.findings)+1}",
                    title="Privileged container detected",
                    description="Container runs in privileged mode",
                    severity=Severity.CRITICAL,
                    cvss_score=9.8,
                    attack_scenario="Privileged container has full host access",
                    remediation="Remove privileged: true, use specific capabilities instead",
                    evidence=["privileged: true"],
                    affected_components=["SecurityContext"]
                ))
            
            # Check for host network
            if pod_security_context.get('hostNetwork', False):
                self.findings.append(SecurityFinding(
                    id=f"PRIV-{len(self.findings)+1}",
                    title="Host network access",
                    description="Pod uses host network namespace",
                    severity=Severity.HIGH,
                    cvss_score=8.0,
                    attack_scenario="Host network access enables network-based attacks",
                    remediation="Remove hostNetwork: true",
                    evidence=["hostNetwork: true"],
                    affected_components=["PodSecurityContext"]
                ))
            
            # Check for host PID
            if pod_security_context.get('hostPID', False):
                self.findings.append(SecurityFinding(
                    id=f"PRIV-{len(self.findings)+1}",
                    title="Host PID namespace access",
                    description="Pod uses host PID namespace",
                    severity=Severity.HIGH,
                    cvss_score=7.9,
                    attack_scenario="Host PID access enables process manipulation",
                    remediation="Remove hostPID: true",
                    evidence=["hostPID: true"],
                    affected_components=["PodSecurityContext"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking privileged contexts: {e}")
    
    def _audit_attack_surface(self):
        """Analyze attack surface and lateral movement opportunities"""
        
        # Check exposed services
        self._check_exposed_services()
        
        # Test ingress security
        self._test_ingress_security()
        
        # Check monitoring exposure
        self._check_monitoring_exposure()
    
    def _check_exposed_services(self):
        """Check for exposed services and ports"""
        try:
            service_file = self.helm_path / "templates" / "service.yaml"
            content = service_file.read_text()
            
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            service_type = values.get('service', {}).get('type', 'ClusterIP')
            
            # Check for external exposure
            if service_type in ['NodePort', 'LoadBalancer']:
                self.findings.append(SecurityFinding(
                    id=f"EXPOSURE-{len(self.findings)+1}",
                    title=f"Service externally exposed via {service_type}",
                    description=f"Service uses {service_type} for external access",
                    severity=Severity.MEDIUM,
                    cvss_score=5.7,
                    attack_scenario="External exposure increases attack surface",
                    remediation="Use ClusterIP with Ingress controller for controlled access",
                    evidence=[f"Service type: {service_type}"],
                    affected_components=["Service"]
                ))
            
            # Check for multiple ports
            port_count = content.count('port:')
            if port_count > 2:  # More than standard HTTP + health
                self.findings.append(SecurityFinding(
                    id=f"EXPOSURE-{len(self.findings)+1}",
                    title="Multiple ports exposed",
                    description=f"Service exposes {port_count} ports",
                    severity=Severity.LOW,
                    cvss_score=3.8,
                    attack_scenario="Multiple ports increase attack surface",
                    remediation="Expose only necessary ports",
                    evidence=[f"Port count: {port_count}"],
                    affected_components=["Service"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking exposed services: {e}")
    
    def _test_ingress_security(self):
        """Test ingress controller security"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            ingress = values.get('ingress', {})
            
            if ingress.get('enabled', False):
                # Check TLS configuration
                tls_config = ingress.get('tls', [])
                if not tls_config:
                    self.findings.append(SecurityFinding(
                        id=f"INGRESS-{len(self.findings)+1}",
                        title="Ingress without TLS",
                        description="Ingress exposed without TLS encryption",
                        severity=Severity.HIGH,
                        cvss_score=7.4,
                        attack_scenario="Unencrypted traffic enables man-in-the-middle attacks",
                        remediation="Configure TLS certificates for ingress",
                        evidence=["No TLS configuration in ingress"],
                        affected_components=["Ingress"]
                    ))
                
                # Check for security annotations
                annotations = ingress.get('annotations', {})
                security_annotations = [
                    'nginx.ingress.kubernetes.io/ssl-redirect',
                    'nginx.ingress.kubernetes.io/force-ssl-redirect',
                    'nginx.ingress.kubernetes.io/secure-backends'
                ]
                
                missing_security = [
                    ann for ann in security_annotations 
                    if ann not in annotations
                ]
                
                if missing_security:
                    self.findings.append(SecurityFinding(
                        id=f"INGRESS-{len(self.findings)+1}",
                        title="Missing security annotations in ingress",
                        description=f"Ingress lacks {len(missing_security)} security annotations",
                        severity=Severity.MEDIUM,
                        cvss_score=5.6,
                        attack_scenario="Missing security headers enable various attacks",
                        remediation="Add security annotations to ingress",
                        evidence=missing_security,
                        affected_components=["Ingress"]
                    ))
                    
        except Exception as e:
            print(f"⚠️  Error testing ingress security: {e}")
    
    def _check_monitoring_exposure(self):
        """Check monitoring and metrics exposure"""
        try:
            values_file = self.helm_path / "values.yaml"
            with open(values_file) as f:
                values = yaml.safe_load(f)
            
            # Check Prometheus annotations
            pod_annotations = values.get('podAnnotations', {})
            
            if 'prometheus.io/scrape' in pod_annotations:
                # Check if metrics are exposed without authentication
                self.findings.append(SecurityFinding(
                    id=f"METRICS-{len(self.findings)+1}",
                    title="Metrics exposed without authentication",
                    description="Prometheus metrics exposed without access control",
                    severity=Severity.LOW,
                    cvss_score=4.2,
                    attack_scenario="Exposed metrics reveal system information",
                    remediation="Implement authentication for metrics endpoints",
                    evidence=["prometheus.io/scrape annotation present"],
                    affected_components=["Metrics"]
                ))
                
        except Exception as e:
            print(f"⚠️  Error checking monitoring exposure: {e}")
    
    def _generate_report(self) -> Dict:
        """Generate comprehensive adversarial validation report"""
        
        # Sort findings by severity and CVSS score
        self.findings.sort(key=lambda x: (x.severity.value, -x.cvss_score))
        
        # Generate severity statistics
        severity_stats = {}
        for severity in Severity:
            count = len([f for f in self.findings if f.severity == severity])
            severity_stats[severity.value] = count
        
        # Calculate risk score
        risk_score = sum(f.cvss_score for f in self.findings) / len(self.findings) if self.findings else 0
        
        # Generate 80/20 prioritized remediation
        critical_high_findings = [
            f for f in self.findings 
            if f.severity in [Severity.CRITICAL, Severity.HIGH]
        ]
        
        report = {
            "metadata": {
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
                "validator_version": "1.0.0",
                "target": f"BitActor K8s Deployment ({self.namespace})",
                "methodology": "80/20 Adversarial Security Validation"
            },
            "executive_summary": {
                "total_findings": len(self.findings),
                "severity_breakdown": severity_stats,
                "average_risk_score": round(risk_score, 2),
                "critical_issues": len([f for f in self.findings if f.severity == Severity.CRITICAL]),
                "high_priority_fixes": len(critical_high_findings),
                "overall_risk_level": self._calculate_risk_level(risk_score)
            },
            "findings": [
                {
                    "id": f.id,
                    "title": f.title,
                    "description": f.description,
                    "severity": f.severity.value,
                    "cvss_score": f.cvss_score,
                    "attack_scenario": f.attack_scenario,
                    "remediation": f.remediation,
                    "evidence": f.evidence,
                    "affected_components": f.affected_components
                }
                for f in self.findings
            ],
            "80_20_priorities": {
                "critical_path": [
                    {
                        "priority": i + 1,
                        "finding_id": f.id,
                        "title": f.title,
                        "impact": "80% risk reduction" if i < 3 else "20% risk reduction"
                    }
                    for i, f in enumerate(critical_high_findings[:10])
                ],
                "quick_wins": [
                    f.id for f in self.findings 
                    if f.severity == Severity.MEDIUM and f.cvss_score > 5.0
                ][:5]
            },
            "attack_vectors": self._generate_attack_vectors(),
            "compliance_status": self._assess_compliance(),
            "recommendations": self._generate_recommendations()
        }
        
        return report
    
    def _calculate_risk_level(self, risk_score: float) -> str:
        """Calculate overall risk level"""
        if risk_score >= 8.0:
            return "CRITICAL"
        elif risk_score >= 6.0:
            return "HIGH"
        elif risk_score >= 4.0:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _generate_attack_vectors(self) -> List[Dict]:
        """Generate potential attack vector scenarios"""
        return [
            {
                "name": "Container Escape",
                "likelihood": "MEDIUM",
                "impact": "HIGH",
                "steps": [
                    "Exploit container misconfiguration",
                    "Escalate privileges within container",
                    "Break out of container to host",
                    "Gain host-level access"
                ]
            },
            {
                "name": "Lateral Movement",
                "likelihood": "HIGH",
                "impact": "MEDIUM",
                "steps": [
                    "Compromise pod via application vulnerability",
                    "Abuse overprivileged service account",
                    "Access other pods in namespace",
                    "Pivot to other namespaces"
                ]
            },
            {
                "name": "Data Exfiltration",
                "likelihood": "MEDIUM",
                "impact": "HIGH",
                "steps": [
                    "Gain access to pod",
                    "Exploit network policy gaps",
                    "Access sensitive data sources",
                    "Exfiltrate via DNS tunneling"
                ]
            }
        ]
    
    def _assess_compliance(self) -> Dict:
        """Assess compliance with security standards"""
        standards = {
            "CIS_Kubernetes": {
                "score": max(0, 100 - len([f for f in self.findings if f.severity in [Severity.CRITICAL, Severity.HIGH]]) * 10),
                "critical_controls": [
                    "RBAC enabled",
                    "Network policies configured", 
                    "Pod security contexts enforced",
                    "Resource limits set"
                ]
            },
            "NIST_800_190": {
                "score": max(0, 100 - len([f for f in self.findings if 'container' in f.title.lower()]) * 15),
                "requirements": [
                    "Container image security",
                    "Runtime protection",
                    "Host OS security"
                ]
            },
            "Pod_Security_Standards": {
                "score": max(0, 100 - len([f for f in self.findings if f.severity == Severity.CRITICAL]) * 20),
                "profile": "Restricted" if len([f for f in self.findings if f.severity == Severity.CRITICAL]) == 0 else "Baseline"
            }
        }
        
        return standards
    
    def _generate_recommendations(self) -> List[str]:
        """Generate prioritized recommendations"""
        recommendations = [
            "Implement Pod Security Standards 'restricted' profile",
            "Enable runtime security monitoring (Falco/Twistlock)",
            "Implement service mesh for mTLS and traffic control",
            "Configure admission controllers for policy enforcement",
            "Implement secrets management solution",
            "Enable comprehensive audit logging",
            "Regular vulnerability scanning of images",
            "Network segmentation with strict policies",
            "Implement least-privilege RBAC",
            "Monitor and alert on security events"
        ]
        
        return recommendations[:5]  # Top 5 recommendations

def main():
    """Main execution function"""
    if len(sys.argv) < 2:
        print("Usage: python3 adversarial_k8s_security_validator.py <k8s_path> [namespace]")
        sys.exit(1)
    
    k8s_path = sys.argv[1]
    namespace = sys.argv[2] if len(sys.argv) > 2 else "bitactor"
    
    validator = AdversarialK8sValidator(k8s_path, namespace)
    report = validator.run_comprehensive_audit()
    
    # Save report
    report_file = Path(k8s_path) / "adversarial_security_report.json"
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    # Print summary
    print(f"\n📊 ADVERSARIAL VALIDATION COMPLETE")
    print(f"Report saved to: {report_file}")
    print(f"Total findings: {report['executive_summary']['total_findings']}")
    print(f"Risk level: {report['executive_summary']['overall_risk_level']}")
    print(f"Critical issues: {report['executive_summary']['critical_issues']}")
    
    # Print top 3 critical findings
    critical_findings = [f for f in validator.findings if f.severity == Severity.CRITICAL][:3]
    if critical_findings:
        print(f"\n🚨 TOP CRITICAL ISSUES:")
        for i, finding in enumerate(critical_findings, 1):
            print(f"  {i}. {finding.title} (CVSS: {finding.cvss_score})")
    
    return len(validator.findings)

if __name__ == "__main__":
    sys.exit(main())