#!/usr/bin/env python3
"""
Kubernetes Adversarial Penetration Testing Suite
Tests RBAC bypass, privilege escalation, container escape, and security policy violations
"""

import json
import yaml
import re
import time
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
import tempfile
import subprocess
from dataclasses import dataclass

@dataclass
class SecurityVulnerability:
    """Security vulnerability representation"""
    severity: str  # critical, high, medium, low
    category: str
    description: str
    affected_resource: str
    attack_vector: str
    mitigation: str
    cve_reference: Optional[str] = None

class K8sAdversarialPenetrationTester:
    """Comprehensive K8s adversarial penetration testing framework"""
    
    def __init__(self):
        self.results = {}
        self.vulnerabilities = []
        self.terraform_dir = Path(__file__).parent / "terraform"
        self.attack_patterns = self._load_attack_patterns()
        
    def _load_attack_patterns(self) -> Dict[str, Any]:
        """Load known K8s attack patterns and exploits"""
        return {
            'rbac_bypass': [
                {
                    'name': 'Wildcard Permission Abuse',
                    'pattern': r'\*',
                    'description': 'Wildcard permissions allow unrestricted access',
                    'severity': 'critical'
                },
                {
                    'name': 'Overprivileged Service Account',
                    'verbs': ['*', 'create', 'delete', 'patch'],
                    'resources': ['*', 'secrets', 'configmaps', 'pods'],
                    'description': 'Service account has excessive permissions',
                    'severity': 'high'
                },
                {
                    'name': 'Cluster Admin Binding',
                    'role_name': 'cluster-admin',
                    'description': 'Direct cluster-admin role binding creates god-mode access',
                    'severity': 'critical'
                }
            ],
            'privilege_escalation': [
                {
                    'name': 'Privileged Container',
                    'security_context': {'privileged': True},
                    'description': 'Privileged containers can escape to host',
                    'severity': 'critical'
                },
                {
                    'name': 'Host PID Namespace',
                    'security_context': {'hostPID': True},
                    'description': 'Host PID access enables process manipulation',
                    'severity': 'high'
                },
                {
                    'name': 'Host Network',
                    'security_context': {'hostNetwork': True},
                    'description': 'Host network access bypasses network policies',
                    'severity': 'high'
                },
                {
                    'name': 'Root User',
                    'security_context': {'runAsUser': 0},
                    'description': 'Running as root increases attack surface',
                    'severity': 'medium'
                }
            ],
            'container_escape': [
                {
                    'name': 'Host Path Mount',
                    'volume_type': 'hostPath',
                    'dangerous_paths': ['/var/run/docker.sock', '/proc', '/sys', '/dev', '/'],
                    'description': 'Host path mounts enable container escape',
                    'severity': 'critical'
                },
                {
                    'name': 'Capabilities Abuse',
                    'dangerous_caps': ['SYS_ADMIN', 'SYS_PTRACE', 'SYS_MODULE', 'DAC_READ_SEARCH'],
                    'description': 'Dangerous capabilities enable privilege escalation',
                    'severity': 'high'
                }
            ],
            'network_policy_bypass': [
                {
                    'name': 'Missing Network Policy',
                    'description': 'No network policies allow unrestricted pod communication',
                    'severity': 'medium'
                },
                {
                    'name': 'Overly Permissive Network Policy',
                    'selectors': ['{}', 'matchLabels: {}'],
                    'description': 'Empty selectors allow access to all pods/namespaces',
                    'severity': 'high'
                }
            ]
        }

    def analyze_rbac_configuration(self) -> Dict[str, Any]:
        """Analyze RBAC configuration for privilege escalation vulnerabilities"""
        print("🔍 ADVERSARIAL TEST: Analyzing RBAC configuration...")
        
        rbac_issues = []
        
        # Analyze terraform files for RBAC resources
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Check for cluster role bindings
            cluster_role_matches = re.findall(
                r'resource\s+"kubernetes_cluster_role"\s+"([^"]+)"\s*{([^}]+)}', 
                content, re.MULTILINE | re.DOTALL
            )
            
            for role_name, role_config in cluster_role_matches:
                # Check for wildcard permissions
                if '*' in role_config:
                    rbac_issues.append(SecurityVulnerability(
                        severity='critical',
                        category='rbac_bypass',
                        description=f'Cluster role {role_name} uses wildcard permissions',
                        affected_resource=f'kubernetes_cluster_role.{role_name}',
                        attack_vector='Wildcard RBAC permissions allow unrestricted cluster access',
                        mitigation='Replace wildcard permissions with specific resource and verb grants'
                    ))
                
                # Check for dangerous verbs
                dangerous_verbs = ['create', 'delete', 'patch', 'update']
                dangerous_resources = ['secrets', 'serviceaccounts', 'roles', 'rolebindings']
                
                for verb in dangerous_verbs:
                    if f'"{verb}"' in role_config:
                        for resource in dangerous_resources:
                            if f'"{resource}"' in role_config:
                                rbac_issues.append(SecurityVulnerability(
                                    severity='high',
                                    category='privilege_escalation',
                                    description=f'Role {role_name} can {verb} {resource}',
                                    affected_resource=f'kubernetes_cluster_role.{role_name}',
                                    attack_vector=f'Can {verb} {resource} to escalate privileges',
                                    mitigation=f'Restrict {verb} permissions on {resource}'
                                ))
            
            # Check role bindings
            role_binding_matches = re.findall(
                r'resource\s+"kubernetes_role_binding"\s+"([^"]+)"\s*{([^}]+)}',
                content, re.MULTILINE | re.DOTALL
            )
            
            for binding_name, binding_config in role_binding_matches:
                # Check for system:masters or cluster-admin bindings
                if 'cluster-admin' in binding_config or 'system:masters' in binding_config:
                    rbac_issues.append(SecurityVulnerability(
                        severity='critical',
                        category='rbac_bypass',
                        description=f'Role binding {binding_name} grants cluster-admin access',
                        affected_resource=f'kubernetes_role_binding.{binding_name}',
                        attack_vector='Cluster admin access provides unrestricted cluster control',
                        mitigation='Use least-privilege principle with specific role permissions'
                    ))
        
        self.vulnerabilities.extend(rbac_issues)
        
        return {
            'rbac_analysis': {
                'total_issues': len(rbac_issues),
                'critical_issues': len([v for v in rbac_issues if v.severity == 'critical']),
                'high_issues': len([v for v in rbac_issues if v.severity == 'high']),
                'vulnerabilities': [
                    {
                        'severity': v.severity,
                        'category': v.category,
                        'description': v.description,
                        'resource': v.affected_resource,
                        'attack_vector': v.attack_vector,
                        'mitigation': v.mitigation
                    } for v in rbac_issues
                ]
            }
        }

    def test_container_security_contexts(self) -> Dict[str, Any]:
        """Test for container escape vulnerabilities"""
        print("🔍 ADVERSARIAL TEST: Analyzing container security contexts...")
        
        container_issues = []
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Check for privileged containers
            if 'privileged' in content and 'true' in content:
                container_issues.append(SecurityVulnerability(
                    severity='critical',
                    category='container_escape',
                    description='Privileged container detected',
                    affected_resource='container_security_context',
                    attack_vector='Privileged containers can escape to host system',
                    mitigation='Remove privileged: true and use specific capabilities instead'
                ))
            
            # Check for dangerous host mounts
            dangerous_host_paths = ['/var/run/docker.sock', '/proc', '/sys', '/dev', '/']
            for path in dangerous_host_paths:
                if path in content:
                    container_issues.append(SecurityVulnerability(
                        severity='critical' if path in ['/var/run/docker.sock', '/'] else 'high',
                        category='container_escape',
                        description=f'Dangerous host path mount: {path}',
                        affected_resource='hostPath_volume',
                        attack_vector=f'Host path {path} enables container escape',
                        mitigation=f'Remove host path mount {path} or use read-only access'
                    ))
            
            # Check for host network/PID
            if 'hostNetwork' in content and 'true' in content:
                container_issues.append(SecurityVulnerability(
                    severity='high',
                    category='privilege_escalation',
                    description='Host network access detected',
                    affected_resource='pod_security_context',
                    attack_vector='Host network access bypasses network policies',
                    mitigation='Remove hostNetwork: true and use proper service networking'
                ))
            
            if 'hostPID' in content and 'true' in content:
                container_issues.append(SecurityVulnerability(
                    severity='high',
                    category='privilege_escalation',
                    description='Host PID namespace access detected',
                    affected_resource='pod_security_context',
                    attack_vector='Host PID access enables process manipulation',
                    mitigation='Remove hostPID: true unless absolutely necessary'
                ))
            
            # Check for root user
            if 'runAsUser' in content and ('0' in content or 'root' in content):
                container_issues.append(SecurityVulnerability(
                    severity='medium',
                    category='privilege_escalation',
                    description='Container running as root user',
                    affected_resource='security_context',
                    attack_vector='Root user increases attack surface and privilege escalation risk',
                    mitigation='Use non-root user (runAsUser: 1000) and runAsNonRoot: true'
                ))
        
        self.vulnerabilities.extend(container_issues)
        
        return {
            'container_security_analysis': {
                'total_issues': len(container_issues),
                'critical_issues': len([v for v in container_issues if v.severity == 'critical']),
                'high_issues': len([v for v in container_issues if v.severity == 'high']),
                'vulnerabilities': [
                    {
                        'severity': v.severity,
                        'category': v.category,
                        'description': v.description,
                        'resource': v.affected_resource,
                        'attack_vector': v.attack_vector,
                        'mitigation': v.mitigation
                    } for v in container_issues
                ]
            }
        }

    def analyze_network_policies(self) -> Dict[str, Any]:
        """Analyze network policies for bypass vulnerabilities"""
        print("🔍 ADVERSARIAL TEST: Analyzing network policies...")
        
        network_issues = []
        network_policies_found = 0
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Count network policies
            network_policy_matches = re.findall(
                r'resource\s+"kubernetes_network_policy"\s+"([^"]+)"',
                content
            )
            network_policies_found += len(network_policy_matches)
            
            # Check for overly permissive selectors
            if 'match_labels = {}' in content:
                network_issues.append(SecurityVulnerability(
                    severity='high',
                    category='network_policy_bypass',
                    description='Empty match_labels selector in network policy',
                    affected_resource='kubernetes_network_policy',
                    attack_vector='Empty selectors allow access to all pods/namespaces',
                    mitigation='Use specific labels for pod/namespace selection'
                ))
            
            # Check for missing egress policies
            ingress_count = content.count('ingress {')
            egress_count = content.count('egress {')
            
            if ingress_count > 0 and egress_count == 0:
                network_issues.append(SecurityVulnerability(
                    severity='medium',
                    category='network_policy_bypass',
                    description='Network policy has ingress rules but no egress restrictions',
                    affected_resource='kubernetes_network_policy',
                    attack_vector='Missing egress rules allow unrestricted outbound communication',
                    mitigation='Add explicit egress rules to restrict outbound traffic'
                ))
        
        # Check if any network policies exist at all
        if network_policies_found == 0:
            network_issues.append(SecurityVulnerability(
                severity='medium',
                category='network_policy_bypass',
                description='No network policies found in configuration',
                affected_resource='namespace',
                attack_vector='Without network policies, all pod communication is allowed',
                mitigation='Implement network policies to restrict pod-to-pod communication'
            ))
        
        self.vulnerabilities.extend(network_issues)
        
        return {
            'network_policy_analysis': {
                'network_policies_found': network_policies_found,
                'total_issues': len(network_issues),
                'critical_issues': len([v for v in network_issues if v.severity == 'critical']),
                'high_issues': len([v for v in network_issues if v.severity == 'high']),
                'vulnerabilities': [
                    {
                        'severity': v.severity,
                        'category': v.category,
                        'description': v.description,
                        'resource': v.affected_resource,
                        'attack_vector': v.attack_vector,
                        'mitigation': v.mitigation
                    } for v in network_issues
                ]
            }
        }

    def test_secret_security(self) -> Dict[str, Any]:
        """Test for secret management vulnerabilities"""
        print("🔍 ADVERSARIAL TEST: Analyzing secret management...")
        
        secret_issues = []
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Check for hardcoded secrets
            secret_patterns = [
                (r'password\s*=\s*"[^"]+', 'Hardcoded password detected'),
                (r'api_key\s*=\s*"[^"]+', 'Hardcoded API key detected'),
                (r'token\s*=\s*"[^"]+', 'Hardcoded token detected'),
                (r'secret\s*=\s*"[^"]+', 'Hardcoded secret detected'),
            ]
            
            for pattern, description in secret_patterns:
                if re.search(pattern, content, re.IGNORECASE):
                    secret_issues.append(SecurityVulnerability(
                        severity='high',
                        category='secret_exposure',
                        description=description,
                        affected_resource='terraform_configuration',
                        attack_vector='Hardcoded secrets in configuration can be extracted',
                        mitigation='Use Kubernetes secrets or external secret management'
                    ))
            
            # Check for base64encoded but visible secrets
            if 'base64encode' in content and '"' in content:
                secret_issues.append(SecurityVulnerability(
                    severity='medium',
                    category='secret_exposure',
                    description='Base64 encoded secret visible in configuration',
                    affected_resource='kubernetes_secret',
                    attack_vector='Base64 encoding is not encryption, secrets are easily decoded',
                    mitigation='Use proper secret management with encryption at rest'
                ))
            
            # Check for overly permissive secret access
            if 'secrets' in content and 'verbs' in content:
                if '"*"' in content or '"get"' in content:
                    secret_issues.append(SecurityVulnerability(
                        severity='high',
                        category='secret_access',
                        description='Overly permissive secret access permissions',
                        affected_resource='rbac_rule',
                        attack_vector='Broad secret access enables credential theft',
                        mitigation='Restrict secret access to specific secrets and necessary verbs'
                    ))
        
        self.vulnerabilities.extend(secret_issues)
        
        return {
            'secret_security_analysis': {
                'total_issues': len(secret_issues),
                'critical_issues': len([v for v in secret_issues if v.severity == 'critical']),
                'high_issues': len([v for v in secret_issues if v.severity == 'high']),
                'vulnerabilities': [
                    {
                        'severity': v.severity,
                        'category': v.category,
                        'description': v.description,
                        'resource': v.affected_resource,
                        'attack_vector': v.attack_vector,
                        'mitigation': v.mitigation
                    } for v in secret_issues
                ]
            }
        }

    def simulate_attack_scenarios(self) -> Dict[str, Any]:
        """Simulate realistic attack scenarios"""
        print("🔍 ADVERSARIAL TEST: Simulating attack scenarios...")
        
        attack_scenarios = [
            {
                'name': 'Kubernetes Dashboard RCE (CVE-2018-18264)',
                'description': 'Exploit dashboard service account for cluster access',
                'attack_steps': [
                    'Identify dashboard service account',
                    'Check for overprivileged permissions',
                    'Exploit dashboard to gain cluster access'
                ],
                'impact': 'Full cluster compromise',
                'severity': 'critical'
            },
            {
                'name': 'Service Account Token Theft',
                'description': 'Extract service account tokens from containers',
                'attack_steps': [
                    'Access container filesystem',
                    'Read /var/run/secrets/kubernetes.io/serviceaccount/token',
                    'Use token for API access'
                ],
                'impact': 'Unauthorized API access with service account privileges',
                'severity': 'high'
            },
            {
                'name': 'Container Escape via Docker Socket',
                'description': 'Escape container using mounted Docker socket',
                'attack_steps': [
                    'Access mounted /var/run/docker.sock',
                    'Create privileged container',
                    'Mount host filesystem',
                    'Escape to host system'
                ],
                'impact': 'Host system compromise',
                'severity': 'critical'
            },
            {
                'name': 'RBAC Privilege Escalation',
                'description': 'Abuse wildcard permissions to escalate privileges',
                'attack_steps': [
                    'Identify wildcard RBAC permissions',
                    'Create malicious resources',
                    'Escalate to cluster admin'
                ],
                'impact': 'Cluster admin privileges',
                'severity': 'critical'
            }
        ]
        
        # Simulate testing each scenario against the configuration
        scenario_results = []
        for scenario in attack_scenarios:
            # Simulate vulnerability detection
            vulnerable = False
            findings = []
            
            # Check if configuration is vulnerable to this scenario
            if scenario['name'] == 'Container Escape via Docker Socket':
                for vuln in self.vulnerabilities:
                    if 'docker.sock' in vuln.description.lower():
                        vulnerable = True
                        findings.append(f"Found vulnerability: {vuln.description}")
            
            elif scenario['name'] == 'RBAC Privilege Escalation':
                for vuln in self.vulnerabilities:
                    if vuln.category == 'rbac_bypass' and 'wildcard' in vuln.description.lower():
                        vulnerable = True
                        findings.append(f"Found vulnerability: {vuln.description}")
            
            scenario_results.append({
                'scenario': scenario['name'],
                'vulnerable': vulnerable,
                'severity': scenario['severity'],
                'impact': scenario['impact'],
                'findings': findings,
                'attack_steps': scenario['attack_steps']
            })
        
        return {
            'attack_simulation': {
                'total_scenarios': len(attack_scenarios),
                'vulnerable_scenarios': len([s for s in scenario_results if s['vulnerable']]),
                'critical_vulnerabilities': len([s for s in scenario_results if s['vulnerable'] and s['severity'] == 'critical']),
                'scenarios': scenario_results
            }
        }

    def generate_penetration_report(self) -> Dict[str, Any]:
        """Generate comprehensive penetration testing report"""
        print("📊 Generating comprehensive penetration testing report...")
        
        # Categorize vulnerabilities by severity
        critical_vulns = [v for v in self.vulnerabilities if v.severity == 'critical']
        high_vulns = [v for v in self.vulnerabilities if v.severity == 'high']
        medium_vulns = [v for v in self.vulnerabilities if v.severity == 'medium']
        low_vulns = [v for v in self.vulnerabilities if v.severity == 'low']
        
        # Calculate risk score
        risk_score = (len(critical_vulns) * 10) + (len(high_vulns) * 7) + (len(medium_vulns) * 4) + (len(low_vulns) * 1)
        max_possible_score = 100  # Adjust based on testing scope
        risk_percentage = min((risk_score / max_possible_score) * 100, 100)
        
        # Determine overall security posture
        if risk_percentage >= 80:
            security_posture = 'CRITICAL'
        elif risk_percentage >= 60:
            security_posture = 'HIGH_RISK'
        elif risk_percentage >= 40:
            security_posture = 'MEDIUM_RISK'
        elif risk_percentage >= 20:
            security_posture = 'LOW_RISK'
        else:
            security_posture = 'SECURE'
        
        return {
            'penetration_testing_report': {
                'timestamp': time.time(),
                'testing_duration': 'Static Analysis',
                'scope': 'Kubernetes Terraform Configuration',
                'methodology': 'OWASP Kubernetes Security Testing Guide',
                'risk_assessment': {
                    'overall_risk_score': risk_score,
                    'risk_percentage': risk_percentage,
                    'security_posture': security_posture
                },
                'vulnerability_summary': {
                    'total_vulnerabilities': len(self.vulnerabilities),
                    'critical': len(critical_vulns),
                    'high': len(high_vulns),
                    'medium': len(medium_vulns),
                    'low': len(low_vulns)
                },
                'top_vulnerabilities': [
                    {
                        'severity': v.severity,
                        'category': v.category,
                        'description': v.description,
                        'resource': v.affected_resource,
                        'attack_vector': v.attack_vector,
                        'mitigation': v.mitigation
                    } for v in sorted(self.vulnerabilities, 
                                    key=lambda x: {'critical': 4, 'high': 3, 'medium': 2, 'low': 1}[x.severity], 
                                    reverse=True)[:10]
                ],
                'remediation_priority': [
                    'Fix all CRITICAL vulnerabilities immediately',
                    'Address HIGH severity issues within 7 days',
                    'Plan remediation for MEDIUM issues within 30 days',
                    'Review LOW severity findings for completeness'
                ]
            }
        }

    def run_comprehensive_adversarial_testing(self) -> Dict[str, Any]:
        """Execute complete adversarial penetration testing suite"""
        print("🎯 EXECUTING KUBERNETES ADVERSARIAL PENETRATION TESTING")
        print("=" * 70)
        
        start_time = time.perf_counter()
        all_results = {}
        
        # Execute all adversarial tests
        all_results.update(self.analyze_rbac_configuration())
        all_results.update(self.test_container_security_contexts())
        all_results.update(self.analyze_network_policies())
        all_results.update(self.test_secret_security())
        all_results.update(self.simulate_attack_scenarios())
        all_results.update(self.generate_penetration_report())
        
        total_time = time.perf_counter() - start_time
        
        # Add execution summary
        summary = {
            'adversarial_testing_summary': {
                'total_duration': total_time,
                'tests_executed': 5,
                'vulnerabilities_found': len(self.vulnerabilities),
                'critical_vulnerabilities': len([v for v in self.vulnerabilities if v.severity == 'critical']),
                'high_vulnerabilities': len([v for v in self.vulnerabilities if v.severity == 'high']),
                'security_testing_complete': True,
                'timestamp': time.time()
            }
        }
        
        all_results.update(summary)
        return all_results

def main():
    """Execute K8s adversarial penetration testing"""
    tester = K8sAdversarialPenetrationTester()
    results = tester.run_comprehensive_adversarial_testing()
    
    # Save results
    results_file = Path("k8s_adversarial_penetration_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    # Display summary
    summary = results['adversarial_testing_summary']
    report = results.get('penetration_testing_report', {})
    
    print(f"\n🎯 ADVERSARIAL PENETRATION TESTING SUMMARY")
    print("=" * 60)
    print(f"Total Duration: {summary['total_duration']:.2f}s")
    print(f"Tests Executed: {summary['tests_executed']}")
    print(f"Total Vulnerabilities: {summary['vulnerabilities_found']}")
    print(f"Critical Vulnerabilities: {summary['critical_vulnerabilities']}")
    print(f"High Vulnerabilities: {summary['high_vulnerabilities']}")
    
    if report:
        risk_assessment = report.get('risk_assessment', {})
        print(f"Security Posture: {risk_assessment.get('security_posture', 'UNKNOWN')}")
        print(f"Risk Score: {risk_assessment.get('risk_percentage', 0):.1f}%")
    
    print(f"Results saved to: {results_file}")
    
    if summary['critical_vulnerabilities'] > 0:
        print("🚨 CRITICAL VULNERABILITIES DETECTED - Immediate action required")
        return 1
    elif summary['high_vulnerabilities'] > 0:
        print("⚠️ HIGH SEVERITY VULNERABILITIES DETECTED - Review and remediate")
        return 1
    else:
        print("✅ ADVERSARIAL TESTING COMPLETE - No critical vulnerabilities detected")
        return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())