#!/usr/bin/env python3
"""
Enhanced Kubernetes Adversarial Penetration Testing Suite
Improved logic for detecting real security vulnerabilities vs false positives
"""

import json
import yaml
import re
import time
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
import tempfile
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

class EnhancedK8sAdversarialTester:
    """Enhanced K8s adversarial penetration testing with better logic"""
    
    def __init__(self):
        self.results = {}
        self.vulnerabilities = []
        self.terraform_dir = Path(__file__).parent / "terraform"
        
    def analyze_container_security_contexts(self) -> Dict[str, Any]:
        """Enhanced analysis for container security contexts"""
        print("🔍 ENHANCED ADVERSARIAL TEST: Analyzing container security contexts...")
        
        container_issues = []
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Look for actual privileged container configurations
            privileged_patterns = [
                r'security_context\s*{[^}]*privileged\s*=\s*true',
                r'privileged\s*=\s*true',
                r'allowPrivilegedContainer\s*=\s*true'
            ]
            
            for pattern in privileged_patterns:
                if re.search(pattern, content, re.MULTILINE | re.DOTALL):
                    container_issues.append(SecurityVulnerability(
                        severity='critical',
                        category='container_escape',
                        description=f'Privileged container configuration found in {tf_file.name}',
                        affected_resource='container_security_context',
                        attack_vector='Privileged containers can escape to host system',
                        mitigation='Set privileged = false and use specific capabilities instead'
                    ))
            
            # Look for actual dangerous host path mounts (not ingress paths)
            host_path_patterns = [
                r'host_path\s*{[^}]*path\s*=\s*"/"',
                r'host_path\s*{[^}]*path\s*=\s*"/var/run/docker\.sock"',
                r'host_path\s*{[^}]*path\s*=\s*"/proc"',
                r'host_path\s*{[^}]*path\s*=\s*"/sys"',
                r'host_path\s*{[^}]*path\s*=\s*"/dev"',
                r'hostPath:\s*path:\s*"/"',
                r'hostPath:\s*path:\s*/var/run/docker\.sock'
            ]
            
            for pattern in host_path_patterns:
                matches = re.findall(pattern, content, re.MULTILINE | re.DOTALL)
                for match in matches:
                    # Extract the path from the match
                    path_match = re.search(r'["/][^"]*["/]', match)
                    if path_match:
                        path = path_match.group().strip('"')
                        severity = 'critical' if path in ['/var/run/docker.sock', '/'] else 'high'
                        container_issues.append(SecurityVulnerability(
                            severity=severity,
                            category='container_escape',
                            description=f'Dangerous host path mount detected: {path} in {tf_file.name}',
                            affected_resource='hostPath_volume',
                            attack_vector=f'Host path {path} enables container escape',
                            mitigation=f'Remove host path mount {path} or use read-only access with emptyDir'
                        ))
            
            # Check for host network/PID configurations
            host_access_patterns = [
                r'host_network\s*=\s*true',
                r'host_pid\s*=\s*true',
                r'host_ipc\s*=\s*true',
                r'hostNetwork:\s*true',
                r'hostPID:\s*true',
                r'hostIPC:\s*true'
            ]
            
            for pattern in host_access_patterns:
                if re.search(pattern, content, re.MULTILINE):
                    host_type = 'network' if 'network' in pattern.lower() else 'pid' if 'pid' in pattern.lower() else 'ipc'
                    container_issues.append(SecurityVulnerability(
                        severity='high',
                        category='privilege_escalation',
                        description=f'Host {host_type} access enabled in {tf_file.name}',
                        affected_resource='pod_security_context',
                        attack_vector=f'Host {host_type} access enables privilege escalation',
                        mitigation=f'Set host{host_type.capitalize()} = false unless absolutely necessary'
                    ))
            
            # Check for root user execution
            root_user_patterns = [
                r'run_as_user\s*=\s*0',
                r'runAsUser:\s*0',
                r'run_as_non_root\s*=\s*false'
            ]
            
            for pattern in root_user_patterns:
                if re.search(pattern, content, re.MULTILINE):
                    container_issues.append(SecurityVulnerability(
                        severity='medium',
                        category='privilege_escalation',
                        description=f'Container running as root user in {tf_file.name}',
                        affected_resource='security_context',
                        attack_vector='Root user increases attack surface and privilege escalation risk',
                        mitigation='Use non-root user (runAsUser: 1000) and runAsNonRoot: true'
                    ))
        
        self.vulnerabilities.extend(container_issues)
        
        return {
            'enhanced_container_security_analysis': {
                'total_issues': len(container_issues),
                'critical_issues': len([v for v in container_issues if v.severity == 'critical']),
                'high_issues': len([v for v in container_issues if v.severity == 'high']),
                'medium_issues': len([v for v in container_issues if v.severity == 'medium']),
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

    def analyze_rbac_configuration(self) -> Dict[str, Any]:
        """Enhanced RBAC analysis"""
        print("🔍 ENHANCED ADVERSARIAL TEST: Analyzing RBAC configuration...")
        
        rbac_issues = []
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Look for wildcard permissions in RBAC rules
            wildcard_patterns = [
                r'verbs\s*=\s*\[\s*"\*"\s*\]',
                r'resources\s*=\s*\[\s*"\*"\s*\]',
                r'api_groups\s*=\s*\[\s*"\*"\s*\]'
            ]
            
            for pattern in wildcard_patterns:
                if re.search(pattern, content, re.MULTILINE):
                    rbac_issues.append(SecurityVulnerability(
                        severity='critical',
                        category='rbac_bypass',
                        description=f'Wildcard RBAC permissions found in {tf_file.name}',
                        affected_resource='kubernetes_role',
                        attack_vector='Wildcard permissions allow unrestricted access to cluster resources',
                        mitigation='Replace wildcard permissions with specific resource and verb grants'
                    ))
            
            # Look for cluster-admin role bindings
            cluster_admin_patterns = [
                r'cluster-admin',
                r'system:masters'
            ]
            
            for pattern in cluster_admin_patterns:
                if re.search(pattern, content) and 'role_binding' in content.lower():
                    rbac_issues.append(SecurityVulnerability(
                        severity='critical',
                        category='rbac_bypass',
                        description=f'Cluster admin role binding found in {tf_file.name}',
                        affected_resource='kubernetes_cluster_role_binding',
                        attack_vector='Cluster admin access provides unrestricted cluster control',
                        mitigation='Use least-privilege principle with specific role permissions'
                    ))
        
        self.vulnerabilities.extend(rbac_issues)
        
        return {
            'enhanced_rbac_analysis': {
                'total_issues': len(rbac_issues),
                'critical_issues': len([v for v in rbac_issues if v.severity == 'critical']),
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

    def analyze_secret_management(self) -> Dict[str, Any]:
        """Enhanced secret management analysis"""
        print("🔍 ENHANCED ADVERSARIAL TEST: Analyzing secret management...")
        
        secret_issues = []
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Look for hardcoded secrets (not base64encode function calls)
            hardcoded_patterns = [
                r'password\s*=\s*"[^"]{8,}"',  # Hardcoded passwords
                r'api_key\s*=\s*"[a-zA-Z0-9]{20,}"',  # Hardcoded API keys
                r'token\s*=\s*"[a-zA-Z0-9]{16,}"',  # Hardcoded tokens
                r'secret\s*=\s*"[^"]{8,}"'  # Hardcoded secrets
            ]
            
            for pattern in hardcoded_patterns:
                matches = re.findall(pattern, content, re.IGNORECASE)
                for match in matches:
                    # Skip if it's using base64encode function or placeholder values
                    if 'base64encode' not in match and 'placeholder' not in match.lower() and 'example' not in match.lower():
                        secret_issues.append(SecurityVulnerability(
                            severity='high',
                            category='secret_exposure',
                            description=f'Hardcoded secret detected in {tf_file.name}: {match[:30]}...',
                            affected_resource='terraform_configuration',
                            attack_vector='Hardcoded secrets in configuration can be extracted by attackers',
                            mitigation='Use Kubernetes secrets, HashiCorp Vault, or external secret management'
                        ))
            
            # Look for overly permissive secret access
            if 'secrets' in content and 'verbs' in content:
                secret_verb_patterns = [
                    r'resources\s*=\s*\[[^\]]*"secrets"[^\]]*\][^}]*verbs\s*=\s*\[[^\]]*"\*"[^\]]*\]',
                    r'resources\s*=\s*\[[^\]]*"secrets"[^\]]*\][^}]*verbs\s*=\s*\[[^\]]*"create"[^\]]*"delete"[^\]]*\]'
                ]
                
                for pattern in secret_verb_patterns:
                    if re.search(pattern, content, re.MULTILINE | re.DOTALL):
                        secret_issues.append(SecurityVulnerability(
                            severity='high',
                            category='secret_access',
                            description=f'Overly permissive secret access in {tf_file.name}',
                            affected_resource='rbac_rule',
                            attack_vector='Broad secret access enables credential theft and privilege escalation',
                            mitigation='Restrict secret access to specific secrets and necessary verbs only'
                        ))
        
        self.vulnerabilities.extend(secret_issues)
        
        return {
            'enhanced_secret_analysis': {
                'total_issues': len(secret_issues),
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

    def analyze_network_policies(self) -> Dict[str, Any]:
        """Enhanced network policy analysis"""
        print("🔍 ENHANCED ADVERSARIAL TEST: Analyzing network policies...")
        
        network_issues = []
        network_policies_found = 0
        
        for tf_file in self.terraform_dir.glob('*.tf'):
            content = tf_file.read_text()
            
            # Count actual network policies
            network_policy_resources = re.findall(
                r'resource\s+"kubernetes_network_policy"\s+"[^"]+"\s*{',
                content
            )
            network_policies_found += len(network_policy_resources)
            
            # Look for overly permissive selectors (empty match_labels)
            permissive_selector_patterns = [
                r'pod_selector\s*{\s*}\s*',  # Empty pod selector
                r'namespace_selector\s*{\s*}\s*',  # Empty namespace selector
                r'match_labels\s*=\s*{\s*}\s*'  # Empty match labels
            ]
            
            for pattern in permissive_selector_patterns:
                if re.search(pattern, content, re.MULTILINE | re.DOTALL):
                    network_issues.append(SecurityVulnerability(
                        severity='high',
                        category='network_policy_bypass',
                        description=f'Overly permissive network policy selector in {tf_file.name}',
                        affected_resource='kubernetes_network_policy',
                        attack_vector='Empty selectors allow unrestricted access to all pods/namespaces',
                        mitigation='Use specific labels for pod/namespace selection'
                    ))
        
        # Check if any network policies exist
        if network_policies_found == 0:
            network_issues.append(SecurityVulnerability(
                severity='medium',
                category='network_policy_bypass',
                description='No network policies found in Terraform configuration',
                affected_resource='namespace',
                attack_vector='Without network policies, all pod-to-pod communication is allowed',
                mitigation='Implement network policies to enforce microsegmentation'
            ))
        
        self.vulnerabilities.extend(network_issues)
        
        return {
            'enhanced_network_policy_analysis': {
                'network_policies_found': network_policies_found,
                'total_issues': len(network_issues),
                'high_issues': len([v for v in network_issues if v.severity == 'high']),
                'medium_issues': len([v for v in network_issues if v.severity == 'medium']),
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

    def generate_enhanced_report(self) -> Dict[str, Any]:
        """Generate enhanced penetration testing report"""
        print("📊 Generating enhanced penetration testing report...")
        
        # Categorize vulnerabilities by severity
        critical_vulns = [v for v in self.vulnerabilities if v.severity == 'critical']
        high_vulns = [v for v in self.vulnerabilities if v.severity == 'high']
        medium_vulns = [v for v in self.vulnerabilities if v.severity == 'medium']
        low_vulns = [v for v in self.vulnerabilities if v.severity == 'low']
        
        # Calculate risk score (more sophisticated)
        risk_score = (len(critical_vulns) * 10) + (len(high_vulns) * 6) + (len(medium_vulns) * 3) + (len(low_vulns) * 1)
        max_possible_score = 50  # Adjusted for enhanced testing
        risk_percentage = min((risk_score / max_possible_score) * 100, 100)
        
        # Determine overall security posture
        if risk_percentage >= 70:
            security_posture = 'CRITICAL'
        elif risk_percentage >= 50:
            security_posture = 'HIGH_RISK'
        elif risk_percentage >= 30:
            security_posture = 'MEDIUM_RISK'
        elif risk_percentage >= 10:
            security_posture = 'LOW_RISK'
        else:
            security_posture = 'SECURE'
        
        return {
            'enhanced_penetration_testing_report': {
                'timestamp': time.time(),
                'testing_methodology': 'Enhanced K8s Security Testing with Pattern Matching',
                'scope': 'Kubernetes Terraform Configuration - Advanced Analysis',
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
                    'Address all CRITICAL vulnerabilities immediately',
                    'Fix HIGH severity issues within 24 hours', 
                    'Plan remediation for MEDIUM issues within 7 days',
                    'Review LOW severity findings for completeness'
                ],
                'false_positive_reduction': True,
                'pattern_matching_improved': True
            }
        }

    def run_enhanced_adversarial_testing(self) -> Dict[str, Any]:
        """Execute enhanced adversarial penetration testing suite"""
        print("🎯 EXECUTING ENHANCED KUBERNETES ADVERSARIAL PENETRATION TESTING")
        print("=" * 70)
        
        start_time = time.perf_counter()
        all_results = {}
        
        # Execute enhanced tests
        all_results.update(self.analyze_container_security_contexts())
        all_results.update(self.analyze_rbac_configuration())
        all_results.update(self.analyze_secret_management())
        all_results.update(self.analyze_network_policies())
        all_results.update(self.generate_enhanced_report())
        
        total_time = time.perf_counter() - start_time
        
        # Add execution summary
        summary = {
            'enhanced_testing_summary': {
                'total_duration': total_time,
                'tests_executed': 4,
                'vulnerabilities_found': len(self.vulnerabilities),
                'critical_vulnerabilities': len([v for v in self.vulnerabilities if v.severity == 'critical']),
                'high_vulnerabilities': len([v for v in self.vulnerabilities if v.severity == 'high']),
                'medium_vulnerabilities': len([v for v in self.vulnerabilities if v.severity == 'medium']),
                'enhanced_detection': True,
                'false_positive_reduction': True,
                'timestamp': time.time()
            }
        }
        
        all_results.update(summary)
        return all_results

def main():
    """Execute enhanced K8s adversarial penetration testing"""
    tester = EnhancedK8sAdversarialTester()
    results = tester.run_enhanced_adversarial_testing()
    
    # Save results
    results_file = Path("enhanced_k8s_adversarial_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    # Display summary
    summary = results['enhanced_testing_summary']
    report = results.get('enhanced_penetration_testing_report', {})
    
    print(f"\n🎯 ENHANCED ADVERSARIAL PENETRATION TESTING SUMMARY")
    print("=" * 60)
    print(f"Total Duration: {summary['total_duration']:.2f}s")
    print(f"Tests Executed: {summary['tests_executed']}")
    print(f"Total Vulnerabilities: {summary['vulnerabilities_found']}")
    print(f"Critical Vulnerabilities: {summary['critical_vulnerabilities']}")
    print(f"High Vulnerabilities: {summary['high_vulnerabilities']}")
    print(f"Medium Vulnerabilities: {summary['medium_vulnerabilities']}")
    
    if report:
        risk_assessment = report.get('risk_assessment', {})
        print(f"Security Posture: {risk_assessment.get('security_posture', 'UNKNOWN')}")
        print(f"Risk Score: {risk_assessment.get('risk_percentage', 0):.1f}%")
    
    print(f"Enhanced Detection: {'✅ YES' if summary.get('enhanced_detection') else '❌ NO'}")
    print(f"False Positive Reduction: {'✅ YES' if summary.get('false_positive_reduction') else '❌ NO'}")
    print(f"Results saved to: {results_file}")
    
    if summary['critical_vulnerabilities'] > 0:
        print("🚨 CRITICAL VULNERABILITIES DETECTED - Immediate action required")
        return 1
    elif summary['high_vulnerabilities'] > 0:
        print("⚠️ HIGH SEVERITY VULNERABILITIES DETECTED - Review and remediate")
        return 1
    else:
        print("✅ ENHANCED ADVERSARIAL TESTING COMPLETE - No critical vulnerabilities detected")
        return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())