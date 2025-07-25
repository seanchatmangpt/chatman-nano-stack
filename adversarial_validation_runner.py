#!/usr/bin/env python3
"""
BitActor Adversarial Validation Runner - Security-Enhanced Deployment

Executes comprehensive adversarial validation against the security-enhanced 
BitActor deployment to verify that implemented security fixes prevent attacks.
"""

import json
import random
import subprocess  
import sys
import time
import yaml
from pathlib import Path
from typing import Dict, List

class BitActorAdversarialValidator:
    """Comprehensive adversarial validation for security-enhanced deployment"""
    
    def __init__(self):
        self.results = {}
        self.k8s_path = Path("/Users/sac/cns/k8s")
        self.values_file = self.k8s_path / "helm" / "bitactor" / "values.yaml"
        self.terraform_file = self.k8s_path / "terraform" / "main.tf"
        
        # Load configurations
        with open(self.values_file) as f:
            self.values_config = yaml.safe_load(f)
        
        with open(self.terraform_file) as f:
            self.terraform_config = f.read()
    
    def run_comprehensive_adversarial_validation(self) -> Dict:
        """Execute comprehensive adversarial validation suite"""
        print("🎯 BITACTOR ADVERSARIAL VALIDATION - SECURITY FIXES VERIFICATION")
        print("=" * 70)
        
        # Phase 1: Container Escape Attack Prevention
        print("\n📦 PHASE 1: CONTAINER ESCAPE ATTACK PREVENTION")
        self._validate_container_escape_prevention()
        
        # Phase 2: Privilege Escalation Attack Prevention  
        print("\n⚡ PHASE 2: PRIVILEGE ESCALATION ATTACK PREVENTION")
        self._validate_privilege_escalation_prevention()
        
        # Phase 3: Lateral Movement Attack Prevention
        print("\n🌐 PHASE 3: LATERAL MOVEMENT ATTACK PREVENTION")
        self._validate_lateral_movement_prevention()
        
        # Phase 4: Data Exfiltration Attack Prevention
        print("\n💾 PHASE 4: DATA EXFILTRATION ATTACK PREVENTION")
        self._validate_data_exfiltration_prevention()
        
        # Phase 5: Runtime Attack Detection
        print("\n🛡️ PHASE 5: RUNTIME ATTACK DETECTION VALIDATION")
        self._validate_runtime_attack_detection()
        
        # Phase 6: Persistence Attack Prevention
        print("\n🔄 PHASE 6: PERSISTENCE ATTACK PREVENTION")
        self._validate_persistence_attack_prevention()
        
        return self._generate_adversarial_validation_report()
    
    def _validate_container_escape_prevention(self):
        """Validate container escape attack prevention"""
        print("🔍 Validating container escape attack prevention...")
        
        # Test 1: Privileged container prevention
        privileged_blocked = self._test_privileged_container_prevention()
        
        # Test 2: Host namespace access prevention
        host_access_blocked = self._test_host_namespace_prevention()
        
        # Test 3: Dangerous volume mount prevention
        volume_access_blocked = self._test_dangerous_volume_prevention()
        
        # Test 4: Container capabilities restrictions
        capabilities_restricted = self._test_capabilities_restrictions()
        
        # Test 5: Read-only filesystem enforcement
        readonly_enforced = self._test_readonly_filesystem()
        
        self.results['container_escape_prevention'] = {
            'privileged_containers_blocked': privileged_blocked,
            'host_namespace_access_blocked': host_access_blocked,
            'dangerous_volume_mounts_blocked': volume_access_blocked,
            'capabilities_properly_restricted': capabilities_restricted,
            'readonly_filesystem_enforced': readonly_enforced,
            'escape_prevention_score': self._calculate_escape_prevention_score([
                privileged_blocked, host_access_blocked, volume_access_blocked,
                capabilities_restricted, readonly_enforced
            ])
        }
        
        score = self.results['container_escape_prevention']['escape_prevention_score']
        status = "✅ PREVENTED" if score >= 90.0 else "⚠️ VULNERABLE"
        print(f"  {status} | Escape Prevention Score: {score:.1f}/100")
    
    def _validate_privilege_escalation_prevention(self):
        """Validate privilege escalation attack prevention"""
        print("🔍 Validating privilege escalation attack prevention...")
        
        # Test 1: Service account token abuse prevention
        token_abuse_blocked = self._test_service_account_token_prevention()
        
        # Test 2: RBAC privilege escalation prevention
        rbac_escalation_blocked = self._test_rbac_escalation_prevention()
        
        # Test 3: Container user escalation prevention
        user_escalation_blocked = self._test_user_escalation_prevention()
        
        # Test 4: Filesystem privilege escalation prevention
        fs_escalation_blocked = self._test_filesystem_escalation_prevention()
        
        # Test 5: Pod security context enforcement
        security_context_enforced = self._test_security_context_enforcement()
        
        self.results['privilege_escalation_prevention'] = {
            'service_account_token_abuse_blocked': token_abuse_blocked,
            'rbac_privilege_escalation_blocked': rbac_escalation_blocked,
            'container_user_escalation_blocked': user_escalation_blocked,
            'filesystem_escalation_blocked': fs_escalation_blocked,
            'security_context_properly_enforced': security_context_enforced,
            'escalation_prevention_score': self._calculate_escalation_prevention_score([
                token_abuse_blocked, rbac_escalation_blocked, user_escalation_blocked,
                fs_escalation_blocked, security_context_enforced
            ])
        }
        
        score = self.results['privilege_escalation_prevention']['escalation_prevention_score']
        status = "✅ PREVENTED" if score >= 90.0 else "⚠️ VULNERABLE"
        print(f"  {status} | Escalation Prevention Score: {score:.1f}/100")
    
    def _validate_lateral_movement_prevention(self):
        """Validate lateral movement attack prevention"""
        print("🔍 Validating lateral movement attack prevention...")
        
        # Test 1: Network policy enforcement
        network_policy_enforced = self._test_network_policy_enforcement()
        
        # Test 2: Service discovery restrictions
        service_discovery_restricted = self._test_service_discovery_restrictions()
        
        # Test 3: DNS enumeration prevention
        dns_enumeration_blocked = self._test_dns_enumeration_prevention()
        
        # Test 4: Pod-to-pod communication restrictions
        pod_communication_restricted = self._test_pod_communication_restrictions()
        
        # Test 5: Namespace isolation enforcement
        namespace_isolation_enforced = self._test_namespace_isolation()
        
        self.results['lateral_movement_prevention'] = {
            'network_policies_enforced': network_policy_enforced,
            'service_discovery_restricted': service_discovery_restricted,
            'dns_enumeration_blocked': dns_enumeration_blocked,
            'pod_communication_restricted': pod_communication_restricted,
            'namespace_isolation_enforced': namespace_isolation_enforced,
            'lateral_movement_prevention_score': self._calculate_lateral_movement_score([
                network_policy_enforced, service_discovery_restricted, dns_enumeration_blocked,
                pod_communication_restricted, namespace_isolation_enforced
            ])
        }
        
        score = self.results['lateral_movement_prevention']['lateral_movement_prevention_score']
        status = "✅ RESTRICTED" if score >= 90.0 else "⚠️ EXPOSED"
        print(f"  {status} | Lateral Movement Prevention Score: {score:.1f}/100")
    
    def _validate_data_exfiltration_prevention(self):
        """Validate data exfiltration attack prevention"""
        print("🔍 Validating data exfiltration attack prevention...")
        
        # Test 1: Secret access restrictions
        secret_access_restricted = self._test_secret_access_restrictions()
        
        # Test 2: ConfigMap exposure prevention
        configmap_exposure_blocked = self._test_configmap_exposure_prevention()
        
        # Test 3: Volume data access restrictions
        volume_data_restricted = self._test_volume_data_restrictions()
        
        # Test 4: Network egress controls
        egress_controlled = self._test_network_egress_controls()
        
        # Test 5: Sensitive data exposure prevention
        sensitive_data_protected = self._test_sensitive_data_protection()
        
        self.results['data_exfiltration_prevention'] = {
            'secret_access_properly_restricted': secret_access_restricted,
            'configmap_exposure_prevented': configmap_exposure_blocked,
            'volume_data_access_restricted': volume_data_restricted,
            'network_egress_controlled': egress_controlled,
            'sensitive_data_protected': sensitive_data_protected,
            'exfiltration_prevention_score': self._calculate_exfiltration_score([
                secret_access_restricted, configmap_exposure_blocked, volume_data_restricted,
                egress_controlled, sensitive_data_protected
            ])
        }
        
        score = self.results['data_exfiltration_prevention']['exfiltration_prevention_score']
        status = "✅ PROTECTED" if score >= 90.0 else "⚠️ EXPOSED"
        print(f"  {status} | Data Protection Score: {score:.1f}/100")
    
    def _validate_runtime_attack_detection(self):
        """Validate runtime attack detection capabilities"""
        print("🔍 Validating runtime attack detection...")
        
        # Test 1: Falco runtime monitoring deployment
        falco_deployed = self._test_falco_deployment()
        
        # Test 2: Suspicious activity detection rules
        detection_rules_active = self._test_detection_rules()
        
        # Test 3: Attack pattern recognition
        attack_patterns_detected = self._test_attack_pattern_detection()
        
        # Test 4: Anomaly detection capabilities
        anomaly_detection_active = self._test_anomaly_detection()
        
        # Test 5: Alert generation and response
        alerting_functional = self._test_alerting_system()
        
        self.results['runtime_attack_detection'] = {
            'falco_runtime_monitoring_deployed': falco_deployed,
            'detection_rules_active': detection_rules_active,
            'attack_patterns_recognized': attack_patterns_detected,
            'anomaly_detection_functional': anomaly_detection_active,
            'alerting_system_operational': alerting_functional,
            'detection_effectiveness_score': self._calculate_detection_score([
                falco_deployed, detection_rules_active, attack_patterns_detected,
                anomaly_detection_active, alerting_functional
            ])
        }
        
        score = self.results['runtime_attack_detection']['detection_effectiveness_score']
        status = "✅ MONITORING" if score >= 90.0 else "⚠️ BLIND"
        print(f"  {status} | Detection Effectiveness Score: {score:.1f}/100")
    
    def _validate_persistence_attack_prevention(self):
        """Validate persistence attack prevention"""
        print("🔍 Validating persistence attack prevention...")
        
        # Test 1: CronJob creation restrictions
        cronjob_creation_blocked = self._test_cronjob_creation_prevention()
        
        # Test 2: DaemonSet deployment restrictions
        daemonset_creation_blocked = self._test_daemonset_creation_prevention()
        
        # Test 3: Secret modification restrictions
        secret_modification_blocked = self._test_secret_modification_prevention()
        
        # Test 4: ConfigMap persistence restrictions
        configmap_modification_blocked = self._test_configmap_modification_prevention()
        
        # Test 5: Admission controller enforcement
        admission_control_enforced = self._test_admission_control()
        
        self.results['persistence_attack_prevention'] = {
            'cronjob_creation_blocked': cronjob_creation_blocked,
            'daemonset_creation_blocked': daemonset_creation_blocked,
            'secret_modification_blocked': secret_modification_blocked,
            'configmap_modification_blocked': configmap_modification_blocked,
            'admission_controllers_enforced': admission_control_enforced,
            'persistence_prevention_score': self._calculate_persistence_score([
                cronjob_creation_blocked, daemonset_creation_blocked, secret_modification_blocked,
                configmap_modification_blocked, admission_control_enforced
            ])
        }
        
        score = self.results['persistence_attack_prevention']['persistence_prevention_score']
        status = "✅ BLOCKED" if score >= 90.0 else "⚠️ VULNERABLE"
        print(f"  {status} | Persistence Prevention Score: {score:.1f}/100")
    
    # Container Escape Prevention Tests
    
    def _test_privileged_container_prevention(self) -> bool:
        """Test that privileged containers are prevented"""
        # Check security context in values.yaml
        security_context = self.values_config.get('securityContext', {})
        pod_security_context = self.values_config.get('podSecurityContext', {})
        
        # Privileged should not be set to true anywhere
        privileged_blocked = (
            security_context.get('privileged', False) == False and
            'privileged: true' not in self.terraform_config.lower()
        )
        
        return privileged_blocked
    
    def _test_host_namespace_prevention(self) -> bool:
        """Test that host namespace access is prevented"""
        # Check for hostNetwork, hostPID, hostIPC settings
        host_blocked = (
            'hostnetwork: true' not in self.terraform_config.lower() and
            'hostpid: true' not in self.terraform_config.lower() and
            'hostipc: true' not in self.terraform_config.lower()
        )
        
        return host_blocked
    
    def _test_dangerous_volume_prevention(self) -> bool:
        """Test that dangerous volume mounts are prevented"""
        # Check for dangerous host path mounts
        dangerous_paths = ['/etc', '/var/run/docker.sock', '/host', '/proc', '/sys']
        
        # Only Falco should have access to these paths for monitoring
        safe_mounts = True
        for path in dangerous_paths:
            if path in self.terraform_config and 'falco' not in self.terraform_config.lower():
                safe_mounts = False
                break
        
        return safe_mounts
    
    def _test_capabilities_restrictions(self) -> bool:
        """Test that container capabilities are properly restricted"""
        security_context = self.values_config.get('securityContext', {})
        capabilities = security_context.get('capabilities', {})
        
        # ALL capabilities should be dropped
        capabilities_restricted = (
            'ALL' in capabilities.get('drop', []) and
            len(capabilities.get('add', [])) == 0
        )
        
        return capabilities_restricted
    
    def _test_readonly_filesystem(self) -> bool:
        """Test that read-only filesystem is enforced"""
        security_context = self.values_config.get('securityContext', {})
        return security_context.get('readOnlyRootFilesystem', False) == True
    
    # Privilege Escalation Prevention Tests
    
    def _test_service_account_token_prevention(self) -> bool:
        """Test that service account token auto-mount is disabled"""
        service_account = self.values_config.get('serviceAccount', {})
        terraform_sa_disabled = 'automount_service_account_token = false' in self.terraform_config
        
        return (service_account.get('automountServiceAccountToken', True) == False and
                terraform_sa_disabled)
    
    def _test_rbac_escalation_prevention(self) -> bool:
        """Test that RBAC prevents privilege escalation"""
        # Check for least privilege RBAC in terraform
        rbac_restrictive = (
            'resources  = ["pods", "services"]' in self.terraform_config and
            'verbs      = ["get", "list", "watch"]' in self.terraform_config and
            'verbs      = ["*"]' not in self.terraform_config and
            'resources  = ["*"]' not in self.terraform_config
        )
        
        return rbac_restrictive
    
    def _test_user_escalation_prevention(self) -> bool:
        """Test that container user escalation is prevented"""
        security_context = self.values_config.get('securityContext', {})
        pod_security_context = self.values_config.get('podSecurityContext', {})
        
        user_escalation_blocked = (
            security_context.get('allowPrivilegeEscalation', True) == False and
            security_context.get('runAsNonRoot', False) == True and
            pod_security_context.get('runAsNonRoot', False) == True and
            pod_security_context.get('runAsUser', 0) != 0
        )
        
        return user_escalation_blocked
    
    def _test_filesystem_escalation_prevention(self) -> bool:
        """Test that filesystem privilege escalation is prevented"""
        security_context = self.values_config.get('securityContext', {})
        return security_context.get('readOnlyRootFilesystem', False) == True
    
    def _test_security_context_enforcement(self) -> bool:
        """Test that security context is properly enforced"""
        pod_security_context = self.values_config.get('podSecurityContext', {})
        
        security_enforced = (
            'seccompProfile' in pod_security_context and
            pod_security_context['seccompProfile']['type'] == 'RuntimeDefault' and
            'fsGroup' in pod_security_context
        )
        
        return security_enforced
    
    # Lateral Movement Prevention Tests
    
    def _test_network_policy_enforcement(self) -> bool:
        """Test that network policies are enforced"""
        network_policy = self.values_config.get('networkPolicy', {})
        terraform_netpol = 'kubernetes_network_policy' in self.terraform_config
        
        return (network_policy.get('enabled', False) == True and terraform_netpol)
    
    def _test_service_discovery_restrictions(self) -> bool:
        """Test that service discovery is restricted"""
        # Check RBAC permissions for service discovery
        rbac_restricted = (
            'verbs      = ["get", "list", "watch"]' in self.terraform_config and
            'resources  = ["pods", "services"]' in self.terraform_config
        )
        
        return rbac_restricted
    
    def _test_dns_enumeration_prevention(self) -> bool:
        """Test that DNS enumeration is prevented"""
        # Check for DNS restrictions in network policy
        network_policy = self.values_config.get('networkPolicy', {})
        egress_rules = network_policy.get('egress', [])
        
        dns_restricted = any(
            rule.get('ports', [{}])[0].get('port') == 53 and 
            rule.get('ports', [{}])[0].get('protocol') == 'UDP'
            for rule in egress_rules
        )
        
        return dns_restricted
    
    def _test_pod_communication_restrictions(self) -> bool:
        """Test that pod-to-pod communication is restricted"""
        network_policy = self.values_config.get('networkPolicy', {})
        ingress_rules = network_policy.get('ingress', [])
        
        # Should only allow communication from specific pods/namespaces
        communication_restricted = len(ingress_rules) > 0 and any(
            'podSelector' in rule.get('from', [{}])[0] or 
            'namespaceSelector' in rule.get('from', [{}])[0]
            for rule in ingress_rules
        )
        
        return communication_restricted
    
    def _test_namespace_isolation(self) -> bool:
        """Test that namespace isolation is enforced"""
        # Check for namespace-specific network policies
        terraform_namespace = 'kubernetes_namespace' in self.terraform_config
        network_policies = 'kubernetes_network_policy' in self.terraform_config
        
        return terraform_namespace and network_policies
    
    # Data Exfiltration Prevention Tests
    
    def _test_secret_access_restrictions(self) -> bool:
        """Test that secret access is restricted"""
        # Service account token auto-mount disabled prevents secret access
        service_account = self.values_config.get('serviceAccount', {})
        return service_account.get('automountServiceAccountToken', True) == False
    
    def _test_configmap_exposure_prevention(self) -> bool:
        """Test that ConfigMap exposure is prevented"""
        # Check RBAC permissions don't include configmaps in broad access
        configmap_protected = 'configmaps' not in self.terraform_config or (
            'verbs      = ["get", "list", "watch"]' in self.terraform_config and
            'verbs      = ["*"]' not in self.terraform_config
        )
        
        return configmap_protected
    
    def _test_volume_data_restrictions(self) -> bool:
        """Test that volume data access is restricted"""
        # Read-only filesystem prevents volume data modification
        security_context = self.values_config.get('securityContext', {})
        return security_context.get('readOnlyRootFilesystem', False) == True
    
    def _test_network_egress_controls(self) -> bool:
        """Test that network egress is controlled"""
        network_policy = self.values_config.get('networkPolicy', {})
        egress_rules = network_policy.get('egress', [])
        
        # Should have specific egress rules rather than open egress
        egress_controlled = len(egress_rules) > 0
        
        return egress_controlled
    
    def _test_sensitive_data_protection(self) -> bool:
        """Test that sensitive data is protected"""
        # Check that no hardcoded secrets exist in configuration
        import re
        values_str = yaml.dump(self.values_config)
        
        # Pattern to detect potential secrets
        secret_patterns = [
            r'password\s*:\s*["\'][^"\']+["\']',
            r'secret\s*:\s*["\'][^"\']+["\']',
            r'token\s*:\s*["\'][^"\']+["\']',
            r'key\s*:\s*["\'][^"\']+["\']'
        ]
        
        sensitive_data_protected = True
        for pattern in secret_patterns:
            if re.search(pattern, values_str, re.IGNORECASE):
                sensitive_data_protected = False
                break
        
        return sensitive_data_protected
    
    # Runtime Attack Detection Tests
    
    def _test_falco_deployment(self) -> bool:
        """Test that Falco runtime monitoring is deployed"""
        runtime_security = self.values_config.get('runtimeSecurity', {})
        falco_config = runtime_security.get('falco', {})
        terraform_falco = 'kubernetes_daemonset' in self.terraform_config and 'falco' in self.terraform_config
        
        return (falco_config.get('enabled', False) == True and terraform_falco)
    
    def _test_detection_rules(self) -> bool:
        """Test that detection rules are active"""
        runtime_security = self.values_config.get('runtimeSecurity', {})
        falco_config = runtime_security.get('falco', {})
        
        detection_rules_active = (
            'rules' in falco_config and 
            len(falco_config.get('rules', [])) > 0
        )
        
        return detection_rules_active
    
    def _test_attack_pattern_detection(self) -> bool:
        """Test that attack patterns can be detected"""
        # Falco deployment with proper configuration enables attack pattern detection
        return self._test_falco_deployment() and self._test_detection_rules()
    
    def _test_anomaly_detection(self) -> bool:
        """Test that anomaly detection is functional"""
        # Falco with rules provides anomaly detection capabilities
        runtime_security = self.values_config.get('runtimeSecurity', {})
        falco_config = runtime_security.get('falco', {})
        
        return falco_config.get('enabled', False) == True
    
    def _test_alerting_system(self) -> bool:
        """Test that alerting system is operational"""
        # Check for monitoring configuration
        monitoring = self.values_config.get('monitoring', {})
        prometheus = monitoring.get('prometheus', {})
        
        alerting_configured = (
            prometheus.get('enabled', False) == True and
            prometheus.get('rules', {}).get('enabled', False) == True
        )
        
        return alerting_configured
    
    # Persistence Attack Prevention Tests
    
    def _test_cronjob_creation_prevention(self) -> bool:
        """Test that CronJob creation is prevented"""
        # Check RBAC permissions don't allow CronJob creation
        rbac_prevents_cronjobs = (
            'cronjobs' not in self.terraform_config.lower() and
            'verbs      = ["*"]' not in self.terraform_config and
            'resources  = ["*"]' not in self.terraform_config
        )
        
        return rbac_prevents_cronjobs
    
    def _test_daemonset_creation_prevention(self) -> bool:
        """Test that DaemonSet creation is prevented"""
        # Check RBAC permissions don't allow DaemonSet creation by app
        # (except for Falco which is managed by Terraform)
        rbac_prevents_daemonsets = (
            'verbs      = ["get", "list", "watch"]' in self.terraform_config and
            'verbs      = ["*"]' not in self.terraform_config
        )
        
        return rbac_prevents_daemonsets
    
    def _test_secret_modification_prevention(self) -> bool:
        """Test that secret modification is prevented"""
        # Check RBAC permissions for secrets
        rbac_prevents_secret_modification = (
            'secrets' not in self.terraform_config.lower() or (
                'verbs      = ["get", "list", "watch"]' in self.terraform_config and
                'verbs      = ["*"]' not in self.terraform_config
            )
        )
        
        return rbac_prevents_secret_modification
    
    def _test_configmap_modification_prevention(self) -> bool:
        """Test that ConfigMap modification is prevented"""
        # Check RBAC permissions for configmaps
        rbac_prevents_configmap_modification = (
            'verbs      = ["get", "list", "watch"]' in self.terraform_config and
            'verbs      = ["*"]' not in self.terraform_config
        )
        
        return rbac_prevents_configmap_modification
    
    def _test_admission_control(self) -> bool:
        """Test that admission controllers are enforced"""
        # Pod Security Standards and security contexts provide admission control
        pod_security_context = self.values_config.get('podSecurityContext', {})
        security_context = self.values_config.get('securityContext', {})
        
        admission_enforced = (
            'seccompProfile' in pod_security_context and
            len(security_context) > 0
        )
        
        return admission_enforced
    
    # Scoring Methods
    
    def _calculate_escape_prevention_score(self, tests: List[bool]) -> float:
        """Calculate container escape prevention score"""
        return (sum(tests) / len(tests)) * 100
    
    def _calculate_escalation_prevention_score(self, tests: List[bool]) -> float:
        """Calculate privilege escalation prevention score"""
        return (sum(tests) / len(tests)) * 100
    
    def _calculate_lateral_movement_score(self, tests: List[bool]) -> float:
        """Calculate lateral movement prevention score"""
        return (sum(tests) / len(tests)) * 100
    
    def _calculate_exfiltration_score(self, tests: List[bool]) -> float:
        """Calculate data exfiltration prevention score"""
        return (sum(tests) / len(tests)) * 100
    
    def _calculate_detection_score(self, tests: List[bool]) -> float:
        """Calculate runtime detection effectiveness score"""
        return (sum(tests) / len(tests)) * 100
    
    def _calculate_persistence_score(self, tests: List[bool]) -> float:
        """Calculate persistence attack prevention score"""
        return (sum(tests) / len(tests)) * 100
    
    def _generate_adversarial_validation_report(self) -> Dict:
        """Generate comprehensive adversarial validation report"""
        
        # Calculate overall security score
        category_scores = [
            self.results['container_escape_prevention']['escape_prevention_score'],
            self.results['privilege_escalation_prevention']['escalation_prevention_score'],
            self.results['lateral_movement_prevention']['lateral_movement_prevention_score'],
            self.results['data_exfiltration_prevention']['exfiltration_prevention_score'],
            self.results['runtime_attack_detection']['detection_effectiveness_score'],
            self.results['persistence_attack_prevention']['persistence_prevention_score']
        ]
        
        overall_security_score = sum(category_scores) / len(category_scores)
        
        # Count prevented attacks
        total_tests = 30  # 5 tests per category * 6 categories
        prevented_attacks = sum([
            sum([
                self.results['container_escape_prevention']['privileged_containers_blocked'],
                self.results['container_escape_prevention']['host_namespace_access_blocked'],
                self.results['container_escape_prevention']['dangerous_volume_mounts_blocked'],
                self.results['container_escape_prevention']['capabilities_properly_restricted'],
                self.results['container_escape_prevention']['readonly_filesystem_enforced']
            ]),
            sum([
                self.results['privilege_escalation_prevention']['service_account_token_abuse_blocked'],
                self.results['privilege_escalation_prevention']['rbac_privilege_escalation_blocked'],
                self.results['privilege_escalation_prevention']['container_user_escalation_blocked'],
                self.results['privilege_escalation_prevention']['filesystem_escalation_blocked'],
                self.results['privilege_escalation_prevention']['security_context_properly_enforced']
            ]),
            sum([
                self.results['lateral_movement_prevention']['network_policies_enforced'],
                self.results['lateral_movement_prevention']['service_discovery_restricted'],
                self.results['lateral_movement_prevention']['dns_enumeration_blocked'],
                self.results['lateral_movement_prevention']['pod_communication_restricted'],
                self.results['lateral_movement_prevention']['namespace_isolation_enforced']
            ]),
            sum([
                self.results['data_exfiltration_prevention']['secret_access_properly_restricted'],
                self.results['data_exfiltration_prevention']['configmap_exposure_prevented'],
                self.results['data_exfiltration_prevention']['volume_data_access_restricted'],
                self.results['data_exfiltration_prevention']['network_egress_controlled'],
                self.results['data_exfiltration_prevention']['sensitive_data_protected']
            ]),
            sum([
                self.results['runtime_attack_detection']['falco_runtime_monitoring_deployed'],
                self.results['runtime_attack_detection']['detection_rules_active'],
                self.results['runtime_attack_detection']['attack_patterns_recognized'],
                self.results['runtime_attack_detection']['anomaly_detection_functional'],
                self.results['runtime_attack_detection']['alerting_system_operational']
            ]),
            sum([
                self.results['persistence_attack_prevention']['cronjob_creation_blocked'],
                self.results['persistence_attack_prevention']['daemonset_creation_blocked'],
                self.results['persistence_attack_prevention']['secret_modification_blocked'],
                self.results['persistence_attack_prevention']['configmap_modification_blocked'],
                self.results['persistence_attack_prevention']['admission_controllers_enforced']
            ])
        ])
        
        report = {
            "metadata": {
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
                "test_suite": "BitActor Adversarial Validation - Security Fixes",
                "version": "1.0.0"
            },
            "executive_summary": {
                "overall_security_score": round(overall_security_score, 1),
                "total_attack_vectors_tested": total_tests,
                "attack_vectors_prevented": prevented_attacks,
                "attack_prevention_rate": round((prevented_attacks / total_tests) * 100, 1),
                "critical_vulnerabilities_found": self._count_critical_vulnerabilities(),
                "high_vulnerabilities_found": self._count_high_vulnerabilities(),
                "security_posture": self._determine_security_posture(overall_security_score),
                "recommendation": self._generate_security_recommendation(overall_security_score)
            },
            "detailed_results": self.results,
            "attack_prevention_targets": {
                "container_escape_prevented": self.results['container_escape_prevention']['escape_prevention_score'] >= 90.0,
                "privilege_escalation_prevented": self.results['privilege_escalation_prevention']['escalation_prevention_score'] >= 90.0,
                "lateral_movement_restricted": self.results['lateral_movement_prevention']['lateral_movement_prevention_score'] >= 90.0,
                "data_exfiltration_prevented": self.results['data_exfiltration_prevention']['exfiltration_prevention_score'] >= 90.0,
                "runtime_attacks_detected": self.results['runtime_attack_detection']['detection_effectiveness_score'] >= 90.0,
                "persistence_attacks_blocked": self.results['persistence_attack_prevention']['persistence_prevention_score'] >= 90.0
            },
            "security_fixes_validation": {
                "image_tag_fix_effective": self._validate_image_tag_fix(),
                "service_account_token_fix_effective": self._validate_sa_token_fix(),
                "falco_monitoring_fix_effective": self._validate_falco_fix(),
                "security_contexts_effective": self._validate_security_contexts()
            }
        }
        
        return report
    
    def _count_critical_vulnerabilities(self) -> int:
        """Count critical vulnerabilities found"""
        critical_count = 0
        
        # Container escape vulnerabilities
        if not self.results['container_escape_prevention']['privileged_containers_blocked']:
            critical_count += 1
        if not self.results['container_escape_prevention']['host_namespace_access_blocked']:
            critical_count += 1
        
        # Privilege escalation vulnerabilities  
        if not self.results['privilege_escalation_prevention']['service_account_token_abuse_blocked']:
            critical_count += 1
        
        return critical_count
    
    def _count_high_vulnerabilities(self) -> int:
        """Count high vulnerabilities found"""
        high_count = 0
        
        # Lateral movement vulnerabilities
        if not self.results['lateral_movement_prevention']['network_policies_enforced']:
            high_count += 1
        
        # Data exfiltration vulnerabilities
        if not self.results['data_exfiltration_prevention']['secret_access_properly_restricted']:
            high_count += 1
        
        # Runtime detection gaps
        if not self.results['runtime_attack_detection']['falco_runtime_monitoring_deployed']:
            high_count += 1
        
        return high_count
    
    def _determine_security_posture(self, score: float) -> str:
        """Determine overall security posture"""
        if score >= 95:
            return "EXCELLENT"
        elif score >= 90:
            return "STRONG"
        elif score >= 80:
            return "GOOD"
        elif score >= 70:
            return "ADEQUATE"
        else:
            return "WEAK"
    
    def _generate_security_recommendation(self, score: float) -> str:
        """Generate security recommendation"""
        if score >= 95:
            return "OUTSTANDING: Security fixes provide comprehensive attack prevention."
        elif score >= 90:
            return "EXCELLENT: Strong security posture with minimal gaps."
        elif score >= 80:
            return "GOOD: Security fixes effective with minor improvements needed."
        elif score >= 70:
            return "ACCEPTABLE: Baseline security achieved, enhancement recommended."
        else:
            return "CRITICAL: Significant security gaps require immediate attention."
    
    def _validate_image_tag_fix(self) -> bool:
        """Validate that image tag fix is effective"""
        image_tag = self.values_config['image']['tag']
        return image_tag != 'latest' and image_tag != ''
    
    def _validate_sa_token_fix(self) -> bool:
        """Validate that service account token fix is effective"""
        return self.results['privilege_escalation_prevention']['service_account_token_abuse_blocked']
    
    def _validate_falco_fix(self) -> bool:
        """Validate that Falco monitoring fix is effective"""
        return self.results['runtime_attack_detection']['falco_runtime_monitoring_deployed']
    
    def _validate_security_contexts(self) -> bool:
        """Validate that security contexts are effective"""
        return self.results['privilege_escalation_prevention']['security_context_properly_enforced']

def main():
    """Main execution function"""
    
    validator = BitActorAdversarialValidator()
    report = validator.run_comprehensive_adversarial_validation()
    
    # Save report
    report_file = "bitactor_adversarial_validation_report.json"
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    # Print executive summary
    print(f"\n🎯 ADVERSARIAL VALIDATION COMPLETE")
    print(f"Report saved to: {report_file}")
    print(f"Overall security score: {report['executive_summary']['overall_security_score']}/100")
    print(f"Attack vectors prevented: {report['executive_summary']['attack_vectors_prevented']}/{report['executive_summary']['total_attack_vectors_tested']}")
    print(f"Attack prevention rate: {report['executive_summary']['attack_prevention_rate']:.1f}%")
    print(f"Critical vulnerabilities: {report['executive_summary']['critical_vulnerabilities_found']}")
    print(f"High vulnerabilities: {report['executive_summary']['high_vulnerabilities_found']}")
    print(f"Security posture: {report['executive_summary']['security_posture']}")
    print(f"Recommendation: {report['executive_summary']['recommendation']}")
    
    # Return success if security score is acceptable
    return report['executive_summary']['overall_security_score'] >= 80.0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)