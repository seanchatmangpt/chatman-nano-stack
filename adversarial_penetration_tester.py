#!/usr/bin/env python3
"""
BitActor Kubernetes Adversarial Penetration Testing Framework

This script performs comprehensive penetration testing against the BitActor K8s deployment
to identify container escape vectors, privilege escalation paths, and security vulnerabilities.

CRITICAL: This tool is for defensive security testing only. Use only in authorized environments.
"""

import json
import subprocess
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional
import base64
import tempfile

class AttackVector(Enum):
    CONTAINER_ESCAPE = "CONTAINER_ESCAPE"
    PRIVILEGE_ESCALATION = "PRIVILEGE_ESCALATION"
    LATERAL_MOVEMENT = "LATERAL_MOVEMENT"
    DATA_EXFILTRATION = "DATA_EXFILTRATION"
    PERSISTENCE = "PERSISTENCE"
    RECONNAISSANCE = "RECONNAISSANCE"

class Severity(Enum):
    CRITICAL = "CRITICAL"
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"

@dataclass
class PenTestResult:
    """Penetration test result"""
    attack_vector: AttackVector
    test_name: str
    success: bool
    severity: Severity
    description: str
    evidence: List[str]
    remediation: str
    cvss_score: float

class AdversarialPenetrationTester:
    """Comprehensive penetration testing framework for K8s"""
    
    def __init__(self, namespace: str = "bitactor"):
        self.namespace = namespace
        self.results: List[PenTestResult] = []
        self.target_pods: List[str] = []
        
    def run_penetration_campaign(self) -> Dict:
        """Execute comprehensive penetration testing campaign"""
        print("🎯 ADVERSARIAL PENETRATION TESTING - SECURITY VALIDATION")
        print("=" * 70)
        
        # Phase 0: Reconnaissance
        print("\n🔍 PHASE 0: RECONNAISSANCE & TARGET DISCOVERY")
        self._reconnaissance_phase()
        
        # Phase 1: Container Escape Testing
        print("\n📦 PHASE 1: CONTAINER ESCAPE ATTACK VECTORS")
        self._test_container_escape()
        
        # Phase 2: Privilege Escalation
        print("\n⚡ PHASE 2: PRIVILEGE ESCALATION TESTING")
        self._test_privilege_escalation()
        
        # Phase 3: Lateral Movement
        print("\n🌐 PHASE 3: LATERAL MOVEMENT & NETWORK ATTACKS")
        self._test_lateral_movement()
        
        # Phase 4: Data Exfiltration
        print("\n💾 PHASE 4: DATA EXFILTRATION VECTORS")
        self._test_data_exfiltration()
        
        # Phase 5: Persistence Mechanisms
        print("\n🔄 PHASE 5: PERSISTENCE & BACKDOOR TESTING")
        self._test_persistence()
        
        return self._generate_pentest_report()
    
    def _reconnaissance_phase(self):
        """Gather intelligence about the target environment"""
        
        print("🔍 Discovering target pods...")
        self._discover_targets()
        
        print("🔍 Analyzing service accounts...")
        self._analyze_service_accounts()
        
        print("🔍 Mapping network topology...")
        self._map_network_topology()
        
        print("🔍 Identifying attack surface...")
        self._identify_attack_surface()
    
    def _discover_targets(self):
        """Discover target pods and services"""
        
        try:
            # Get all pods in namespace
            result = subprocess.run([
                "kubectl", "get", "pods",
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                pods_data = json.loads(result.stdout)
                self.target_pods = [
                    pod["metadata"]["name"] 
                    for pod in pods_data.get("items", [])
                    if pod.get("status", {}).get("phase") == "Running"
                ]
                
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.RECONNAISSANCE,
                    test_name="Target Discovery",
                    success=True,
                    severity=Severity.LOW,
                    description=f"Discovered {len(self.target_pods)} running target pods",
                    evidence=[f"Pods: {', '.join(self.target_pods)}"],
                    remediation="Normal reconnaissance - no action needed",
                    cvss_score=2.0
                ))
                
                print(f"  ✅ Discovered {len(self.target_pods)} target pods")
            else:
                print(f"  ❌ Failed to discover targets: {result.stderr}")
                
        except Exception as e:
            print(f"  ❌ Target discovery failed: {e}")
    
    def _analyze_service_accounts(self):
        """Analyze service account permissions and tokens"""
        
        try:
            # Check service account permissions
            result = subprocess.run([
                "kubectl", "auth", "can-i", "--list",
                "-n", self.namespace,
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                permissions = result.stdout
                
                # Check for dangerous permissions
                dangerous_perms = ["*", "create", "delete", "patch"]
                found_dangerous = [
                    perm for perm in dangerous_perms 
                    if perm in permissions.lower()
                ]
                
                if found_dangerous:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                        test_name="Service Account Permission Analysis",
                        success=True,
                        severity=Severity.HIGH,
                        description="Service account has potentially dangerous permissions",
                        evidence=[f"Permissions: {', '.join(found_dangerous)}"],
                        remediation="Implement least-privilege service account permissions",
                        cvss_score=7.5
                    ))
                    print(f"  🚨 Found dangerous permissions: {', '.join(found_dangerous)}")
                else:
                    print(f"  ✅ Service account permissions appear restricted")
                    
        except Exception as e:
            print(f"  ❌ Service account analysis failed: {e}")
    
    def _map_network_topology(self):
        """Map network topology and identify communication paths"""
        
        try:
            # Check network policies
            result = subprocess.run([
                "kubectl", "get", "networkpolicy",
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                policies_data = json.loads(result.stdout)
                policy_count = len(policies_data.get("items", []))
                
                if policy_count == 0:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.LATERAL_MOVEMENT,
                        test_name="Network Policy Analysis",
                        success=True,
                        severity=Severity.HIGH,
                        description="No network policies found - unrestricted pod communication",
                        evidence=["Zero NetworkPolicy resources"],
                        remediation="Implement network policies to restrict pod communication",
                        cvss_score=7.3
                    ))
                    print(f"  🚨 No network policies - unrestricted communication possible")
                else:
                    print(f"  ✅ Found {policy_count} network policies")
                    
        except Exception as e:
            print(f"  ❌ Network topology mapping failed: {e}")
    
    def _identify_attack_surface(self):
        """Identify exposed services and attack surface"""
        
        try:
            # Check for exposed services
            result = subprocess.run([
                "kubectl", "get", "services",
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                services_data = json.loads(result.stdout)
                
                for service in services_data.get("items", []):
                    service_type = service.get("spec", {}).get("type", "ClusterIP")
                    service_name = service.get("metadata", {}).get("name", "unknown")
                    
                    if service_type in ["NodePort", "LoadBalancer"]:
                        self.results.append(PenTestResult(
                            attack_vector=AttackVector.RECONNAISSANCE,
                            test_name="Exposed Service Discovery",
                            success=True,
                            severity=Severity.MEDIUM,
                            description=f"Service {service_name} exposed externally via {service_type}",
                            evidence=[f"Service: {service_name}, Type: {service_type}"],
                            remediation="Consider using Ingress controller for controlled access",
                            cvss_score=5.4
                        ))
                        print(f"  🔍 Found exposed service: {service_name} ({service_type})")
                        
        except Exception as e:
            print(f"  ❌ Attack surface identification failed: {e}")
    
    def _test_container_escape(self):
        """Test container escape attack vectors"""
        
        if not self.target_pods:
            print("  ❌ No target pods available for testing")
            return
        
        target_pod = self.target_pods[0]  # Use first pod as target
        
        # Test 1: Check for privileged containers
        self._test_privileged_containers(target_pod)
        
        # Test 2: Check for dangerous volume mounts
        self._test_dangerous_volume_mounts(target_pod)
        
        # Test 3: Check for writable /proc or /sys mounts
        self._test_proc_sys_mounts(target_pod)
        
        # Test 4: Test container capabilities
        self._test_container_capabilities(target_pod)
        
        # Test 5: Test host namespace access
        self._test_host_namespace_access(target_pod)
    
    def _test_privileged_containers(self, pod_name: str):
        """Test for privileged container execution"""
        
        try:
            result = subprocess.run([
                "kubectl", "get", "pod", pod_name,
                "-n", self.namespace,
                "-o", "jsonpath={.spec.containers[*].securityContext.privileged}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                if "true" in result.stdout:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Privileged Container Check",
                        success=True,
                        severity=Severity.CRITICAL,
                        description="Container running in privileged mode",
                        evidence=[f"Pod {pod_name} has privileged: true"],
                        remediation="Remove privileged: true, use specific capabilities instead",
                        cvss_score=9.8
                    ))
                    print(f"  🚨 CRITICAL: Privileged container detected in {pod_name}")
                else:
                    print(f"  ✅ No privileged containers detected")
                    
        except Exception as e:
            print(f"  ❌ Privileged container check failed: {e}")
    
    def _test_dangerous_volume_mounts(self, pod_name: str):
        """Test for dangerous host volume mounts"""
        
        try:
            result = subprocess.run([
                "kubectl", "get", "pod", pod_name,
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                pod_data = json.loads(result.stdout)
                containers = pod_data.get("spec", {}).get("containers", [])
                
                dangerous_mounts = [
                    "/var/run/docker.sock",
                    "/proc",
                    "/sys",
                    "/dev",
                    "/host",
                    "/etc/passwd",
                    "/etc/shadow"
                ]
                
                for container in containers:
                    volume_mounts = container.get("volumeMounts", [])
                    for mount in volume_mounts:
                        mount_path = mount.get("mountPath", "")
                        
                        for dangerous_path in dangerous_mounts:
                            if dangerous_path in mount_path:
                                severity = Severity.CRITICAL if dangerous_path == "/var/run/docker.sock" else Severity.HIGH
                                cvss = 9.1 if dangerous_path == "/var/run/docker.sock" else 7.8
                                
                                self.results.append(PenTestResult(
                                    attack_vector=AttackVector.CONTAINER_ESCAPE,
                                    test_name="Dangerous Volume Mount Check",
                                    success=True,
                                    severity=severity,
                                    description=f"Dangerous host path mounted: {mount_path}",
                                    evidence=[f"Pod: {pod_name}, Mount: {mount_path}"],
                                    remediation="Remove dangerous host volume mounts",
                                    cvss_score=cvss
                                ))
                                print(f"  🚨 Dangerous mount detected: {mount_path}")
                
        except Exception as e:
            print(f"  ❌ Volume mount check failed: {e}")
    
    def _test_proc_sys_mounts(self, pod_name: str):
        """Test for writable /proc or /sys mounts"""
        
        try:
            # Test if we can write to /proc or /sys from within the container
            test_command = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "test -w /proc && echo 'WRITABLE_PROC' || echo 'READONLY_PROC'; test -w /sys && echo 'WRITABLE_SYS' || echo 'READONLY_SYS'"
            ]
            
            result = subprocess.run(test_command, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                output = result.stdout.strip()
                
                if "WRITABLE_PROC" in output:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Writable /proc Check",
                        success=True,
                        severity=Severity.HIGH,
                        description="Container has write access to /proc filesystem",
                        evidence=[f"Pod {pod_name} can write to /proc"],
                        remediation="Mount /proc as read-only",
                        cvss_score=8.2
                    ))
                    print(f"  🚨 Writable /proc detected in {pod_name}")
                
                if "WRITABLE_SYS" in output:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Writable /sys Check",
                        success=True,
                        severity=Severity.HIGH,
                        description="Container has write access to /sys filesystem",
                        evidence=[f"Pod {pod_name} can write to /sys"],
                        remediation="Mount /sys as read-only",
                        cvss_score=8.0
                    ))
                    print(f"  🚨 Writable /sys detected in {pod_name}")
                    
                if "READONLY_PROC" in output and "READONLY_SYS" in output:
                    print(f"  ✅ /proc and /sys properly protected")
                    
        except Exception as e:
            print(f"  ❌ /proc /sys mount check failed: {e}")
    
    def _test_container_capabilities(self, pod_name: str):
        """Test container Linux capabilities"""
        
        try:
            # Check capabilities from pod spec
            result = subprocess.run([
                "kubectl", "get", "pod", pod_name,
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                pod_data = json.loads(result.stdout)
                containers = pod_data.get("spec", {}).get("containers", [])
                
                dangerous_caps = ["SYS_ADMIN", "SYS_PTRACE", "SYS_MODULE", "DAC_OVERRIDE", "SETUID", "SETGID"]
                
                for container in containers:
                    security_context = container.get("securityContext", {})
                    capabilities = security_context.get("capabilities", {})
                    add_caps = capabilities.get("add", [])
                    drop_caps = capabilities.get("drop", [])
                    
                    # Check for dangerous capabilities
                    found_dangerous = [cap for cap in add_caps if cap in dangerous_caps]
                    
                    if found_dangerous:
                        self.results.append(PenTestResult(
                            attack_vector=AttackVector.CONTAINER_ESCAPE,
                            test_name="Dangerous Capabilities Check",
                            success=True,
                            severity=Severity.HIGH,
                            description=f"Container has dangerous capabilities: {found_dangerous}",
                            evidence=[f"Pod: {pod_name}, Capabilities: {found_dangerous}"],
                            remediation="Remove dangerous capabilities, use minimal set",
                            cvss_score=8.2
                        ))
                        print(f"  🚨 Dangerous capabilities: {', '.join(found_dangerous)}")
                    
                    # Check if ALL capabilities are dropped
                    if "ALL" not in drop_caps:
                        self.results.append(PenTestResult(
                            attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                            test_name="Capabilities Not Dropped",
                            success=True,
                            severity=Severity.MEDIUM,
                            description="Container retains unnecessary Linux capabilities",
                            evidence=[f"Pod: {pod_name}, Drop: {drop_caps}"],
                            remediation="Drop ALL capabilities by default",
                            cvss_score=6.4
                        ))
                        print(f"  ⚠️  Not all capabilities dropped")
                    else:
                        print(f"  ✅ All capabilities properly dropped")
                        
        except Exception as e:
            print(f"  ❌ Capabilities check failed: {e}")
    
    def _test_host_namespace_access(self, pod_name: str):
        """Test for host namespace access"""
        
        try:
            result = subprocess.run([
                "kubectl", "get", "pod", pod_name,
                "-n", self.namespace,
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                pod_data = json.loads(result.stdout)
                spec = pod_data.get("spec", {})
                
                # Check host network
                if spec.get("hostNetwork", False):
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Host Network Access",
                        success=True,
                        severity=Severity.HIGH,
                        description="Pod uses host network namespace",
                        evidence=[f"Pod {pod_name} has hostNetwork: true"],
                        remediation="Remove hostNetwork: true",
                        cvss_score=8.0
                    ))
                    print(f"  🚨 Host network access detected")
                
                # Check host PID
                if spec.get("hostPID", False):
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Host PID Access",
                        success=True,
                        severity=Severity.HIGH,
                        description="Pod uses host PID namespace",
                        evidence=[f"Pod {pod_name} has hostPID: true"],
                        remediation="Remove hostPID: true",
                        cvss_score=7.9
                    ))
                    print(f"  🚨 Host PID access detected")
                
                # Check host IPC
                if spec.get("hostIPC", False):
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.CONTAINER_ESCAPE,
                        test_name="Host IPC Access",
                        success=True,
                        severity=Severity.MEDIUM,
                        description="Pod uses host IPC namespace",
                        evidence=[f"Pod {pod_name} has hostIPC: true"],
                        remediation="Remove hostIPC: true",
                        cvss_score=6.8
                    ))
                    print(f"  🚨 Host IPC access detected")
                    
        except Exception as e:
            print(f"  ❌ Host namespace check failed: {e}")
    
    def _test_privilege_escalation(self):
        """Test privilege escalation attack vectors"""
        
        if not self.target_pods:
            print("  ❌ No target pods available for testing")
            return
        
        target_pod = self.target_pods[0]
        
        # Test 1: Service account token abuse
        self._test_service_account_token_abuse(target_pod)
        
        # Test 2: RBAC privilege escalation
        self._test_rbac_privilege_escalation(target_pod)
        
        # Test 3: Container user escalation
        self._test_container_user_escalation(target_pod)
        
        # Test 4: File system privilege escalation
        self._test_filesystem_privilege_escalation(target_pod)
    
    def _test_service_account_token_abuse(self, pod_name: str):
        """Test service account token abuse"""
        
        try:
            # Check if service account token is mounted and accessible
            test_command = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "ls -la /var/run/secrets/kubernetes.io/serviceaccount/ 2>/dev/null && echo 'TOKEN_MOUNTED' || echo 'NO_TOKEN'"
            ]
            
            result = subprocess.run(test_command, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "TOKEN_MOUNTED" in result.stdout:
                # Try to use the token
                token_test = [
                    "kubectl", "exec", pod_name,
                    "-n", self.namespace,
                    "--", "sh", "-c",
                    "TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token); curl -s -k -H \"Authorization: Bearer $TOKEN\" https://kubernetes.default.svc/api/v1/namespaces 2>/dev/null | grep -q 'items' && echo 'TOKEN_WORKS' || echo 'TOKEN_BLOCKED'"
                ]
                
                token_result = subprocess.run(token_test, capture_output=True, text=True, timeout=30)
                
                if token_result.returncode == 0 and "TOKEN_WORKS" in token_result.stdout:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                        test_name="Service Account Token Abuse",
                        success=True,
                        severity=Severity.HIGH,
                        description="Service account token can access Kubernetes API",
                        evidence=[f"Pod {pod_name} can use mounted SA token"],
                        remediation="Set automountServiceAccountToken: false if not needed",
                        cvss_score=7.6
                    ))
                    print(f"  🚨 Service account token abuse possible")
                else:
                    print(f"  ✅ Service account token properly restricted")
            else:
                print(f"  ✅ No service account token mounted")
                
        except Exception as e:
            print(f"  ❌ Service account token test failed: {e}")
    
    def _test_rbac_privilege_escalation(self, pod_name: str):
        """Test RBAC privilege escalation paths"""
        
        try:
            # Test what the pod's service account can do
            test_commands = [
                ("pods", "list", "can list pods"),
                ("secrets", "get", "can access secrets"),
                ("*", "*", "has wildcard permissions"),
                ("nodes", "list", "can list nodes")
            ]
            
            for resource, verb, description in test_commands:
                auth_test = [
                    "kubectl", "auth", "can-i", verb, resource,
                    "-n", self.namespace,
                    "--as=system:serviceaccount:" + self.namespace + ":default"
                ]
                
                result = subprocess.run(auth_test, capture_output=True, text=True, timeout=30)
                
                if result.returncode == 0 and "yes" in result.stdout.lower():
                    severity = Severity.HIGH if resource == "*" or resource == "secrets" else Severity.MEDIUM
                    cvss = 8.5 if resource == "*" else 7.0 if resource == "secrets" else 5.5
                    
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                        test_name=f"RBAC Escalation - {resource}",
                        success=True,
                        severity=severity,
                        description=f"Service account {description}",
                        evidence=[f"can-i {verb} {resource}: yes"],
                        remediation="Implement least-privilege RBAC",
                        cvss_score=cvss
                    ))
                    print(f"  🚨 RBAC escalation: {description}")
                    
        except Exception as e:
            print(f"  ❌ RBAC escalation test failed: {e}")
    
    def _test_container_user_escalation(self, pod_name: str):
        """Test container user privilege escalation"""
        
        try:
            # Check current user in container
            user_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "id"
            ]
            
            result = subprocess.run(user_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                output = result.stdout.strip()
                
                if "uid=0(root)" in output:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                        test_name="Root User Detection",
                        success=True,
                        severity=Severity.HIGH,
                        description="Container running as root user",
                        evidence=[f"Pod {pod_name}: {output}"],
                        remediation="Configure runAsNonRoot: true and specify runAsUser",
                        cvss_score=7.8
                    ))
                    print(f"  🚨 Container running as root")
                else:
                    print(f"  ✅ Container running as non-root user")
                    
        except Exception as e:
            print(f"  ❌ User escalation test failed: {e}")
    
    def _test_filesystem_privilege_escalation(self, pod_name: str):
        """Test filesystem-based privilege escalation"""
        
        try:
            # Check for writable filesystem
            fs_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "touch /test_write 2>/dev/null && rm /test_write && echo 'WRITABLE_ROOT' || echo 'READONLY_ROOT'"
            ]
            
            result = subprocess.run(fs_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                if "WRITABLE_ROOT" in result.stdout:
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.PRIVILEGE_ESCALATION,
                        test_name="Writable Root Filesystem",
                        success=True,
                        severity=Severity.MEDIUM,
                        description="Container has writable root filesystem",
                        evidence=[f"Pod {pod_name} can write to /"],
                        remediation="Set readOnlyRootFilesystem: true",
                        cvss_score=6.2
                    ))
                    print(f"  ⚠️  Writable root filesystem detected")
                else:
                    print(f"  ✅ Read-only root filesystem")
                    
        except Exception as e:
            print(f"  ❌ Filesystem escalation test failed: {e}")
    
    def _test_lateral_movement(self):
        """Test lateral movement attack vectors"""
        
        if not self.target_pods:
            print("  ❌ No target pods available for testing")
            return
        
        target_pod = self.target_pods[0]
        
        # Test 1: Network connectivity
        self._test_network_connectivity(target_pod)
        
        # Test 2: DNS enumeration
        self._test_dns_enumeration(target_pod)
        
        # Test 3: Service discovery
        self._test_service_discovery(target_pod)
        
        # Test 4: Pod-to-pod communication
        self._test_pod_communication(target_pod)
    
    def _test_network_connectivity(self, pod_name: str):
        """Test network connectivity for lateral movement"""
        
        try:
            # Test connectivity to other services
            targets = [
                ("kubernetes.default.svc", "443", "Kubernetes API"),
                ("8.8.8.8", "53", "External DNS"),
                ("169.254.169.254", "80", "Metadata service")
            ]
            
            for target, port, description in targets:
                conn_test = [
                    "kubectl", "exec", pod_name,
                    "-n", self.namespace,
                    "--", "sh", "-c",
                    f"timeout 5 nc -z {target} {port} 2>/dev/null && echo 'CONNECTED' || echo 'BLOCKED'"
                ]
                
                result = subprocess.run(conn_test, capture_output=True, text=True, timeout=30)
                
                if result.returncode == 0 and "CONNECTED" in result.stdout:
                    severity = Severity.HIGH if "metadata" in description.lower() else Severity.MEDIUM
                    cvss = 7.2 if "metadata" in description.lower() else 5.8
                    
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.LATERAL_MOVEMENT,
                        test_name=f"Network Connectivity - {description}",
                        success=True,
                        severity=severity,
                        description=f"Pod can connect to {description}",
                        evidence=[f"Connection to {target}:{port} successful"],
                        remediation="Implement network policies to restrict connectivity",
                        cvss_score=cvss
                    ))
                    print(f"  🔍 Can connect to {description}")
                    
        except Exception as e:
            print(f"  ❌ Network connectivity test failed: {e}")
    
    def _test_dns_enumeration(self, pod_name: str):
        """Test DNS enumeration capabilities"""
        
        try:
            # Test DNS queries for enumeration
            dns_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "nslookup kubernetes.default.svc 2>/dev/null | grep -q 'Address' && echo 'DNS_WORKS' || echo 'DNS_BLOCKED'"
            ]
            
            result = subprocess.run(dns_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "DNS_WORKS" in result.stdout:
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.LATERAL_MOVEMENT,
                    test_name="DNS Enumeration",
                    success=True,
                    severity=Severity.MEDIUM,
                    description="Pod can perform DNS enumeration",
                    evidence=["DNS queries to kubernetes.default.svc successful"],
                    remediation="Consider DNS filtering or monitoring",
                    cvss_score=5.4
                ))
                print(f"  🔍 DNS enumeration possible")
            else:
                print(f"  ✅ DNS queries restricted")
                
        except Exception as e:
            print(f"  ❌ DNS enumeration test failed: {e}")
    
    def _test_service_discovery(self, pod_name: str):
        """Test service discovery capabilities"""
        
        try:
            # Test service discovery via environment variables
            env_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "env | grep -i service | wc -l"
            ]
            
            result = subprocess.run(env_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                service_count = int(result.stdout.strip()) if result.stdout.strip().isdigit() else 0
                
                if service_count > 5:  # Arbitrary threshold
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.LATERAL_MOVEMENT,
                        test_name="Service Discovery via Environment",
                        success=True,
                        severity=Severity.LOW,
                        description=f"Pod can discover {service_count} services via environment variables",
                        evidence=[f"Found {service_count} service environment variables"],
                        remediation="Consider limiting service environment variables",
                        cvss_score=4.1
                    ))
                    print(f"  🔍 Can discover {service_count} services via env vars")
                    
        except Exception as e:
            print(f"  ❌ Service discovery test failed: {e}")
    
    def _test_pod_communication(self, pod_name: str):
        """Test pod-to-pod communication"""
        
        try:
            # Get other pods in namespace for communication test
            result = subprocess.run([
                "kubectl", "get", "pods",
                "-n", self.namespace,
                "-o", "jsonpath={.items[*].status.podIP}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and result.stdout.strip():
                pod_ips = result.stdout.strip().split()
                
                if len(pod_ips) > 1:  # More than just our target pod
                    test_ip = pod_ips[0] if pod_ips[0] != pod_name else pod_ips[1]
                    
                    comm_test = [
                        "kubectl", "exec", pod_name,
                        "-n", self.namespace,
                        "--", "sh", "-c",
                        f"timeout 3 nc -z {test_ip} 9090 2>/dev/null && echo 'CONNECTED' || echo 'BLOCKED'"
                    ]
                    
                    comm_result = subprocess.run(comm_test, capture_output=True, text=True, timeout=30)
                    
                    if comm_result.returncode == 0 and "CONNECTED" in comm_result.stdout:
                        self.results.append(PenTestResult(
                            attack_vector=AttackVector.LATERAL_MOVEMENT,
                            test_name="Pod-to-Pod Communication",
                            success=True,
                            severity=Severity.MEDIUM,
                            description="Unrestricted pod-to-pod communication",
                            evidence=[f"Connection from {pod_name} to {test_ip}:9090"],
                            remediation="Implement network policies for pod isolation",
                            cvss_score=6.1
                        ))
                        print(f"  🔍 Unrestricted pod-to-pod communication")
                    else:
                        print(f"  ✅ Pod-to-pod communication restricted")
                        
        except Exception as e:
            print(f"  ❌ Pod communication test failed: {e}")
    
    def _test_data_exfiltration(self):
        """Test data exfiltration vectors"""
        
        if not self.target_pods:
            print("  ❌ No target pods available for testing")
            return
        
        target_pod = self.target_pods[0]
        
        # Test 1: Secret access
        self._test_secret_access(target_pod)
        
        # Test 2: ConfigMap access
        self._test_configmap_access(target_pod)
        
        # Test 3: Volume data access
        self._test_volume_data_access(target_pod)
        
        # Test 4: Network exfiltration
        self._test_network_exfiltration(target_pod)
    
    def _test_secret_access(self, pod_name: str):
        """Test access to Kubernetes secrets"""
        
        try:
            # Check if pod can access secrets in namespace
            secret_test = [
                "kubectl", "auth", "can-i", "get", "secrets",
                "-n", self.namespace,
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ]
            
            result = subprocess.run(secret_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "yes" in result.stdout.lower():
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.DATA_EXFILTRATION,
                    test_name="Secret Access Test",
                    success=True,
                    severity=Severity.HIGH,
                    description="Pod can access Kubernetes secrets",
                    evidence=["Service account can get secrets"],
                    remediation="Restrict secret access to necessary services only",
                    cvss_score=8.1
                ))
                print(f"  🚨 Can access secrets")
            else:
                print(f"  ✅ Secret access restricted")
                
        except Exception as e:
            print(f"  ❌ Secret access test failed: {e}")
    
    def _test_configmap_access(self, pod_name: str):
        """Test access to ConfigMaps"""
        
        try:
            # Check mounted ConfigMaps
            cm_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "find /etc -name '*.yaml' -o -name '*.yml' 2>/dev/null | head -5"
            ]
            
            result = subprocess.run(cm_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and result.stdout.strip():
                config_files = result.stdout.strip().split('\n')
                
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.DATA_EXFILTRATION,
                    test_name="ConfigMap Access",
                    success=True,
                    severity=Severity.MEDIUM,
                    description=f"Pod has access to {len(config_files)} configuration files",
                    evidence=[f"Config files: {', '.join(config_files[:3])}"],
                    remediation="Review ConfigMap contents for sensitive data",
                    cvss_score=5.7
                ))
                print(f"  🔍 Can access {len(config_files)} config files")
                
        except Exception as e:
            print(f"  ❌ ConfigMap access test failed: {e}")
    
    def _test_volume_data_access(self, pod_name: str):
        """Test access to volume data"""
        
        try:
            # Check volume mounts for data
            volume_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "df -h | grep -v tmpfs | tail -n +2"
            ]
            
            result = subprocess.run(volume_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and result.stdout.strip():
                volumes = len(result.stdout.strip().split('\n'))
                
                if volumes > 2:  # More than root and one other
                    self.results.append(PenTestResult(
                        attack_vector=AttackVector.DATA_EXFILTRATION,
                        test_name="Volume Data Access",
                        success=True,
                        severity=Severity.MEDIUM,
                        description=f"Pod has access to {volumes} mounted volumes",
                        evidence=[f"Volume count: {volumes}"],
                        remediation="Review volume contents for sensitive data",
                        cvss_score=5.3
                    ))
                    print(f"  🔍 Can access {volumes} mounted volumes")
                    
        except Exception as e:
            print(f"  ❌ Volume data access test failed: {e}")
    
    def _test_network_exfiltration(self, pod_name: str):
        """Test network-based data exfiltration"""
        
        try:
            # Test external network connectivity for exfiltration
            external_test = [
                "kubectl", "exec", pod_name,
                "-n", self.namespace,
                "--", "sh", "-c",
                "timeout 5 nc -z 8.8.8.8 53 2>/dev/null && echo 'EXTERNAL_ACCESS' || echo 'BLOCKED'"
            ]
            
            result = subprocess.run(external_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "EXTERNAL_ACCESS" in result.stdout:
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.DATA_EXFILTRATION,
                    test_name="External Network Access",
                    success=True,
                    severity=Severity.MEDIUM,
                    description="Pod can access external networks for data exfiltration",
                    evidence=["Connection to 8.8.8.8:53 successful"],
                    remediation="Implement egress network policies",
                    cvss_score=6.3
                ))
                print(f"  🔍 External network access possible")
            else:
                print(f"  ✅ External network access blocked")
                
        except Exception as e:
            print(f"  ❌ Network exfiltration test failed: {e}")
    
    def _test_persistence(self):
        """Test persistence mechanism vectors"""
        
        # Test 1: CronJob creation
        self._test_cronjob_persistence()
        
        # Test 2: DaemonSet creation
        self._test_daemonset_persistence()
        
        # Test 3: Secret modification
        self._test_secret_persistence()
        
        # Test 4: ConfigMap modification
        self._test_configmap_persistence()
    
    def _test_cronjob_persistence(self):
        """Test CronJob creation for persistence"""
        
        try:
            # Test if service account can create cronjobs
            cronjob_test = [
                "kubectl", "auth", "can-i", "create", "cronjobs",
                "-n", self.namespace,
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ]
            
            result = subprocess.run(cronjob_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "yes" in result.stdout.lower():
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.PERSISTENCE,
                    test_name="CronJob Creation Test",
                    success=True,
                    severity=Severity.HIGH,
                    description="Service account can create CronJobs for persistence",
                    evidence=["can-i create cronjobs: yes"],
                    remediation="Restrict CronJob creation permissions",
                    cvss_score=7.8
                ))
                print(f"  🚨 Can create CronJobs for persistence")
            else:
                print(f"  ✅ CronJob creation restricted")
                
        except Exception as e:
            print(f"  ❌ CronJob persistence test failed: {e}")
    
    def _test_daemonset_persistence(self):
        """Test DaemonSet creation for persistence"""
        
        try:
            # Test if service account can create daemonsets
            ds_test = [
                "kubectl", "auth", "can-i", "create", "daemonsets",
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ]
            
            result = subprocess.run(ds_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "yes" in result.stdout.lower():
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.PERSISTENCE,
                    test_name="DaemonSet Creation Test",
                    success=True,
                    severity=Severity.CRITICAL,
                    description="Service account can create DaemonSets for cluster-wide persistence",
                    evidence=["can-i create daemonsets: yes"],
                    remediation="Strictly control DaemonSet creation permissions",
                    cvss_score=9.2
                ))
                print(f"  🚨 Can create DaemonSets for persistence")
            else:
                print(f"  ✅ DaemonSet creation restricted")
                
        except Exception as e:
            print(f"  ❌ DaemonSet persistence test failed: {e}")
    
    def _test_secret_persistence(self):
        """Test secret modification for persistence"""
        
        try:
            # Test if service account can patch secrets
            secret_patch_test = [
                "kubectl", "auth", "can-i", "patch", "secrets",
                "-n", self.namespace,
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ]
            
            result = subprocess.run(secret_patch_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "yes" in result.stdout.lower():
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.PERSISTENCE,
                    test_name="Secret Modification Test",
                    success=True,
                    severity=Severity.HIGH,
                    description="Service account can modify secrets for persistence",
                    evidence=["can-i patch secrets: yes"],
                    remediation="Restrict secret modification permissions",
                    cvss_score=8.0
                ))
                print(f"  🚨 Can modify secrets for persistence")
            else:
                print(f"  ✅ Secret modification restricted")
                
        except Exception as e:
            print(f"  ❌ Secret persistence test failed: {e}")
    
    def _test_configmap_persistence(self):
        """Test ConfigMap modification for persistence"""
        
        try:
            # Test if service account can patch configmaps
            cm_patch_test = [
                "kubectl", "auth", "can-i", "patch", "configmaps",
                "-n", self.namespace,
                "--as=system:serviceaccount:" + self.namespace + ":default"
            ]
            
            result = subprocess.run(cm_patch_test, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and "yes" in result.stdout.lower():
                self.results.append(PenTestResult(
                    attack_vector=AttackVector.PERSISTENCE,
                    test_name="ConfigMap Modification Test",
                    success=True,
                    severity=Severity.MEDIUM,
                    description="Service account can modify ConfigMaps for persistence",
                    evidence=["can-i patch configmaps: yes"],
                    remediation="Restrict ConfigMap modification permissions",
                    cvss_score=6.5
                ))
                print(f"  🚨 Can modify ConfigMaps for persistence")
            else:
                print(f"  ✅ ConfigMap modification restricted")
                
        except Exception as e:
            print(f"  ❌ ConfigMap persistence test failed: {e}")
    
    def _generate_pentest_report(self) -> Dict:
        """Generate comprehensive penetration testing report"""
        
        # Sort results by severity and CVSS score
        severity_order = {Severity.CRITICAL: 0, Severity.HIGH: 1, Severity.MEDIUM: 2, Severity.LOW: 3}
        self.results.sort(key=lambda x: (severity_order[x.severity], -x.cvss_score))
        
        # Calculate statistics
        total_tests = len(self.results)
        successful_attacks = len([r for r in self.results if r.success])
        
        # Count by severity
        severity_counts = {}
        for severity in Severity:
            severity_counts[severity.value] = len([r for r in self.results if r.severity == severity])
        
        # Count by attack vector
        vector_counts = {}
        for vector in AttackVector:
            vector_counts[vector.value] = len([r for r in self.results if r.attack_vector == vector])
        
        # Calculate risk score
        if self.results:
            total_cvss = sum(r.cvss_score for r in self.results)
            avg_cvss = total_cvss / len(self.results)
        else:
            avg_cvss = 0
        
        # Generate attack chain analysis
        attack_chains = self._analyze_attack_chains()
        
        report = {
            "metadata": {
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
                "target_namespace": self.namespace,
                "total_tests_performed": total_tests,
                "methodology": "Adversarial Penetration Testing"
            },
            "executive_summary": {
                "successful_attacks": successful_attacks,
                "attack_success_rate": round((successful_attacks / total_tests) * 100, 1) if total_tests > 0 else 0,
                "severity_breakdown": severity_counts,
                "attack_vector_breakdown": vector_counts,
                "average_cvss_score": round(avg_cvss, 2),
                "risk_level": self._calculate_risk_level(avg_cvss, severity_counts),
                "critical_vulnerabilities": severity_counts.get("CRITICAL", 0),
                "high_vulnerabilities": severity_counts.get("HIGH", 0)
            },
            "detailed_findings": [
                {
                    "id": f"PEN-{i+1:03d}",
                    "attack_vector": result.attack_vector.value,
                    "test_name": result.test_name,
                    "success": result.success,
                    "severity": result.severity.value,
                    "cvss_score": result.cvss_score,
                    "description": result.description,
                    "evidence": result.evidence,
                    "remediation": result.remediation
                }
                for i, result in enumerate(self.results)
            ],
            "attack_chain_analysis": attack_chains,
            "exploitation_scenarios": self._generate_exploitation_scenarios(),
            "recommendations": [
                "Implement Pod Security Standards 'restricted' profile",
                "Enable comprehensive runtime security monitoring",
                "Implement least-privilege RBAC across all service accounts",
                "Deploy network policies for microsegmentation",
                "Regular penetration testing and security assessments",
                "Implement container image scanning and signing",
                "Enable audit logging for all security events",
                "Deploy admission controllers for policy enforcement",
                "Implement secrets management with rotation",
                "Regular security training for development teams"
            ]
        }
        
        return report
    
    def _analyze_attack_chains(self) -> List[Dict]:
        """Analyze potential attack chains from findings"""
        
        chains = []
        
        # Look for common attack progressions
        has_container_escape = any(r.attack_vector == AttackVector.CONTAINER_ESCAPE for r in self.results if r.success)
        has_privilege_escalation = any(r.attack_vector == AttackVector.PRIVILEGE_ESCALATION for r in self.results if r.success)
        has_lateral_movement = any(r.attack_vector == AttackVector.LATERAL_MOVEMENT for r in self.results if r.success)
        has_persistence = any(r.attack_vector == AttackVector.PERSISTENCE for r in self.results if r.success)
        
        if has_container_escape and has_privilege_escalation:
            chains.append({
                "name": "Container Escape → Privilege Escalation",
                "likelihood": "HIGH",
                "impact": "CRITICAL",
                "steps": [
                    "Exploit container misconfiguration",
                    "Escape to host system",
                    "Escalate privileges on host",
                    "Gain cluster-wide access"
                ]
            })
        
        if has_privilege_escalation and has_lateral_movement:
            chains.append({
                "name": "Privilege Escalation → Lateral Movement",
                "likelihood": "MEDIUM",
                "impact": "HIGH",
                "steps": [
                    "Abuse service account permissions",
                    "Access other pods/namespaces",
                    "Pivot through network",
                    "Compromise additional workloads"
                ]
            })
        
        if has_lateral_movement and has_persistence:
            chains.append({
                "name": "Lateral Movement → Persistence",
                "likelihood": "MEDIUM",
                "impact": "HIGH",
                "steps": [
                    "Move between pods/services",
                    "Identify persistence opportunities",
                    "Create backdoor mechanisms",
                    "Maintain long-term access"
                ]
            })
        
        return chains
    
    def _generate_exploitation_scenarios(self) -> List[Dict]:
        """Generate realistic exploitation scenarios"""
        
        scenarios = []
        
        # Scenario based on findings
        critical_findings = [r for r in self.results if r.severity == Severity.CRITICAL and r.success]
        high_findings = [r for r in self.results if r.severity == Severity.HIGH and r.success]
        
        if critical_findings:
            scenarios.append({
                "name": "Critical Path Exploitation",
                "description": "Exploitation via critical vulnerabilities",
                "steps": [f"Exploit: {f.test_name}" for f in critical_findings[:3]],
                "impact": "Complete system compromise",
                "likelihood": "HIGH"
            })
        
        if high_findings:
            scenarios.append({
                "name": "Multi-Vector Attack",
                "description": "Chained exploitation of multiple high-severity issues",
                "steps": [f"Exploit: {f.test_name}" for f in high_findings[:3]],
                "impact": "Significant security breach",
                "likelihood": "MEDIUM"
            })
        
        return scenarios
    
    def _calculate_risk_level(self, avg_cvss: float, severity_counts: Dict) -> str:
        """Calculate overall risk level"""
        
        if severity_counts.get("CRITICAL", 0) > 0:
            return "CRITICAL"
        elif severity_counts.get("HIGH", 0) > 2:
            return "HIGH"
        elif avg_cvss >= 7.0:
            return "HIGH"
        elif avg_cvss >= 5.0:
            return "MEDIUM"
        else:
            return "LOW"

def main():
    """Main execution function"""
    if len(sys.argv) < 2:
        print("Usage: python3 adversarial_penetration_tester.py <namespace>")
        sys.exit(1)
    
    namespace = sys.argv[1]
    
    pentester = AdversarialPenetrationTester(namespace)
    report = pentester.run_penetration_campaign()
    
    # Save report
    report_file = f"adversarial_pentest_report_{namespace}.json"
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    # Print summary
    print(f"\n🎯 ADVERSARIAL PENETRATION TEST COMPLETE")
    print(f"Report saved to: {report_file}")
    print(f"Tests performed: {report['metadata']['total_tests_performed']}")
    print(f"Successful attacks: {report['executive_summary']['successful_attacks']}")
    print(f"Risk level: {report['executive_summary']['risk_level']}")
    print(f"Critical vulnerabilities: {report['executive_summary']['critical_vulnerabilities']}")
    print(f"High vulnerabilities: {report['executive_summary']['high_vulnerabilities']}")
    
    # Print top 3 critical findings
    critical_findings = [f for f in report['detailed_findings'] if f['severity'] == 'CRITICAL'][:3]
    if critical_findings:
        print(f"\n🚨 TOP CRITICAL FINDINGS:")
        for i, finding in enumerate(critical_findings, 1):
            print(f"  {i}. {finding['test_name']} (CVSS: {finding['cvss_score']})")
    
    return report['executive_summary']['critical_vulnerabilities']

if __name__ == "__main__":
    sys.exit(main())