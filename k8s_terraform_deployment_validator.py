#!/usr/bin/env python3
"""
K8s Terraform Deployment Validator
Comprehensive validation of Terraform-deployed K8s infrastructure with service mesh and inter-service communication
"""

import subprocess
import json
import time
import asyncio
import aiohttp
import yaml
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path
import re

@dataclass
class ValidationResult:
    """Validation result data structure"""
    component: str
    test_name: str
    status: str  # PASS, FAIL, WARN, SKIP
    details: Dict[str, Any]
    error_message: Optional[str] = None
    execution_time_s: float = 0.0

class K8sTerraformDeploymentValidator:
    """Comprehensive validator for Terraform-deployed K8s infrastructure"""
    
    def __init__(self, terraform_dir: str = "/Users/sac/cns/terraform"):
        self.terraform_dir = Path(terraform_dir)
        self.validation_results = []
        self.namespace = "cns-system"
        
    def validate_terraform_syntax(self) -> List[ValidationResult]:
        """Validate Terraform configuration syntax"""
        results = []
        
        print("🔍 Validating Terraform syntax...")
        
        # Test 1: Terraform init
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'terraform', 'init', '-input=false'
            ], cwd=self.terraform_dir, capture_output=True, text=True, timeout=300)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_init",
                    status="PASS",
                    details={
                        'initialization_successful': True,
                        'providers_downloaded': True
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_init",
                    status="FAIL",
                    details={'initialization_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="terraform",
                test_name="terraform_init",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 2: Terraform validate
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'terraform', 'validate'
            ], cwd=self.terraform_dir, capture_output=True, text=True, timeout=60)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_validate",
                    status="PASS",
                    details={
                        'syntax_valid': True,
                        'configuration_valid': True
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_validate",
                    status="FAIL",
                    details={'syntax_errors_found': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="terraform",
                test_name="terraform_validate",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 3: Terraform plan
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'terraform', 'plan', '-input=false', '-detailed-exitcode'
            ], cwd=self.terraform_dir, capture_output=True, text=True, timeout=300)
            
            execution_time = time.perf_counter() - start_time
            
            # Exit code 0 = no changes, 1 = error, 2 = changes planned
            if result.returncode in [0, 2]:
                changes_planned = result.returncode == 2
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_plan",
                    status="PASS",
                    details={
                        'plan_successful': True,
                        'changes_planned': changes_planned,
                        'plan_exit_code': result.returncode
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="terraform",
                    test_name="terraform_plan",
                    status="FAIL",
                    details={'plan_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="terraform",
                test_name="terraform_plan",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    def validate_kubernetes_cluster_connectivity(self) -> List[ValidationResult]:
        """Validate Kubernetes cluster connectivity"""
        results = []
        
        print("🔍 Validating Kubernetes cluster connectivity...")
        
        # Test 1: kubectl cluster-info
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'cluster-info'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0 and 'running at' in result.stdout:
                results.append(ValidationResult(
                    component="kubernetes",
                    test_name="cluster_connectivity",
                    status="PASS",
                    details={
                        'cluster_accessible': True,
                        'cluster_info': result.stdout.strip()
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="kubernetes",
                    test_name="cluster_connectivity",
                    status="FAIL",
                    details={'cluster_not_accessible': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="kubernetes",
                test_name="cluster_connectivity",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 2: Check nodes
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'nodes', '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                nodes_data = json.loads(result.stdout)
                nodes = nodes_data.get('items', [])
                ready_nodes = [
                    node for node in nodes
                    if any(
                        condition.get('type') == 'Ready' and condition.get('status') == 'True'
                        for condition in node.get('status', {}).get('conditions', [])
                    )
                ]
                
                results.append(ValidationResult(
                    component="kubernetes",
                    test_name="nodes_status",
                    status="PASS" if ready_nodes else "FAIL",
                    details={
                        'total_nodes': len(nodes),
                        'ready_nodes': len(ready_nodes),
                        'nodes_ready': len(ready_nodes) > 0
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="kubernetes",
                    test_name="nodes_status",
                    status="FAIL",
                    details={'nodes_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="kubernetes",
                test_name="nodes_status",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    def validate_namespace_and_resources(self) -> List[ValidationResult]:
        """Validate namespace creation and basic resources"""
        results = []
        
        print("🔍 Validating namespace and basic resources...")
        
        # Test 1: Check namespace exists
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'namespace', self.namespace, '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                namespace_data = json.loads(result.stdout)
                phase = namespace_data.get('status', {}).get('phase', '')
                
                results.append(ValidationResult(
                    component="namespace",
                    test_name="namespace_exists",
                    status="PASS" if phase == "Active" else "WARN",
                    details={
                        'namespace_exists': True,
                        'namespace_phase': phase,
                        'namespace_name': self.namespace
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="namespace",
                    test_name="namespace_exists",
                    status="FAIL",
                    details={'namespace_not_found': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="namespace",
                test_name="namespace_exists",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 2: Check ConfigMaps
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'configmaps', '-n', self.namespace, '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                configmaps_data = json.loads(result.stdout)
                configmaps = configmaps_data.get('items', [])
                expected_configmaps = ['cns-config', 'cns-security-config']
                found_configmaps = [cm.get('metadata', {}).get('name', '') for cm in configmaps]
                
                results.append(ValidationResult(
                    component="configmaps",
                    test_name="configmaps_deployed",
                    status="PASS" if any(cm in found_configmaps for cm in expected_configmaps) else "WARN",
                    details={
                        'total_configmaps': len(configmaps),
                        'found_configmaps': found_configmaps,
                        'expected_configmaps': expected_configmaps
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="configmaps",
                    test_name="configmaps_deployed",
                    status="FAIL",
                    details={'configmaps_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="configmaps",
                test_name="configmaps_deployed",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    def validate_service_mesh_deployment(self) -> List[ValidationResult]:
        """Validate service mesh (Istio/Linkerd) deployment"""
        results = []
        
        print("🔍 Validating service mesh deployment...")
        
        # Test 1: Check Istio installation
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'namespace', 'istio-system', '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                # Check Istio pods
                pods_result = subprocess.run([
                    'kubectl', 'get', 'pods', '-n', 'istio-system', '-o', 'json'
                ], capture_output=True, text=True, timeout=30)
                
                if pods_result.returncode == 0:
                    pods_data = json.loads(pods_result.stdout)
                    pods = pods_data.get('items', [])
                    running_pods = [
                        pod for pod in pods
                        if pod.get('status', {}).get('phase') == 'Running'
                    ]
                    
                    results.append(ValidationResult(
                        component="service_mesh",
                        test_name="istio_installation",
                        status="PASS" if len(running_pods) > 0 else "WARN",
                        details={
                            'istio_namespace_exists': True,
                            'total_pods': len(pods),
                            'running_pods': len(running_pods),
                            'istio_operational': len(running_pods) > 0
                        },
                        execution_time_s=execution_time
                    ))
                else:
                    results.append(ValidationResult(
                        component="service_mesh",
                        test_name="istio_installation",
                        status="WARN",
                        details={
                            'istio_namespace_exists': True,
                            'pods_check_failed': True
                        },
                        error_message=pods_result.stderr,
                        execution_time_s=execution_time
                    ))
            else:
                # Check Linkerd installation
                linkerd_result = subprocess.run([
                    'kubectl', 'get', 'namespace', 'linkerd', '-o', 'json'
                ], capture_output=True, text=True, timeout=30)
                
                if linkerd_result.returncode == 0:
                    results.append(ValidationResult(
                        component="service_mesh",
                        test_name="linkerd_installation",
                        status="PASS",
                        details={
                            'linkerd_namespace_exists': True,
                            'service_mesh_type': 'linkerd'
                        },
                        execution_time_s=execution_time
                    ))
                else:
                    results.append(ValidationResult(
                        component="service_mesh",
                        test_name="service_mesh_installation",
                        status="WARN",
                        details={
                            'no_service_mesh_detected': True,
                            'istio_not_found': True,
                            'linkerd_not_found': True
                        },
                        error_message="No service mesh installation detected",
                        execution_time_s=execution_time
                    ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="service_mesh",
                test_name="service_mesh_installation",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 2: Check service mesh injection
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'namespace', self.namespace, 
                '-o', 'jsonpath={.metadata.labels}'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                labels_str = result.stdout.strip()
                injection_enabled = (
                    'istio-injection:enabled' in labels_str or
                    'linkerd.io/inject:enabled' in labels_str
                )
                
                results.append(ValidationResult(
                    component="service_mesh",
                    test_name="injection_configuration",
                    status="PASS" if injection_enabled else "WARN",
                    details={
                        'injection_enabled': injection_enabled,
                        'namespace_labels': labels_str
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="service_mesh",
                    test_name="injection_configuration",
                    status="FAIL",
                    details={'injection_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="service_mesh",
                test_name="injection_configuration",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    def validate_microservices_deployment(self) -> List[ValidationResult]:
        """Validate microservices deployment"""
        results = []
        
        print("🔍 Validating microservices deployment...")
        
        expected_services = {
            'cns-protection-service': 8080,
            'cns-gateway-service': 8081,
            'cns-analytics-service': 8082,
            'cns-monitor-service': 8083
        }
        
        # Test 1: Check deployments
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'deployments', '-n', self.namespace, '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                deployments_data = json.loads(result.stdout)
                deployments = deployments_data.get('items', [])
                
                deployment_status = {}
                for deployment in deployments:
                    name = deployment.get('metadata', {}).get('name', '')
                    status = deployment.get('status', {})
                    ready_replicas = status.get('readyReplicas', 0)
                    replicas = status.get('replicas', 0)
                    
                    deployment_status[name] = {
                        'ready_replicas': ready_replicas,
                        'total_replicas': replicas,
                        'ready': ready_replicas == replicas and replicas > 0
                    }
                
                all_ready = all(status['ready'] for status in deployment_status.values())
                
                results.append(ValidationResult(
                    component="microservices",
                    test_name="deployments_status",
                    status="PASS" if all_ready else "WARN",
                    details={
                        'total_deployments': len(deployments),
                        'deployment_status': deployment_status,
                        'all_deployments_ready': all_ready
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="microservices",
                    test_name="deployments_status",
                    status="FAIL",
                    details={'deployments_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="microservices",
                test_name="deployments_status",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        # Test 2: Check services
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'services', '-n', self.namespace, '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                services_data = json.loads(result.stdout)
                services = services_data.get('items', [])
                
                found_services = {}
                for service in services:
                    name = service.get('metadata', {}).get('name', '')
                    ports = service.get('spec', {}).get('ports', [])
                    cluster_ip = service.get('spec', {}).get('clusterIP', '')
                    
                    if ports:
                        port = ports[0].get('port', 0)
                        found_services[name] = {
                            'port': port,
                            'cluster_ip': cluster_ip,
                            'has_endpoints': cluster_ip not in ['None', '']
                        }
                
                missing_services = set(expected_services.keys()) - set(found_services.keys())
                
                results.append(ValidationResult(
                    component="microservices",
                    test_name="services_status",
                    status="PASS" if not missing_services else "WARN",
                    details={
                        'expected_services': expected_services,
                        'found_services': found_services,
                        'missing_services': list(missing_services),
                        'all_services_deployed': len(missing_services) == 0
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="microservices",
                    test_name="services_status",
                    status="FAIL",
                    details={'services_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="microservices",
                test_name="services_status",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    def validate_network_policies(self) -> List[ValidationResult]:
        """Validate network policies"""
        results = []
        
        print("🔍 Validating network policies...")
        
        # Test 1: Check network policies exist
        start_time = time.perf_counter()
        try:
            result = subprocess.run([
                'kubectl', 'get', 'networkpolicies', '-n', self.namespace, '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            execution_time = time.perf_counter() - start_time
            
            if result.returncode == 0:
                netpol_data = json.loads(result.stdout)
                netpols = netpol_data.get('items', [])
                
                expected_policies = [
                    'cns-main-network-policy',
                    'cns-inter-pod-communication'
                ]
                
                found_policies = [
                    policy.get('metadata', {}).get('name', '')
                    for policy in netpols
                ]
                
                missing_policies = set(expected_policies) - set(found_policies)
                
                results.append(ValidationResult(
                    component="network_policies",
                    test_name="network_policies_deployed",
                    status="PASS" if not missing_policies else "WARN",
                    details={
                        'total_policies': len(netpols),
                        'expected_policies': expected_policies,
                        'found_policies': found_policies,
                        'missing_policies': list(missing_policies),
                        'all_policies_deployed': len(missing_policies) == 0
                    },
                    execution_time_s=execution_time
                ))
            else:
                results.append(ValidationResult(
                    component="network_policies",
                    test_name="network_policies_deployed",
                    status="FAIL",
                    details={'network_policies_check_failed': True},
                    error_message=result.stderr,
                    execution_time_s=execution_time
                ))
                
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="network_policies",
                test_name="network_policies_deployed",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    async def validate_inter_service_communication(self) -> List[ValidationResult]:
        """Validate inter-service communication"""
        results = []
        
        print("🔍 Validating inter-service communication...")
        
        services = {
            'gateway': 'cns-gateway-service.cns-system.svc.cluster.local:8081',
            'monitor': 'cns-monitor-service.cns-system.svc.cluster.local:8083'
        }
        
        # Test 1: Service reachability
        start_time = time.perf_counter()
        try:
            reachability_results = {}
            
            connector = aiohttp.TCPConnector()
            timeout = aiohttp.ClientTimeout(total=10)
            
            async with aiohttp.ClientSession(connector=connector, timeout=timeout) as session:
                for service_name, endpoint in services.items():
                    try:
                        url = f"http://{endpoint}/health"
                        async with session.get(url) as response:
                            reachability_results[service_name] = {
                                'reachable': True,
                                'status_code': response.status,
                                'response_time_ms': 0  # Simplified for this test
                            }
                    except Exception as e:
                        reachability_results[service_name] = {
                            'reachable': False,
                            'error': str(e)
                        }
            
            execution_time = time.perf_counter() - start_time
            
            reachable_services = sum(1 for result in reachability_results.values() if result.get('reachable', False))
            total_services = len(services)
            
            results.append(ValidationResult(
                component="inter_service_communication",
                test_name="service_reachability",
                status="PASS" if reachable_services == total_services else "WARN",
                details={
                    'reachability_results': reachability_results,
                    'reachable_services': reachable_services,
                    'total_services': total_services,
                    'all_services_reachable': reachable_services == total_services
                },
                execution_time_s=execution_time
            ))
            
        except Exception as e:
            execution_time = time.perf_counter() - start_time
            results.append(ValidationResult(
                component="inter_service_communication",
                test_name="service_reachability",
                status="FAIL",
                details={},
                error_message=str(e),
                execution_time_s=execution_time
            ))
        
        return results
    
    async def run_comprehensive_validation(self) -> Dict[str, Any]:
        """Run comprehensive Terraform deployment validation"""
        print("🎯 RUNNING COMPREHENSIVE K8S TERRAFORM DEPLOYMENT VALIDATION")
        print("=" * 80)
        
        start_time = time.perf_counter()
        all_results = []
        
        # Validation phases
        validation_phases = [
            ("Terraform Syntax", self.validate_terraform_syntax()),
            ("Kubernetes Connectivity", self.validate_kubernetes_cluster_connectivity()),
            ("Namespace & Resources", self.validate_namespace_and_resources()),
            ("Service Mesh", self.validate_service_mesh_deployment()),
            ("Microservices", self.validate_microservices_deployment()),
            ("Network Policies", self.validate_network_policies()),
            ("Inter-Service Communication", await self.validate_inter_service_communication())
        ]
        
        for phase_name, phase_results in validation_phases:
            print(f"\n🔍 Running {phase_name} Validation...")
            
            if asyncio.iscoroutine(phase_results):
                phase_results = await phase_results
                
            all_results.extend(phase_results)
            
            # Display phase results
            passed = sum(1 for r in phase_results if r.status == "PASS")
            warnings = sum(1 for r in phase_results if r.status == "WARN")
            failed = sum(1 for r in phase_results if r.status == "FAIL")
            
            print(f"  ✅ {passed} passed, ⚠️ {warnings} warnings, ❌ {failed} failed")
            
            for result in phase_results:
                if result.status == "FAIL":
                    print(f"    ❌ {result.test_name}: {result.error_message}")
                elif result.status == "WARN":
                    print(f"    ⚠️ {result.test_name}: Warning")
        
        total_time = time.perf_counter() - start_time
        
        # Calculate summary statistics
        total_tests = len(all_results)
        passed_tests = sum(1 for r in all_results if r.status == "PASS")
        warning_tests = sum(1 for r in all_results if r.status == "WARN")
        failed_tests = sum(1 for r in all_results if r.status == "FAIL")
        
        # Calculate deployment readiness score
        deployment_score = (
            (passed_tests / total_tests * 70) +
            (warning_tests / total_tests * 20) +
            (0)  # Failed tests contribute 0
        ) if total_tests > 0 else 0
        
        # Determine deployment status
        if deployment_score >= 90 and failed_tests == 0:
            deployment_status = "READY_FOR_PRODUCTION"
        elif deployment_score >= 80 and failed_tests <= 2:
            deployment_status = "READY_WITH_WARNINGS"
        elif deployment_score >= 60:
            deployment_status = "NEEDS_FIXES"
        else:
            deployment_status = "NOT_READY"
        
        summary = {
            'terraform_deployment_validation': {
                'total_execution_time_s': total_time,
                'total_tests': total_tests,
                'passed_tests': passed_tests,
                'warning_tests': warning_tests,
                'failed_tests': failed_tests,
                'deployment_score': deployment_score,
                'deployment_status': deployment_status,
                'terraform_dir': str(self.terraform_dir),
                'namespace': self.namespace,
                'timestamp': time.time()
            },
            'detailed_results': [
                {
                    'component': r.component,
                    'test_name': r.test_name,
                    'status': r.status,
                    'execution_time_s': r.execution_time_s,
                    'details': r.details,
                    'error_message': r.error_message
                }
                for r in all_results
            ],
            'component_summary': self._generate_component_summary(all_results),
            'recommendations': self._generate_deployment_recommendations(all_results),
            'infrastructure_health': {
                'terraform_valid': any(r.component == "terraform" and r.status == "PASS" for r in all_results),
                'kubernetes_accessible': any(r.component == "kubernetes" and r.status == "PASS" for r in all_results),
                'services_deployed': any(r.component == "microservices" and r.status == "PASS" for r in all_results),
                'networking_configured': any(r.component == "network_policies" and r.status == "PASS" for r in all_results),
                'service_mesh_active': any(r.component == "service_mesh" and r.status == "PASS" for r in all_results)
            }
        }
        
        print(f"\n📊 DEPLOYMENT VALIDATION SUMMARY")
        print("=" * 50)
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {passed_tests}")
        print(f"Warnings: {warning_tests}")
        print(f"Failed: {failed_tests}")
        print(f"Deployment Score: {deployment_score:.1f}/100")
        print(f"Status: {deployment_status}")
        
        return summary
    
    def _generate_component_summary(self, results: List[ValidationResult]) -> Dict[str, Any]:
        """Generate summary by component"""
        components = {}
        
        for result in results:
            if result.component not in components:
                components[result.component] = {
                    'total_tests': 0,
                    'passed': 0,
                    'warnings': 0,
                    'failed': 0
                }
            
            components[result.component]['total_tests'] += 1
            if result.status == "PASS":
                components[result.component]['passed'] += 1
            elif result.status == "WARN":
                components[result.component]['warnings'] += 1
            elif result.status == "FAIL":
                components[result.component]['failed'] += 1
        
        return components
    
    def _generate_deployment_recommendations(self, results: List[ValidationResult]) -> List[str]:
        """Generate deployment recommendations"""
        recommendations = []
        
        # Check for specific issues
        terraform_failed = any(r.component == "terraform" and r.status == "FAIL" for r in results)
        if terraform_failed:
            recommendations.append("Fix Terraform configuration errors before deployment")
        
        k8s_failed = any(r.component == "kubernetes" and r.status == "FAIL" for r in results)
        if k8s_failed:
            recommendations.append("Ensure Kubernetes cluster is accessible and nodes are ready")
        
        services_issues = any(r.component == "microservices" and r.status in ["FAIL", "WARN"] for r in results)
        if services_issues:
            recommendations.append("Review microservices deployment and ensure all pods are running")
        
        mesh_issues = any(r.component == "service_mesh" and r.status in ["FAIL", "WARN"] for r in results)
        if mesh_issues:
            recommendations.append("Verify service mesh installation and configuration")
        
        network_issues = any(r.component == "network_policies" and r.status in ["FAIL", "WARN"] for r in results)
        if network_issues:
            recommendations.append("Review network policies for proper security configuration")
        
        if not recommendations:
            recommendations.append("Deployment validation successful - ready for production")
        
        return recommendations

async def main():
    """Execute the comprehensive deployment validation"""
    validator = K8sTerraformDeploymentValidator()
    results = await validator.run_comprehensive_validation()
    
    # Save results
    results_file = Path("k8s_terraform_deployment_validation_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\n💾 Results saved to: {results_file}")
    
    # Evaluate deployment readiness
    deployment_score = results['terraform_deployment_validation']['deployment_score']
    failed_tests = results['terraform_deployment_validation']['failed_tests']
    deployment_status = results['terraform_deployment_validation']['deployment_status']
    
    if deployment_status in ["READY_FOR_PRODUCTION", "READY_WITH_WARNINGS"] and failed_tests <= 2:
        print("🎉 K8S TERRAFORM DEPLOYMENT VALIDATION PASSED")
        return 0
    else:
        print("⚠️ K8S TERRAFORM DEPLOYMENT VALIDATION FAILED")
        print(f"  Status: {deployment_status}")
        print(f"  Score: {deployment_score:.1f}/100")
        print(f"  Failed Tests: {failed_tests}")
        return 1

if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)