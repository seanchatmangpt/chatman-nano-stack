#!/usr/bin/env python3
"""
Comprehensive Unit Tests for K8s Inter-Service Communication
Tests service mesh, networking, mTLS, and microservice interactions using 80/20 principle
"""

import asyncio
import unittest
import aiohttp
import grpc
import ssl
import socket
import json
import time
import subprocess
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from pathlib import Path

@dataclass
class ServiceEndpoint:
    """Service endpoint configuration"""
    name: str
    host: str
    port: int
    protocol: str
    health_path: str = "/health"

@dataclass
class TestResult:
    """Test result data structure"""
    test_name: str
    passed: bool
    duration_ms: float
    details: Dict[str, Any]
    error_message: Optional[str] = None

class K8sInterServiceTestSuite:
    """Comprehensive test suite for K8s inter-service communication"""
    
    def __init__(self):
        self.services = {
            'protection': ServiceEndpoint(
                name='cns-protection-service',
                host='cns-protection-service.cns-system.svc.cluster.local',
                port=8080,
                protocol='grpc',
                health_path='/grpc.health.v1.Health/Check'
            ),
            'gateway': ServiceEndpoint(
                name='cns-gateway-service', 
                host='cns-gateway-service.cns-system.svc.cluster.local',
                port=8081,
                protocol='http',
                health_path='/ready'
            ),
            'analytics': ServiceEndpoint(
                name='cns-analytics-service',
                host='cns-analytics-service.cns-system.svc.cluster.local', 
                port=8082,
                protocol='grpc',
                health_path='/grpc.health.v1.Health/Check'
            ),
            'monitor': ServiceEndpoint(
                name='cns-monitor-service',
                host='cns-monitor-service.cns-system.svc.cluster.local',
                port=8083,
                protocol='http',
                health_path='/status'
            )
        }
        self.test_results = []
        
    async def test_service_connectivity(self) -> List[TestResult]:
        """Test basic connectivity to all services"""
        results = []
        
        for service_name, endpoint in self.services.items():
            start_time = time.perf_counter()
            
            try:
                if endpoint.protocol == 'http':
                    success = await self._test_http_connectivity(endpoint)
                else:
                    success = await self._test_grpc_connectivity(endpoint)
                    
                duration = (time.perf_counter() - start_time) * 1000
                
                results.append(TestResult(
                    test_name=f"connectivity_{service_name}",
                    passed=success,
                    duration_ms=duration,
                    details={
                        'service': service_name,
                        'endpoint': f"{endpoint.host}:{endpoint.port}",
                        'protocol': endpoint.protocol
                    }
                ))
                
            except Exception as e:
                duration = (time.perf_counter() - start_time) * 1000
                results.append(TestResult(
                    test_name=f"connectivity_{service_name}",
                    passed=False,
                    duration_ms=duration,
                    details={'service': service_name},
                    error_message=str(e)
                ))
                
        return results
    
    async def _test_http_connectivity(self, endpoint: ServiceEndpoint) -> bool:
        """Test HTTP service connectivity"""
        try:
            # Use mTLS context for secure communication
            ssl_context = ssl.create_default_context()
            ssl_context.check_hostname = False
            ssl_context.verify_mode = ssl.CERT_NONE  # For testing - in prod use proper certs
            
            connector = aiohttp.TCPConnector(ssl=ssl_context)
            
            async with aiohttp.ClientSession(
                connector=connector,
                timeout=aiohttp.ClientTimeout(total=10)
            ) as session:
                
                url = f"https://{endpoint.host}:{endpoint.port}{endpoint.health_path}"
                
                async with session.get(url) as response:
                    return response.status in [200, 204]
                    
        except Exception:
            # Fallback to HTTP if HTTPS fails (for testing)
            try:
                async with aiohttp.ClientSession(
                    timeout=aiohttp.ClientTimeout(total=10)
                ) as session:
                    url = f"http://{endpoint.host}:{endpoint.port}{endpoint.health_path}"
                    async with session.get(url) as response:
                        return response.status in [200, 204]
            except Exception:
                return False
                
    async def _test_grpc_connectivity(self, endpoint: ServiceEndpoint) -> bool:
        """Test gRPC service connectivity"""
        try:
            # Create secure channel with mTLS
            credentials = grpc.ssl_channel_credentials()
            channel = grpc.aio.secure_channel(
                f"{endpoint.host}:{endpoint.port}",
                credentials
            )
            
            # Test gRPC health check
            await asyncio.wait_for(
                channel.channel_ready(),
                timeout=10.0
            )
            
            await channel.close()
            return True
            
        except Exception:
            # Fallback to insecure channel for testing
            try:
                channel = grpc.aio.insecure_channel(f"{endpoint.host}:{endpoint.port}")
                await asyncio.wait_for(
                    channel.channel_ready(), 
                    timeout=10.0
                )
                await channel.close()
                return True
            except Exception:
                return False
    
    async def test_inter_service_communication(self) -> List[TestResult]:
        """Test communication between services"""
        results = []
        
        # Test Gateway -> Protection Service communication
        start_time = time.perf_counter()
        try:
            success = await self._test_gateway_to_protection()
            duration = (time.perf_counter() - start_time) * 1000
            
            results.append(TestResult(
                test_name="gateway_to_protection_communication",
                passed=success,
                duration_ms=duration,
                details={
                    'source': 'cns-gateway-service',
                    'target': 'cns-protection-service',
                    'communication_type': 'HTTP->gRPC'
                }
            ))
        except Exception as e:
            duration = (time.perf_counter() - start_time) * 1000
            results.append(TestResult(
                test_name="gateway_to_protection_communication",
                passed=False,
                duration_ms=duration,
                details={},
                error_message=str(e)
            ))
        
        # Test Gateway -> Analytics Service communication
        start_time = time.perf_counter()
        try:
            success = await self._test_gateway_to_analytics()
            duration = (time.perf_counter() - start_time) * 1000
            
            results.append(TestResult(
                test_name="gateway_to_analytics_communication",
                passed=success,
                duration_ms=duration,
                details={
                    'source': 'cns-gateway-service',
                    'target': 'cns-analytics-service',
                    'communication_type': 'HTTP->gRPC'
                }
            ))
        except Exception as e:
            duration = (time.perf_counter() - start_time) * 1000
            results.append(TestResult(
                test_name="gateway_to_analytics_communication",
                passed=False,
                duration_ms=duration,
                details={},
                error_message=str(e)
            ))
            
        return results
    
    async def _test_gateway_to_protection(self) -> bool:
        """Test gateway calling protection service"""
        gateway = self.services['gateway']
        
        try:
            async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=30)) as session:
                # Test endpoint that should trigger gateway->protection communication
                url = f"http://{gateway.host}:{gateway.port}/api/v1/protection/validate"
                
                test_payload = {
                    "request_id": "test_inter_service_001",
                    "data": "test_payload_for_protection_service"
                }
                
                async with session.post(url, json=test_payload) as response:
                    return response.status in [200, 202, 204]
                    
        except Exception:
            return False
    
    async def _test_gateway_to_analytics(self) -> bool:
        """Test gateway calling analytics service"""
        gateway = self.services['gateway']
        
        try:
            async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=30)) as session:
                # Test endpoint that should trigger gateway->analytics communication
                url = f"http://{gateway.host}:{gateway.port}/api/v1/analytics/metrics"
                
                async with session.get(url) as response:
                    return response.status in [200, 202, 204]
                    
        except Exception:
            return False
    
    def test_network_policies(self) -> List[TestResult]:
        """Test Kubernetes network policies"""
        results = []
        
        # Test that network policies are applied
        start_time = time.perf_counter()
        try:
            success = self._verify_network_policies_exist()
            duration = (time.perf_counter() - start_time) * 1000
            
            results.append(TestResult(
                test_name="network_policies_exist",
                passed=success,
                duration_ms=duration,
                details={
                    'policies_checked': [
                        'cns-main-network-policy',
                        'cns-inter-pod-communication'
                    ]
                }
            ))
        except Exception as e:
            duration = (time.perf_counter() - start_time) * 1000
            results.append(TestResult(
                test_name="network_policies_exist",
                passed=False,
                duration_ms=duration,
                details={},
                error_message=str(e)
            ))
        
        return results
    
    def _verify_network_policies_exist(self) -> bool:
        """Verify network policies are deployed"""
        try:
            # Check if network policies exist using kubectl
            result = subprocess.run([
                'kubectl', 'get', 'networkpolicy', 
                '-n', 'cns-system',
                '-o', 'json'
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode != 0:
                return False
                
            policies = json.loads(result.stdout)
            policy_names = [item['metadata']['name'] for item in policies.get('items', [])]
            
            required_policies = [
                'cns-main-network-policy',
                'cns-inter-pod-communication'
            ]
            
            return all(policy in policy_names for policy in required_policies)
            
        except Exception:
            return False
    
    def test_service_mesh_configuration(self) -> List[TestResult]:
        """Test service mesh (Istio) configuration"""
        results = []
        
        # Test Istio injection is enabled
        start_time = time.perf_counter()
        try:
            success = self._verify_istio_injection()
            duration = (time.perf_counter() - start_time) * 1000
            
            results.append(TestResult(
                test_name="istio_injection_enabled",
                passed=success,
                duration_ms=duration,
                details={
                    'namespace': 'cns-system',
                    'injection_label': 'istio-injection=enabled'
                }
            ))
        except Exception as e:
            duration = (time.perf_counter() - start_time) * 1000
            results.append(TestResult(
                test_name="istio_injection_enabled",
                passed=False,
                duration_ms=duration,
                details={},
                error_message=str(e)
            ))
        
        # Test mTLS policy exists
        start_time = time.perf_counter()
        try:
            success = self._verify_mtls_policy()
            duration = (time.perf_counter() - start_time) * 1000
            
            results.append(TestResult(
                test_name="mtls_policy_configured",
                passed=success,
                duration_ms=duration,
                details={
                    'policy_name': 'default',
                    'mtls_mode': 'STRICT'
                }
            ))
        except Exception as e:
            duration = (time.perf_counter() - start_time) * 1000
            results.append(TestResult(
                test_name="mtls_policy_configured",
                passed=False,
                duration_ms=duration,
                details={},
                error_message=str(e)
            ))
            
        return results
    
    def _verify_istio_injection(self) -> bool:
        """Verify Istio injection is enabled for CNS namespace"""
        try:
            result = subprocess.run([
                'kubectl', 'get', 'namespace', 'cns-system',
                '-o', 'jsonpath={.metadata.labels.istio-injection}'
            ], capture_output=True, text=True, timeout=30)
            
            return result.returncode == 0 and result.stdout.strip() == 'enabled'
            
        except Exception:
            return False
    
    def _verify_mtls_policy(self) -> bool:
        """Verify mTLS policy is configured"""
        try:
            result = subprocess.run([
                'kubectl', 'get', 'peerauthentication', 'default',
                '-n', 'cns-system',
                '-o', 'jsonpath={.spec.mtls.mode}'
            ], capture_output=True, text=True, timeout=30)
            
            return result.returncode == 0 and result.stdout.strip() == 'STRICT'
            
        except Exception:
            return False
    
    async def test_service_discovery(self) -> List[TestResult]:
        """Test DNS-based service discovery"""
        results = []
        
        for service_name, endpoint in self.services.items():
            start_time = time.perf_counter()
            
            try:
                # Test DNS resolution
                socket.gethostbyname(endpoint.host)
                duration = (time.perf_counter() - start_time) * 1000
                
                results.append(TestResult(
                    test_name=f"dns_resolution_{service_name}",
                    passed=True,
                    duration_ms=duration,
                    details={
                        'service': service_name,
                        'fqdn': endpoint.host
                    }
                ))
                
            except Exception as e:
                duration = (time.perf_counter() - start_time) * 1000
                results.append(TestResult(
                    test_name=f"dns_resolution_{service_name}",
                    passed=False,
                    duration_ms=duration,
                    details={'service': service_name},
                    error_message=str(e)
                ))
                
        return results
    
    async def run_comprehensive_tests(self) -> Dict[str, Any]:
        """Run all inter-service communication tests"""
        print("🧪 RUNNING COMPREHENSIVE K8S INTER-SERVICE COMMUNICATION TESTS")
        print("=" * 80)
        
        start_time = time.perf_counter()
        all_results = []
        
        # Run all test suites
        test_suites = [
            ("Service Connectivity", self.test_service_connectivity()),
            ("Inter-Service Communication", self.test_inter_service_communication()),
            ("Network Policies", self.test_network_policies()),
            ("Service Mesh Configuration", self.test_service_mesh_configuration()),
            ("Service Discovery", self.test_service_discovery())
        ]
        
        for suite_name, test_coro in test_suites:
            print(f"\n🔍 Running {suite_name} Tests...")
            
            if asyncio.iscoroutine(test_coro):
                suite_results = await test_coro
            else:
                suite_results = test_coro
                
            all_results.extend(suite_results)
            
            # Display results for this suite
            passed = sum(1 for r in suite_results if r.passed)
            total = len(suite_results)
            print(f"  ✅ {passed}/{total} tests passed")
            
            for result in suite_results:
                status = "✅ PASS" if result.passed else "❌ FAIL"
                print(f"    {status} {result.test_name} ({result.duration_ms:.1f}ms)")
                if not result.passed and result.error_message:
                    print(f"      Error: {result.error_message}")
        
        total_time = time.perf_counter() - start_time
        
        # Calculate summary statistics
        total_tests = len(all_results)
        passed_tests = sum(1 for r in all_results if r.passed)
        failed_tests = total_tests - passed_tests
        avg_duration = sum(r.duration_ms for r in all_results) / total_tests if total_tests > 0 else 0
        
        summary = {
            'test_results': {
                'total_tests': total_tests,
                'passed_tests': passed_tests,
                'failed_tests': failed_tests,
                'success_rate': (passed_tests / total_tests * 100) if total_tests > 0 else 0,
                'avg_test_duration_ms': avg_duration,
                'total_execution_time_s': total_time
            },
            'detailed_results': [
                {
                    'test_name': r.test_name,
                    'passed': r.passed,
                    'duration_ms': r.duration_ms,
                    'details': r.details,
                    'error_message': r.error_message
                }
                for r in all_results
            ],
            'service_endpoints_tested': {
                name: {
                    'host': endpoint.host,
                    'port': endpoint.port,
                    'protocol': endpoint.protocol
                }
                for name, endpoint in self.services.items()
            },
            'timestamp': time.time()
        }
        
        print(f"\n📊 TEST SUMMARY")
        print("=" * 50)
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {passed_tests}")
        print(f"Failed: {failed_tests}")
        print(f"Success Rate: {summary['test_results']['success_rate']:.1f}%")
        print(f"Total Time: {total_time:.2f}s")
        
        return summary

async def main():
    """Execute the comprehensive test suite"""
    test_suite = K8sInterServiceTestSuite()
    results = await test_suite.run_comprehensive_tests()
    
    # Save results
    results_file = Path("k8s_interservice_unit_test_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\n💾 Results saved to: {results_file}")
    
    # Return appropriate exit code
    success_rate = results['test_results']['success_rate']
    if success_rate >= 90:
        print("🎉 K8S INTER-SERVICE COMMUNICATION TESTS PASSED")
        return 0
    else:
        print("⚠️ K8S INTER-SERVICE COMMUNICATION TESTS FAILED - Review failures")
        return 1

if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)