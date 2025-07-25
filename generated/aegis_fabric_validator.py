#!/usr/bin/env python3
'''
CNS Aegis Fabric - Validation Suite
Generated from TTL specifications
Required pass rate: 100.0%
'''

import subprocess
import time
import json
import sys
from typing import Dict, List, Any

class AegisFabricValidator:
    def __init__(self):
        self.results = []
        self.required_pass_rate = 100.0
        
    def run_validation_gauntlet(self) -> Dict[str, Any]:
        '''Execute comprehensive validation tests'''
        print("🎯 RUNNING AEGIS FABRIC VALIDATION GAUNTLET")
        print("=" * 60)
        
        # Test 1: BitActor compilation
        self.validate_bitactor_compilation()
        
        # Test 2: Kubernetes manifest validation
        self.validate_k8s_manifests()
        
        # Test 3: Terraform validation
        self.validate_terraform()
        
        # Test 4: Docker build
        self.validate_docker_build()
        
        # Calculate results
        passed = sum(1 for r in self.results if r['status'] == 'PASS')
        total = len(self.results)
        pass_rate = (passed / total * 100) if total > 0 else 0
        
        return {
            'total_tests': total,
            'passed': passed,
            'failed': total - passed,
            'pass_rate': pass_rate,
            'validation_passed': pass_rate >= self.required_pass_rate,
            'detailed_results': self.results
        }
    
    def validate_bitactor_compilation(self):
        '''Validate BitActor compiles successfully'''
        start_time = time.perf_counter()
        
        try:
            # Try to compile BitActor
            cmd = ['gcc', '-o', 'bitactor_test', 'bitactor_generated.c', '-lpthread', '-lm']
            proc = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
            
            if proc.returncode == 0:
                # Test the binary
                test_proc = subprocess.run(['./bitactor_test', '--health'], 
                                         capture_output=True, text=True, timeout=5)
                
                if test_proc.returncode == 0 and 'OK' in test_proc.stdout:
                    result = {
                        'test': 'bitactor_compilation',
                        'status': 'PASS',
                        'details': 'Compiled and health check passed'
                    }
                else:
                    result = {
                        'test': 'bitactor_compilation',
                        'status': 'FAIL',
                        'error': 'Health check failed'
                    }
            else:
                result = {
                    'test': 'bitactor_compilation',
                    'status': 'FAIL',
                    'error': proc.stderr
                }
        except Exception as e:
            result = {
                'test': 'bitactor_compilation',
                'status': 'FAIL',
                'error': str(e)
            }
        
        result['duration_s'] = time.perf_counter() - start_time
        self.results.append(result)
    
    def validate_k8s_manifests(self):
        '''Validate Kubernetes manifests'''
        start_time = time.perf_counter()
        
        try:
            # Validate YAML syntax
            import yaml
            with open('aegis_fabric_deployment.yaml', 'r') as f:
                manifests = list(yaml.safe_load_all(f))
            
            if len(manifests) >= 4:  # Namespace, Deployment, Service, NetworkPolicy
                result = {
                    'test': 'k8s_manifests',
                    'status': 'PASS',
                    'details': f'Validated {len(manifests)} manifests'
                }
            else:
                result = {
                    'test': 'k8s_manifests',
                    'status': 'FAIL',
                    'error': f'Expected at least 4 manifests, found {len(manifests)}'
                }
        except Exception as e:
            result = {
                'test': 'k8s_manifests',
                'status': 'FAIL',
                'error': str(e)
            }
        
        result['duration_s'] = time.perf_counter() - start_time
        self.results.append(result)
    
    def validate_terraform(self):
        '''Validate Terraform configuration'''
        start_time = time.perf_counter()
        
        try:
            # Check if terraform file exists and is valid
            with open('aegis_fabric.tf', 'r') as f:
                tf_content = f.read()
            
            if 'resource "kubernetes_deployment"' in tf_content:
                result = {
                    'test': 'terraform_config',
                    'status': 'PASS',
                    'details': 'Terraform configuration validated'
                }
            else:
                result = {
                    'test': 'terraform_config',
                    'status': 'FAIL',
                    'error': 'Missing deployment resource'
                }
        except Exception as e:
            result = {
                'test': 'terraform_config',
                'status': 'FAIL',
                'error': str(e)
            }
        
        result['duration_s'] = time.perf_counter() - start_time
        self.results.append(result)
    
    def validate_docker_build(self):
        '''Validate Dockerfile'''
        start_time = time.perf_counter()
        
        try:
            # Check Dockerfile exists
            with open('Dockerfile.aegis', 'r') as f:
                dockerfile = f.read()
            
            if 'FROM ubuntu:22.04' in dockerfile and 'bitactor' in dockerfile:
                result = {
                    'test': 'docker_build',
                    'status': 'PASS',
                    'details': 'Dockerfile validated'
                }
            else:
                result = {
                    'test': 'docker_build',
                    'status': 'FAIL',
                    'error': 'Invalid Dockerfile structure'
                }
        except Exception as e:
            result = {
                'test': 'docker_build',
                'status': 'FAIL',
                'error': str(e)
            }
        
        result['duration_s'] = time.perf_counter() - start_time
        self.results.append(result)

def main():
    validator = AegisFabricValidator()
    results = validator.run_validation_gauntlet()
    
    # Save results
    with open('aegis_validation_results.json', 'w') as f:
        json.dump(results, f, indent=2)
    
    # Display summary
    print(f"\n📊 VALIDATION SUMMARY")
    print(f"Total Tests: {results['total_tests']}")
    print(f"Passed: {results['passed']}")
    print(f"Failed: {results['failed']}")
    print(f"Pass Rate: {results['pass_rate']:.1f}%")
    
    if results['validation_passed']:
        print("\n✅ AEGIS FABRIC VALIDATION PASSED")
        return 0
    else:
        print("\n❌ AEGIS FABRIC VALIDATION FAILED")
        return 1

if __name__ == "__main__":
    sys.exit(main())