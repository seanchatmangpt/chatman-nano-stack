#!/usr/bin/env python3
"""
CNS Forge REAL Test Suite - 80/20 Implementation
Tests actual code execution instead of generating fake results
"""

import subprocess
import time
import json
import os
import sys
from pathlib import Path

class RealTestSuite:
    def __init__(self):
        self.base_path = Path("/Users/sac/cns")
        self.results = {
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "total_tests": 0,
            "passed_tests": 0,
            "failed_tests": 0,
            "test_results": []
        }
    
    def run_real_test(self, test_name: str, test_func):
        """Run a real test and record actual results"""
        print(f"🧪 Running REAL test: {test_name}")
        
        try:
            result = test_func()
            if result["passed"]:
                print(f"  ✅ {test_name}: PASSED")
                self.results["passed_tests"] += 1
            else:
                print(f"  ❌ {test_name}: FAILED - {result.get('error', 'Unknown error')}")
                self.results["failed_tests"] += 1
            
            self.results["test_results"].append({
                "test_name": test_name,
                "status": "passed" if result["passed"] else "failed",
                "details": result
            })
            
        except Exception as e:
            print(f"  ❌ {test_name}: EXCEPTION - {e}")
            self.results["failed_tests"] += 1
            self.results["test_results"].append({
                "test_name": test_name,
                "status": "failed",
                "details": {"error": str(e), "passed": False}
            })
        
        self.results["total_tests"] += 1
    
    def test_bitactor_compilation(self) -> dict:
        """Test if BitActor C code actually compiles"""
        try:
            # Test compilation
            result = subprocess.run([
                "gcc", "-o", "/tmp/bitactor_test", 
                "-DCNSFORGE_IMPLEMENTATION", "-DBENCHMARK_MODE",
                "generated/bytecode/cnsforge.c", "-lm", "-lpthread"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                # Test execution
                exec_result = subprocess.run(["/tmp/bitactor_test"], 
                                           capture_output=True, text=True, timeout=10)
                return {
                    "passed": exec_result.returncode == 0,
                    "compilation_output": result.stdout,
                    "execution_output": exec_result.stdout if exec_result.returncode == 0 else exec_result.stderr
                }
            else:
                return {
                    "passed": False,
                    "error": f"Compilation failed: {result.stderr}"
                }
                
        except subprocess.TimeoutExpired:
            return {"passed": False, "error": "Test timed out"}
        except Exception as e:
            return {"passed": False, "error": str(e)}
    
    def test_reactor_workflows(self) -> dict:
        """Test if Reactor workflows can actually compile"""
        try:
            # Try to compile a workflow
            result = subprocess.run([
                "elixir", "-e", """
                try do
                  Code.compile_file("generated/reactor_workflows/cybersecuritymesh/cybersecuritymesh_workflow.ex")
                  IO.puts("SUCCESS: Workflow compiled")
                  System.halt(0)
                rescue
                  error ->
                    IO.puts("FAILED: #{inspect(error)}")
                    System.halt(1)
                end
                """
            ], capture_output=True, text=True, timeout=30)
            
            return {
                "passed": result.returncode == 0,
                "output": result.stdout,
                "error": result.stderr if result.returncode != 0 else None
            }
            
        except subprocess.TimeoutExpired:
            return {"passed": False, "error": "Workflow compilation timed out"}
        except Exception as e:
            return {"passed": False, "error": str(e)}
    
    def test_ttl_parsing(self) -> dict:
        """Test if TTL file actually parses and contains claimed concepts"""
        try:
            result = subprocess.run([
                "python3", "-c", """
import rdflib
import sys

try:
    g = rdflib.Graph()
    g.parse('cybersecurity_core.ttl', format='turtle')
    
    classes = len(list(g.subjects(rdflib.RDF.type, rdflib.OWL.Class)))
    props = len(list(g.subjects(rdflib.RDF.type, rdflib.OWL.ObjectProperty)))
    data_props = len(list(g.subjects(rdflib.RDF.type, rdflib.OWL.DatatypeProperty)))
    total = classes + props + data_props
    
    print(f"SUCCESS: TTL parsed with {total} concepts ({classes} classes, {props} obj props, {data_props} data props)")
    
    # Check if we have reasonable semantic content
    if total < 10:
        print(f"WARNING: Only {total} concepts found, expected much more")
        sys.exit(1)
    else:
        sys.exit(0)
        
except Exception as e:
    print(f"FAILED: {e}")
    sys.exit(1)
                """
            ], capture_output=True, text=True, timeout=10)
            
            return {
                "passed": result.returncode == 0,
                "output": result.stdout,
                "error": result.stderr if result.returncode != 0 else None
            }
            
        except Exception as e:
            return {"passed": False, "error": str(e)}
    
    def test_kubernetes_yaml(self) -> dict:
        """Test if Kubernetes YAML is valid and deployable"""
        try:
            result = subprocess.run([
                "python3", "-c", """
import yaml
import sys

try:
    with open('generated/cns_forge_deployment.yaml', 'r') as f:
        docs = list(yaml.safe_load_all(f))
    
    print(f"SUCCESS: YAML parsed {len(docs)} documents")
    
    # Check for common issues
    issues = []
    for doc in docs:
        if doc and isinstance(doc, dict):
            name = doc.get('metadata', {}).get('name', '')
            if name.endswith('-'):
                issues.append(f"Incomplete name: {name}")
            
            # Check containers for image issues
            if 'spec' in doc and 'template' in doc['spec']:
                containers = doc['spec']['template']['spec'].get('containers', [])
                for container in containers:
                    image = container.get('image', '')
                    if image.endswith(':-latest'):
                        issues.append(f"Invalid image tag: {image}")
    
    if issues:
        print(f"ISSUES FOUND: {'; '.join(issues)}")
        sys.exit(1)
    else:
        sys.exit(0)
        
except Exception as e:
    print(f"FAILED: {e}")
    sys.exit(1)
                """
            ], capture_output=True, text=True, timeout=10)
            
            return {
                "passed": result.returncode == 0,
                "output": result.stdout,
                "error": result.stderr if result.returncode != 0 else None
            }
            
        except Exception as e:
            return {"passed": False, "error": str(e)}
    
    def test_performance_claims(self) -> dict:
        """Test if performance claims are realistic vs fake"""
        try:
            # Run our actual BitActor test and measure real performance
            result = subprocess.run([
                "./bitactor_validation_test"
            ], capture_output=True, text=True, timeout=10)
            
            if result.returncode == 0:
                output = result.stdout
                # Look for realistic vs fake metrics
                if "took" in output and "cycles" in output:
                    return {
                        "passed": True,
                        "output": output,
                        "note": "Real performance measured, not fake generated numbers"
                    }
                else:
                    return {
                        "passed": False,
                        "error": "No real performance metrics found in output"
                    }
            else:
                return {
                    "passed": False,
                    "error": f"BitActor test failed: {result.stderr}"
                }
                
        except Exception as e:
            return {"passed": False, "error": str(e)}
    
    def generate_honest_report(self):
        """Generate an honest test report instead of fake 100% success"""
        success_rate = (self.results["passed_tests"] / max(1, self.results["total_tests"])) * 100
        
        print("\n" + "=" * 60)
        print("📊 REAL TEST RESULTS (NOT FAKE)")
        print("=" * 60)
        print(f"Total Tests: {self.results['total_tests']}")
        print(f"Passed: {self.results['passed_tests']}")
        print(f"Failed: {self.results['failed_tests']}")
        print(f"Success Rate: {success_rate:.1f}% (ACTUAL, NOT GENERATED)")
        
        if success_rate < 100:
            print("\n❌ SYSTEM IS NOT 100% WORKING (unlike fake test reports)")
            print("Issues found that need to be fixed:")
            for test in self.results["test_results"]:
                if test["status"] == "failed":
                    print(f"  - {test['test_name']}: {test['details'].get('error', 'Failed')}")
        else:
            print("\n✅ All tests actually passed (rare but possible)")
        
        # Save honest results
        with open("generated/cns_forge_REAL_test_report.json", "w") as f:
            json.dump(self.results, f, indent=2)
        
        print(f"\n📄 Honest report saved to: generated/cns_forge_REAL_test_report.json")
        
        return success_rate
    
    def run_all_tests(self):
        """Run all real tests"""
        print("🚀 CNS FORGE REAL TEST SUITE (NO FAKE RESULTS)")
        print("=" * 60)
        print("Assumption: All code is broken until proven otherwise")
        print()
        
        # Run actual tests
        self.run_real_test("BitActor C Code Compilation & Execution", self.test_bitactor_compilation)
        self.run_real_test("Reactor Workflow Compilation", self.test_reactor_workflows)
        self.run_real_test("TTL Semantic Parsing", self.test_ttl_parsing)
        self.run_real_test("Kubernetes YAML Validation", self.test_kubernetes_yaml)
        self.run_real_test("Performance Claims Reality Check", self.test_performance_claims)
        
        # Generate honest report
        return self.generate_honest_report()

if __name__ == "__main__":
    suite = RealTestSuite()
    success_rate = suite.run_all_tests()
    
    # Exit with appropriate code
    sys.exit(0 if success_rate == 100 else 1)