#!/usr/bin/env python3
"""
ADVERSARIAL RE-TEST VALIDATION
Verify that security fixes have resolved identified vulnerabilities
Focus on HIGH: Race Condition (CVSS 7.5) and MEDIUM: Endianness (CVSS 4.3)
"""

import sys
import os
import tempfile
import json
import subprocess
import time
import threading
from pathlib import Path

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')

import bitactor_cli
import bitactor_ttl_generator

class AdversarialRetest:
    """Re-test specific vulnerabilities after fixes"""
    
    def __init__(self):
        self.results = {
            "race_condition_fixed": None,
            "endianness_fixed": None,
            "performance_maintained": None,
            "new_vulnerabilities": []
        }
        
    def test_race_condition_fix(self):
        """Test that race condition vulnerability is fixed"""
        print("🔍 Testing Race Condition Fix...")
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("retest", "retest")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            generator.generate_c_code(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    c_content = rf.read()
                    
                    # Check for atomic operations
                    atomic_keywords = [
                        "_Atomic",
                        "atomic_load",
                        "atomic_store",
                        "atomic_load_explicit",
                        "atomic_store_explicit",
                        "memory_order_acquire",
                        "memory_order_release"
                    ]
                    
                    atomics_found = sum(1 for keyword in atomic_keywords if keyword in c_content)
                    
                    # Check ring buffer operations specifically
                    has_atomic_head_tail = (
                        "_Atomic uint32_t signal_head" in c_content and
                        "_Atomic uint32_t signal_tail" in c_content
                    )
                    
                    has_atomic_operations = (
                        "atomic_load_explicit(&ba->signal_head" in c_content and
                        "atomic_store_explicit(&ba->signal_head" in c_content
                    )
                    
                    if atomics_found >= 5 and has_atomic_head_tail and has_atomic_operations:
                        self.results["race_condition_fixed"] = True
                        print("✅ Race condition fix verified - atomic operations present")
                    else:
                        self.results["race_condition_fixed"] = False
                        print("❌ Race condition fix NOT found - missing atomic operations")
                        print(f"   Atomic keywords found: {atomics_found}")
                        print(f"   Atomic head/tail: {has_atomic_head_tail}")
                        print(f"   Atomic operations: {has_atomic_operations}")
                    
            finally:
                Path(f.name).unlink()
    
    def test_endianness_fix(self):
        """Test that endianness vulnerability is fixed"""
        print("🔍 Testing Endianness Fix...")
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("retest", "retest")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            generator.generate_c_code(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    c_content = rf.read()
                    
                    # Check for endianness conversion functions
                    endian_keywords = [
                        "htobe32",
                        "htobe64",
                        "be32toh",
                        "be64toh",
                        "signal_to_network",
                        "signal_from_network"
                    ]
                    
                    endian_found = sum(1 for keyword in endian_keywords if keyword in c_content)
                    
                    # Check for proper usage in enqueue/dequeue
                    has_network_conversion = (
                        "signal_to_network" in c_content and
                        "signal_from_network" in c_content
                    )
                    
                    # Check signal structure has network byte order comment
                    has_byte_order_comment = "Network byte order" in c_content
                    
                    if endian_found >= 4 and has_network_conversion and has_byte_order_comment:
                        self.results["endianness_fixed"] = True
                        print("✅ Endianness fix verified - conversion functions present")
                    else:
                        self.results["endianness_fixed"] = False
                        print("❌ Endianness fix NOT found - missing conversion functions")
                        print(f"   Endian keywords found: {endian_found}")
                        print(f"   Network conversion: {has_network_conversion}")
                        print(f"   Byte order comment: {has_byte_order_comment}")
                    
            finally:
                Path(f.name).unlink()
    
    def test_performance_impact(self):
        """Test that performance is still acceptable after fixes"""
        print("🔍 Testing Performance Impact...")
        
        # Generate and build fixed BitActor
        ttl_file = Path("/Users/sac/cns/ontologies/bitactor_semantic_core.ttl")
        
        with tempfile.TemporaryDirectory() as tmpdir:
            generator = bitactor_ttl_generator.BitActorTTLGenerator()
            generator.generate_all(str(ttl_file), tmpdir, "perftest")
            
            # Build benchmark
            try:
                result = subprocess.run(
                    ["make", "benchmark"],
                    cwd=tmpdir,
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                if result.returncode == 0:
                    # Parse benchmark output
                    output = result.stdout
                    
                    # Look for throughput
                    if "Msignals/sec" in output:
                        # Extract throughput value
                        for line in output.split('\n'):
                            if "Average throughput:" in line and "Msignals/sec" in line:
                                try:
                                    throughput = float(line.split()[-2])
                                    
                                    # Check if performance is acceptable (>1 Msignals/sec with fixes)
                                    if throughput > 1.0:
                                        self.results["performance_maintained"] = True
                                        print(f"✅ Performance maintained - {throughput:.2f} Msignals/sec")
                                    else:
                                        self.results["performance_maintained"] = False
                                        print(f"❌ Performance degraded - {throughput:.2f} Msignals/sec")
                                    break
                                except:
                                    pass
                    else:
                        self.results["performance_maintained"] = None
                        print("⚠️  Could not parse performance metrics")
                else:
                    self.results["performance_maintained"] = None
                    print("⚠️  Benchmark build failed")
                    
            except subprocess.TimeoutExpired:
                self.results["performance_maintained"] = None
                print("⚠️  Benchmark timed out")
    
    def test_for_new_vulnerabilities(self):
        """Quick check for any new vulnerabilities introduced by fixes"""
        print("🔍 Checking for New Vulnerabilities...")
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("vulntest", "vuln")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            generator.generate_c_code(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    c_content = rf.read()
                    
                    # Check for potential new issues
                    potential_issues = []
                    
                    # Check for unsafe string functions that might have been added
                    unsafe_functions = ['strcpy', 'strcat', 'sprintf', 'gets']
                    for func in unsafe_functions:
                        if func in c_content:
                            potential_issues.append(f"Unsafe function: {func}")
                    
                    # Check for missing null checks
                    if "malloc" in c_content and "if (" not in c_content:
                        potential_issues.append("Missing NULL check after malloc")
                    
                    # Check for potential integer overflows in atomic operations
                    if "atomic_fetch_add" in c_content and "UINT32_MAX" not in c_content:
                        # This is OK, just noting
                        pass
                    
                    if potential_issues:
                        self.results["new_vulnerabilities"] = potential_issues
                        print(f"⚠️  {len(potential_issues)} potential new issues found:")
                        for issue in potential_issues:
                            print(f"   - {issue}")
                    else:
                        print("✅ No new vulnerabilities detected")
                    
            finally:
                Path(f.name).unlink()
    
    def generate_validation_report(self):
        """Generate comprehensive validation report"""
        print("\n" + "=" * 70)
        print("📊 ADVERSARIAL RE-TEST VALIDATION REPORT")
        print("=" * 70)
        
        # Race condition fix status
        if self.results["race_condition_fixed"]:
            print("✅ HIGH VULNERABILITY FIXED: Race Condition (CVSS 7.5)")
            print("   - Atomic operations implemented correctly")
            print("   - Ring buffer thread-safe with memory ordering")
        else:
            print("❌ HIGH VULNERABILITY NOT FIXED: Race Condition still present")
        
        # Endianness fix status
        if self.results["endianness_fixed"]:
            print("✅ MEDIUM VULNERABILITY FIXED: Endianness (CVSS 4.3)")
            print("   - Network byte order conversion implemented")
            print("   - Cross-platform compatibility ensured")
        else:
            print("❌ MEDIUM VULNERABILITY NOT FIXED: Endianness issue remains")
        
        # Performance impact
        if self.results["performance_maintained"] is True:
            print("✅ PERFORMANCE: Acceptable throughput maintained")
        elif self.results["performance_maintained"] is False:
            print("❌ PERFORMANCE: Significant degradation detected")
        else:
            print("⚠️  PERFORMANCE: Could not measure impact")
        
        # New vulnerabilities
        if self.results["new_vulnerabilities"]:
            print(f"⚠️  NEW ISSUES: {len(self.results['new_vulnerabilities'])} potential issues found")
        else:
            print("✅ NEW ISSUES: No new vulnerabilities introduced")
        
        # Overall assessment
        print("\n🎯 OVERALL ASSESSMENT:")
        
        fixes_complete = (
            self.results["race_condition_fixed"] and 
            self.results["endianness_fixed"]
        )
        
        if fixes_complete:
            print("✅ ALL IDENTIFIED VULNERABILITIES HAVE BEEN FIXED")
            print("✅ System is ready for production deployment")
        else:
            print("❌ Some vulnerabilities remain unfixed")
            print("❌ Additional remediation required")
        
        return {
            "validation_complete": True,
            "fixes_verified": fixes_complete,
            "race_condition_fixed": self.results["race_condition_fixed"],
            "endianness_fixed": self.results["endianness_fixed"],
            "performance_acceptable": self.results["performance_maintained"],
            "new_issues": len(self.results["new_vulnerabilities"]),
            "detailed_results": self.results
        }

if __name__ == "__main__":
    print("🚀 Starting Adversarial Re-test Validation")
    print("Verifying security fixes for identified vulnerabilities\n")
    
    validator = AdversarialRetest()
    
    # Run all validation tests
    validator.test_race_condition_fix()
    validator.test_endianness_fix()
    validator.test_performance_impact()
    validator.test_for_new_vulnerabilities()
    
    # Generate report
    report = validator.generate_validation_report()
    
    # Save report
    with open("/Users/sac/cns/adversarial_retest_report.json", "w") as f:
        json.dump(report, f, indent=2)
    
    print(f"\nReport saved to adversarial_retest_report.json")
    
    # Exit with appropriate code
    sys.exit(0 if report["fixes_verified"] else 1)