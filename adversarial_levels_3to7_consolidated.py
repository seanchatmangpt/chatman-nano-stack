#!/usr/bin/env python3
"""
ADVERSARIAL TESTING LEVELS 3-7: CONSOLIDATED SECURITY & PERFORMANCE TESTING
80/20 approach - Focus on highest impact vulnerabilities and performance issues
"""

import sys
import os
import tempfile
import json
import subprocess
import time
import threading
import concurrent.futures
from pathlib import Path

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')

import bitactor_cli
import bitactor_ttl_generator

class AdversarialLevels3to7:
    """Levels 3-7 adversarial testing: Critical security and performance vulnerabilities"""
    
    def __init__(self):
        self.results = {
            "level3_concurrency": [],
            "level4_security": [],
            "level5_performance": [],
            "level6_platform": [],
            "level7_codegen": []
        }
        self.vulnerabilities = []
        
    def test_level3_concurrency_chaos(self):
        """Level 3: Critical concurrency vulnerability testing (80/20 focus)"""
        print("🔄 Testing Level 3: Critical concurrency vulnerabilities...")
        
        # Focus on most critical: Race conditions in signal processing
        critical_tests = [
            {
                "name": "signal_processing_race",
                "description": "Concurrent signal processing race conditions",
                "test": self._test_signal_race_conditions
            },
            {
                "name": "ring_buffer_corruption", 
                "description": "Ring buffer concurrent access corruption",
                "test": self._test_ring_buffer_corruption
            }
        ]
        
        for test in critical_tests:
            try:
                print(f"  Testing {test['name']}...")
                result = test["test"]()
                self.results["level3_concurrency"].append({
                    "test": test["name"],
                    "description": test["description"],
                    **result
                })
            except Exception as e:
                self.results["level3_concurrency"].append({
                    "test": test["name"],
                    "status": "FAILED",
                    "error": str(e)
                })
    
    def _test_signal_race_conditions(self):
        """Test for race conditions in signal processing"""
        # Generate BitActor implementation
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        ttl_content = """
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:ConcurrentSignal rdf:type :Signal ;
    :name "concurrent_test" ;
    :id 1 .
"""
        
        with tempfile.TemporaryDirectory() as tmpdir:
            with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                f.write(ttl_content)
                f.flush()
                
                generator.load_ttl(f.name)
                generator.generate_all(f.name, tmpdir, "race_test")
                
                # Test concurrent access to generated Python BitActor
                sys.path.insert(0, tmpdir)
                
                try:
                    import race_test_bitactor
                    
                    def concurrent_signal_processing():
                        ba = race_test_bitactor.RaceTestBitActor()
                        for i in range(100):
                            try:
                                signal = race_test_bitactor.RaceTestSignal(
                                    type=race_test_bitactor.RaceTestSignalType.CONCURRENTSIGNAL,
                                    flags=i,
                                    timestamp=i * 1000,
                                    payload=i
                                )
                                ba.process_signal(signal)
                            except:
                                pass
                    
                    # Run concurrent threads
                    threads = []
                    for _ in range(10):
                        t = threading.Thread(target=concurrent_signal_processing)
                        threads.append(t)
                        t.start()
                    
                    for t in threads:
                        t.join()
                    
                    return {"status": "COMPLETED", "threads": 10, "signals_per_thread": 100}
                    
                except ImportError:
                    return {"status": "IMPORT_FAILED", "note": "Generated module not importable"}
                finally:
                    Path(f.name).unlink()
    
    def _test_ring_buffer_corruption(self):
        """Test ring buffer implementation for corruption under concurrent access"""
        # Check generated C code for atomic operations
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("buffer_test", "buf")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            generator.generate_c_code(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    c_content = rf.read()
                    
                    # Check for atomic operations or memory barriers
                    atomic_found = any(keyword in c_content for keyword in [
                        "__atomic_", "_Atomic", "atomic_", "memory_order", "__sync_"
                    ])
                    
                    if not atomic_found:
                        self.vulnerabilities.append({
                            "type": "Race Condition",
                            "level": "HIGH",
                            "description": "Ring buffer implementation lacks atomic operations",
                            "cvss_score": 7.5,
                            "impact": "Memory corruption, data races"
                        })
                    
                    return {
                        "status": "ANALYZED",
                        "atomic_operations_found": atomic_found,
                        "c_code_size": len(c_content)
                    }
                    
            finally:
                Path(f.name).unlink()
    
    def test_level4_security_penetration(self):
        """Level 4: Critical security penetration testing (80/20 focus)"""
        print("🔐 Testing Level 4: Critical security vulnerabilities...")
        
        critical_security_tests = [
            {
                "name": "template_injection",
                "description": "Server-Side Template Injection (SSTI) in Jinja2",
                "test": self._test_template_injection
            },
            {
                "name": "buffer_overflow_c",
                "description": "Buffer overflow in generated C code",
                "test": self._test_c_buffer_overflow
            }
        ]
        
        for test in critical_security_tests:
            try:
                print(f"  Testing {test['name']}...")
                result = test["test"]()
                self.results["level4_security"].append({
                    "test": test["name"],
                    "description": test["description"],
                    **result
                })
            except Exception as e:
                self.results["level4_security"].append({
                    "test": test["name"],
                    "status": "FAILED",
                    "error": str(e)
                })
    
    def _test_template_injection(self):
        """Test for SSTI in Jinja2 templates"""
        # Attempt template injection through signal names
        malicious_payloads = [
            "{{config.__class__.__init__.__globals__['os'].popen('id').read()}}",
            "{{''.__class__.__mro__[2].__subclasses__()[40]('/etc/passwd').read()}}",
            "{{request.application.__globals__.__builtins__.__import__('os').popen('whoami').read()}}"
        ]
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        for i, payload in enumerate(malicious_payloads):
            ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:InjectSignal rdf:type :Signal ;
    :name "{payload}" ;
    :id {i} .
"""
            
            with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                f.write(ttl_content)
                f.flush()
                
                try:
                    generator.load_ttl(f.name)
                    context = generator.generate_context("ssti_test", "ssti")
                    
                    with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as ch:
                        generator.generate_c_code(context, ch.name)
                        
                        with open(ch.name, 'r') as rf:
                            generated_content = rf.read()
                            
                            # Check if payload was executed (signs of command execution)
                            if any(indicator in generated_content for indicator in ['uid=', 'gid=', 'root:', '/bin/']):
                                self.vulnerabilities.append({
                                    "type": "Server-Side Template Injection",
                                    "level": "CRITICAL",
                                    "description": f"SSTI payload executed: {payload[:50]}...",
                                    "cvss_score": 9.8,
                                    "impact": "Remote Code Execution"
                                })
                        
                        Path(ch.name).unlink()
                        
                except Exception:
                    pass
                finally:
                    Path(f.name).unlink()
        
        return {"status": "COMPLETED", "payloads_tested": len(malicious_payloads)}
    
    def _test_c_buffer_overflow(self):
        """Test for buffer overflows in generated C code"""
        # Generate C code with large inputs
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        # Create signal with extremely long name
        long_name = "A" * 10000
        ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:OverflowSignal rdf:type :Signal ;
    :name "{long_name}" ;
    :id 1 .
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(ttl_content)
            f.flush()
            
            try:
                generator.load_ttl(f.name)
                context = generator.generate_context("overflow_test", "ovf")
                
                with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as ch:
                    generator.generate_c_code(context, ch.name)
                    
                    with open(ch.name, 'r') as rf:
                        c_content = rf.read()
                        
                        # Check for unsafe string functions
                        unsafe_functions = ['strcpy', 'strcat', 'sprintf', 'gets']
                        found_unsafe = [func for func in unsafe_functions if func in c_content]
                        
                        if found_unsafe:
                            self.vulnerabilities.append({
                                "type": "Buffer Overflow Risk",
                                "level": "HIGH", 
                                "description": f"Unsafe functions found: {', '.join(found_unsafe)}",
                                "cvss_score": 8.1,
                                "impact": "Memory corruption, potential RCE"
                            })
                        
                        # Check for bounds checking
                        bounds_checking = any(check in c_content for check in [
                            'strlen', 'sizeof', 'bounds', 'length'
                        ])
                        
                        return {
                            "status": "ANALYZED",
                            "unsafe_functions": found_unsafe,
                            "bounds_checking": bounds_checking,
                            "input_length": len(long_name)
                        }
                        
                    Path(ch.name).unlink()
                    
            finally:
                Path(f.name).unlink()
    
    def test_level5_performance_degradation(self):
        """Level 5: Critical performance degradation attacks (80/20 focus)"""
        print("⚡ Testing Level 5: Critical performance vulnerabilities...")
        
        # Focus on algorithmic complexity attacks
        start_time = time.time()
        
        # Test with quadratic complexity attack
        signal_count = 1000
        ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
        
        # Create signals with cross-dependencies (O(n²) complexity)
        for i in range(signal_count):
            deps = [f":Signal{j}" for j in range(i)]  # Each signal depends on all previous
            dep_str = ", ".join(deps)
            ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"perf_{i}\" ; :id {i} ; :depends \"{dep_str}\" .")
        
        ttl_content = "\n".join(ttl_lines)
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(ttl_content)
            f.flush()
            
            try:
                load_start = time.time()
                generator.load_ttl(f.name)
                load_time = time.time() - load_start
                
                extract_start = time.time()
                signals = generator.extract_signals()
                extract_time = time.time() - extract_start
                
                # Check for exponential/quadratic time complexity
                if load_time > 10 or extract_time > 10:  # More than 10 seconds
                    self.vulnerabilities.append({
                        "type": "Algorithmic Complexity Attack",
                        "level": "MEDIUM",
                        "description": f"O(n²) processing time: {load_time + extract_time:.1f}s for {signal_count} signals",
                        "cvss_score": 5.3,
                        "impact": "Denial of Service"
                    })
                
                total_time = time.time() - start_time
                
                self.results["level5_performance"].append({
                    "test": "algorithmic_complexity",
                    "signal_count": signal_count,
                    "load_time": load_time,
                    "extract_time": extract_time,
                    "total_time": total_time,
                    "signals_found": len(signals),
                    "status": "COMPLETED"
                })
                
            finally:
                Path(f.name).unlink()
    
    def test_level6_platform_exploits(self):
        """Level 6: Platform-specific exploit detection (80/20 focus)"""
        print("🖥️ Testing Level 6: Platform-specific vulnerabilities...")
        
        # Focus on architecture-specific code generation issues
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("platform_test", "plat")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            generator.generate_c_code(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    c_content = rf.read()
                    
                    # Check for platform-specific vulnerabilities
                    issues = []
                    
                    # Check for unaligned memory access
                    if '*(' in c_content and 'align' not in c_content.lower():
                        issues.append("Potential unaligned memory access")
                    
                    # Check for endianness handling
                    if any(keyword in c_content for keyword in ['int16', 'int32', 'int64']):
                        if not any(endian in c_content for endian in ['htons', 'ntohl', 'endian']):
                            issues.append("Missing endianness handling")
                    
                    # Check for x86-specific code on other platforms
                    import platform
                    if platform.machine() == 'arm64':
                        if '__rdtsc' in c_content and '__aarch64__' not in c_content:
                            issues.append("x86-specific code on ARM platform")
                    
                    if issues:
                        for issue in issues:
                            self.vulnerabilities.append({
                                "type": "Platform Compatibility",
                                "level": "MEDIUM",
                                "description": issue,
                                "cvss_score": 4.3,
                                "impact": "Platform-specific crashes"
                            })
                    
                    self.results["level6_platform"].append({
                        "test": "platform_compatibility",
                        "platform": platform.machine(),
                        "issues_found": issues,
                        "status": "COMPLETED"
                    })
                    
            finally:
                Path(f.name).unlink()
    
    def test_level7_codegen_corruption(self):
        """Level 7: Code generation corruption testing (80/20 focus)"""
        print("🏗️ Testing Level 7: Code generation vulnerabilities...")
        
        # Focus on template directory traversal and injection
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        # Test template path traversal
        malicious_paths = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32\\config\\sam",
            "/proc/self/environ",
            "file:///etc/hosts"
        ]
        
        path_traversal_found = False
        
        for malicious_path in malicious_paths:
            try:
                # This would be vulnerable if template loading doesn't sanitize paths
                template_path = Path(generator.template_dir) / malicious_path
                if template_path.exists():
                    path_traversal_found = True
                    break
            except:
                pass
        
        if path_traversal_found:
            self.vulnerabilities.append({
                "type": "Path Traversal",
                "level": "HIGH",
                "description": "Template system vulnerable to path traversal",
                "cvss_score": 7.5,
                "impact": "Information disclosure"
            })
        
        # Test build system integrity
        build_integrity_issues = []
        
        context = generator.generate_context("build_test", "build")
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            generator.generate_makefile(context, f.name)
            
            try:
                with open(f.name, 'r') as rf:
                    makefile_content = rf.read()
                    
                    # Check for shell injection vulnerabilities
                    if any(dangerous in makefile_content for dangerous in ['$(shell', '`', '||', '&&']):
                        build_integrity_issues.append("Shell injection risk in Makefile")
                    
                    # Check for hardcoded paths
                    if '/usr/bin' in makefile_content or '/usr/local' in makefile_content:
                        build_integrity_issues.append("Hardcoded system paths")
                    
                    if build_integrity_issues:
                        for issue in build_integrity_issues:
                            self.vulnerabilities.append({
                                "type": "Build System Vulnerability",
                                "level": "MEDIUM",
                                "description": issue,
                                "cvss_score": 5.5,
                                "impact": "Build-time code injection"
                            })
                    
            finally:
                Path(f.name).unlink()
        
        self.results["level7_codegen"].append({
            "test": "codegen_security",
            "path_traversal_found": path_traversal_found,
            "build_issues": build_integrity_issues,
            "status": "COMPLETED"
        })
    
    def run_all_tests(self):
        """Execute all Level 3-7 adversarial tests with 80/20 focus"""
        print("🚀 Starting Levels 3-7 Adversarial Testing: Critical Security & Performance")
        print("=" * 70)
        
        self.test_level3_concurrency_chaos()
        self.test_level4_security_penetration()
        self.test_level5_performance_degradation()
        self.test_level6_platform_exploits()
        self.test_level7_codegen_corruption()
        
        return self.generate_consolidated_report()
    
    def generate_consolidated_report(self):
        """Generate comprehensive consolidated report"""
        print("\n" + "=" * 70)
        print("📊 CONSOLIDATED ADVERSARIAL TESTING REPORT (LEVELS 3-7)")
        print("=" * 70)
        
        total_tests = sum(len(tests) for tests in self.results.values())
        total_vulnerabilities = len(self.vulnerabilities)
        
        print(f"Total Tests Executed: {total_tests}")
        print(f"Total Vulnerabilities Found: {total_vulnerabilities}")
        print()
        
        # Vulnerability summary by severity
        critical = len([v for v in self.vulnerabilities if v['level'] == 'CRITICAL'])
        high = len([v for v in self.vulnerabilities if v['level'] == 'HIGH'])
        medium = len([v for v in self.vulnerabilities if v['level'] == 'MEDIUM'])
        low = len([v for v in self.vulnerabilities if v['level'] == 'LOW'])
        
        print("🚨 VULNERABILITY SEVERITY BREAKDOWN:")
        print(f"  🔴 CRITICAL: {critical}")
        print(f"  🟠 HIGH: {high}")
        print(f"  🟡 MEDIUM: {medium}")
        print(f"  🟢 LOW: {low}")
        print()
        
        # Top vulnerabilities by CVSS score
        if self.vulnerabilities:
            print("🎯 TOP VULNERABILITIES (by CVSS score):")
            sorted_vulns = sorted(self.vulnerabilities, key=lambda x: x.get('cvss_score', 0), reverse=True)
            for i, vuln in enumerate(sorted_vulns[:5], 1):
                cvss = vuln.get('cvss_score', 'N/A')
                print(f"{i}. {vuln['type']} (CVSS: {cvss}): {vuln['description']}")
            print()
        
        # Risk assessment
        if critical > 0:
            risk_level = "CRITICAL"
        elif high > 0:
            risk_level = "HIGH"
        elif medium > 0:
            risk_level = "MEDIUM"
        else:
            risk_level = "LOW"
        
        print(f"🎚️ Overall Risk Level: {risk_level}")
        
        return {
            "levels": "3-7",
            "test_name": "Consolidated Security & Performance Testing",
            "total_tests": total_tests,
            "vulnerabilities": self.vulnerabilities,
            "risk_level": risk_level,
            "severity_breakdown": {
                "critical": critical,
                "high": high,
                "medium": medium,
                "low": low
            },
            "detailed_results": self.results
        }

if __name__ == "__main__":
    tester = AdversarialLevels3to7()
    report = tester.run_all_tests()
    
    # Save report
    with open("/Users/sac/cns/adversarial_levels_3to7_report.json", "w") as f:
        json.dump(report, f, indent=2, default=str)
    
    print(f"Report saved to adversarial_levels_3to7_report.json")