#!/usr/bin/env python3
"""
ADVERSARIAL TESTING LEVEL 1: INPUT VALIDATION ATTACKS
Tests the BitActor system against malicious inputs, boundary conditions, and injection attacks
"""

import sys
import os
import tempfile
import json
import subprocess
import time
from pathlib import Path
from unittest.mock import patch

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')

import bitactor_cli
import bitactor_ttl_generator

class AdversarialLevel1:
    """Level 1 adversarial testing: Input validation attacks"""
    
    def __init__(self):
        self.results = {
            "ttl_fuzzing": [],
            "injection_attacks": [],
            "boundary_tests": [],
            "encoding_attacks": [],
            "buffer_overflow_attempts": [],
            "format_string_attacks": []
        }
        self.vulnerabilities = []
        
    def test_ttl_fuzzing(self):
        """Test TTL parser with malformed RDF triples"""
        print("🔥 Testing TTL fuzzing attacks...")
        
        malformed_ttls = [
            # Malformed triples
            "@prefix : <invalid> .\n:signal a :Signal ; :name \"' OR 1=1 --\" .",
            # Invalid characters
            "@prefix ☠️: <evil> .\n☠️:attack ☠️:payload \"💀\" .",
            # Extremely long lines
            "@prefix : <http://test.com/> .\n:signal :name \"" + "A" * 10000 + "\" .",
            # Nested quotes
            "@prefix : <http://test.com/> .\n:signal :name \"\\\"\\\"\\\"nested\\\"\\\"\\\"\" .",
            # NULL bytes
            "@prefix : <http://test.com/> .\n:signal :name \"test\\x00null\" .",
            # Unicode exploits
            "@prefix : <http://test.com/> .\n:signal :name \"\\u0000\\u001f\\ufeff\" .",
            # Recursive definitions
            "@prefix : <http://test.com/> .\n:signal :includes :signal .",
        ]
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        for i, malformed_ttl in enumerate(malformed_ttls):
            try:
                with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                    f.write(malformed_ttl)
                    f.flush()
                    
                    start_time = time.time()
                    try:
                        generator.load_ttl(f.name)
                        # If it doesn't crash, check for memory usage
                        signals = generator.extract_signals()
                        end_time = time.time()
                        
                        if end_time - start_time > 5:  # Took more than 5 seconds
                            self.vulnerabilities.append({
                                "type": "DoS",
                                "level": "MEDIUM",
                                "description": f"TTL parsing DoS via slow parsing (test {i})",
                                "time": end_time - start_time
                            })
                            
                        self.results["ttl_fuzzing"].append({
                            "test": i,
                            "status": "PARSED",
                            "signals_count": len(signals),
                            "parse_time": end_time - start_time
                        })
                        
                    except Exception as e:
                        end_time = time.time()
                        self.results["ttl_fuzzing"].append({
                            "test": i,
                            "status": "FAILED",
                            "error": str(e),
                            "parse_time": end_time - start_time
                        })
                        
                    Path(f.name).unlink()
                    
            except Exception as e:
                self.results["ttl_fuzzing"].append({
                    "test": i,
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def test_injection_attacks(self):
        """Test injection attacks in signal names and properties"""
        print("💉 Testing injection attacks...")
        
        injection_payloads = [
            # XSS payloads
            "<script>alert('XSS')</script>",
            "javascript:alert('XSS')",
            "<img src=x onerror=alert('XSS')>",
            
            # SQL injection payloads
            "'; DROP TABLE signals; --",
            "' OR '1'='1",
            "' UNION SELECT * FROM users --",
            
            # Command injection
            "; rm -rf /",
            "&& cat /etc/passwd",
            "| nc -l 4444",
            
            # Python code injection
            "__import__('os').system('id')",
            "eval('print(42)')",
            "exec('import subprocess; subprocess.call([\"id\"])')",
            
            # Format string attacks
            "%s%s%s%s%s%s%s%s%s%s",
            "%n%n%n%n%n%n%n%n%n%n",
            "%x%x%x%x%x%x%x%x%x%x",
        ]
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        for i, payload in enumerate(injection_payloads):
            try:
                # Test injection in signal names
                ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:Signal{i} rdf:type :Signal ;
    :name "{payload}" ;
    :id {i} .
"""
                
                with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                    f.write(ttl_content)
                    f.flush()
                    
                    try:
                        generator.load_ttl(f.name)
                        context = generator.generate_context("injection_test", "inj")
                        
                        # Check if payload made it through to generated code
                        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as ch:
                            generator.generate_c_code(context, ch.name)
                            
                            with open(ch.name, 'r') as rf:
                                c_content = rf.read()
                                if payload in c_content:
                                    self.vulnerabilities.append({
                                        "type": "Code Injection",
                                        "level": "HIGH",
                                        "description": f"Injection payload '{payload[:20]}...' found in generated C code",
                                        "payload": payload
                                    })
                            
                            Path(ch.name).unlink()
                        
                        self.results["injection_attacks"].append({
                            "test": i,
                            "payload": payload,
                            "status": "PROCESSED"
                        })
                        
                    except Exception as e:
                        self.results["injection_attacks"].append({
                            "test": i,
                            "payload": payload,
                            "status": "FAILED",
                            "error": str(e)
                        })
                    
                    Path(f.name).unlink()
                    
            except Exception as e:
                self.results["injection_attacks"].append({
                    "test": i,
                    "payload": payload,
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def test_boundary_values(self):
        """Test extreme boundary values"""
        print("🎯 Testing boundary values...")
        
        boundary_values = [
            # Integer boundaries
            2147483647,    # INT_MAX
            -2147483648,   # INT_MIN
            9223372036854775807,   # LONG_MAX
            -9223372036854775808,  # LONG_MIN
            0,
            -1,
            
            # Very large numbers
            10**100,
            -10**100,
            
            # Special float values
            float('inf'),
            float('-inf'),
            float('nan'),
        ]
        
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        for i, value in enumerate(boundary_values):
            try:
                ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:BoundarySignal rdf:type :Signal ;
    :name "boundary_test" ;
    :id {value} .
"""
                
                with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                    f.write(ttl_content)
                    f.flush()
                    
                    try:
                        generator.load_ttl(f.name)
                        signals = generator.extract_signals()
                        
                        self.results["boundary_tests"].append({
                            "test": i,
                            "value": str(value),
                            "status": "PROCESSED",
                            "signals_found": len(signals)
                        })
                        
                    except Exception as e:
                        self.results["boundary_tests"].append({
                            "test": i,
                            "value": str(value),
                            "status": "FAILED",
                            "error": str(e)
                        })
                    
                    Path(f.name).unlink()
                    
            except Exception as e:
                self.results["boundary_tests"].append({
                    "test": i,
                    "value": str(value),
                    "status": "CRASH", 
                    "error": str(e)
                })
    
    def test_encoding_attacks(self):
        """Test various encoding attacks"""
        print("🔤 Testing encoding attacks...")
        
        encoding_payloads = [
            # UTF-8 overlong sequences
            b"\xc0\x80",  # Overlong encoding of NULL
            b"\xc1\x9c",  # Overlong encoding of backslash
            
            # UTF-16 surrogates
            "\ud800\udc00",  # Valid surrogate pair
            "\ud800",        # Orphaned high surrogate
            "\udc00",        # Orphaned low surrogate
            
            # Unicode normalization attacks
            "ℬ𝒾𝓉𝒜𝒸𝓉𝑜𝓇",  # Mathematical script
            "𝐁𝐢𝐭𝐀𝐜𝐭𝐨𝐫",  # Mathematical bold
            
            # RTL override attacks
            "\u202e\u0041\u0042",  # RTL override + AB
            
            # Zero-width characters
            "Bit\u200bActor",   # Zero-width space
            "Bit\u200cActor",   # Zero-width non-joiner
            "Bit\ufeffActor",   # Zero-width no-break space
        ]
        
        for i, payload in enumerate(encoding_payloads):
            try:
                if isinstance(payload, bytes):
                    # Try to decode as UTF-8
                    try:
                        payload_str = payload.decode('utf-8')
                    except UnicodeDecodeError:
                        payload_str = payload.decode('utf-8', errors='replace')
                else:
                    payload_str = payload
                
                ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:EncodingSignal rdf:type :Signal ;
    :name "{payload_str}" ;
    :id {i} .
"""
                
                generator = bitactor_ttl_generator.BitActorTTLGenerator()
                
                with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False, encoding='utf-8') as f:
                    f.write(ttl_content)
                    f.flush()
                    
                    try:
                        generator.load_ttl(f.name)
                        signals = generator.extract_signals()
                        
                        self.results["encoding_attacks"].append({
                            "test": i,
                            "payload": repr(payload),
                            "status": "PROCESSED",
                            "signals_found": len(signals)
                        })
                        
                    except Exception as e:
                        self.results["encoding_attacks"].append({
                            "test": i,
                            "payload": repr(payload),
                            "status": "FAILED",
                            "error": str(e)
                        })
                    
                    Path(f.name).unlink()
                    
            except Exception as e:
                self.results["encoding_attacks"].append({
                    "test": i,
                    "payload": repr(payload),
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def test_buffer_overflow_attempts(self):
        """Test potential buffer overflow scenarios"""
        print("💥 Testing buffer overflow attempts...")
        
        # Test extremely long signal names
        long_payloads = [
            "A" * 1000,      # 1KB
            "A" * 10000,     # 10KB  
            "A" * 100000,    # 100KB
            "A" * 1000000,   # 1MB
        ]
        
        for i, payload in enumerate(long_payloads):
            try:
                ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:OverflowSignal rdf:type :Signal ;
    :name "{payload}" ;
    :id {i} .
"""
                
                generator = bitactor_ttl_generator.BitActorTTLGenerator()
                
                start_time = time.time()
                with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                    f.write(ttl_content)
                    f.flush()
                    
                    try:
                        generator.load_ttl(f.name)
                        
                        # Try to generate C code - this is where buffer overflows might occur
                        context = generator.generate_context("overflow_test", "ovf")
                        
                        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as ch:
                            generator.generate_c_code(context, ch.name)
                            
                            # Check if generated file is reasonable size
                            file_size = Path(ch.name).stat().st_size
                            if file_size > 10 * 1024 * 1024:  # > 10MB
                                self.vulnerabilities.append({
                                    "type": "Resource Exhaustion",
                                    "level": "MEDIUM",
                                    "description": f"Generated C file is {file_size} bytes for {len(payload)} char input",
                                    "input_size": len(payload),
                                    "output_size": file_size
                                })
                            
                            Path(ch.name).unlink()
                        
                        end_time = time.time()
                        
                        self.results["buffer_overflow_attempts"].append({
                            "test": i,
                            "input_size": len(payload),
                            "status": "PROCESSED", 
                            "processing_time": end_time - start_time
                        })
                        
                    except Exception as e:
                        end_time = time.time()
                        self.results["buffer_overflow_attempts"].append({
                            "test": i,
                            "input_size": len(payload),
                            "status": "FAILED",
                            "error": str(e),
                            "processing_time": end_time - start_time
                        })
                    
                    Path(f.name).unlink()
                    
            except Exception as e:
                self.results["buffer_overflow_attempts"].append({
                    "test": i,
                    "input_size": len(payload),
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def run_all_tests(self):
        """Execute all Level 1 adversarial tests"""
        print("🚀 Starting Level 1 Adversarial Testing: Input Validation Attacks")
        print("=" * 70)
        
        self.test_ttl_fuzzing()
        self.test_injection_attacks()
        self.test_boundary_values()
        self.test_encoding_attacks()
        self.test_buffer_overflow_attempts()
        
        return self.generate_report()
    
    def generate_report(self):
        """Generate comprehensive test report"""
        print("\n" + "=" * 70)
        print("📊 LEVEL 1 ADVERSARIAL TESTING REPORT")
        print("=" * 70)
        
        total_tests = sum(len(tests) for tests in self.results.values())
        total_vulnerabilities = len(self.vulnerabilities)
        
        print(f"Total Tests Executed: {total_tests}")
        print(f"Vulnerabilities Found: {total_vulnerabilities}")
        print()
        
        # Test category summary
        for category, tests in self.results.items():
            passed = len([t for t in tests if t["status"] == "PROCESSED"])
            failed = len([t for t in tests if t["status"] == "FAILED"])
            crashed = len([t for t in tests if t["status"] == "CRASH"])
            
            print(f"{category.upper().replace('_', ' ')}:")
            print(f"  ✅ Processed: {passed}")
            print(f"  ❌ Failed: {failed}")
            print(f"  💥 Crashed: {crashed}")
            print()
        
        # Vulnerability summary
        if self.vulnerabilities:
            print("🚨 VULNERABILITIES DISCOVERED:")
            for i, vuln in enumerate(self.vulnerabilities, 1):
                print(f"{i}. {vuln['type']} ({vuln['level']}): {vuln['description']}")
            print()
        else:
            print("✅ No critical vulnerabilities found in Level 1 testing")
            print()
        
        # Risk assessment
        risk_level = "LOW"
        if any(v['level'] == 'HIGH' for v in self.vulnerabilities):
            risk_level = "HIGH"
        elif any(v['level'] == 'MEDIUM' for v in self.vulnerabilities):
            risk_level = "MEDIUM"
        
        print(f"Overall Risk Level: {risk_level}")
        
        return {
            "level": "1",
            "test_name": "Input Validation Attacks", 
            "total_tests": total_tests,
            "vulnerabilities": self.vulnerabilities,
            "risk_level": risk_level,
            "detailed_results": self.results
        }

if __name__ == "__main__":
    tester = AdversarialLevel1()
    report = tester.run_all_tests()
    
    # Save report
    with open("/Users/sac/cns/adversarial_level1_report.json", "w") as f:
        json.dump(report, f, indent=2, default=str)
    
    print(f"Report saved to adversarial_level1_report.json")