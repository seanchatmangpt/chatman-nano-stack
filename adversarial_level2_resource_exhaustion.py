#!/usr/bin/env python3
"""
ADVERSARIAL TESTING LEVEL 2: RESOURCE EXHAUSTION ATTACKS
Tests the BitActor system against memory bombs, CPU exhaustion, file descriptor attacks
"""

import sys
import os
import tempfile
import json
import subprocess
import time
import threading
import psutil
import gc
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')

import bitactor_cli
import bitactor_ttl_generator

class AdversarialLevel2:
    """Level 2 adversarial testing: Resource exhaustion attacks"""
    
    def __init__(self):
        self.results = {
            "memory_bombs": [],
            "cpu_exhaustion": [],
            "file_descriptor_exhaustion": [],
            "disk_flooding": [],
            "recursive_generation": []
        }
        self.vulnerabilities = []
        self.process = psutil.Process()
        
    def monitor_resources(self, duration=10):
        """Monitor system resources during attacks"""
        start_memory = self.process.memory_info().rss
        start_cpu = self.process.cpu_percent()
        
        max_memory = start_memory
        max_cpu = start_cpu
        
        end_time = time.time() + duration
        while time.time() < end_time:
            try:
                current_memory = self.process.memory_info().rss
                current_cpu = self.process.cpu_percent(interval=0.1)
                
                max_memory = max(max_memory, current_memory)
                max_cpu = max(max_cpu, current_cpu)
                
                # Check for dangerous memory usage (>1GB)
                if current_memory > 1024 * 1024 * 1024:
                    return {
                        "start_memory": start_memory,
                        "max_memory": max_memory,
                        "max_cpu": max_cpu,
                        "status": "DANGEROUS_MEMORY",
                        "exceeded_limit": True
                    }
                    
            except Exception:
                break
                
        return {
            "start_memory": start_memory, 
            "max_memory": max_memory,
            "max_cpu": max_cpu,
            "status": "COMPLETED",
            "exceeded_limit": False
        }
    
    def test_memory_bombs(self):
        """Test memory exhaustion attacks"""
        print("💣 Testing memory bomb attacks...")
        
        memory_attacks = [
            # Massive signal definitions
            {
                "name": "massive_signals",
                "signal_count": 10000,
                "description": "Generate 10K signals"
            },
            {
                "name": "huge_signal_names", 
                "signal_count": 100,
                "signal_name_size": 100000,
                "description": "100 signals with 100KB names each"
            },
            {
                "name": "recursive_properties",
                "signal_count": 1000,
                "property_depth": 50,
                "description": "1K signals with deep property nesting"
            }
        ]
        
        for attack in memory_attacks:
            print(f"  Testing {attack['name']}...")
            
            try:
                start_time = time.time()
                
                # Generate malicious TTL
                if attack["name"] == "massive_signals":
                    ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
                    
                    for i in range(attack["signal_count"]):
                        ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"signal_{i}\" ; :id {i} .")
                    
                    ttl_content = "\n".join(ttl_lines)
                    
                elif attack["name"] == "huge_signal_names":
                    ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
                    
                    for i in range(attack["signal_count"]):
                        huge_name = "A" * attack["signal_name_size"]
                        ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"{huge_name}\" ; :id {i} .")
                    
                    ttl_content = "\n".join(ttl_lines)
                    
                elif attack["name"] == "recursive_properties":
                    ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
                    
                    for i in range(attack["signal_count"]):
                        properties = []
                        for j in range(attack["property_depth"]):
                            properties.append(f":prop{j} \"value{j}\"")
                        
                        prop_str = " ; ".join(properties)
                        ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"signal_{i}\" ; :id {i} ; {prop_str} .")
                    
                    ttl_content = "\n".join(ttl_lines)
                
                # Monitor resources while processing
                def run_attack():
                    generator = bitactor_ttl_generator.BitActorTTLGenerator()
                    
                    with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                        f.write(ttl_content)
                        f.flush()
                        
                        try:
                            generator.load_ttl(f.name)
                            signals = generator.extract_signals()
                            
                            # Try to generate code
                            context = generator.generate_context("memory_bomb", "mb")
                            
                            with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as ch:
                                generator.generate_c_code(context, ch.name)
                                file_size = Path(ch.name).stat().st_size
                                Path(ch.name).unlink()
                            
                            return {
                                "signals_processed": len(signals),
                                "generated_file_size": file_size,
                                "status": "SUCCESS"
                            }
                            
                        except Exception as e:
                            return {
                                "status": "FAILED",
                                "error": str(e)
                            }
                        finally:
                            Path(f.name).unlink()
                
                # Run attack with resource monitoring
                attack_thread = threading.Thread(target=run_attack)
                attack_thread.start()
                
                resources = self.monitor_resources(30)  # 30 second timeout
                
                attack_thread.join(timeout=30)
                
                end_time = time.time()
                processing_time = end_time - start_time
                
                # Check for memory vulnerabilities
                memory_increase = resources["max_memory"] - resources["start_memory"]
                memory_mb = memory_increase / (1024 * 1024)
                
                if memory_mb > 500:  # More than 500MB increase
                    self.vulnerabilities.append({
                        "type": "Memory Exhaustion",
                        "level": "HIGH",
                        "description": f"Memory bomb '{attack['name']}' caused {memory_mb:.1f}MB memory increase",
                        "memory_increase_mb": memory_mb
                    })
                
                self.results["memory_bombs"].append({
                    "attack": attack["name"],
                    "description": attack["description"],
                    "processing_time": processing_time,
                    "memory_increase_mb": memory_mb,
                    "max_cpu": resources["max_cpu"],
                    "status": resources["status"],
                    "exceeded_limit": resources["exceeded_limit"]
                })
                
            except Exception as e:
                self.results["memory_bombs"].append({
                    "attack": attack["name"],
                    "description": attack["description"],
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def test_cpu_exhaustion(self):
        """Test CPU exhaustion attacks"""
        print("🔥 Testing CPU exhaustion attacks...")
        
        cpu_attacks = [
            {
                "name": "complex_signal_graph",
                "description": "Create complex signal dependency graph"
            },
            {
                "name": "exponential_handlers", 
                "description": "Create exponential handler complexity"
            },
            {
                "name": "recursive_generation",
                "description": "Trigger recursive code generation"
            }
        ]
        
        for attack in cpu_attacks:
            print(f"  Testing {attack['name']}...")
            
            try:
                start_time = time.time()
                start_cpu = self.process.cpu_percent()
                
                if attack["name"] == "complex_signal_graph":
                    # Create a complex interconnected signal graph
                    ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
                    
                    # Create signals that reference each other in complex patterns
                    for i in range(1000):
                        deps = []
                        for j in range(min(10, i)):  # Each signal depends on up to 10 previous signals
                            deps.append(f":Signal{j}")
                        
                        dep_str = ", ".join(deps) if deps else ""
                        ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"signal_{i}\" ; :id {i} ; :dependencies \"{dep_str}\" .")
                    
                    ttl_content = "\n".join(ttl_lines)
                
                elif attack["name"] == "exponential_handlers":
                    # Create handlers with exponential complexity
                    ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
                    
                    for i in range(20):  # 2^20 = 1M combinations
                        handler_complexity = "nested" * (i + 1)
                        ttl_lines.append(f":Handler{i} rdf:type :Handler ; :name \"handler_{i}\" ; :complexity \"{handler_complexity}\" .")
                    
                    ttl_content = "\n".join(ttl_lines)
                
                elif attack["name"] == "recursive_generation":
                    # Create patterns that might cause recursive generation
                    ttl_content = """
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:RecursiveSignal rdf:type :Signal ;
    :name "recursive" ;
    :id 1 ;
    :generates :RecursiveSignal .

:SelfReferencing rdf:type :Handler ;
    :name "self_ref" ;
    :handles :SelfReferencing .
"""
                
                # Process with timeout
                generator = bitactor_ttl_generator.BitActorTTLGenerator()
                
                def run_cpu_attack():
                    with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                        f.write(ttl_content)
                        f.flush()
                        
                        try:
                            generator.load_ttl(f.name)
                            signals = generator.extract_signals()
                            handlers = generator.extract_handlers()
                            
                            # Generate all code types (CPU intensive)
                            context = generator.generate_context("cpu_bomb", "cpu")
                            
                            with tempfile.TemporaryDirectory() as tmpdir:
                                generator.generate_c_code(context, f"{tmpdir}/test.h")
                                generator.generate_python_code(context, f"{tmpdir}/test.py")
                                generator.generate_erlang_code(context, f"{tmpdir}/test.erl")
                                generator.generate_test_code(context, f"{tmpdir}/test.c")
                                generator.generate_benchmark_code(context, f"{tmpdir}/bench.c")
                                generator.generate_makefile(context, f"{tmpdir}/Makefile")
                            
                            return {
                                "signals": len(signals),
                                "handlers": len(handlers),
                                "status": "SUCCESS"
                            }
                            
                        except Exception as e:
                            return {
                                "status": "FAILED",
                                "error": str(e)
                            }
                        finally:
                            Path(f.name).unlink()
                
                # Run with timeout
                with ThreadPoolExecutor(max_workers=1) as executor:
                    future = executor.submit(run_cpu_attack)
                    
                    try:
                        result = future.result(timeout=30)  # 30 second timeout
                        end_time = time.time()
                        
                        processing_time = end_time - start_time
                        end_cpu = self.process.cpu_percent()
                        
                        # Check for CPU exhaustion
                        if processing_time > 10:  # Took more than 10 seconds
                            self.vulnerabilities.append({
                                "type": "CPU Exhaustion",
                                "level": "MEDIUM",
                                "description": f"CPU attack '{attack['name']}' took {processing_time:.1f} seconds",
                                "processing_time": processing_time
                            })
                        
                        self.results["cpu_exhaustion"].append({
                            "attack": attack["name"],
                            "description": attack["description"],
                            "processing_time": processing_time,
                            "cpu_usage": end_cpu - start_cpu,
                            "status": "COMPLETED"
                        })
                        
                    except Exception as e:
                        self.results["cpu_exhaustion"].append({
                            "attack": attack["name"],
                            "description": attack["description"], 
                            "status": "TIMEOUT_OR_FAILED",
                            "error": str(e)
                        })
                
            except Exception as e:
                self.results["cpu_exhaustion"].append({
                    "attack": attack["name"],
                    "description": attack["description"],
                    "status": "CRASH",
                    "error": str(e)
                })
    
    def test_file_descriptor_exhaustion(self):
        """Test file descriptor exhaustion attacks"""
        print("📁 Testing file descriptor exhaustion...")
        
        try:
            # Get current FD count
            start_fd_count = len(os.listdir('/proc/self/fd'))
            
            # Attempt to exhaust file descriptors
            temp_files = []
            max_attempts = 1000
            
            start_time = time.time()
            
            for i in range(max_attempts):
                try:
                    # Create generator instance with TTL file
                    generator = bitactor_ttl_generator.BitActorTTLGenerator()
                    
                    ttl_content = f"""
@prefix : <http://test.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:Signal{i} rdf:type :Signal ;
    :name "fd_exhaust_{i}" ;
    :id {i} .
"""
                    
                    f = tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False)
                    f.write(ttl_content)
                    f.flush()
                    temp_files.append(f.name)
                    
                    # Don't close the file - keep FD open
                    generator.load_ttl(f.name)
                    
                    # Generate multiple output files without closing
                    context = generator.generate_context(f"fd_test_{i}", f"fd{i}")
                    
                    out_files = []
                    for ext in ['.h', '.py', '.erl', '.c']:
                        out_f = tempfile.NamedTemporaryFile(mode='w', suffix=ext, delete=False)
                        out_files.append(out_f.name)
                        temp_files.append(out_f.name)
                    
                    # Check current FD count
                    current_fd_count = len(os.listdir('/proc/self/fd'))
                    
                    if current_fd_count > start_fd_count + 500:  # Significant FD increase
                        self.vulnerabilities.append({
                            "type": "File Descriptor Leak",
                            "level": "MEDIUM", 
                            "description": f"FD count increased from {start_fd_count} to {current_fd_count}",
                            "fd_increase": current_fd_count - start_fd_count
                        })
                        break
                        
                except OSError as e:
                    if "Too many open files" in str(e):
                        self.vulnerabilities.append({
                            "type": "FD Exhaustion",
                            "level": "HIGH",
                            "description": f"Exhausted file descriptors after {i} iterations",
                            "iterations": i
                        })
                        break
                except Exception as e:
                    break
            
            end_time = time.time()
            final_fd_count = len(os.listdir('/proc/self/fd'))
            
            # Cleanup
            for temp_file in temp_files:
                try:
                    Path(temp_file).unlink()
                except:
                    pass
            
            self.results["file_descriptor_exhaustion"].append({
                "test": "fd_exhaustion_attack",
                "iterations": i,
                "start_fd_count": start_fd_count,
                "final_fd_count": final_fd_count,
                "fd_increase": final_fd_count - start_fd_count,
                "processing_time": end_time - start_time,
                "status": "COMPLETED"
            })
            
        except Exception as e:
            self.results["file_descriptor_exhaustion"].append({
                "test": "fd_exhaustion_attack",
                "status": "FAILED",
                "error": str(e)
            })
    
    def test_disk_flooding(self):
        """Test disk space exhaustion attacks"""
        print("💾 Testing disk flooding attacks...")
        
        try:
            # Get initial disk usage
            disk_usage = psutil.disk_usage('/Users/sac/cns')
            start_free = disk_usage.free
            
            # Attempt to flood disk with large generated files
            generator = bitactor_ttl_generator.BitActorTTLGenerator()
            
            # Create massive TTL
            signal_count = 50000  # 50K signals
            ttl_lines = ["@prefix : <http://test.com/> .", "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .", ""]
            
            for i in range(signal_count):
                # Each signal has a large name to increase file size
                large_name = "X" * 1000  # 1KB per signal name
                ttl_lines.append(f":Signal{i} rdf:type :Signal ; :name \"{large_name}\" ; :id {i} .")
            
            ttl_content = "\n".join(ttl_lines)
            
            start_time = time.time()
            
            with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
                f.write(ttl_content)
                f.flush()
                ttl_file_size = Path(f.name).stat().st_size
                
                try:
                    generator.load_ttl(f.name)
                    context = generator.generate_context("disk_bomb", "disk")
                    
                    # Generate multiple large files
                    with tempfile.TemporaryDirectory() as tmpdir:
                        tmpdir_path = Path(tmpdir)
                        
                        # Generate all code types
                        generator.generate_c_code(context, tmpdir_path / "massive.h")
                        generator.generate_python_code(context, tmpdir_path / "massive.py")
                        generator.generate_erlang_code(context, tmpdir_path / "massive.erl")
                        generator.generate_test_code(context, tmpdir_path / "massive_test.c")
                        generator.generate_benchmark_code(context, tmpdir_path / "massive_bench.c")
                        
                        # Calculate total generated size
                        total_size = sum(f.stat().st_size for f in tmpdir_path.glob("*"))
                        
                        end_time = time.time()
                        
                        # Check for excessive disk usage
                        size_mb = total_size / (1024 * 1024)
                        if size_mb > 100:  # More than 100MB generated
                            self.vulnerabilities.append({
                                "type": "Disk Space Exhaustion",
                                "level": "MEDIUM",
                                "description": f"Generated {size_mb:.1f}MB from {signal_count} signals",
                                "generated_size_mb": size_mb,
                                "signal_count": signal_count
                            })
                        
                        self.results["disk_flooding"].append({
                            "test": "massive_generation",
                            "input_signals": signal_count,
                            "ttl_size_mb": ttl_file_size / (1024 * 1024),
                            "generated_size_mb": size_mb,
                            "processing_time": end_time - start_time,
                            "status": "COMPLETED"
                        })
                        
                finally:
                    Path(f.name).unlink()
                    
        except Exception as e:
            self.results["disk_flooding"].append({
                "test": "massive_generation", 
                "status": "FAILED",
                "error": str(e)
            })
    
    def run_all_tests(self):
        """Execute all Level 2 adversarial tests"""
        print("🚀 Starting Level 2 Adversarial Testing: Resource Exhaustion Attacks")
        print("=" * 70)
        
        self.test_memory_bombs()
        self.test_cpu_exhaustion()
        self.test_file_descriptor_exhaustion()
        self.test_disk_flooding()
        
        return self.generate_report()
    
    def generate_report(self):
        """Generate comprehensive test report"""
        print("\n" + "=" * 70)
        print("📊 LEVEL 2 ADVERSARIAL TESTING REPORT")
        print("=" * 70)
        
        total_tests = sum(len(tests) for tests in self.results.values())
        total_vulnerabilities = len(self.vulnerabilities)
        
        print(f"Total Tests Executed: {total_tests}")
        print(f"Vulnerabilities Found: {total_vulnerabilities}")
        print()
        
        # Test category summary
        for category, tests in self.results.items():
            if tests:
                completed = len([t for t in tests if t.get("status") in ["COMPLETED", "SUCCESS"]])
                failed = len([t for t in tests if t.get("status") in ["FAILED", "TIMEOUT_OR_FAILED"]])
                crashed = len([t for t in tests if t.get("status") == "CRASH"])
                
                print(f"{category.upper().replace('_', ' ')}:")
                print(f"  ✅ Completed: {completed}")
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
            print("✅ No critical vulnerabilities found in Level 2 testing")
            print()
        
        # Risk assessment
        risk_level = "LOW"
        if any(v['level'] == 'HIGH' for v in self.vulnerabilities):
            risk_level = "HIGH"
        elif any(v['level'] == 'MEDIUM' for v in self.vulnerabilities):
            risk_level = "MEDIUM"
        
        print(f"Overall Risk Level: {risk_level}")
        
        return {
            "level": "2",
            "test_name": "Resource Exhaustion Attacks",
            "total_tests": total_tests,
            "vulnerabilities": self.vulnerabilities,
            "risk_level": risk_level,
            "detailed_results": self.results
        }

if __name__ == "__main__":
    tester = AdversarialLevel2()
    report = tester.run_all_tests()
    
    # Save report
    with open("/Users/sac/cns/adversarial_level2_report.json", "w") as f:
        json.dump(report, f, indent=2, default=str)
    
    print(f"Report saved to adversarial_level2_report.json")