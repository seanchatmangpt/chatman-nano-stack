#!/usr/bin/env python3

import subprocess
import os
import json
import sys
from pathlib import Path

def run_command(cmd, cwd=None):
    """Run shell command and return output"""
    try:
        result = subprocess.run(cmd, shell=True, cwd=cwd, 
                              capture_output=True, text=True, check=True)
        return result.stdout, result.stderr
    except subprocess.CalledProcessError as e:
        print(f"Error running command '{cmd}': {e}")
        print(f"Stdout: {e.stdout}")
        print(f"Stderr: {e.stderr}")
        return "", e.stderr

def compile_with_coverage():
    """Compile tests with coverage instrumentation"""
    print("🔧 Compiling tests with coverage instrumentation...")
    
    coverage_flags = "-fprofile-arcs -ftest-coverage --coverage"
    
    # Compile news validation test
    cmd1 = f"gcc -O0 -g {coverage_flags} -Wall -Wextra -std=c99 " \
           f"bitactor/tests/test_news_validation_mac.c " \
           f"-o bitactor/tests/test_news_validation_coverage"
    
    # Compile advanced tick test  
    cmd2 = f"gcc -O0 -g {coverage_flags} -Wall -Wextra -std=c99 " \
           f"bitactor/tests/test_advanced_tick_optimization.c " \
           f"-o bitactor/tests/test_advanced_tick_coverage"
    
    # Compile bitfiber test
    cmd3 = f"gcc -O0 -g {coverage_flags} -Wall -Wextra -std=c99 " \
           f"bitactor/tests/test_bitfiber_integration.c " \
           f"-o bitactor/tests/test_bitfiber_coverage"
    
    for cmd in [cmd1, cmd2, cmd3]:
        stdout, stderr = run_command(cmd)
        if stderr and "error:" in stderr.lower():
            print(f"❌ Compilation failed: {stderr}")
            return False
    
    print("✅ Compilation with coverage completed")
    return True

def run_coverage_tests():
    """Run all tests to generate coverage data"""
    print("🧪 Running tests to generate coverage data...")
    
    test_executables = [
        "bitactor/tests/test_news_validation_coverage",
        "bitactor/tests/test_advanced_tick_coverage", 
        "bitactor/tests/test_bitfiber_coverage"
    ]
    
    results = {}
    
    for test in test_executables:
        if os.path.exists(test):
            print(f"Running {test}...")
            stdout, stderr = run_command(f"./{test}")
            
            # Extract test results
            if "✅ All tests passed!" in stdout:
                results[test] = "PASSED"
                print(f"✅ {test} - All tests passed")
            else:
                results[test] = "FAILED"
                print(f"❌ {test} - Some tests failed")
        else:
            print(f"⚠️ Test executable {test} not found")
            results[test] = "NOT_FOUND"
    
    return results

def generate_coverage_report():
    """Generate coverage report using gcov"""
    print("📊 Generating coverage report...")
    
    # Find all .gcda files (coverage data)
    gcda_files = list(Path(".").rglob("*.gcda"))
    if not gcda_files:
        print("⚠️ No coverage data files found")
        return {}
    
    coverage_data = {}
    
    # Process each source file
    source_files = [
        "bitactor/tests/test_news_validation_mac.c",
        "bitactor/tests/test_advanced_tick_optimization.c",
        "bitactor/tests/test_bitfiber_integration.c"
    ]
    
    total_lines = 0
    covered_lines = 0
    
    for source_file in source_files:
        if os.path.exists(source_file):
            # Run gcov on the source file
            gcov_cmd = f"gcov -b -c {source_file}"
            stdout, stderr = run_command(gcov_cmd)
            
            # Parse gcov output
            lines_in_file = 0
            lines_covered = 0
            
            if os.path.exists(f"{os.path.basename(source_file)}.gcov"):
                with open(f"{os.path.basename(source_file)}.gcov", 'r') as f:
                    for line in f:
                        if line.strip() and not line.startswith(' ' * 8 + '-'):
                            lines_in_file += 1
                            if not line.startswith(' ' * 8 + '#####'):
                                lines_covered += 1
            
            if lines_in_file > 0:
                coverage_percent = (lines_covered / lines_in_file) * 100
                coverage_data[source_file] = {
                    'lines_total': lines_in_file,
                    'lines_covered': lines_covered,
                    'coverage_percent': coverage_percent
                }
                
                total_lines += lines_in_file
                covered_lines += lines_covered
                
                print(f"📈 {source_file}: {lines_covered}/{lines_in_file} lines ({coverage_percent:.1f}%)")
    
    # Calculate overall coverage
    if total_lines > 0:
        overall_coverage = (covered_lines / total_lines) * 100
        coverage_data['overall'] = {
            'lines_total': total_lines,
            'lines_covered': covered_lines,
            'coverage_percent': overall_coverage
        }
        
        print(f"\n📊 Overall Coverage: {covered_lines}/{total_lines} lines ({overall_coverage:.1f}%)")
    
    return coverage_data

def create_coverage_mermaid(coverage_data):
    """Create a Mermaid diagram for coverage visualization"""
    if 'overall' not in coverage_data:
        return ""
    
    overall = coverage_data['overall']
    coverage_percent = overall['coverage_percent']
    
    # Determine color based on coverage
    if coverage_percent >= 80:
        color = "green"
        status = "EXCELLENT"
    elif coverage_percent >= 70:
        color = "yellow" 
        status = "GOOD"
    else:
        color = "red"
        status = "NEEDS_IMPROVEMENT"
    
    mermaid = f"""
```mermaid
graph TD
    A[BitActor Integration Tests<br/>Total Lines: {overall['lines_total']}] --> B[Coverage Analysis]
    B --> C[Lines Covered: {overall['lines_covered']}]
    B --> D[Coverage: {coverage_percent:.1f}%]
    D --> E[Status: {status}]
    
    style A fill:#f9f9f9
    style B fill:#e1f5fe
    style C fill:#e8f5e8
    style D fill:#{color}
    style E fill:#{color}
```
"""
    
    return mermaid

def main():
    """Main coverage analysis workflow"""
    print("🎯 BitActor Integration Gap Coverage Analysis")
    print("=" * 50)
    
    # Step 1: Compile with coverage
    if not compile_with_coverage():
        print("❌ Failed to compile with coverage instrumentation")
        sys.exit(1)
    
    # Step 2: Run tests
    test_results = run_coverage_tests()
    
    # Step 3: Generate coverage report
    coverage_data = generate_coverage_report()
    
    # Step 4: Analyze results
    if 'overall' in coverage_data:
        overall_coverage = coverage_data['overall']['coverage_percent']
        
        print(f"\n🎯 COVERAGE ANALYSIS RESULTS")
        print(f"=" * 30)
        print(f"Overall Coverage: {overall_coverage:.1f}%")
        print(f"Target Coverage: 80.0%")
        
        if overall_coverage >= 80.0:
            print("✅ Coverage target achieved!")
            status = "PASSED"
        else:
            print("❌ Coverage target not met")
            status = "FAILED"
        
        # Generate Mermaid diagram
        mermaid_chart = create_coverage_mermaid(coverage_data)
        print(f"\n📊 Coverage Visualization:")
        print(mermaid_chart)
        
        # Save results
        results = {
            'timestamp': '2025-07-23T19:33:00Z',
            'coverage_data': coverage_data,
            'test_results': test_results,
            'status': status,
            'mermaid_chart': mermaid_chart
        }
        
        with open('coverage_analysis_results.json', 'w') as f:
            json.dump(results, f, indent=2)
        
        print(f"\n💾 Results saved to coverage_analysis_results.json")
        
        return 0 if status == "PASSED" else 1
    else:
        print("❌ No coverage data generated")
        return 1

if __name__ == "__main__":
    sys.exit(main())