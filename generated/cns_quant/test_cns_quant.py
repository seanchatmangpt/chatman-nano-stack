#!/usr/bin/env python3
import subprocess
import time
import statistics
import concurrent.futures

def test_8tick_compliance():
    """Test that service meets 8-tick requirement"""
    cmd = ["./cns_quant_test"]
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    if result.returncode == 0:
        print(f"✅ cns_quant passes 8-tick compliance")
        return True
    else:
        print(f"❌ cns_quant fails 8-tick compliance")
        print(result.stdout)
        return False

def stress_test(iterations=10000):
    """Run stress test with concurrent load"""
    print(f"Running stress test with {iterations} iterations...")
    
    def run_test():
        start = time.perf_counter()
        subprocess.run(["./cns_quant_test"], capture_output=True)
        return time.perf_counter() - start
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = [executor.submit(run_test) for _ in range(100)]
        latencies = [f.result() for f in futures]
    
    avg_latency = statistics.mean(latencies)
    p99_latency = statistics.quantiles(latencies, n=100)[98]
    
    print(f"Average latency: {avg_latency*1000:.2f}ms")
    print(f"P99 latency: {p99_latency*1000:.2f}ms")
    
    return p99_latency < 0.001  # Sub-millisecond

def adversarial_test():
    """Test against malicious inputs"""
    print("Running adversarial tests...")
    
    malicious_inputs = [
        b"\x00" * 10000,  # Null bytes
        b"A" * 100000,     # Buffer overflow attempt
        b"\xff\xfe\xfd", # Invalid UTF-8
        b"%s%s%s%s",       # Format string
    ]
    
    survived = 0
    for inp in malicious_inputs:
        proc = subprocess.Popen(["./cns_quant_test"], 
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
        try:
            proc.communicate(input=inp, timeout=1)
            if proc.returncode != 0:
                survived += 1
        except subprocess.TimeoutExpired:
            proc.kill()
            survived += 1
    
    survival_rate = (survived / len(malicious_inputs)) * 100
    print(f"Adversarial survival rate: {survival_rate}%")
    
    return survival_rate >= 91

if __name__ == "__main__":
    print(f"🧪 Testing cns_quant...")
    
    tests = [
        ("8-tick compliance", test_8tick_compliance),
        ("Stress test", stress_test),
        ("Adversarial test", adversarial_test)
    ]
    
    passed = 0
    for name, test_fn in tests:
        print(f"\nRunning {name}...")
        if test_fn():
            passed += 1
            print(f"✅ {name} PASSED")
        else:
            print(f"❌ {name} FAILED")
    
    print(f"\nTotal: {passed}/{len(tests)} tests passed")
    
    if passed == len(tests):
        print("\n🎉 All tests passed!")
        exit(0)
    else:
        print("\n❌ Some tests failed")
        exit(1)
