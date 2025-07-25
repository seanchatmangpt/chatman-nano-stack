#!/usr/bin/env python3
import subprocess

def test_adversarial_inputs():
    malicious_inputs = [
        "'; DROP TABLE users; --",
        "<script>alert('xss')</script>",
        "../../../etc/passwd",
        "A" * 10000,
        "\x00" * 1000
    ]
    
    survived = 0
    for inp in malicious_inputs:
        result = subprocess.run([
            f"./bin/cns_litigator_test",
            "--input", inp
        ], capture_output=True, timeout=1)
        
        if result.returncode != 0:
            survived += 1
    
    survival_rate = (survived / len(malicious_inputs)) * 100
    return survival_rate

if __name__ == "__main__":
    rate = test_adversarial_inputs()
    print(f"Adversarial survival rate: {rate:.1f}%")
    assert rate >= 91.0, "Failed to meet 91% survival rate requirement" 