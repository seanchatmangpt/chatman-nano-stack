#!/usr/bin/env python3
"""
Demo script showing BitActor CLI usage
"""

import subprocess
import sys

def run_command(cmd):
    """Run a command and print output"""
    print(f"\n🏃 Running: {' '.join(cmd)}")
    print("─" * 60)
    result = subprocess.run(cmd, capture_output=True, text=True)
    print(result.stdout)
    if result.stderr:
        print(f"⚠️  {result.stderr}")
    return result.returncode == 0

def main():
    print("🚀 BitActor CLI Demo")
    print("=" * 60)
    
    # Show help
    run_command(["python3", "bitactor_cli.py", "--help"])
    
    # List signals
    print("\n📋 Listing signals in TTL ontology:")
    run_command(["python3", "bitactor_cli.py", "list-signals", "ontologies/bitactor_semantic_core.ttl"])
    
    # Validate TTL
    print("\n✅ Validating TTL file:")
    run_command(["python3", "bitactor_cli.py", "validate", "ontologies/bitactor_semantic_core.ttl"])
    
    # Generate code
    print("\n🔧 Generating BitActor code:")
    run_command(["python3", "bitactor_cli.py", "generate", 
                 "ontologies/bitactor_semantic_core.ttl", 
                 "generated/demo_output", 
                 "demo"])
    
    # Check generated files
    print("\n📁 Generated files:")
    run_command(["ls", "-la", "generated/demo_output/"])
    
    # Run C tests directly
    print("\n🧪 Running C tests directly:")
    subprocess.run(["make", "-C", "generated/demo_output", "clean"], capture_output=True)
    if run_command(["make", "-C", "generated/demo_output", "test"]):
        print("✅ C tests passed!")
    
    # Test Python module
    print("\n🐍 Testing Python module:")
    test_code = '''
import sys
sys.path.insert(0, 'generated/demo_output')
from demo_bitactor import DemoBitActor, DemoSignal

ba = DemoBitActor()
print("✅ Python BitActor instantiated")

signal = DemoSignal(type=1, payload=0xDEAD)
result = ba.process_signal(signal)
print(f"✅ Signal processed successfully")

stats = ba.get_stats()
print(f"📊 Stats: {stats['signals_processed']} signals processed")
'''
    
    with open("test_python_demo.py", "w") as f:
        f.write(test_code)
    
    if run_command([sys.executable, "test_python_demo.py"]):
        print("✅ Python tests passed!")
    
    # Clean up
    subprocess.run(["rm", "test_python_demo.py"], capture_output=True)
    
    print("\n" + "=" * 60)
    print("✨ BitActor CLI Demo Complete!")
    print("\n💡 The CLI provides:")
    print("  • TTL validation")
    print("  • Code generation for C, Python, and Erlang")
    print("  • Self-checking across languages")
    print("  • Performance benchmarking")
    print("\n🎯 Next: Try 'python3 bitactor_cli.py self-check generated/demo_output'")

if __name__ == "__main__":
    main()