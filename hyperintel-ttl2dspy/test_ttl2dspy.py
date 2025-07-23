#!/usr/bin/env python3
"""
Test TTL2DSPy Transpiler
Comprehensive testing of the TTL to DSPy Signature transpiler
"""

import subprocess
import sys
import tempfile
from pathlib import Path


def create_test_ontology() -> str:
    """Create test ontology with SHACL shapes"""
    return """
@prefix : <http://test.example/> .
@prefix cns: <http://cns.io/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology definition
: a owl:Ontology ;
    rdfs:label "Test Ontology for TTL2DSPy" ;
    rdfs:comment "Test ontology demonstrating SHACL to DSPy conversion" .

# Classes
:TradingOrder a owl:Class ;
    rdfs:label "Trading Order" ;
    rdfs:comment "Represents a trading order in the system" .

:RiskAssessment a owl:Class ;
    rdfs:label "Risk Assessment" ;
    rdfs:comment "AI-powered risk assessment for trading decisions" .

# Properties
:orderPrice a owl:DatatypeProperty ;
    rdfs:label "Order Price" ;
    rdfs:domain :TradingOrder ;
    rdfs:range xsd:decimal .

:orderQuantity a owl:DatatypeProperty ;
    rdfs:label "Order Quantity" ;
    rdfs:domain :TradingOrder ;
    rdfs:range xsd:integer .

:symbol a owl:DatatypeProperty ;
    rdfs:label "Trading Symbol" ;
    rdfs:domain :TradingOrder ;
    rdfs:range xsd:string .

:riskScore a owl:DatatypeProperty ;
    rdfs:label "Risk Score" ;
    rdfs:domain :RiskAssessment ;
    rdfs:range xsd:float .

:recommendation a owl:DatatypeProperty ;
    rdfs:label "AI Recommendation" ;
    rdfs:domain :RiskAssessment ;
    rdfs:range xsd:string .

# SHACL Shapes - Pattern 1: Direct property shapes
:TradingOrderPriceShape a sh:PropertyShape ;
    sh:targetClass :TradingOrder ;
    sh:path :orderPrice ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0.0 ;
    rdfs:comment "Order price must be positive decimal value" .

:TradingOrderQuantityShape a sh:PropertyShape ;
    sh:targetClass :TradingOrder ;
    sh:path :orderQuantity ;
    sh:datatype xsd:integer ;
    sh:minInclusive 1 ;
    rdfs:comment "Order quantity must be positive integer" .

:TradingOrderSymbolShape a sh:PropertyShape ;
    sh:targetClass :TradingOrder ;
    sh:path :symbol ;
    sh:datatype xsd:string ;
    sh:maxLength 10 ;
    rdfs:comment "Trading symbol for the order" .

# SHACL Shapes - Pattern 2: Node shape with property links
:RiskAssessmentShape a sh:NodeShape ;
    sh:targetClass :RiskAssessment ;
    sh:property [
        sh:path :riskScore ;
        sh:datatype xsd:float ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 1.0 ;
        rdfs:comment "Normalized risk score between 0 and 1"
    ] ;
    sh:property [
        sh:path :recommendation ;
        sh:datatype xsd:string ;
        cns:outputField true ;
        rdfs:comment "AI-generated trading recommendation"
    ] .
"""

def create_complex_ontology() -> str:
    """Create more complex ontology for advanced testing"""
    return """
@prefix : <http://complex.example/> .
@prefix cns: <http://cns.io/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

: a owl:Ontology ;
    rdfs:label "Complex Test Ontology" .

# Classes with challenging names
:AI-Agent a owl:Class ;
    rdfs:label "AI Agent" .

:Market_Data a owl:Class ;
    rdfs:label "Market Data" .

:Order123 a owl:Class ;
    rdfs:label "Order Type 123" .

# Properties with name collisions
:has-ID a owl:DatatypeProperty ;
    rdfs:domain :AI-Agent ;
    rdfs:range xsd:string .

:has_id a owl:DatatypeProperty ;
    rdfs:domain :Market_Data ;
    rdfs:range xsd:string .

:metadata a owl:DatatypeProperty ;
    rdfs:domain :AI-Agent ;
    rdfs:range xsd:string .

# SHACL shapes
:AIAgentShape a sh:NodeShape ;
    sh:targetClass :AI-Agent ;
    sh:property [
        sh:path :has-ID ;
        sh:datatype xsd:string ;
        rdfs:comment "Unique identifier for AI agent"
    ] ;
    sh:property [
        sh:path :metadata ;
        sh:datatype xsd:string ;
        rdfs:comment "Agent metadata information"
    ] .

:MarketDataShape a sh:NodeShape ;
    sh:targetClass :Market_Data ;
    sh:property [
        sh:path :has_id ;
        sh:datatype xsd:string ;
        rdfs:comment "Market data identifier"
    ] .
"""

def test_basic_conversion():
    """Test basic TTL to DSPy conversion"""
    print("🧪 Testing basic TTL to DSPy conversion...")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Create test ontology
        test_ttl = tmpdir / "test.ttl"
        test_ttl.write_text(create_test_ontology())

        # Create output file
        output_py = tmpdir / "signatures.py"

        # Run transpiler
        result = subprocess.run([
            sys.executable, "ttl2dspy.py",
            str(test_ttl), str(output_py),
            "--verbose"
        ], capture_output=True, text=True)

        print(f"Exit code: {result.returncode}")
        if result.stdout:
            print(f"STDOUT: {result.stdout}")
        if result.stderr:
            print(f"STDERR: {result.stderr}")

        # Check if output file was created
        if output_py.exists():
            print("✅ Output file created successfully")

            # Read and display generated code
            generated_code = output_py.read_text()
            print("\n📄 Generated DSPy signatures:")
            print("="*60)
            print(generated_code)
            print("="*60)

            # Try to import and validate
            try:
                # Create a test module
                test_module = tmpdir / "test_import.py"
                test_module.write_text(f"""
import sys
sys.path.insert(0, '{tmpdir}')

try:
    import signatures
    print("✅ Import successful")
    print(f"Available signatures: {{signatures.__all__}}")
    
    # Test signature instantiation
    for sig_name in signatures.__all__:
        sig_class = getattr(signatures, sig_name)
        print(f"  - {{sig_name}}: {{sig_class.__doc__[:50]}}...")
        
except Exception as e:
    print(f"❌ Import failed: {{e}}")
                """)

                import_result = subprocess.run([
                    sys.executable, str(test_module)
                ], capture_output=True, text=True)

                print("\n🔍 Import test results:")
                print(import_result.stdout)
                if import_result.stderr:
                    print(f"Import errors: {import_result.stderr}")

            except Exception as e:
                print(f"❌ Could not test import: {e}")

        else:
            print("❌ Output file not created")
            return False

    return result.returncode == 0

def test_batch_mode():
    """Test batch processing mode"""
    print("\n🧪 Testing batch mode...")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Create multiple test ontologies
        ontologies = {
            "trading.ttl": create_test_ontology(),
            "complex.ttl": create_complex_ontology()
        }

        for filename, content in ontologies.items():
            (tmpdir / filename).write_text(content)

        # Create output directory
        output_dir = tmpdir / "signatures"

        # Run batch transpilation
        result = subprocess.run([
            sys.executable, "ttl2dspy.py",
            str(tmpdir / "*.ttl"), str(output_dir),
            "--batch", "--verbose"
        ], capture_output=True, text=True)

        print(f"Batch exit code: {result.returncode}")
        print(f"Batch output: {result.stdout}")
        if result.stderr:
            print(f"Batch errors: {result.stderr}")

        # Check generated files
        if output_dir.exists():
            generated_files = list(output_dir.glob("*.py"))
            print(f"✅ Generated {len(generated_files)} files:")
            for file in generated_files:
                print(f"  - {file.name}")
        else:
            print("❌ Output directory not created")
            return False

    return result.returncode == 0

def test_merge_mode():
    """Test merge mode"""
    print("\n🧪 Testing merge mode...")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Create multiple test ontologies
        (tmpdir / "trading.ttl").write_text(create_test_ontology())
        (tmpdir / "complex.ttl").write_text(create_complex_ontology())

        # Create merged output
        merged_output = tmpdir / "all_signatures.py"

        # Run merge transpilation
        result = subprocess.run([
            sys.executable, "ttl2dspy.py",
            str(tmpdir), str(merged_output),
            "--merge", "--verbose"
        ], capture_output=True, text=True)

        print(f"Merge exit code: {result.returncode}")
        print(f"Merge output: {result.stdout}")
        if result.stderr:
            print(f"Merge errors: {result.stderr}")

        # Check merged file
        if merged_output.exists():
            print("✅ Merged file created")
            content = merged_output.read_text()

            # Count signatures in merged file
            signature_count = content.count("class ") - 1  # Subtract imports
            print(f"📊 Merged signatures: {signature_count}")

            # Show part of merged content
            lines = content.split('\n')
            print("\n📄 Merged file preview:")
            print('\n'.join(lines[:20]) + "\n...")

        else:
            print("❌ Merged file not created")
            return False

    return result.returncode == 0

def test_error_handling():
    """Test error handling"""
    print("\n🧪 Testing error handling...")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        # Create invalid TTL file
        invalid_ttl = tmpdir / "invalid.ttl"
        invalid_ttl.write_text("This is not valid TTL content @@@")

        # Try to process invalid file
        result = subprocess.run([
            sys.executable, "ttl2dspy.py",
            str(invalid_ttl), str(tmpdir / "output.py"),
            "--verbose"
        ], capture_output=True, text=True)

        print(f"Error test exit code: {result.returncode}")
        print(f"Error test output: {result.stdout}")
        if result.stderr:
            print(f"Error test stderr: {result.stderr}")

        # Should return error code
        if result.returncode != 0:
            print("✅ Error handling works correctly")
            return True
        else:
            print("❌ Should have returned error code")
            return False

def run_comprehensive_test():
    """Run all tests"""
    print("🚀 Running TTL2DSPy Comprehensive Test Suite")
    print("="*60)

    tests = [
        ("Basic Conversion", test_basic_conversion),
        ("Batch Mode", test_batch_mode),
        ("Merge Mode", test_merge_mode),
        ("Error Handling", test_error_handling)
    ]

    results = []

    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append((test_name, result))
            status = "✅ PASSED" if result else "❌ FAILED"
            print(f"\n{status}: {test_name}")
        except Exception as e:
            results.append((test_name, False))
            print(f"\n❌ FAILED: {test_name} - {str(e)}")

    # Summary
    print("\n" + "="*60)
    print("🏆 TEST SUMMARY")
    print("="*60)

    passed = sum(1 for _, result in results if result)
    total = len(results)

    for test_name, result in results:
        status = "✅" if result else "❌"
        print(f"{status} {test_name}")

    print(f"\nOverall: {passed}/{total} tests passed ({(passed/total)*100:.1f}%)")

    if passed == total:
        print("🎉 All tests passed! TTL2DSPy is ready for production.")
    else:
        print("⚠️  Some tests failed. Review implementation.")

    return passed == total

if __name__ == "__main__":
    success = run_comprehensive_test()
    sys.exit(0 if success else 1)
