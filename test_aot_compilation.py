#!/usr/bin/env python3
"""
Test script for AOT compilation lifecycle
Tests both aot_lifecycle.py and shacl_compiler.py
"""

import asyncio
import sys
from pathlib import Path
from aot_lifecycle import AOTLifecycleManager, SourceSpec, CompilationTarget

async def test_aot_compilation():
    """Test the AOT compilation pipeline"""
    
    # Find test files
    test_owl_file = Path("/Users/sac/cns/owl_compiler_tests/test_cases/basic_ontology.ttl")
    test_shacl_file = Path("/Users/sac/cns/owl_compiler_tests/test_cases/shacl_constraints.ttl")
    
    # Check if test files exist
    if not test_owl_file.exists():
        print(f"Warning: OWL test file not found: {test_owl_file}")
        # Use a minimal test case
        test_owl_file = Path("/tmp/test_ontology.ttl")
        test_owl_file.write_text("""
@prefix : <http://example.org/test#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:TestClass a owl:Class ;
    rdfs:label "Test Class" ;
    rdfs:comment "A simple test class" .

:testProperty a owl:DatatypeProperty ;
    rdfs:domain :TestClass ;
    rdfs:range xsd:string ;
    rdfs:label "test property" .
""")
    
    if not test_shacl_file.exists():
        print(f"Warning: SHACL test file not found: {test_shacl_file}")
        # Use a minimal SHACL test case
        test_shacl_file = Path("/tmp/test_shacl.ttl")
        test_shacl_file.write_text("""
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix : <http://example.org/test#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:TestShape a sh:NodeShape ;
    sh:targetClass :TestClass ;
    sh:property [
        sh:path :testProperty ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Test property is required and must be a string" ;
    ] .
""")
    
    # Configure compilation
    config = {
        'parallel_compilation': True,
        'max_workers': 4,  # Add missing config
        'cache_enabled': False,  # Disable cache for testing
        'validation_enabled': True,
        'benchmark_generation': True,
        'eightfold_integration': True,
        'optimization_level': 'O2',
        'cleanup_on_failure': False,  # Keep files for inspection
        'debug_artifacts': True  # Keep artifacts even on success
    }
    
    # Create lifecycle manager
    manager = AOTLifecycleManager(config)
    
    # Create source specification
    source_spec = SourceSpec(
        owl_files=[test_owl_file] if test_owl_file.exists() else [],
        shacl_files=[test_shacl_file] if test_shacl_file.exists() else []
    )
    
    # Create compilation targets
    targets = [
        CompilationTarget(
            name="test_validator",
            platform="linux",
            architecture="x86_64",
            optimization_level="O2",
            debug_symbols=True,
            eightfold_optimizations=True
        )
    ]
    
    # Run compilation
    print("Starting AOT compilation test...")
    print("================================")
    print(f"OWL files: {source_spec.owl_files}")
    print(f"SHACL files: {source_spec.shacl_files}")
    print(f"Targets: {[t.name for t in targets]}")
    print()
    
    try:
        results = await manager.compile(source_spec, targets)
        
        print("\nCompilation Results:")
        print("===================")
        
        for result in results:
            print(f"\nTarget: {result.target.name}")
            print(f"Success: {result.success}")
            print(f"Artifacts: {list(result.artifacts.keys())}")
            
            # Print metrics
            if result.metrics:
                print("\nStage Metrics:")
                for stage, metrics in result.metrics.items():
                    if metrics.status.value == 'completed':
                        print(f"  {stage.value}: {metrics.duration} - {metrics.status.value}")
                        if metrics.artifacts_produced:
                            print(f"    Artifacts: {', '.join(metrics.artifacts_produced)}")
            
            # Check generated files
            if result.success and 'generated_code_dir' in result.artifacts:
                code_dir = result.artifacts['generated_code_dir']
                print(f"\nGenerated files in {code_dir}:")
                for file_path in code_dir.glob("*"):
                    print(f"  - {file_path.name} ({file_path.stat().st_size} bytes)")
                
                # Show a sample of generated code
                validation_h = code_dir / "validation.h"
                if validation_h.exists():
                    print("\nSample of validation.h:")
                    print("------------------------")
                    lines = validation_h.read_text().split('\n')[:20]
                    print('\n'.join(lines))
                    if len(lines) == 20:
                        print("... (truncated)")
        
        # Return success status
        return any(r.success for r in results)
        
    except Exception as e:
        print(f"\nError during compilation: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Main entry point"""
    success = asyncio.run(test_aot_compilation())
    
    if success:
        print("\n✓ AOT compilation test completed successfully!")
        sys.exit(0)
    else:
        print("\n✗ AOT compilation test failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()