# Implementation Guide: AOT Knowledge Compilation

## Overview

This guide explains how to use the AOT compilation pipeline to transform OWL/SHACL specifications into high-performance C validation code.

## Quick Start

### 1. Prepare Your Specifications

Create OWL ontology (`my_ontology.ttl`):
```turtle
@prefix : <http://example.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:Person a owl:Class ;
    rdfs:label "Person" .

:hasAge a owl:DatatypeProperty ;
    rdfs:domain :Person ;
    rdfs:range xsd:integer .
```

Create SHACL constraints (`my_constraints.ttl`):
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix : <http://example.org/ontology#> .

:PersonShape a sh:NodeShape ;
    sh:targetClass :Person ;
    sh:property [
        sh:path :hasAge ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .
```

### 2. Run the Compiler

```python
from aot_lifecycle import AOTLifecycleManager, SourceSpec, CompilationTarget

# Configure compilation
config = {
    'parallel_compilation': True,
    'benchmark_generation': True,
    'optimization_level': 'O3'
}

# Create manager
manager = AOTLifecycleManager(config)

# Specify sources
sources = SourceSpec(
    owl_files=[Path("my_ontology.ttl")],
    shacl_files=[Path("my_constraints.ttl")]
)

# Define target
target = CompilationTarget(
    name="my_validator",
    platform="linux",
    architecture="x86_64"
)

# Compile
results = await manager.compile(sources, [target])
```

### 3. Use Generated Code

The compiler generates:
- `ontology.h/c` - Class definitions and relationships
- `shacl_validation.h/c` - Constraint validation functions
- `benchmark.c` - Performance testing harness

Example usage:
```c
#include "ontology.h"
#include "shacl_validation.h"

int main() {
    // Create instance
    Person_t* person = person_create();
    person->hasAge = 25;
    
    // Validate
    if (person_validate(person)) {
        printf("Valid person\n");
    }
    
    person_destroy(person);
    return 0;
}
```

## Architecture Details

### Compilation Pipeline

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│   Parser    │ --> │  Optimizer   │ --> │  Generator  │
│             │     │              │     │             │
│ • RDF/TTL   │     │ • Dead code  │     │ • Templates │
│ • SHACL     │     │ • Inlining   │     │ • C code    │
│ • OWL       │     │ • Bit masks  │     │ • Makefiles │
└─────────────┘     └──────────────┘     └─────────────┘
```

### Generated Code Structure

```
generated/
├── ontology.h          # Class definitions
├── ontology.c          # Implementation
├── shacl_validation.h  # Validation API
├── shacl_validation.c  # Constraint checks
├── runtime_support.h   # Helper functions
└── benchmark.c         # Performance tests
```

### Performance Optimizations

1. **Compile-Time Evaluation**
   - Property IDs converted to bit positions
   - String literals become integer constants
   - Constraint ordering optimized

2. **Runtime Efficiency**
   - Zero allocations in validation path
   - Branch-free constraint checking
   - Cache-friendly data layout

3. **Platform Specific**
   - SIMD instructions where applicable
   - Optimal alignment for target CPU
   - Inline assembly for critical paths

## Advanced Features

### Custom Templates

Create custom code generation templates:

```python
# Add custom template
compiler.template_manager.add_template('custom.c.j2', '''
// Generated validation for {{ class.name }}
bool validate_{{ class.name|lower }}({{ class.name }}_t* obj) {
    {% for constraint in class.constraints %}
    if (!check_{{ constraint.type }}(obj)) return false;
    {% endfor %}
    return true;
}
''')
```

### Eightfold Path Integration

Map classes to optimization stages:

```turtle
@prefix eh: <http://cns.io/eightfold#> .

:CriticalClass eh:stage "Right Action" .  # Hot path optimization
:CacheClass eh:stage "Right Mindfulness" . # Memory optimization
```

### Benchmarking

Generated benchmark code includes:
- Cycle-accurate timing
- Cache warming
- Statistical analysis
- Mermaid diagram output

Run benchmarks:
```bash
./benchmark > results.md
```

## Troubleshooting

### Common Issues

**Q: Compilation fails with "undefined reference"**
A: Ensure runtime_support.h is in the include path

**Q: Validation always returns true**
A: Check that constraints are properly linked to classes

**Q: Performance below expectations**
A: Enable -O3 optimization and platform-specific flags

### Performance Tuning

1. **Profile First**
   ```bash
   perf record ./validator
   perf report
   ```

2. **Check Assembly**
   ```bash
   objdump -d validator | grep validate_
   ```

3. **Measure Cache Misses**
   ```bash
   perf stat -e cache-misses ./validator
   ```

## Best Practices

1. **Keep Specifications Simple**
   - Fewer constraints = faster validation
   - Use bit flags over strings
   - Avoid deep inheritance

2. **Optimize for Your Platform**
   - Use native word sizes
   - Align data structures
   - Enable CPU-specific optimizations

3. **Test Thoroughly**
   - Validate edge cases
   - Benchmark with real data
   - Check memory safety with sanitizers

## Future Roadmap

- WebAssembly compilation target
- GPU-accelerated validation
- Distributed validation clusters
- Hardware synthesis (FPGA/ASIC)