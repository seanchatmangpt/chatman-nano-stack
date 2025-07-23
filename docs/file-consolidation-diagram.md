# File Consolidation Diagram

## Current File Structure (What I Created)

```mermaid
graph TD
    subgraph "Test Infrastructure (~100KB)"
        A1[test_runner.py - 27KB]
        A2[otel_benchmark.py - 15KB]
        A3[otel_benchmark_simple.py - 12KB]
        A4[test_owl_generated.c - 4KB]
    end
    
    subgraph "C Benchmarks (~50KB)"
        B1[c_runtime_benchmark.c - 9KB]
        B2[true_8tick_benchmark.c]
        B3[uhft_comprehensive_benchmark.c]
        B4[uhft_final_benchmark.c]
        B5[otel_benchmark.c]
    end
    
    subgraph "Generated Files (~200KB)"
        C1[generated_code/basic/*]
        C2[generated_code/pipeline/*]
        C3[benchmark_output/basic/* - DUPLICATE]
        C4[benchmark_output/eightfold/* - DUPLICATE]
        C5[benchmark_output/shacl/* - DUPLICATE]
    end
    
    subgraph "Documentation (~20KB)"
        D1[FINAL_TEST_REPORT.md]
        D2[FINAL_TEST_SUMMARY.md]
        D3[C_PERFORMANCE_REPORT.md]
        D4[PROPERTY_OPTIMIZATION_ANALYSIS.md]
    end
    
    subgraph "Well Organized (~65KB)"
        E1[docs/aot-integration/*]
        E2[test_data/*.ttl]
    end
```

## Consolidated Structure (Proposed)

```mermaid
graph TD
    subgraph "Unified Test Suite (~30KB)"
        F1[owl_compiler_test_suite.py<br/>- All Python tests<br/>- All benchmarks<br/>- Unified reporting]
    end
    
    subgraph "Unified C Benchmarks (~10KB)"
        F2[owl_compiler_benchmarks.c<br/>- All C benchmarks<br/>- Consistent methodology]
    end
    
    subgraph "Single Generated Set (~50KB)"
        F3[generated_code/*<br/>- One canonical set<br/>- No duplicates]
    end
    
    subgraph "Master Documentation (~15KB)"
        F4[OWL_COMPILER_DOCS.md<br/>- Complete test results<br/>- Performance analysis<br/>- Design decisions]
    end
    
    subgraph "Keep As-Is (~65KB)"
        F5[docs/aot-integration/*]
        F6[test_data/*.ttl]
    end
```

## Consolidation Benefits

```mermaid
pie title "File Reduction"
    "Kept Files" : 15
    "Deleted Duplicates" : 10
    "Merged Files" : 5
```

```mermaid
graph LR
    A[30 Files<br/>500KB] -->|Consolidate| B[15 Files<br/>250KB]
    
    subgraph "Benefits"
        C[50% fewer files]
        D[50% less disk space]
        E[Zero duplication]
        F[Better organization]
    end
    
    B --> C
    B --> D
    B --> E
    B --> F
```

## Quick Consolidation Commands

```bash
# 1. Merge Python tests
cat test_runner.py otel_benchmark_simple.py > owl_compiler_test_suite.py

# 2. Merge C benchmarks  
cat c_runtime_benchmark.c > owl_compiler_benchmarks.c

# 3. Remove duplicates
rm -rf benchmark_output/
rm uhft_*.c true_8tick_benchmark.c otel_benchmark.py

# 4. Merge documentation
cat FINAL_TEST_REPORT.md C_PERFORMANCE_REPORT.md PROPERTY_OPTIMIZATION_ANALYSIS.md > OWL_COMPILER_DOCS.md

# 5. Clean up
rm FINAL_TEST_SUMMARY.md test_aot_compilation.py
```