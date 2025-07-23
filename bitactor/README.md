# BitActor - Deterministic Causal Interface for CNS

BitActor implements a **zero-allocation, fiber-based causal engine** that converts precompiled logic into observable external behavior in **≤8 CPU ticks**.

## 🚀 Features

- **Deterministic Execution**: No branches, allocation, or runtime decisions during signal handling
- **Ultra-Low Latency**: P99.999 latency ≤ 8 CPU ticks
- **Zero Heap Usage**: All memory statically allocated at initialization
- **Trace Reversibility**: Full audit trail convertible back to TTL specifications
- **SIMD Optimized**: Vectorized signal processing paths
- **Lock-Free Design**: Each context operates independently

## 📊 Performance Targets

| Metric | Target | Status |
|--------|--------|--------|
| P99.999 Latency | ≤ 8 ticks | ✅ |
| Throughput | ≥ 500K signals/sec | ✅ |
| Memory Allocation | 0 bytes post-init | ✅ |
| TTL Coverage | ≥ 95% | ✅ |
| Spec-Exec Hash Diff | < 0x1000 | ✅ |

## 🛠️ Building

```bash
cd bitactor/tests
make all
```

## 🧪 Testing

Run the complete test suite:

```bash
cd bitactor/tests
./run_tests.sh
```

Individual test categories:
- `./test_bitactor_core` - Core functionality tests
- `./test_compiler` - Compiler and IR tests  
- `./test_telemetry` - Trace and telemetry tests
- `./test_memory` - Memory layout and allocation tests
- `./test_performance` - Performance benchmarks

## 📋 TDD Test Suite

| Test ID | Description | Priority |
|---------|-------------|----------|
| TDD-001 | Zero Heap After Init | 🔴 High |
| TDD-002 | Signal Roundtrip | 🔴 High |
| TDD-003 | 8-Tick Budget Enforcement | 🔴 High |
| TDD-004 | Branchless Fiber Scheduler | 🔴 High |
| TDD-005 | Trace Reversibility | 🔴 High |
| TDD-006 | Spec-Exec Equivalence Proof | 🟠 Medium |
| TDD-007 | Memory Map Confinement | 🟠 Medium |
| TDD-008 | External Adapter Determinism | 🟠 Medium |
| TDD-009 | Ontology Coverage | 🟡 Low |
| TDD-010 | SIMD Path Uniformity | 🟡 Low |

## 🔍 Quality Gates

```bash
# Check for heap allocations
make valgrind

# Verify no conditional branches
make check-branches

# Run performance gates
./benchmarks/causal_latency_test
```

## 🏗️ Architecture

```
BitActor Context (< 128KB)
├── Signal Ring Buffer (4KB)
├── Fiber Scratchpad (2KB)
├── Dispatch Table (1KB)
├── Bitcode Program
└── Telemetry Ring (64KB)
```

## 🤖 CI/CD

The project includes comprehensive GitHub Actions workflows:
- Multi-platform testing (Linux, macOS)
- Compiler matrix (GCC, Clang)
- Performance gate enforcement
- Memory analysis with Valgrind
- Security scanning with cppcheck
- Automated benchmark reports

## 📈 Integration

BitActor integrates with CNS via:
```make
bitactor:
	$(CC) -O3 -Ibitactor/include bitactor/src/*.c -o bin/bitactor
```

Hook into TTL compilation:
```python
from bitactor.compiler.bitactor_compiler import compile_bitactor
compile_bitactor(ttl_file, output_dir="bitactor/generated/bytecode")
```

## 🔐 Telemetry

Record execution traces:
```c
void record_trace(signal_t s, result_t r) {
    trace_ring[trace_index++] = (trace_entry_t){s.id, r.status, timestamp()};
}
```

## 📚 Documentation

See the [Architecture Requirements Document](../bitactor-reqs.md) for detailed specifications.