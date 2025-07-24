# Mock Implementation Replacement Plan

## Executive Summary

This plan outlines the systematic replacement of mock implementations with production-ready code from the existing CNS/BitActor codebase. The analysis identified 3 main mock files containing simplified test implementations that can be replaced with real, optimized code.

## Mock Files Analysis

### 1. BitActor Core Mock (`bitactor/tests/mock_bitactor.c`)
**Mock Functions:**
- `bitactor_init()` - Simple initialization
- `bitactor_tick()` - Simulated signal processing (1-8 ticks)
- `bitactor_enqueue()` - Basic queue management
- `bitactor_drain()` - Process queued signals
- `bitactor_stats()` - Basic statistics

**Real Implementation:** `bitactor/src/bitactor.c`
- Full engine with fiber scheduling
- Deterministic ≤8 tick execution
- Static dispatch tables
- Integrated telemetry
- Pre-allocated memory pools

### 2. News Validation Mock (`tests/mock_implementations.c`)
**Mock Functions:**
- `bitactor_verify_fast()` - Simple capability check
- `validate_claim_8tick()` - Basic credibility validation

**Real Implementations:**
- `bitactor/src/news_validation_optimized.c` - SIMD-optimized batch processing
- `bitactor/src/news_validation_real_10ns.c` - Ultra-fast 10ns validation

### 3. Telemetry Mock (`bitactor/tests/mock_telemetry.c`)
**Mock Functions:**
- `telemetry_init()` - Basic initialization
- `telemetry_record()` - Simple frame recording
- `telemetry_enable()` - Enable flag
- `telemetry_get_last_frame()` - Frame retrieval

**Real Implementation:** `bitactor/src/bitactor_telemetry.c`
- Reversible trace system
- Zero-allocation ring buffer
- Trace operation sequences
- Blake3-like integrity hashing

## Implementation Strategy

### Phase 1: Create Test Adapter Layer

Create `bitactor/tests/test_adapters.c`:

```c
/*
 * Test Adapters - Bridge test interfaces to real implementations
 * Maintains backward compatibility while using production code
 */

#include "../include/bitactor/bitactor.h"
#include "../src/bitactor.c"  // Real implementation
#include "../src/bitactor_telemetry.c"
#include "../src/news_validation_optimized.c"

// Adapter for simplified test interface
bool bitactor_verify_fast(fast_proof_t* proof) {
    if (!proof) return false;
    
    // Map to real validation using bitactor engine
    static bitactor_engine_t* engine = NULL;
    if (!engine) {
        engine = bitactor_init();
    }
    
    // Create signal from proof
    signal_t sig = {
        .id = proof->hash & 0xFFFFFFFF,
        .kind = 0x01,  // Verification signal
        .payload = proof->capability,
        .timestamp = 0
    };
    
    // Process through real engine
    result_t result = bitactor_tick(engine, &sig);
    return result.status == BITACTOR_OK && proof->capability != 0;
}

// News validation adapter using real implementation
uint32_t validate_claim_8tick(claim_t* claim, source_info_t* source) {
    if (!claim || !source) {
        return STATUS_UNVERIFIED;
    }
    
    // Initialize credibility table if needed
    init_credibility_table();
    
    // Use real fast validation
    uint32_t credibility = check_source_credibility_fast(source->source_id);
    
    // Map to test expected values
    uint32_t result = 0;
    if (credibility >= 80 && claim->confidence >= 70) {
        result |= STATUS_VERIFIED;
    } else if (credibility < 50 || claim->confidence < 60) {
        result |= STATUS_DISPUTED;
    } else {
        result |= STATUS_UNVERIFIED;
    }
    
    // Consider evidence as in mock
    if (claim->evidence_mask == 0 && (result & STATUS_VERIFIED)) {
        result &= ~STATUS_VERIFIED;
        result |= STATUS_UNVERIFIED;
    }
    
    return result;
}
```

### Phase 2: Update Build Configuration

#### Modify `tests/Makefile`:

```makefile
# Toggle between mock and real implementations
USE_REAL_IMPL ?= 1

ifeq ($(USE_REAL_IMPL),1)
    # Use real implementations with adapters
    TEST_SOURCES += bitactor/tests/test_adapters.c
    TEST_SOURCES += bitactor/src/bitactor.c
    TEST_SOURCES += bitactor/src/bitactor_telemetry.c
    TEST_SOURCES += bitactor/src/news_validation_optimized.c
    CFLAGS += -DUSE_REAL_IMPLEMENTATIONS
else
    # Use mocks for isolated testing
    TEST_SOURCES += tests/mock_implementations.c
    TEST_SOURCES += bitactor/tests/mock_bitactor.c
    TEST_SOURCES += bitactor/tests/mock_telemetry.c
    CFLAGS += -DMOCK_NEWS_VALIDATOR
endif
```

#### Modify `bitactor/tests/Makefile`:

```makefile
# Real implementation testing
REAL_IMPL_TESTS = test_bitactor_real_integration \
                  test_news_validation_real \
                  test_telemetry_real

# Add targets for real implementation tests
test_bitactor_real_integration: test_bitactor_real_integration.c
	$(CC) $(CFLAGS) -DUSE_REAL_IMPL $< ../src/bitactor.c -o $@

test_news_validation_real: test_news_validation_real.c  
	$(CC) $(CFLAGS) -DUSE_REAL_IMPL $< ../src/news_validation_optimized.c -o $@
```

### Phase 3: Create Performance Validation Tests

Create `bitactor/tests/test_real_vs_mock_performance.c`:

```c
/*
 * Performance comparison between mock and real implementations
 */

#include <stdio.h>
#include <time.h>
#include "../include/bitactor/bitactor.h"

// Test configurations
#define TEST_ITERATIONS 1000000
#define BATCH_SIZE 8

void benchmark_mock_vs_real(void) {
    printf("=== Mock vs Real Implementation Benchmark ===\n\n");
    
    // Test 1: BitActor tick performance
    {
        bitactor_engine_t* engine = bitactor_init();
        signal_t test_signal = {
            .id = 0x12345678,
            .kind = 0x02,
            .payload = 0xDEADBEEF,
            .timestamp = 0
        };
        
        clock_t start = clock();
        for (int i = 0; i < TEST_ITERATIONS; i++) {
            result_t result = bitactor_tick(engine, &test_signal);
        }
        clock_t end = clock();
        
        double time_spent = ((double)(end - start)) / CLOCKS_PER_SEC;
        double ns_per_tick = (time_spent * 1e9) / TEST_ITERATIONS;
        
        printf("BitActor tick: %.2f ns/operation\n", ns_per_tick);
        printf("  Status: %s\n", ns_per_tick <= 10.0 ? "PASS ✓" : "FAIL ✗");
        
        bitactor_destroy(engine);
    }
    
    // Test 2: News validation performance
    {
        claim_t test_claim = {
            .claim_hash = 0x123456789ABCDEF0,
            .source_id = 0x42,
            .confidence = 85,
            .evidence_mask = 0xFF
        };
        
        source_info_t test_source = {
            .source_id = 0x42,
            .credibility = 90,
            .accuracy_rate = 95
        };
        
        clock_t start = clock();
        for (int i = 0; i < TEST_ITERATIONS; i++) {
            uint32_t result = validate_claim_8tick(&test_claim, &test_source);
        }
        clock_t end = clock();
        
        double time_spent = ((double)(end - start)) / CLOCKS_PER_SEC;
        double ns_per_validation = (time_spent * 1e9) / TEST_ITERATIONS;
        
        printf("\nNews validation: %.2f ns/operation\n", ns_per_validation);
        printf("  Status: %s\n", ns_per_validation <= 10.0 ? "PASS ✓" : "FAIL ✗");
    }
}
```

### Phase 4: Migration Steps

1. **Week 1: Setup & Testing**
   - Create test adapter layer
   - Set up conditional compilation
   - Run existing tests with adapters

2. **Week 2: Performance Validation**
   - Create performance benchmarks
   - Compare mock vs real timings
   - Identify any bottlenecks

3. **Week 3: Integration**
   - Update all test files to use adapters
   - Remove direct mock dependencies
   - Update CI/CD pipelines

4. **Week 4: Cleanup**
   - Archive mock files
   - Document behavior differences
   - Update test documentation

## Benefits

1. **Test Accuracy**: Tests validate real implementation behavior
2. **Performance Testing**: Actual performance characteristics measured
3. **Integration Coverage**: Better end-to-end testing
4. **Code Reuse**: Eliminates duplicate mock code
5. **Maintenance**: Single implementation to maintain

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Timing differences break tests | High | Add timing tolerance ranges |
| Memory allocation patterns differ | Medium | Use test-specific allocators |
| Mock-specific test dependencies | Medium | Maintain adapter compatibility layer |
| Performance overhead in tests | Low | Use conditional fast paths |

## Success Criteria

- [ ] All existing tests pass with real implementations
- [ ] Performance meets or exceeds mock implementations
- [ ] No memory leaks introduced
- [ ] CI/CD pipeline remains stable
- [ ] Documentation updated

## Timeline

- **Total Duration**: 4 weeks
- **Resources**: 1 developer full-time
- **Review Points**: Weekly progress reviews

## Conclusion

Replacing mock implementations with real code will improve test quality and eliminate mock/production discrepancies. The adapter layer approach ensures backward compatibility while enabling a gradual migration path.