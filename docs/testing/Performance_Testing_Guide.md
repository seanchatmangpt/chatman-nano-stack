# Performance Testing Guide for CNS

## Ultra-Low Latency Testing Methodology

**Version:** 1.0  
**Focus:** Sub-8-tick performance validation  
**Target:** CNS real-time systems

---

## 📊 Performance Requirements

| Metric | Target | Critical | Measurement Method |
|--------|--------|----------|-------------------|
| **P50 Latency** | ≤ 6 ticks | ≤ 8 ticks | 10K sample statistical analysis |
| **P99 Latency** | ≤ 7 ticks | ≤ 8 ticks | 10K sample statistical analysis |
| **P99.999 Latency** | ≤ 8 ticks | ≤ 8 ticks | 100K sample analysis (critical) |
| **Throughput** | ≥ 500K signals/sec | ≥ 100K signals/sec | Sustained load testing |
| **Memory Usage** | 0 bytes post-init | 0 bytes | Valgrind massif analysis |
| **Cache Efficiency** | Random ≤ 3x sequential | Random ≤ 5x sequential | Access pattern comparison |

---

## 🔬 Measurement Techniques

### Cycle-Accurate Timing

```c
// Primary timing method - CPU cycle counter
static inline uint64_t rdtsc_precise(void) {
#if defined(__x86_64__) || defined(__i386__)
    unsigned int lo, hi;
    __asm__ __volatile__ (
        "cpuid\n\t"          // Serialize instruction pipeline
        "rdtsc\n\t"          // Read time stamp counter
        : "=a" (lo), "=d" (hi)
        :
        : "ebx", "ecx"
    );
    return ((uint64_t)hi << 32) | lo;
#elif defined(__aarch64__)
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    #error "Unsupported architecture for cycle counting"
#endif
}

// Warm-up sequence to stabilize measurements
void performance_warmup(void (*test_func)(void)) {
    for (int i = 0; i < 10000; i++) {
        test_func();
    }
}

// Statistical measurement with outlier filtering
typedef struct {
    uint64_t min;
    uint64_t max;
    uint64_t p50;
    uint64_t p90;
    uint64_t p99;
    uint64_t p999;
    uint64_t p9999;
    uint64_t p99999;
    double mean;
    double stddev;
} latency_stats_t;

latency_stats_t measure_latency_distribution(
    void (*test_func)(void), 
    int samples
) {
    uint64_t* measurements = malloc(samples * sizeof(uint64_t));
    
    // Collect samples
    for (int i = 0; i < samples; i++) {
        uint64_t start = rdtsc_precise();
        test_func();
        uint64_t end = rdtsc_precise();
        measurements[i] = end - start;
    }
    
    // Sort for percentile calculation
    qsort(measurements, samples, sizeof(uint64_t), compare_uint64);
    
    latency_stats_t stats = {
        .min = measurements[0],
        .max = measurements[samples - 1],
        .p50 = measurements[samples / 2],
        .p90 = measurements[(int)(samples * 0.90)],
        .p99 = measurements[(int)(samples * 0.99)],
        .p999 = measurements[(int)(samples * 0.999)],
        .p9999 = measurements[(int)(samples * 0.9999)],
        .p99999 = measurements[samples - 1]
    };
    
    // Calculate mean and standard deviation
    uint64_t sum = 0;
    for (int i = 0; i < samples; i++) {
        sum += measurements[i];
    }
    stats.mean = (double)sum / samples;
    
    double variance = 0;
    for (int i = 0; i < samples; i++) {
        double diff = measurements[i] - stats.mean;
        variance += diff * diff;
    }
    stats.stddev = sqrt(variance / samples);
    
    free(measurements);
    return stats;
}
```

### Memory Usage Analysis

```c
// Precise heap tracking using system calls
size_t get_precise_heap_usage(void) {
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return usage.ru_maxrss; // Peak resident set size
}

// Memory pattern analysis
typedef struct {
    size_t baseline;
    size_t peak;
    size_t current;
    int allocations;
    int deallocations;
    size_t total_allocated;
} memory_profile_t;

memory_profile_t profile_memory_usage(void (*test_func)(void)) {
    memory_profile_t profile = {0};
    
    profile.baseline = get_precise_heap_usage();
    
    // Override malloc/free for tracking
    #ifdef MEMORY_PROFILING
    install_malloc_hooks();
    #endif
    
    test_func();
    
    profile.current = get_precise_heap_usage();
    profile.peak = get_peak_usage();
    
    #ifdef MEMORY_PROFILING
    profile.allocations = get_malloc_count();
    profile.deallocations = get_free_count();
    profile.total_allocated = get_total_allocated();
    remove_malloc_hooks();
    #endif
    
    return profile;
}
```

### Cache Performance Analysis

```c
// Cache miss measurement using performance counters
typedef struct {
    uint64_t l1_misses;
    uint64_t l2_misses;
    uint64_t l3_misses;
    uint64_t instructions;
    uint64_t cycles;
    double cache_efficiency;
} cache_stats_t;

cache_stats_t measure_cache_performance(void (*test_func)(void)) {
    cache_stats_t stats = {0};
    
    #ifdef __linux__
    // Use perf_event_open for precise hardware counters
    int fd_l1 = perf_event_open(&pe_l1_misses, 0, -1, -1, 0);
    int fd_l2 = perf_event_open(&pe_l2_misses, 0, -1, -1, 0);
    int fd_cycles = perf_event_open(&pe_cycles, 0, -1, -1, 0);
    
    // Reset counters
    ioctl(fd_l1, PERF_EVENT_IOC_RESET, 0);
    ioctl(fd_l2, PERF_EVENT_IOC_RESET, 0);
    ioctl(fd_cycles, PERF_EVENT_IOC_RESET, 0);
    
    // Enable counting
    ioctl(fd_l1, PERF_EVENT_IOC_ENABLE, 0);
    ioctl(fd_l2, PERF_EVENT_IOC_ENABLE, 0);
    ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, 0);
    
    test_func();
    
    // Disable and read counters
    ioctl(fd_l1, PERF_EVENT_IOC_DISABLE, 0);
    ioctl(fd_l2, PERF_EVENT_IOC_DISABLE, 0);
    ioctl(fd_cycles, PERF_EVENT_IOC_DISABLE, 0);
    
    read(fd_l1, &stats.l1_misses, sizeof(uint64_t));
    read(fd_l2, &stats.l2_misses, sizeof(uint64_t));
    read(fd_cycles, &stats.cycles, sizeof(uint64_t));
    
    close(fd_l1);
    close(fd_l2);
    close(fd_cycles);
    
    stats.cache_efficiency = 1.0 - ((double)stats.l1_misses / stats.cycles);
    #endif
    
    return stats;
}
```

---

## 🧪 Performance Test Categories

### 1. Latency Tests

```c
SCENARIO("Single signal processing meets latency requirements") {
    bitactor_engine_t* engine;
    latency_stats_t stats;
    
    GIVEN("a warmed-up BitActor engine",
        engine = bitactor_init();
        performance_warmup(warmup_single_signal);
    );
    
    WHEN("10,000 signals are processed with latency measurement",
        stats = measure_latency_distribution(
            process_single_signal, 
            10000
        );
    );
    
    THEN("all latency percentiles meet requirements",
        printf("       P50:    %4lu ticks\n", stats.p50);
        printf("       P99:    %4lu ticks\n", stats.p99);
        printf("       P99.9:  %4lu ticks\n", stats.p999);
        printf("       P99.99: %4lu ticks\n", stats.p9999);
        printf("       P99.999:%4lu ticks\n", stats.p99999);
        printf("       Mean:   %6.2f ticks\n", stats.mean);
        printf("       StdDev: %6.2f ticks\n", stats.stddev);
        
        EXPECT_LE(stats.p50, 6);      // Target requirement
        EXPECT_LE(stats.p99, 7);      // Target requirement  
        EXPECT_LE(stats.p999, 8);     // Target requirement
        EXPECT_LE(stats.p9999, 8);    // Critical requirement
        EXPECT_LE(stats.p99999, 8);   // Critical requirement
    );
    
    AND("latency variance is acceptable",
        EXPECT_LT(stats.stddev, 2.0); // Low jitter requirement
        EXPECT_LT(stats.max - stats.min, 20); // Bounded variance
    );
} END_SCENARIO
```

### 2. Throughput Tests

```c
SCENARIO("System maintains throughput under sustained load") {
    bitactor_engine_t* engine;
    const int LOAD_DURATION_MS = 1000;
    const int TARGET_THROUGHPUT = 500000; // 500K signals/sec
    
    GIVEN("a BitActor engine configured for high throughput",
        engine = bitactor_init();
        configure_for_throughput(engine);
    );
    
    WHEN("signals are processed for exactly one second",
        uint64_t signals_processed = 0;
        uint64_t start_time = get_monotonic_time_ns();
        uint64_t end_time = start_time + (LOAD_DURATION_MS * 1000000ULL);
        
        while (get_monotonic_time_ns() < end_time) {
            signal_t signal = generate_next_signal();
            bitactor_tick(engine, &signal);
            signals_processed++;
        }
        
        uint64_t actual_duration = get_monotonic_time_ns() - start_time;
        double signals_per_second = 
            (double)signals_processed / (actual_duration / 1e9);
    );
    
    THEN("throughput meets or exceeds target",
        printf("       Signals processed: %lu\n", signals_processed);
        printf("       Throughput: %.0f signals/sec\n", signals_per_second);
        printf("       Target: %d signals/sec\n", TARGET_THROUGHPUT);
        
        EXPECT_GE(signals_per_second, TARGET_THROUGHPUT);
    );
    
    AND("latency remains bounded during high throughput",
        // Sample latency during sustained load
        uint64_t latency_samples[100];
        for (int i = 0; i < 100; i++) {
            signal_t signal = generate_next_signal();
            uint64_t start = rdtsc_precise();
            bitactor_tick(engine, &signal);
            latency_samples[i] = rdtsc_precise() - start;
        }
        
        // Sort and check P99
        qsort(latency_samples, 100, sizeof(uint64_t), compare_uint64);
        uint64_t p99_under_load = latency_samples[99];
        
        printf("       P99 under load: %lu ticks\n", p99_under_load);
        EXPECT_LE(p99_under_load, 10); // Slightly relaxed under load
    );
} END_SCENARIO
```

### 3. Memory Performance Tests

```c
SCENARIO("Memory access patterns optimize cache usage") {
    bitactor_engine_t* engine;
    cache_stats_t sequential_stats, random_stats;
    
    GIVEN("a BitActor engine with cache-aligned data structures",
        engine = bitactor_init();
        verify_cache_alignment(engine);
    );
    
    WHEN("sequential access patterns are measured",
        sequential_stats = measure_cache_performance(
            test_sequential_access
        );
    );
    
    AND("random access patterns are measured",
        random_stats = measure_cache_performance(
            test_random_access
        );
    );
    
    THEN("cache efficiency remains high for both patterns",
        printf("       Sequential L1 misses: %lu\n", sequential_stats.l1_misses);
        printf("       Random L1 misses:     %lu\n", random_stats.l1_misses);
        printf("       Sequential efficiency: %.3f\n", sequential_stats.cache_efficiency);
        printf("       Random efficiency:     %.3f\n", random_stats.cache_efficiency);
        
        EXPECT_GT(sequential_stats.cache_efficiency, 0.95);
        EXPECT_GT(random_stats.cache_efficiency, 0.80);
    );
    
    AND("random access penalty is acceptable",
        double penalty_ratio = (double)random_stats.l1_misses / 
                              sequential_stats.l1_misses;
        printf("       Cache miss penalty: %.2fx\n", penalty_ratio);
        EXPECT_LT(penalty_ratio, 3.0); // Random ≤ 3x sequential
    );
} END_SCENARIO
```

### 4. Stress Testing

```c
SCENARIO("System remains stable under extreme load") {
    bitactor_engine_t* engine;
    const int STRESS_SIGNALS = 10000000; // 10M signals
    
    GIVEN("a BitActor engine prepared for stress testing",
        engine = bitactor_init();
        configure_for_stress_test(engine);
    );
    
    WHEN("10 million signals are processed continuously",
        uint64_t start_time = get_monotonic_time_ns();
        uint64_t max_latency = 0;
        uint64_t total_latency = 0;
        int timeout_count = 0;
        
        for (int i = 0; i < STRESS_SIGNALS; i++) {
            signal_t signal = generate_stress_signal(i);
            
            uint64_t signal_start = rdtsc_precise();
            result_t result = bitactor_tick(engine, &signal);
            uint64_t latency = rdtsc_precise() - signal_start;
            
            total_latency += latency;
            if (latency > max_latency) {
                max_latency = latency;
            }
            
            if (latency > 8) {
                timeout_count++;
            }
            
            // Verify no errors
            EXPECT_EQ(result.status, BITACTOR_OK);
            
            // Progress reporting
            if (i % 1000000 == 0) {
                printf("       Processed %dM signals...\n", i / 1000000);
            }
        }
        
        uint64_t total_time = get_monotonic_time_ns() - start_time;
        double avg_latency = (double)total_latency / STRESS_SIGNALS;
        double throughput = (double)STRESS_SIGNALS / (total_time / 1e9);
    );
    
    THEN("system maintains performance throughout the test",
        printf("       Total time: %.3f seconds\n", total_time / 1e9);
        printf("       Throughput: %.0f signals/sec\n", throughput);
        printf("       Average latency: %.2f ticks\n", avg_latency);
        printf("       Maximum latency: %lu ticks\n", max_latency);
        printf("       Timeout violations: %d (%.4f%%)\n", 
               timeout_count, (timeout_count * 100.0) / STRESS_SIGNALS);
        
        EXPECT_LE(max_latency, 8);     // No signal exceeds budget
        EXPECT_LT(timeout_count, STRESS_SIGNALS / 10000); // <0.01% violations
        EXPECT_GE(throughput, 100000); // Minimum throughput maintained
    );
    
    AND("memory usage remains constant",
        memory_profile_t profile = get_current_memory_profile();
        EXPECT_EQ(profile.allocations, 0); // No allocations during stress
        EXPECT_EQ(profile.current, profile.baseline); // No growth
    );
} END_SCENARIO
```

---

## 📈 Performance Regression Detection

### Automated Performance Gates

```bash
# CI/CD Performance Gate Script
#!/bin/bash

echo "🚀 Running CNS Performance Gates..."

# Run performance tests
cd bitactor/tests
make test-performance > perf_results.txt 2>&1

# Check P99.999 latency requirement
if ! grep -q "P99.999=[0-8]" perf_results.txt; then
    echo "❌ GATE FAILURE: P99.999 latency exceeds 8 ticks"
    cat perf_results.txt
    exit 1
fi

# Check throughput requirement  
if ! grep -qE "Throughput: [5-9][0-9]{5}|[0-9]{7,}" perf_results.txt; then
    echo "❌ GATE FAILURE: Throughput below 500K signals/sec"
    cat perf_results.txt
    exit 1
fi

# Check memory allocation
if ! grep -q "Memory allocation: 0 bytes" perf_results.txt; then
    echo "❌ GATE FAILURE: Unexpected memory allocation detected"
    cat perf_results.txt
    exit 1
fi

echo "✅ All performance gates passed!"
```

### Historical Performance Tracking

```python
# performance_tracker.py
import json
import matplotlib.pyplot as plt
from datetime import datetime

class PerformanceTracker:
    def __init__(self, history_file="performance_history.json"):
        self.history_file = history_file
        self.load_history()
    
    def record_performance(self, test_results):
        """Record performance test results with timestamp"""
        entry = {
            "timestamp": datetime.now().isoformat(),
            "commit_hash": self.get_git_commit(),
            "p50_latency": test_results["p50"],
            "p99_latency": test_results["p99"], 
            "p99999_latency": test_results["p99999"],
            "throughput": test_results["throughput"],
            "memory_usage": test_results["memory"]
        }
        
        self.history.append(entry)
        self.save_history()
    
    def detect_regression(self, current_results, window=10):
        """Detect performance regressions compared to recent history"""
        if len(self.history) < window:
            return None
            
        recent = self.history[-window:]
        baseline_p99999 = sum(r["p99999_latency"] for r in recent) / len(recent)
        baseline_throughput = sum(r["throughput"] for r in recent) / len(recent)
        
        regressions = []
        
        # Check latency regression (>10% increase)
        if current_results["p99999"] > baseline_p99999 * 1.1:
            regressions.append(f"P99.999 latency increased by {
                ((current_results['p99999'] / baseline_p99999) - 1) * 100:.1f}%")
        
        # Check throughput regression (>5% decrease)  
        if current_results["throughput"] < baseline_throughput * 0.95:
            regressions.append(f"Throughput decreased by {
                (1 - (current_results['throughput'] / baseline_throughput)) * 100:.1f}%")
        
        return regressions if regressions else None
    
    def generate_report(self):
        """Generate performance trend report"""
        if not self.history:
            return "No performance history available"
        
        # Create trend plots
        timestamps = [datetime.fromisoformat(r["timestamp"]) for r in self.history]
        p99999_values = [r["p99999_latency"] for r in self.history]
        throughput_values = [r["throughput"] for r in self.history]
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8))
        
        # P99.999 latency trend
        ax1.plot(timestamps, p99999_values, 'b-', linewidth=2)
        ax1.axhline(y=8, color='r', linestyle='--', label='8-tick limit')
        ax1.set_ylabel('P99.999 Latency (ticks)')
        ax1.set_title('CNS Performance Trends')
        ax1.legend()
        ax1.grid(True)
        
        # Throughput trend
        ax2.plot(timestamps, throughput_values, 'g-', linewidth=2)
        ax2.axhline(y=500000, color='r', linestyle='--', label='500K target')
        ax2.set_ylabel('Throughput (signals/sec)')
        ax2.set_xlabel('Date')
        ax2.legend()
        ax2.grid(True)
        
        plt.tight_layout()
        plt.savefig('performance_trends.png', dpi=300, bbox_inches='tight')
        
        return "Performance trend report generated: performance_trends.png"
```

---

## 🔧 Debugging Performance Issues

### Latency Spike Analysis

```c
// Detailed latency spike investigation
typedef struct {
    uint64_t timestamp;
    uint64_t latency;
    signal_t signal;
    uint32_t cpu_state;
    uint32_t cache_state;
} spike_event_t;

void investigate_latency_spikes(void) {
    const int SAMPLES = 100000;
    spike_event_t events[1000]; // Top 1% spikes
    int spike_count = 0;
    
    bitactor_engine_t* engine = bitactor_init();
    
    for (int i = 0; i < SAMPLES; i++) {
        signal_t signal = generate_test_signal(i);
        
        uint64_t start = rdtsc_precise();
        bitactor_tick(engine, &signal);
        uint64_t latency = rdtsc_precise() - start;
        
        // Record spikes (top 1%)
        if (latency > 10 && spike_count < 1000) {
            events[spike_count] = (spike_event_t){
                .timestamp = start,
                .latency = latency,
                .signal = signal,
                .cpu_state = get_cpu_state(),
                .cache_state = get_cache_state()
            };
            spike_count++;
        }
    }
    
    // Analyze spike patterns
    printf("Performance Spike Analysis:\n");
    printf("==========================\n");
    printf("Total spikes: %d out of %d samples (%.3f%%)\n", 
           spike_count, SAMPLES, (spike_count * 100.0) / SAMPLES);
    
    // Group by signal type
    int spike_by_type[256] = {0};
    for (int i = 0; i < spike_count; i++) {
        spike_by_type[events[i].signal.kind]++;
    }
    
    printf("\nSpikes by signal type:\n");
    for (int i = 0; i < 256; i++) {
        if (spike_by_type[i] > 0) {
            printf("  Type 0x%02X: %d spikes\n", i, spike_by_type[i]);
        }
    }
    
    // Timeline analysis
    printf("\nSpike timeline (first 10):\n");
    for (int i = 0; i < min(10, spike_count); i++) {
        printf("  [%lu] %lu ticks - Signal 0x%02X payload 0x%08X\n",
               events[i].timestamp,
               events[i].latency,
               events[i].signal.kind,
               events[i].signal.payload);
    }
}
```

### Memory Layout Analysis

```c
// Analyze memory layout for cache optimization
void analyze_memory_layout(void) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    printf("CNS Memory Layout Analysis:\n");
    printf("===========================\n");
    
    uintptr_t base = (uintptr_t)&ctx;
    printf("Base address:     0x%016lx\n", base);
    
    printf("\nComponent addresses and offsets:\n");
    printf("Signal ring:      +0x%04lx (0x%016lx)\n", 
           (uintptr_t)ctx.signal_ring - base, (uintptr_t)ctx.signal_ring);
    printf("Fiber scratch:    +0x%04lx (0x%016lx)\n",
           (uintptr_t)ctx.fiber_scratch - base, (uintptr_t)ctx.fiber_scratch);
    printf("Dispatch table:   +0x%04lx (0x%016lx)\n",
           (uintptr_t)ctx.dispatch_table - base, (uintptr_t)ctx.dispatch_table);
    printf("Telemetry ring:   +0x%04lx (0x%016lx)\n",
           (uintptr_t)ctx.telemetry_ring - base, (uintptr_t)ctx.telemetry_ring);
    
    printf("\nCache line alignment (64-byte boundaries):\n");
    printf("Signal ring:      %s\n", 
           ((uintptr_t)ctx.signal_ring % 64 == 0) ? "✅ Aligned" : "❌ Misaligned");
    printf("Fiber scratch:    %s\n",
           ((uintptr_t)ctx.fiber_scratch % 64 == 0) ? "✅ Aligned" : "❌ Misaligned");
    printf("Dispatch table:   %s\n",
           ((uintptr_t)ctx.dispatch_table % 64 == 0) ? "✅ Aligned" : "❌ Misaligned");
    printf("Telemetry ring:   %s\n",
           ((uintptr_t)ctx.telemetry_ring % 64 == 0) ? "✅ Aligned" : "❌ Misaligned");
    
    printf("\nTotal memory footprint: %zu bytes (%.1f KB)\n",
           sizeof(bitactor_context_t), sizeof(bitactor_context_t) / 1024.0);
}
```

---

## 📋 Performance Test Checklist

### Pre-Test Setup
- [ ] CPU governor set to performance mode
- [ ] System isolated from interrupts
- [ ] Memory allocated and pinned
- [ ] Caches warmed up
- [ ] Background processes minimized

### During Testing  
- [ ] Multiple measurement runs
- [ ] Statistical significance verified
- [ ] Outliers identified and analyzed
- [ ] Environmental factors recorded
- [ ] System stability confirmed

### Post-Test Analysis
- [ ] Results compared to baseline
- [ ] Regressions identified
- [ ] Performance characteristics documented
- [ ] Optimization opportunities noted
- [ ] Results archived for trends

### Automated Gates
- [ ] P99.999 ≤ 8 ticks verified
- [ ] Throughput ≥ 500K signals/sec confirmed
- [ ] Zero heap allocation validated
- [ ] Cache efficiency within bounds
- [ ] Memory layout optimized

---

This guide provides the foundation for maintaining CNS's extreme performance requirements through systematic, automated testing at nanosecond precision.