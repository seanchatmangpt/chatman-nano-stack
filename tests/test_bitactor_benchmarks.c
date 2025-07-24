/*
 * BitActor Performance Benchmarks - REAL IMPLEMENTATION
 * Cycle-accurate performance testing with production code
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <math.h>

// Include REAL BitActor implementation headers
#include "../bitactor/include/bitactor/bitactor.h"
#include "../bitactor/include/bitactor/bitactor_dispatch.h"
#include "../bitactor/include/bitactor/bitactor_telemetry.h"

// Forward declare functions from real implementation
extern bitactor_engine_t* bitactor_init(void);
extern void bitactor_destroy(bitactor_engine_t* engine);
extern result_t bitactor_tick(bitactor_engine_t* engine, signal_t* signal);
extern bool bitactor_enqueue(bitactor_engine_t* engine, signal_t* signal);
extern uint32_t bitactor_drain(bitactor_engine_t* engine, uint32_t max_signals);
extern bool bitactor_is_ready(const bitactor_engine_t* engine);
extern int bitactor_register(bitactor_engine_t* engine, uint8_t signal_kind, 
                             bitactor_handler_fn handler);

// Benchmark Framework
#define BENCHMARK(name) printf("\n⚡ Benchmark: %s\n", name)
#define METRIC(name, value, unit) printf("   %s: %.2f %s\n", name, value, unit)

// Hardware cycle counter support
static inline uint64_t get_hardware_cycles(void) {
#ifdef __aarch64__
    uint64_t val;
    __asm__ volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#elif defined(__x86_64__)
    unsigned int lo, hi;
    __asm__ volatile ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Get CPU frequency (approximate)
static double get_cpu_frequency(void) {
    uint64_t start = get_hardware_cycles();
    usleep(100000); // 100ms
    uint64_t end = get_hardware_cycles();
    return (double)(end - start) / 100000.0; // cycles per microsecond
}

// Benchmark handlers
static result_t minimal_handler(signal_t* signal, void* context) {
    (void)context;
    result_t result = {
        .signal_id = signal->id,
        .status = BITACTOR_OK,
        .ticks = 1,
        .exec_hash = 0x12345678,
        .result = signal->payload,
        .flags = 0,
        .fiber_id = 0
    };
    return result;
}

static result_t compute_intensive_handler(signal_t* signal, void* context) {
    (void)context;
    // Simulate computation
    uint64_t result_value = signal->payload;
    for (int i = 0; i < 10; i++) {
        result_value = (result_value * 0x5DEECE66DLL + 0xBLL) & ((1LL << 48) - 1);
    }
    
    result_t result = {
        .signal_id = signal->id,
        .status = BITACTOR_OK,
        .ticks = 4,
        .exec_hash = 0x87654321,
        .result = result_value,
        .flags = 0,
        .fiber_id = 0
    };
    return result;
}

static result_t zero_tick_handler(signal_t* signal, void* context) {
    (void)context;
    result_t result = {
        .signal_id = signal->id,
        .status = BITACTOR_OK,
        .ticks = 0,
        .exec_hash = 0x5A4E00,
        .result = signal->payload,
        .flags = ZERO_TICK_FLAG,
        .fiber_id = 0
    };
    return result;
}

// =============================================================================
// BENCHMARKS
// =============================================================================

void benchmark_initialization_overhead(void) {
    BENCHMARK("Engine Initialization Overhead");
    
    const int iterations = 100;
    uint64_t total_cycles = 0;
    
    for (int i = 0; i < iterations; i++) {
        uint64_t start = get_hardware_cycles();
        bitactor_engine_t* engine = bitactor_init();
        uint64_t end = get_hardware_cycles();
        
        total_cycles += (end - start);
        bitactor_destroy(engine);
    }
    
    double avg_cycles = (double)total_cycles / iterations;
    double cpu_freq = get_cpu_frequency();
    double avg_microseconds = avg_cycles / cpu_freq;
    
    METRIC("Average initialization", avg_cycles, "cycles");
    METRIC("Average initialization", avg_microseconds, "microseconds");
    METRIC("Iterations", (double)iterations, "count");
}

void benchmark_signal_processing_latency(void) {
    BENCHMARK("Signal Processing Latency");
    
    bitactor_engine_t* engine = bitactor_init();
    bitactor_register(engine, 1, minimal_handler);
    bitactor_register(engine, 2, compute_intensive_handler);
    bitactor_register(engine, 3, zero_tick_handler);
    
    const int warmup = 1000;
    const int iterations = 100000;
    
    // Warmup
    for (int i = 0; i < warmup; i++) {
        signal_t signal = {
            .id = i,
            .type = 0,
            .payload = i,
            .flags = 0,
            .timestamp = get_hardware_cycles(),
            .kind = 1,
            .priority = 128,
            .context = 0
        };
        bitactor_tick(engine, &signal);
    }
    
    // Measure different handler types
    struct {
        const char* name;
        uint8_t kind;
        uint64_t total_cycles;
        uint64_t min_cycles;
        uint64_t max_cycles;
    } measurements[] = {
        {"Minimal handler", 1, 0, UINT64_MAX, 0},
        {"Compute intensive", 2, 0, UINT64_MAX, 0},
        {"Zero-tick handler", 3, 0, UINT64_MAX, 0}
    };
    
    for (int type = 0; type < 3; type++) {
        for (int i = 0; i < iterations; i++) {
            signal_t signal = {
                .id = i,
                .type = 0,
                .payload = i,
                .flags = (type == 2) ? ZERO_TICK_FLAG : 0,
                .timestamp = get_hardware_cycles(),
                .kind = measurements[type].kind,
                .priority = 128,
                .context = 0
            };
            
            uint64_t start = get_hardware_cycles();
            bitactor_tick(engine, &signal);
            uint64_t end = get_hardware_cycles();
            
            uint64_t cycles = end - start;
            measurements[type].total_cycles += cycles;
            if (cycles < measurements[type].min_cycles) {
                measurements[type].min_cycles = cycles;
            }
            if (cycles > measurements[type].max_cycles) {
                measurements[type].max_cycles = cycles;
            }
        }
    }
    
    double cpu_freq = get_cpu_frequency();
    
    for (int i = 0; i < 3; i++) {
        printf("\n   %s:\n", measurements[i].name);
        double avg_cycles = (double)measurements[i].total_cycles / iterations;
        METRIC("     Average latency", avg_cycles, "cycles");
        METRIC("     Average latency", avg_cycles / cpu_freq, "microseconds");
        METRIC("     Minimum latency", (double)measurements[i].min_cycles, "cycles");
        METRIC("     Maximum latency", (double)measurements[i].max_cycles, "cycles");
    }
    
    bitactor_destroy(engine);
}

void benchmark_throughput(void) {
    BENCHMARK("Maximum Throughput");
    
    bitactor_engine_t* engine = bitactor_init();
    bitactor_register(engine, 1, minimal_handler);
    
    const int duration_seconds = 1;
    uint64_t operations = 0;
    
    uint64_t start_time = get_hardware_cycles();
    uint64_t end_time = start_time + (uint64_t)(get_cpu_frequency() * 1000000 * duration_seconds);
    
    while (get_hardware_cycles() < end_time) {
        signal_t signal = {
            .id = operations,
            .type = 0,
            .payload = operations,
            .flags = 0,
            .timestamp = get_hardware_cycles(),
            .kind = 1,
            .priority = 128,
            .context = 0
        };
        
        bitactor_tick(engine, &signal);
        operations++;
    }
    
    uint64_t actual_end = get_hardware_cycles();
    double actual_duration = (double)(actual_end - start_time) / (get_cpu_frequency() * 1000000);
    double throughput = operations / actual_duration;
    
    METRIC("Operations completed", (double)operations, "count");
    METRIC("Duration", actual_duration, "seconds");
    METRIC("Throughput", throughput / 1000000.0, "million ops/sec");
    METRIC("Average latency", actual_duration * 1000000000.0 / operations, "nanoseconds");
    
    bitactor_destroy(engine);
}

void benchmark_zero_tick_optimization(void) {
    BENCHMARK("Zero-Tick Optimization Performance");
    
    bitactor_engine_t* engine = bitactor_init();
    bitactor_register(engine, 1, minimal_handler);
    bitactor_register(engine, 99, zero_tick_handler);
    
    const int iterations = 1000000;
    
    // Measure regular processing
    uint64_t regular_start = get_hardware_cycles();
    for (int i = 0; i < iterations; i++) {
        signal_t signal = {
            .id = i,
            .type = 0,
            .payload = i,
            .flags = 0,
            .timestamp = get_hardware_cycles(),
            .kind = 1,
            .priority = 128,
            .context = 0
        };
        bitactor_tick(engine, &signal);
    }
    uint64_t regular_end = get_hardware_cycles();
    
    // Measure zero-tick processing
    uint64_t zero_tick_start = get_hardware_cycles();
    for (int i = 0; i < iterations; i++) {
        signal_t signal = {
            .id = i,
            .type = 0,
            .payload = i,
            .flags = ZERO_TICK_FLAG,
            .timestamp = get_hardware_cycles(),
            .kind = 99,
            .priority = 255,
            .context = 0
        };
        bitactor_tick(engine, &signal);
    }
    uint64_t zero_tick_end = get_hardware_cycles();
    
    uint64_t regular_cycles = regular_end - regular_start;
    uint64_t zero_tick_cycles = zero_tick_end - zero_tick_start;
    
    double cpu_freq = get_cpu_frequency();
    double regular_time = regular_cycles / cpu_freq / 1000.0; // milliseconds
    double zero_tick_time = zero_tick_cycles / cpu_freq / 1000.0;
    double improvement = ((double)regular_cycles - zero_tick_cycles) / regular_cycles * 100.0;
    
    METRIC("Regular processing", regular_time, "milliseconds");
    METRIC("Zero-tick processing", zero_tick_time, "milliseconds");
    METRIC("Performance improvement", improvement, "percent");
    METRIC("Regular avg cycles/op", (double)regular_cycles / iterations, "cycles");
    METRIC("Zero-tick avg cycles/op", (double)zero_tick_cycles / iterations, "cycles");
    
    bitactor_destroy(engine);
}

void benchmark_queue_operations(void) {
    BENCHMARK("Queue Operations Performance");
    
    bitactor_engine_t* engine = bitactor_init();
    
    const int batch_size = 1000;
    const int batches = 100;
    
    uint64_t enqueue_total = 0;
    uint64_t drain_total = 0;
    
    for (int batch = 0; batch < batches; batch++) {
        // Measure enqueue performance
        uint64_t enqueue_start = get_hardware_cycles();
        for (int i = 0; i < batch_size; i++) {
            signal_t signal = {
                .id = batch * batch_size + i,
                .type = 0,
                .payload = i,
                .flags = 0,
                .timestamp = get_hardware_cycles(),
                .kind = 1,
                .priority = 128,
                .context = 0
            };
            bitactor_enqueue(engine, &signal);
        }
        uint64_t enqueue_end = get_hardware_cycles();
        enqueue_total += (enqueue_end - enqueue_start);
        
        // Measure drain performance
        uint64_t drain_start = get_hardware_cycles();
        uint32_t drained = bitactor_drain(engine, batch_size);
        uint64_t drain_end = get_hardware_cycles();
        drain_total += (drain_end - drain_start);
        
        assert(drained == batch_size);
    }
    
    double total_operations = batch_size * batches;
    double enqueue_avg = enqueue_total / total_operations;
    double drain_avg = drain_total / total_operations;
    
    METRIC("Enqueue avg cycles/op", enqueue_avg, "cycles");
    METRIC("Drain avg cycles/op", drain_avg, "cycles");
    METRIC("Total operations", total_operations * 2, "count");
    
    bitactor_destroy(engine);
}

// Thread data structure
typedef struct {
    bitactor_engine_t* engine;
    int thread_id;
    int operations;
    uint64_t total_cycles;
} thread_data_t;

// Thread worker function
static void* concurrent_thread_worker(void* arg) {
    thread_data_t* data = (thread_data_t*)arg;
    data->total_cycles = 0;
    
    for (int i = 0; i < data->operations; i++) {
        signal_t signal = {
            .id = (data->thread_id << 20) | i,
            .type = 0,
            .payload = i,
            .flags = 0,
            .timestamp = get_hardware_cycles(),
            .kind = 1,
            .priority = 128,
            .context = data->thread_id
        };
        
        uint64_t start = get_hardware_cycles();
        bitactor_tick(data->engine, &signal);
        uint64_t end = get_hardware_cycles();
        
        data->total_cycles += (end - start);
    }
    
    return NULL;
}

void benchmark_concurrent_access(void) {
    BENCHMARK("Concurrent Access Performance");
    
    bitactor_engine_t* engine = bitactor_init();
    bitactor_register(engine, 1, minimal_handler);
    
    const int num_threads = 8;
    const int ops_per_thread = 100000;
    
    thread_data_t thread_data[num_threads];
    pthread_t threads[num_threads];
    
    // Initialize thread data
    for (int i = 0; i < num_threads; i++) {
        thread_data[i].engine = engine;
        thread_data[i].thread_id = i;
        thread_data[i].operations = ops_per_thread;
    }
    
    // Start timing
    uint64_t start_time = get_hardware_cycles();
    
    // Launch threads
    for (int i = 0; i < num_threads; i++) {
        pthread_create(&threads[i], NULL, concurrent_thread_worker, &thread_data[i]);
    }
    
    // Wait for completion
    for (int i = 0; i < num_threads; i++) {
        pthread_join(threads[i], NULL);
    }
    
    uint64_t end_time = get_hardware_cycles();
    
    // Calculate metrics
    uint64_t total_thread_cycles = 0;
    for (int i = 0; i < num_threads; i++) {
        total_thread_cycles += thread_data[i].total_cycles;
    }
    
    double wall_time = (end_time - start_time) / get_cpu_frequency() / 1000.0; // milliseconds
    double total_ops = num_threads * ops_per_thread;
    double throughput = total_ops / (wall_time / 1000.0); // ops per second
    double avg_latency = total_thread_cycles / total_ops;
    
    METRIC("Threads", (double)num_threads, "count");
    METRIC("Total operations", total_ops, "count");
    METRIC("Wall time", wall_time, "milliseconds");
    METRIC("Aggregate throughput", throughput / 1000000.0, "million ops/sec");
    METRIC("Average latency", avg_latency, "cycles");
    
    bitactor_destroy(engine);
}

// =============================================================================
// MAIN
// =============================================================================

int main(void) {
    printf("🚀 BITACTOR PERFORMANCE BENCHMARKS\n");
    printf("=====================================\n");
    printf("CPU Frequency: %.2f MHz\n", get_cpu_frequency());
    
    benchmark_initialization_overhead();
    benchmark_signal_processing_latency();
    benchmark_throughput();
    benchmark_zero_tick_optimization();
    benchmark_queue_operations();
    benchmark_concurrent_access();
    
    printf("\n✅ All benchmarks completed\n");
    return 0;
}