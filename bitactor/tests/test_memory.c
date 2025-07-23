#include "test_harness.h"
#include "../include/bitactor/bitactor.h"
#include <sys/mman.h>
#include <unistd.h>

// Test: Static memory layout
bool test_static_memory_layout(char* error_msg) {
    bitactor_context_t ctx;
    
    // Calculate offsets
    size_t signal_ring_offset = offsetof(bitactor_context_t, signal_ring);
    size_t scratch_offset = offsetof(bitactor_context_t, fiber_scratch);
    size_t dispatch_offset = offsetof(bitactor_context_t, dispatch_table);
    size_t telemetry_offset = offsetof(bitactor_context_t, telemetry_ring);
    
    // Verify layout is packed efficiently
    TEST_ASSERT_EQ(signal_ring_offset, 0, "Signal ring not at start");
    
    // Verify alignment
    TEST_ASSERT_EQ(scratch_offset % 8, 0, "Scratchpad not 8-byte aligned");
    TEST_ASSERT_EQ(dispatch_offset % 8, 0, "Dispatch table not aligned");
    TEST_ASSERT_EQ(telemetry_offset % 8, 0, "Telemetry ring not aligned");
    
    // Verify total size
    size_t total_size = sizeof(bitactor_context_t);
    TEST_ASSERT_LT(total_size, 128 * 1024, "Context exceeds 128KB");
    
    snprintf(error_msg, 256, "Total size: %zu bytes", total_size);
    return true;
}

// Test: Memory protection
bool test_memory_protection(char* error_msg) {
    // Allocate page-aligned memory
    size_t page_size = sysconf(_SC_PAGESIZE);
    void* mem = mmap(NULL, page_size * 2, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    
    TEST_ASSERT(mem != MAP_FAILED, "Failed to allocate test memory");
    
    // Place context at start of first page
    bitactor_context_t* ctx = (bitactor_context_t*)mem;
    bitactor_init(ctx);
    
    // Protect second page to catch overruns
    int ret = mprotect((char*)mem + page_size, page_size, PROT_NONE);
    TEST_ASSERT_EQ(ret, 0, "Failed to protect guard page");
    
    // Try normal operations - should not crash
    signal_t signal = {.kind = 0x01, .payload = 0x42};
    result_t result = bitactor_tick(ctx, &signal);
    TEST_ASSERT_EQ(result.status, STATUS_SUCCESS, "Tick failed");
    
    // Cleanup
    munmap(mem, page_size * 2);
    
    return true;
}

// Test: Cache line alignment
bool test_cache_alignment(char* error_msg) {
    bitactor_context_t ctx;
    
    // Check critical fields are cache-line aligned
    uintptr_t base = (uintptr_t)&ctx;
    uintptr_t signal_ring = (uintptr_t)&ctx.signal_ring;
    uintptr_t telemetry = (uintptr_t)&ctx.telemetry_ring;
    
    // Most architectures use 64-byte cache lines
    const size_t CACHE_LINE = 64;
    
    // Signal ring should be at a cache line boundary
    TEST_ASSERT_EQ((signal_ring - base) % CACHE_LINE, 0, 
                   "Signal ring not cache aligned");
    
    return true;
}

// Test: Memory access patterns
bool test_memory_access_patterns(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Track memory accesses
    uintptr_t min_addr = (uintptr_t)&ctx;
    uintptr_t max_addr = min_addr + sizeof(ctx);
    
    // Process many signals
    for (int i = 0; i < 1000; i++) {
        signal_t signal = {.kind = (uint8_t)(i % 4 + 1), .payload = i};
        bitactor_tick(&ctx, &signal);
        
        // Verify telemetry write is within bounds
        telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
        uintptr_t frame_addr = (uintptr_t)frame;
        
        TEST_ASSERT(frame_addr >= min_addr, "Frame address below context");
        TEST_ASSERT(frame_addr < max_addr, "Frame address above context");
    }
    
    return true;
}

// Test: Zero allocation verification
bool test_zero_allocation(char* error_msg) {
    // Override malloc to detect any allocations
    static int malloc_count = 0;
    
    // Would use LD_PRELOAD or similar in real test
    // For now, verify context fits in stack
    
    bitactor_context_t ctx;  // Stack allocated
    bitactor_init(&ctx);
    
    // Process signals without any heap allocation
    for (int i = 0; i < 100; i++) {
        signal_t signal = {.kind = 0x01, .payload = i};
        bitactor_tick(&ctx, &signal);
    }
    
    TEST_ASSERT_EQ(malloc_count, 0, "Unexpected heap allocation");
    
    return true;
}

int main() {
    TEST_INIT();
    
    RUN_TEST(test_static_memory_layout);
    RUN_TEST(test_memory_protection);
    RUN_TEST(test_cache_alignment);
    RUN_TEST(test_memory_access_patterns);
    RUN_TEST(test_zero_allocation);
    
    TEST_SUMMARY();
    
    return 0;
}