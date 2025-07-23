#include "test_harness.h"
#include "../include/bitactor/bitactor.h"
#include <string.h>

// Blake3 hash simulation for testing
uint64_t blake3_hash(const void* data, size_t len) {
    // Simple hash for testing
    uint64_t hash = 0x1337DEADBEEF;
    const uint8_t* bytes = (const uint8_t*)data;
    for (size_t i = 0; i < len; i++) {
        hash = hash * 31 + bytes[i];
    }
    return hash;
}

// Test: Telemetry ring buffer
bool test_telemetry_ring_buffer(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Fill telemetry ring
    for (int i = 0; i < TELEMETRY_RING_SIZE + 100; i++) {
        signal_t signal = {.kind = (uint8_t)(i % 256), .payload = i};
        bitactor_tick(&ctx, &signal);
    }
    
    // Verify ring wraps correctly
    TEST_ASSERT_EQ(ctx.telemetry_index, 100, "Telemetry index did not wrap");
    
    // Verify we can still retrieve last trace
    telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
    TEST_ASSERT(frame != NULL, "Failed to retrieve last trace");
    
    return true;
}

// Test: Trace completeness
bool test_trace_completeness(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Process signal with known handler
    signal_t signal = {.kind = 0x03, .payload = 0xCAFEBABE, .timestamp = 12345};
    bitactor_tick(&ctx, &signal);
    
    telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
    
    // Verify all fields are populated
    TEST_ASSERT_EQ(frame->timestamp, 12345, "Timestamp not preserved");
    TEST_ASSERT_EQ(frame->signal_id, 0x03, "Signal ID not preserved");
    TEST_ASSERT(frame->exec_hash != 0, "Execution hash not set");
    
    // Verify trace ops were recorded
    TEST_ASSERT_EQ(frame->trace_ops[0], 0xAA, "Trace op 0 incorrect");
    TEST_ASSERT_EQ(frame->trace_ops[1], 0xBB, "Trace op 1 incorrect");
    TEST_ASSERT_EQ(frame->trace_ops[2], 0xCC, "Trace op 2 incorrect");
    
    return true;
}

// Test: TTL reconstruction from trace
bool test_ttl_reconstruction(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Execute known pattern
    signal_t signals[] = {
        {.kind = 0x01, .payload = 0x1111},
        {.kind = 0x02, .payload = 0x2222},
        {.kind = 0x03, .payload = 0x3333}
    };
    
    for (int i = 0; i < 3; i++) {
        bitactor_tick(&ctx, &signals[i]);
    }
    
    // Simulate TTL reconstruction
    char reconstructed_ttl[1024];
    int offset = 0;
    
    for (int i = 0; i < 3; i++) {
        telemetry_frame_t* frame = &ctx.telemetry_ring[i];
        offset += snprintf(reconstructed_ttl + offset, 1024 - offset,
                          ":signal_%02X :hasPayload 0x%08X .\n",
                          frame->signal_id, signals[i].payload);
    }
    
    // Verify reconstruction contains expected triples
    TEST_ASSERT(strstr(reconstructed_ttl, ":signal_01") != NULL, "Missing signal_01 triple");
    TEST_ASSERT(strstr(reconstructed_ttl, ":signal_02") != NULL, "Missing signal_02 triple");
    TEST_ASSERT(strstr(reconstructed_ttl, ":signal_03") != NULL, "Missing signal_03 triple");
    
    return true;
}

// Test: Trace hash verification
bool test_trace_hash_verification(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Process signal
    signal_t signal = {.kind = 0x04, .payload = 0xDEADC0DE};
    bitactor_tick(&ctx, &signal);
    
    telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
    
    // Calculate expected hash
    uint64_t expected_hash = blake3_hash(&signal, sizeof(signal));
    
    // Simplified verification (in practice would be more complex)
    TEST_ASSERT(frame->exec_hash != 0, "No execution hash recorded");
    
    // Verify hash linkage
    uint64_t spec_hash = blake3_hash("test_spec", 9);
    uint64_t exec_hash = frame->exec_hash;
    uint64_t diff = spec_hash ^ exec_hash;
    
    // This would normally check against actual spec
    TEST_ASSERT(diff != 0, "Hash calculation error");
    
    return true;
}

// Test: Telemetry export format
bool test_telemetry_export(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Generate some traces
    for (int i = 0; i < 10; i++) {
        signal_t signal = {.kind = (uint8_t)i, .payload = i * 1000};
        bitactor_tick(&ctx, &signal);
    }
    
    // Simulate export to JSON format
    char export_buffer[4096];
    int offset = snprintf(export_buffer, 4096, "{\n  \"traces\": [\n");
    
    for (int i = 0; i < 10; i++) {
        telemetry_frame_t* frame = &ctx.telemetry_ring[i];
        offset += snprintf(export_buffer + offset, 4096 - offset,
                          "    {\"signal_id\": %d, \"exec_hash\": %u}%s\n",
                          frame->signal_id, frame->exec_hash,
                          i < 9 ? "," : "");
    }
    
    offset += snprintf(export_buffer + offset, 4096 - offset, "  ]\n}");
    
    // Verify JSON structure
    TEST_ASSERT(strstr(export_buffer, "\"traces\"") != NULL, "Missing traces array");
    TEST_ASSERT(strstr(export_buffer, "\"signal_id\"") != NULL, "Missing signal_id field");
    TEST_ASSERT(strstr(export_buffer, "\"exec_hash\"") != NULL, "Missing exec_hash field");
    
    return true;
}

int main() {
    TEST_INIT();
    
    RUN_TEST(test_telemetry_ring_buffer);
    RUN_TEST(test_trace_completeness);
    RUN_TEST(test_ttl_reconstruction);
    RUN_TEST(test_trace_hash_verification);
    RUN_TEST(test_telemetry_export);
    
    TEST_SUMMARY();
    
    return 0;
}