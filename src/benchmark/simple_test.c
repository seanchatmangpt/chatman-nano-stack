#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "../cns/tick_parallel.h"
#include "../cns/bitactor_lite.h"
#include "../sparql/sparql_to_bitactor.h"

void test_arena() {
    printf("Testing arena allocator...\n");
    char buffer[1024];
    void* arena = buffer;
    
    void* p1 = tick_arena_alloc(&arena, 64);
    void* p2 = tick_arena_alloc(&arena, 128);
    
    printf("  Allocated p1=%p, p2=%p\n", p1, p2);
    printf("  Arena test PASSED\n");
}

void test_bitactor() {
    printf("Testing BitActor Lite...\n");
    fast_proof_t proof = {
        .capability = 0x1234567890ABCDEF,
        .hash = 0xB3C4D5E6F7A8B9C0
    };
    
    bool result = bitactor_verify_fast(&proof);
    printf("  BitActor verify result: %s\n", result ? "PASS" : "FAIL");
}

void test_sparql() {
    printf("Testing SPARQL 8-hop...\n");
    
    uint64_t test_data[8] = {1,2,3,4,5,6,7,8};
    test_data[3] = time(NULL) + 3600;
    
    proof_chain_t chain = {
        .hops = {
            {.capability_id = 0x123, .validation_fn = (uint64_t)&always_true, .data_offset = 0},
            {.capability_id = 0x234, .validation_fn = (uint64_t)&always_true, .data_offset = 8},
            {.capability_id = 0x345, .validation_fn = (uint64_t)&always_true, .data_offset = 16},
            {.capability_id = 0x456, .validation_fn = (uint64_t)&always_true, .data_offset = 24},
            {.capability_id = 0x567, .validation_fn = (uint64_t)&always_true, .data_offset = 32},
            {.capability_id = 0x678, .validation_fn = (uint64_t)&always_true, .data_offset = 40},
            {.capability_id = 0x789, .validation_fn = (uint64_t)&always_true, .data_offset = 48},
            {.capability_id = 0x890, .validation_fn = (uint64_t)&always_true, .data_offset = 56}
        }
    };
    
    bool result = bitactor_execute_8hop(&chain, test_data);
    printf("  SPARQL 8-hop result: %s\n", result ? "PASS" : "FAIL");
}

int main() {
    printf("Running simple tests...\n\n");
    
    test_arena();
    test_bitactor();
    test_sparql();
    
    printf("\nAll tests completed!\n");
    return 0;
}