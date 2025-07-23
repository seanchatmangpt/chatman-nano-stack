#ifndef BITACTOR_BLAKE3_H
#define BITACTOR_BLAKE3_H

#include <stdint.h>
#include <stddef.h>

// Blake3 hash size (256 bits)
#define BLAKE3_OUT_LEN 32
#define BLAKE3_KEY_LEN 32
#define BLAKE3_BLOCK_LEN 64
#define BLAKE3_CHUNK_LEN 1024

// Blake3 context for incremental hashing
typedef struct {
    uint32_t cv[8];
    uint64_t chunk_counter;
    uint8_t buf[BLAKE3_BLOCK_LEN];
    uint8_t buf_len;
    uint8_t blocks_compressed;
    uint8_t flags;
} blake3_hasher;

// BitActor-specific Blake3 operations (zero-allocation)
typedef struct {
    uint8_t spec_hash[BLAKE3_OUT_LEN];    // Hash of TTL specification
    uint8_t exec_hash[BLAKE3_OUT_LEN];    // Hash of compiled bytecode
    uint8_t trace_hash[BLAKE3_OUT_LEN];   // Hash of execution trace
    uint32_t verification_xor;            // XOR for spec-exec proof
    bool verified;                        // Verification status
} bitactor_hash_state_t;

// Core Blake3 API for BitActor
void bitactor_blake3_init(blake3_hasher* self);
void bitactor_blake3_init_keyed(blake3_hasher* self, const uint8_t key[BLAKE3_KEY_LEN]);
void bitactor_blake3_update(blake3_hasher* self, const void* input, size_t input_len);
void bitactor_blake3_finalize(const blake3_hasher* self, uint8_t* out, size_t out_len);
void bitactor_blake3_hash(const void* input, size_t input_len, uint8_t* out, size_t out_len);

// BitActor hash verification functions
void bitactor_hash_init(bitactor_hash_state_t* state);
void bitactor_hash_spec(bitactor_hash_state_t* state, const void* ttl_data, size_t ttl_len);
void bitactor_hash_exec(bitactor_hash_state_t* state, const void* bytecode, size_t code_len);
void bitactor_hash_trace(bitactor_hash_state_t* state, const void* trace_data, size_t trace_len);
bool bitactor_hash_verify(bitactor_hash_state_t* state, uint32_t max_diff);

// Inline helpers for performance
__attribute__((always_inline))
static inline uint32_t bitactor_hash_xor_diff(const uint8_t* hash1, const uint8_t* hash2) {
    uint32_t diff = 0;
    for (int i = 0; i < BLAKE3_OUT_LEN; i += 4) {
        uint32_t a = *(uint32_t*)(hash1 + i);
        uint32_t b = *(uint32_t*)(hash2 + i);
        diff += __builtin_popcount(a ^ b);
    }
    return diff;
}

__attribute__((always_inline))
static inline void bitactor_hash_clear(bitactor_hash_state_t* state) {
    __builtin_memset(state, 0, sizeof(*state));
}

// SIMD-optimized Blake3 compression (AVX2)
#ifdef __AVX2__
void bitactor_blake3_compress_avx2(uint32_t cv[8], const uint8_t block[BLAKE3_BLOCK_LEN], 
                                   uint8_t block_len, uint64_t counter, uint8_t flags);
#endif

#endif