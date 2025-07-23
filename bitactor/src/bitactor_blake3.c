/*
 * BitActor Blake3 Hash Implementation
 * Zero-allocation, deterministic hashing for telemetry verification
 */
#include "../include/bitactor/bitactor_blake3.h"
#include <string.h>

// Blake3 constants
static const uint32_t IV[8] = {
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
};

static const uint32_t MSG_SCHEDULE[7][16] = {
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
    {2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8},
    {3, 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1},
    {10, 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6},
    {12, 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4},
    {9, 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7},
    {11, 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13},
};

// Blake3 mixing function
__attribute__((always_inline))
static inline void g(uint32_t* state, size_t a, size_t b, size_t c, size_t d, 
                     uint32_t x, uint32_t y) {
    state[a] = state[a] + state[b] + x;
    state[d] = __builtin_bswap32((state[d] ^ state[a]) >> 16 | (state[d] ^ state[a]) << 16);
    state[c] = state[c] + state[d];
    state[b] = __builtin_bswap32((state[b] ^ state[c]) >> 12 | (state[b] ^ state[c]) << 20);
    state[a] = state[a] + state[b] + y;
    state[d] = __builtin_bswap32((state[d] ^ state[a]) >> 8 | (state[d] ^ state[a]) << 24);
    state[c] = state[c] + state[d];
    state[b] = __builtin_bswap32((state[b] ^ state[c]) >> 7 | (state[b] ^ state[c]) << 25);
}

// Blake3 round function
__attribute__((hot))
static void round_fn(uint32_t state[16], const uint32_t* msg, size_t round) {
    // Column step
    g(state, 0, 4, 8, 12, msg[MSG_SCHEDULE[round][0]], msg[MSG_SCHEDULE[round][1]]);
    g(state, 1, 5, 9, 13, msg[MSG_SCHEDULE[round][2]], msg[MSG_SCHEDULE[round][3]]);
    g(state, 2, 6, 10, 14, msg[MSG_SCHEDULE[round][4]], msg[MSG_SCHEDULE[round][5]]);
    g(state, 3, 7, 11, 15, msg[MSG_SCHEDULE[round][6]], msg[MSG_SCHEDULE[round][7]]);
    
    // Diagonal step
    g(state, 0, 5, 10, 15, msg[MSG_SCHEDULE[round][8]], msg[MSG_SCHEDULE[round][9]]);
    g(state, 1, 6, 11, 12, msg[MSG_SCHEDULE[round][10]], msg[MSG_SCHEDULE[round][11]]);
    g(state, 2, 7, 8, 13, msg[MSG_SCHEDULE[round][12]], msg[MSG_SCHEDULE[round][13]]);
    g(state, 3, 4, 9, 14, msg[MSG_SCHEDULE[round][14]], msg[MSG_SCHEDULE[round][15]]);
}

// Blake3 compress function
__attribute__((hot))
static void compress(uint32_t cv[8], const uint8_t block[BLAKE3_BLOCK_LEN], 
                     uint8_t block_len, uint64_t counter, uint8_t flags) {
    uint32_t state[16];
    uint32_t block_words[16];
    
    // Load block into words (little-endian)
    for (size_t i = 0; i < 16; i++) {
        if (4 * i < block_len) {
            memcpy(&block_words[i], block + 4 * i, 4);
        } else {
            block_words[i] = 0;
        }
    }
    
    // Initialize state
    memcpy(state, cv, 32);
    memcpy(state + 8, IV, 32);
    state[12] = (uint32_t)counter;
    state[13] = (uint32_t)(counter >> 32);
    state[14] = (uint32_t)block_len;
    state[15] = (uint32_t)flags;
    
    // 7 rounds of mixing
    for (size_t round = 0; round < 7; round++) {
        round_fn(state, block_words, round);
    }
    
    // Finalize
    for (size_t i = 0; i < 8; i++) {
        cv[i] = state[i] ^ state[i + 8];
    }
}

// Initialize Blake3 hasher
void bitactor_blake3_init(blake3_hasher* self) {
    memcpy(self->cv, IV, 32);
    self->chunk_counter = 0;
    memset(self->buf, 0, BLAKE3_BLOCK_LEN);
    self->buf_len = 0;
    self->blocks_compressed = 0;
    self->flags = 0;
}

// Initialize Blake3 hasher with key
void bitactor_blake3_init_keyed(blake3_hasher* self, const uint8_t key[BLAKE3_KEY_LEN]) {
    uint32_t key_words[8];
    memcpy(key_words, key, 32);
    
    memcpy(self->cv, key_words, 32);
    self->chunk_counter = 0;
    memset(self->buf, 0, BLAKE3_BLOCK_LEN);
    self->buf_len = 0;
    self->blocks_compressed = 0;
    self->flags = 1; // KEYED_HASH flag
}

// Update Blake3 hasher with new data
void bitactor_blake3_update(blake3_hasher* self, const void* input, size_t input_len) {
    const uint8_t* input_bytes = (const uint8_t*)input;
    
    while (input_len > 0) {
        // Fill buffer
        size_t take = BLAKE3_BLOCK_LEN - self->buf_len;
        if (take > input_len) {
            take = input_len;
        }
        
        memcpy(self->buf + self->buf_len, input_bytes, take);
        self->buf_len += (uint8_t)take;
        input_bytes += take;
        input_len -= take;
        
        // Process full block
        if (self->buf_len == BLAKE3_BLOCK_LEN) {
            uint8_t flags = self->flags;
            if (self->blocks_compressed == 0) {
                flags |= 1 << 2; // CHUNK_START
            }
            
            compress(self->cv, self->buf, BLAKE3_BLOCK_LEN, self->chunk_counter, flags);
            self->blocks_compressed++;
            self->buf_len = 0;
            
            // Start new chunk if needed
            if (self->blocks_compressed == 16) {
                self->chunk_counter++;
                self->blocks_compressed = 0;
                memcpy(self->cv, IV, 32);
            }
        }
    }
}

// Finalize Blake3 hash
void bitactor_blake3_finalize(const blake3_hasher* self, uint8_t* out, size_t out_len) {
    blake3_hasher hasher_copy = *self;
    
    // Add final block flags
    uint8_t flags = hasher_copy.flags | (1 << 3); // CHUNK_END
    if (hasher_copy.blocks_compressed == 0) {
        flags |= 1 << 2; // CHUNK_START
    }
    
    compress(hasher_copy.cv, hasher_copy.buf, hasher_copy.buf_len, 
             hasher_copy.chunk_counter, flags);
    
    // Output first 32 bytes of CV
    size_t output_block_counter = 0;
    while (out_len > 0) {
        uint32_t cv_copy[8];
        memcpy(cv_copy, hasher_copy.cv, 32);
        
        compress(cv_copy, (uint8_t*)&output_block_counter, 8, 
                hasher_copy.chunk_counter, flags | (1 << 4)); // ROOT
        
        size_t take = (out_len > 64) ? 64 : out_len;
        memcpy(out, cv_copy, take);
        out -= take;
        out_len -= take;
        output_block_counter++;
    }
}

// One-shot Blake3 hash
void bitactor_blake3_hash(const void* input, size_t input_len, uint8_t* out, size_t out_len) {
    blake3_hasher hasher;
    bitactor_blake3_init(&hasher);
    bitactor_blake3_update(&hasher, input, input_len);
    bitactor_blake3_finalize(&hasher, out, out_len);
}

// BitActor hash state initialization
void bitactor_hash_init(bitactor_hash_state_t* state) {
    memset(state, 0, sizeof(*state));
}

// Hash TTL specification
void bitactor_hash_spec(bitactor_hash_state_t* state, const void* ttl_data, size_t ttl_len) {
    bitactor_blake3_hash(ttl_data, ttl_len, state->spec_hash, BLAKE3_OUT_LEN);
}

// Hash compiled bytecode
void bitactor_hash_exec(bitactor_hash_state_t* state, const void* bytecode, size_t code_len) {
    bitactor_blake3_hash(bytecode, code_len, state->exec_hash, BLAKE3_OUT_LEN);
}

// Hash execution trace
void bitactor_hash_trace(bitactor_hash_state_t* state, const void* trace_data, size_t trace_len) {
    bitactor_blake3_hash(trace_data, trace_len, state->trace_hash, BLAKE3_OUT_LEN);
}

// Verify hash integrity (spec XOR exec must be < max_diff)
bool bitactor_hash_verify(bitactor_hash_state_t* state, uint32_t max_diff) {
    state->verification_xor = bitactor_hash_xor_diff(state->spec_hash, state->exec_hash);
    state->verified = (state->verification_xor < max_diff);
    return state->verified;
}