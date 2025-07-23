#ifndef BITACTOR_LITE_H
#define BITACTOR_LITE_H

#include <stdint.h>
#include <stdbool.h>

typedef struct {
    uint64_t capability;
    uint64_t hash;
} fast_proof_t;

typedef struct {
    uint64_t symbol;
    uint64_t price;
    uint64_t volume;
    uint64_t timestamp;
} quote_t;

typedef struct {
    uint64_t type;
    uint64_t symbol;
    uint64_t price;
    uint64_t quantity;
} order_t;

static inline bool bitactor_verify_fast(fast_proof_t* proof) {
    uint64_t expected = proof->capability ^ 0xDEADBEEFCAFEBABE;
    return proof->hash == expected;
}

#endif