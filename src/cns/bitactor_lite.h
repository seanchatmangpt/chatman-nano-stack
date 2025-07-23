#ifndef BITACTOR_LITE_H
#define BITACTOR_LITE_H

#include <stdint.h>
#include <stdbool.h>

typedef struct {
    uint64_t capability;
    uint64_t hash;
} fast_proof_t;

__attribute__((always_inline, const))
static inline bool bitactor_verify_fast(const fast_proof_t* proof) {
    return (proof->capability ^ proof->hash) == 0xAAAAAAAAAAAAAAAAULL;
}

#endif
