#ifndef BITACTOR_PORTABLE_H
#define BITACTOR_PORTABLE_H

#include <stdint.h>

// Architecture detection and portable intrinsics
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
    #define BITACTOR_X86 1
    #include <immintrin.h>
    
    static inline uint64_t bitactor_rdtsc(void) {
        return __rdtsc();
    }
    
    #ifdef __AVX2__
        #define BITACTOR_SIMD_AVX2 1
    #endif
    
#elif defined(__aarch64__) || defined(_M_ARM64) || defined(__arm__)
    #define BITACTOR_ARM 1
    
    static inline uint64_t bitactor_rdtsc(void) {
        uint64_t val;
        #ifdef __aarch64__
            asm volatile("mrs %0, cntvct_el0" : "=r"(val));
        #else
            // ARM32 fallback
            val = 0; // Use system clock or performance counter
        #endif
        return val;
    }
    
    // ARM NEON equivalents for SIMD (simplified)
    typedef struct { uint64_t v[4]; } bitactor_simd256_t;
    
    static inline bitactor_simd256_t bitactor_simd_and(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] & b.v[i];
        }
        return result;
    }
    
    static inline bitactor_simd256_t bitactor_simd_or(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] | b.v[i];
        }
        return result;
    }
    
    static inline bitactor_simd256_t bitactor_simd_xor(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] ^ b.v[i];
        }
        return result;
    }
    
#else
    #define BITACTOR_GENERIC 1
    
    static inline uint64_t bitactor_rdtsc(void) {
        // Generic fallback using clock
        return (uint64_t)clock();
    }
    
    typedef struct { uint64_t v[4]; } bitactor_simd256_t;
    
    static inline bitactor_simd256_t bitactor_simd_and(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] & b.v[i];
        }
        return result;
    }
    
    static inline bitactor_simd256_t bitactor_simd_or(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] | b.v[i];
        }
        return result;
    }
    
    static inline bitactor_simd256_t bitactor_simd_xor(bitactor_simd256_t a, bitactor_simd256_t b) {
        bitactor_simd256_t result;
        for (int i = 0; i < 4; i++) {
            result.v[i] = a.v[i] ^ b.v[i];
        }
        return result;
    }
#endif

// Portable memory operations
static inline void bitactor_prefetch(const void* addr, int rw, int locality) {
    #if defined(__GNUC__) || defined(__clang__)
        __builtin_prefetch(addr, rw, locality);
    #else
        (void)addr; (void)rw; (void)locality;
    #endif
}

// Portable atomic operations
static inline uint32_t bitactor_atomic_load(volatile uint32_t* ptr) {
    #if defined(__GNUC__) || defined(__clang__)
        return __atomic_load_n(ptr, __ATOMIC_ACQUIRE);
    #else
        return *ptr;
    #endif
}

static inline void bitactor_atomic_store(volatile uint32_t* ptr, uint32_t val) {
    #if defined(__GNUC__) || defined(__clang__)
        __atomic_store_n(ptr, val, __ATOMIC_RELEASE);
    #else
        *ptr = val;
    #endif
}

// Portable utility functions
static inline uint32_t bitactor_min(uint32_t a, uint32_t b) {
    return (a < b) ? a : b;
}

static inline uint32_t bitactor_max(uint32_t a, uint32_t b) {
    return (a > b) ? a : b;
}

static inline uint64_t bitactor_min64(uint64_t a, uint64_t b) {
    return (a < b) ? a : b;
}

static inline uint64_t bitactor_max64(uint64_t a, uint64_t b) {
    return (a > b) ? a : b;
}

#endif