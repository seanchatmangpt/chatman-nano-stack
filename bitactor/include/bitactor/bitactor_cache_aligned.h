/*
 * BitActor Cache-Aligned Structures - 80/20 Win #3
 * SWARM: Cache_Optimizer Implementation
 * 
 * Optimizes data structures for CPU cache efficiency
 * Target: -100+ cycles through better cache utilization
 */
#ifndef BITACTOR_CACHE_ALIGNED_H
#define BITACTOR_CACHE_ALIGNED_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* CPU cache line size (64 bytes on modern x86/ARM) */
#define CACHE_LINE_SIZE 64
#define CACHE_ALIGNED __attribute__((aligned(CACHE_LINE_SIZE)))

/* Prefetch hints for upcoming data */
#ifdef __GNUC__
#define PREFETCH_READ(addr) __builtin_prefetch((addr), 0, 3)
#define PREFETCH_WRITE(addr) __builtin_prefetch((addr), 1, 3)
#else
#define PREFETCH_READ(addr) ((void)0)
#define PREFETCH_WRITE(addr) ((void)0)
#endif

/*
 * OPTIMIZED Signal Structure
 * - Frequently accessed fields grouped together
 * - Hot fields in first cache line
 * - Cold fields in second cache line
 * - Total size: exactly 64 bytes (1 cache line)
 */
typedef struct CACHE_ALIGNED {
    /* HOT FIELDS - First 32 bytes (accessed on every signal) */
    uint32_t id;           /* 4 bytes - Signal ID */
    uint8_t type;          /* 1 byte  - Signal type */
    uint8_t flags;         /* 1 byte  - Processing flags */
    uint8_t priority;      /* 1 byte  - Priority level */
    uint8_t kind;          /* 1 byte  - Dispatch kind */
    
    uint64_t payload;      /* 8 bytes - Primary data */
    uint64_t timestamp;    /* 8 bytes - Reception time */
    uint64_t context;      /* 8 bytes - Context pointer */
    
    /* COLD FIELDS - Padding to exactly 64 bytes */
    uint8_t _reserved[32]; /* 32 bytes - Future use / padding */
} signal_opt_t;

/* Verify structure size at compile time */
_Static_assert(sizeof(signal_opt_t) == CACHE_LINE_SIZE, 
               "signal_opt_t must be exactly one cache line");

/*
 * OPTIMIZED Result Structure
 * - Compact to fit multiple results per cache line
 * - 32 bytes allows 2 results per cache line
 */
typedef struct {
    uint32_t signal_id;    /* 4 bytes */
    uint32_t exec_hash;    /* 4 bytes */
    uint64_t result;       /* 8 bytes */
    uint64_t cycles;       /* 8 bytes - Execution cycles */
    uint8_t status;        /* 1 byte */
    uint8_t ticks;         /* 1 byte */
    uint8_t flags;         /* 1 byte */
    uint8_t fiber_id;      /* 1 byte */
    uint8_t _padding[4];   /* 4 bytes - Align to 32 bytes */
} result_opt_t;

_Static_assert(sizeof(result_opt_t) == 32, 
               "result_opt_t must be exactly 32 bytes");

/*
 * OPTIMIZED Dispatch Entry
 * - Handler and frequently accessed data in same cache line
 * - 64 bytes total
 */
typedef struct CACHE_ALIGNED {
    void* handler;         /* 8 bytes - Function pointer */
    void* context;         /* 8 bytes - Handler context */
    uint64_t call_count;   /* 8 bytes - Statistics */
    uint64_t total_cycles; /* 8 bytes - Performance tracking */
    
    uint8_t signal_type;   /* 1 byte */
    uint8_t min_priority;  /* 1 byte */
    uint8_t max_ticks;     /* 1 byte */
    uint8_t flags;         /* 1 byte */
    uint8_t _padding1[4];  /* 4 bytes */
    
    uint8_t _padding2[24]; /* 24 bytes - Align to 64 bytes */
} dispatch_entry_opt_t;

_Static_assert(sizeof(dispatch_entry_opt_t) == CACHE_LINE_SIZE,
               "dispatch_entry_opt_t must be exactly one cache line");

/*
 * OPTIMIZED Batch Processing Structure
 * - Process multiple signals with better cache locality
 * - Prefetch next batch while processing current
 */
#define SIGNAL_BATCH_SIZE 8  /* Process 8 signals at once */

typedef struct CACHE_ALIGNED {
    signal_opt_t signals[SIGNAL_BATCH_SIZE];
    result_opt_t results[SIGNAL_BATCH_SIZE];
    uint32_t count;
    uint32_t processed;
    uint8_t _padding[56];  /* Align to cache line */
} signal_batch_t;

/*
 * OPTIMIZED Ring Buffer for Lock-Free Communication
 * - Producer and consumer indices in separate cache lines
 * - Power-of-2 size for fast modulo
 */
typedef struct {
    /* Producer cache line */
    CACHE_ALIGNED struct {
        volatile uint64_t head;
        uint64_t cached_tail;
        uint8_t _padding[48];
    } producer;
    
    /* Consumer cache line */
    CACHE_ALIGNED struct {
        volatile uint64_t tail;
        uint64_t cached_head;
        uint8_t _padding[48];
    } consumer;
    
    /* Data array (separate cache lines) */
    CACHE_ALIGNED signal_opt_t* data;
    uint64_t size;
    uint64_t mask;  /* size - 1 for fast modulo */
} ring_buffer_t;

/*
 * Inline functions for cache-efficient operations
 */

/* Prefetch signal batch for processing */
static inline void prefetch_signal_batch(const signal_opt_t* signals, uint32_t count) {
    for (uint32_t i = 0; i < count; i++) {
        PREFETCH_READ(&signals[i]);
    }
}

/* Cache-friendly signal copy */
static inline void signal_copy_optimized(signal_opt_t* dst, const signal_opt_t* src) {
    /* Compiler will optimize to efficient memory copy */
    *dst = *src;
}

/* Batch signal processing with prefetch */
static inline void process_signal_batch_optimized(
    const signal_opt_t* signals,
    result_opt_t* results,
    uint32_t count,
    void (*handler)(const signal_opt_t*, result_opt_t*)) {
    
    /* Prefetch entire batch */
    prefetch_signal_batch(signals, count);
    
    /* Process with cache locality */
    for (uint32_t i = 0; i < count; i++) {
        /* Prefetch next signal while processing current */
        if (i + 1 < count) {
            PREFETCH_READ(&signals[i + 1]);
        }
        
        handler(&signals[i], &results[i]);
    }
}

/*
 * Memory layout helpers
 */

/* Ensure structure is cache-aligned */
#define ENSURE_CACHE_ALIGNED(ptr) \
    ((void*)((((uintptr_t)(ptr)) + CACHE_LINE_SIZE - 1) & ~(CACHE_LINE_SIZE - 1)))

/* Calculate padding needed for cache alignment */
#define CACHE_PADDING_SIZE(current_size) \
    ((CACHE_LINE_SIZE - ((current_size) & (CACHE_LINE_SIZE - 1))) & (CACHE_LINE_SIZE - 1))

/* Allocate cache-aligned memory */
static inline void* cache_aligned_alloc(size_t size) {
    /* Round up to cache line multiple */
    size_t aligned_size = (size + CACHE_LINE_SIZE - 1) & ~(CACHE_LINE_SIZE - 1);
    return aligned_alloc(CACHE_LINE_SIZE, aligned_size);
}

/*
 * Performance hints
 */

/* Likely/unlikely branch hints */
#ifdef __GNUC__
#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x) (x)
#define UNLIKELY(x) (x)
#endif

/* Memory barrier for multi-core synchronization */
#define MEMORY_BARRIER() __sync_synchronize()

/* Compare and swap for lock-free operations */
#define CAS(ptr, old, new) __sync_bool_compare_and_swap(ptr, old, new)

#endif /* BITACTOR_CACHE_ALIGNED_H */