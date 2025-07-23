#ifndef CNS_V8_CORE_H
#define CNS_V8_CORE_H

/**
 * CNS v8.0 Core Types - Complete type system
 *
 * This header provides the complete CNS v8.0 core type system
 * implementing the 8T-8H-8M trinity contracts:
 * 
 * - BitActor: Quantum computational units
 * - RingBus: Lock-free inter-actor communication  
 * - Fiber: Cooperative lightweight threading
 * 
 * All types comply with:
 * - 8T: Maximum 8 CPU ticks per operation
 * - 8H: Six Sigma quality (Cpk > 20)
 * - 8M: 8-byte quantum memory alignment
 */

/* Generated code compatibility types */
typedef int64_t long_t;

#include "core/bit_actor.h"
#include "core/ring_bus.h" 
#include "core/fiber.h"

#ifdef __cplusplus
extern "C" {
#endif

// CNS v8.0 System Version
#define CNS_V8_VERSION_MAJOR 8
#define CNS_V8_VERSION_MINOR 0
#define CNS_V8_VERSION_PATCH 0
#define CNS_V8_VERSION_STRING "8.0.0"

// Trinity constants
#define CNS_V8_MAX_TICKS 8
#define CNS_V8_SIX_SIGMA_CPK 20.0  
#define CNS_V8_QUANTUM_ALIGNMENT 8

// Performance multiplier guarantee
#define CNS_V8_PERFORMANCE_MULTIPLIER 26.0

// System initialization
int cns_v8_initialize(void);
int cns_v8_shutdown(void);
const char* cns_v8_version_string(void);

// Global system health
bool cns_v8_system_healthy(void);
int cns_v8_global_health_check(void);
double cns_v8_global_cpk(void);

#ifdef __cplusplus
}
#endif

#endif // CNS_V8_CORE_H