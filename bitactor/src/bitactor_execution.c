/*
 * BitActor Bytecode Execution Engine
 * Deterministic execution with ≤8 CPU tick guarantee
 */
#include "../include/bitactor/bitactor.h"
#include "../include/bitactor/bitactor_blake3.h"
#include <string.h>
#include <x86intrin.h>

// Execution context for bytecode programs
typedef struct {
    uint64_t registers[256];          // Virtual registers
    uint8_t* memory;                  // Scratch memory
    uint32_t pc;                      // Program counter
    uint32_t stack_ptr;               // Stack pointer
    uint64_t cycle_budget;            // Remaining cycles
    bitactor_hash_state_t hash_state; // Hash verification
} exec_context_t;

// Bytecode execution statistics
typedef struct {
    uint64_t total_instructions;
    uint64_t total_cycles;
    uint32_t max_cycles_per_signal;
    uint32_t exceeded_budget_count;
} exec_stats_t;

static exec_stats_t g_exec_stats = {0};

// Branchless execution dispatch table
typedef void (*opcode_handler_t)(exec_context_t* ctx, bitinstr_t instr);

// Forward declarations
static void exec_nop(exec_context_t* ctx, bitinstr_t instr);
static void exec_load(exec_context_t* ctx, bitinstr_t instr);
static void exec_store(exec_context_t* ctx, bitinstr_t instr);
static void exec_add(exec_context_t* ctx, bitinstr_t instr);
static void exec_sub(exec_context_t* ctx, bitinstr_t instr);
static void exec_mul(exec_context_t* ctx, bitinstr_t instr);
static void exec_and(exec_context_t* ctx, bitinstr_t instr);
static void exec_or(exec_context_t* ctx, bitinstr_t instr);
static void exec_xor(exec_context_t* ctx, bitinstr_t instr);
static void exec_shl(exec_context_t* ctx, bitinstr_t instr);
static void exec_shr(exec_context_t* ctx, bitinstr_t instr);
static void exec_cmp(exec_context_t* ctx, bitinstr_t instr);
static void exec_mov(exec_context_t* ctx, bitinstr_t instr);
static void exec_min(exec_context_t* ctx, bitinstr_t instr);
static void exec_max(exec_context_t* ctx, bitinstr_t instr);
static void exec_hash(exec_context_t* ctx, bitinstr_t instr);
static void exec_trace(exec_context_t* ctx, bitinstr_t instr);

// SIMD handlers
static void exec_vload(exec_context_t* ctx, bitinstr_t instr);
static void exec_vstore(exec_context_t* ctx, bitinstr_t instr);
static void exec_vand(exec_context_t* ctx, bitinstr_t instr);
static void exec_vor(exec_context_t* ctx, bitinstr_t instr);
static void exec_vxor(exec_context_t* ctx, bitinstr_t instr);

// Execution dispatch table (256 entries, aligned for cache efficiency)
static const opcode_handler_t OPCODE_HANDLERS[256] __attribute__((aligned(64))) = {
    [0x00] = exec_nop,    [0x01] = exec_load,   [0x02] = exec_store,  [0x03] = exec_add,
    [0x04] = exec_sub,    [0x05] = exec_mul,    [0x06] = exec_xor,    [0x07] = exec_and,
    [0x08] = exec_or,     [0x09] = exec_shl,    [0x0A] = exec_shr,    [0x0B] = exec_cmp,
    [0x0C] = exec_nop,    [0x0D] = exec_nop,    [0x0E] = exec_nop,    [0x0F] = exec_nop,
    [0x10] = exec_trace,  [0x11] = exec_hash,   [0x12] = exec_min,    [0x13] = exec_max,
    [0x14] = exec_mov,    [0x15] = exec_nop,    [0x16] = exec_nop,    [0x17] = exec_nop,
    // SIMD operations
    [0x20] = exec_vload,  [0x21] = exec_vstore, [0x22] = exec_nop,    [0x23] = exec_nop,
    [0x24] = exec_nop,    [0x25] = exec_vand,   [0x26] = exec_vor,    [0x27] = exec_vxor,
    // Fill remaining with no-op
    [0x28 ... 0xFF] = exec_nop
};

// Core execution functions

// Execute single instruction (branchless, 1 cycle target)
__attribute__((hot, always_inline))
static inline void execute_instruction(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t start_cycle = __rdtsc();
    
    // Branchless dispatch using function pointer table
    OPCODE_HANDLERS[instr.opcode](ctx, instr);
    
    // Update cycle budget (branchless)
    uint64_t cycles_used = __rdtsc() - start_cycle;
    ctx->cycle_budget = (ctx->cycle_budget > cycles_used) ? 
                       (ctx->cycle_budget - cycles_used) : 0;
    
    g_exec_stats.total_instructions++;
    g_exec_stats.total_cycles += cycles_used;
}

// Execute bytecode program (main entry point)
__attribute__((hot))
result_t bitactor_execute_program(bitactor_t* ba, const signal_t* signal, 
                                  const bitinstr_t* program, uint32_t program_size) {
    result_t result = {0};
    exec_context_t ctx = {0};
    
    // Initialize execution context
    ctx.memory = ba->scratch;
    ctx.cycle_budget = BITACTOR_TICK_BUDGET;
    ctx.pc = 0;
    
    // Set up initial registers
    ctx.registers[0] = signal->kind;
    ctx.registers[1] = signal->payload;
    ctx.registers[2] = signal->timestamp;
    
    // Initialize hash state for verification
    bitactor_hash_init(&ctx.hash_state);
    bitactor_hash_exec(&ctx.hash_state, program, program_size * sizeof(bitinstr_t));
    
    uint64_t start_total_cycles = __rdtsc();
    
    // Execute program (unrolled for common case of small programs)
    while (ctx.pc < program_size && ctx.cycle_budget > 0) {
        bitinstr_t instr = program[ctx.pc];
        
        // Prefetch next instruction
        if (ctx.pc + 1 < program_size) {
            __builtin_prefetch(&program[ctx.pc + 1], 0, 3);
        }
        
        execute_instruction(&ctx, instr);
        ctx.pc++;
        
        // Early termination on critical budget exhaustion
        if (ctx.cycle_budget == 0) {
            result.status = BITACTOR_TICK_EXCEEDED;
            break;
        }
    }
    
    uint64_t total_cycles = __rdtsc() - start_total_cycles;
    
    // Finalize result
    result.signal_id = signal->kind;
    result.result = ctx.registers[0];  // Return value in R0
    result.ticks = (uint8_t)__builtin_min(total_cycles, 255);
    result.exec_hash = *(uint32_t*)ctx.hash_state.exec_hash;
    
    // Verify tick budget compliance
    if (total_cycles <= BITACTOR_TICK_BUDGET) {
        result.status = (result.status == 0) ? BITACTOR_OK : result.status;
    } else {
        result.status = BITACTOR_TICK_EXCEEDED;
        g_exec_stats.exceeded_budget_count++;
    }
    
    // Update max cycles tracking
    if (total_cycles > g_exec_stats.max_cycles_per_signal) {
        g_exec_stats.max_cycles_per_signal = (uint32_t)total_cycles;
    }
    
    return result;
}

// Opcode implementations (branchless, optimized)

static void exec_nop(exec_context_t* ctx, bitinstr_t instr) {
    (void)ctx; (void)instr;
    // Literally do nothing - 1 cycle
}

static void exec_load(exec_context_t* ctx, bitinstr_t instr) {
    // Load from memory[src1] into register[dst]
    uint32_t addr = (uint32_t)ctx->registers[instr.src1] & (BITACTOR_SCRATCH_SIZE - 8);
    ctx->registers[instr.dst] = *(uint64_t*)(ctx->memory + addr);
}

static void exec_store(exec_context_t* ctx, bitinstr_t instr) {
    // Store register[src1] to memory[dst]
    uint32_t addr = (uint32_t)ctx->registers[instr.dst] & (BITACTOR_SCRATCH_SIZE - 8);
    *(uint64_t*)(ctx->memory + addr) = ctx->registers[instr.src1];
}

static void exec_add(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] + ctx->registers[instr.src2];
}

static void exec_sub(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] - ctx->registers[instr.src2];
}

static void exec_mul(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] * ctx->registers[instr.src2];
}

static void exec_and(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] & ctx->registers[instr.src2];
}

static void exec_or(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] | ctx->registers[instr.src2];
}

static void exec_xor(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1] ^ ctx->registers[instr.src2];
}

static void exec_shl(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t shift = ctx->registers[instr.src2] & 63;
    ctx->registers[instr.dst] = ctx->registers[instr.src1] << shift;
}

static void exec_shr(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t shift = ctx->registers[instr.src2] & 63;
    ctx->registers[instr.dst] = ctx->registers[instr.src1] >> shift;
}

static void exec_cmp(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t a = ctx->registers[instr.src1];
    uint64_t b = ctx->registers[instr.src2];
    ctx->registers[instr.dst] = (a > b) ? 1 : ((a < b) ? -1 : 0);
}

static void exec_mov(exec_context_t* ctx, bitinstr_t instr) {
    ctx->registers[instr.dst] = ctx->registers[instr.src1];
}

static void exec_min(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t a = ctx->registers[instr.src1];
    uint64_t b = ctx->registers[instr.src2];
    ctx->registers[instr.dst] = (a < b) ? a : b;
}

static void exec_max(exec_context_t* ctx, bitinstr_t instr) {
    uint64_t a = ctx->registers[instr.src1];
    uint64_t b = ctx->registers[instr.src2];
    ctx->registers[instr.dst] = (a > b) ? a : b;
}

static void exec_hash(exec_context_t* ctx, bitinstr_t instr) {
    // Hash register value using Blake3
    uint64_t value = ctx->registers[instr.src1];
    uint8_t hash[8];
    bitactor_blake3_hash(&value, sizeof(value), hash, sizeof(hash));
    ctx->registers[instr.dst] = *(uint64_t*)hash;
}

static void exec_trace(exec_context_t* ctx, bitinstr_t instr) {
    // Record trace point for reversibility
    uint64_t trace_data[3] = {
        ctx->pc,
        ctx->registers[instr.src1],
        ctx->registers[instr.src2]
    };
    bitactor_hash_trace(&ctx->hash_state, trace_data, sizeof(trace_data));
}

// SIMD implementations

static void exec_vload(exec_context_t* ctx, bitinstr_t instr) {
    // Load 256-bit vector from memory
    uint32_t addr = (uint32_t)ctx->registers[instr.src1] & (BITACTOR_SCRATCH_SIZE - 32);
    __m256i vec = _mm256_load_si256((__m256i*)(ctx->memory + addr));
    
    // Store in 4 consecutive registers
    uint64_t* regs = (uint64_t*)&vec;
    for (int i = 0; i < 4; i++) {
        ctx->registers[(instr.dst + i) & 255] = regs[i];
    }
}

static void exec_vstore(exec_context_t* ctx, bitinstr_t instr) {
    // Store 256-bit vector to memory
    uint32_t addr = (uint32_t)ctx->registers[instr.dst] & (BITACTOR_SCRATCH_SIZE - 32);
    
    // Load from 4 consecutive registers
    uint64_t regs[4];
    for (int i = 0; i < 4; i++) {
        regs[i] = ctx->registers[(instr.src1 + i) & 255];
    }
    
    __m256i vec = _mm256_load_si256((__m256i*)regs);
    _mm256_store_si256((__m256i*)(ctx->memory + addr), vec);
}

static void exec_vand(exec_context_t* ctx, bitinstr_t instr) {
    // Vector AND operation
    for (int i = 0; i < 4; i++) {
        uint8_t dst_reg = (instr.dst + i) & 255;
        uint8_t src1_reg = (instr.src1 + i) & 255;
        uint8_t src2_reg = (instr.src2 + i) & 255;
        ctx->registers[dst_reg] = ctx->registers[src1_reg] & ctx->registers[src2_reg];
    }
}

static void exec_vor(exec_context_t* ctx, bitinstr_t instr) {
    // Vector OR operation
    for (int i = 0; i < 4; i++) {
        uint8_t dst_reg = (instr.dst + i) & 255;
        uint8_t src1_reg = (instr.src1 + i) & 255;
        uint8_t src2_reg = (instr.src2 + i) & 255;
        ctx->registers[dst_reg] = ctx->registers[src1_reg] | ctx->registers[src2_reg];
    }
}

static void exec_vxor(exec_context_t* ctx, bitinstr_t instr) {
    // Vector XOR operation
    for (int i = 0; i < 4; i++) {
        uint8_t dst_reg = (instr.dst + i) & 255;
        uint8_t src1_reg = (instr.src1 + i) & 255;
        uint8_t src2_reg = (instr.src2 + i) & 255;
        ctx->registers[dst_reg] = ctx->registers[src1_reg] ^ ctx->registers[src2_reg];
    }
}

// Get execution statistics
void bitactor_get_exec_stats(exec_stats_t* stats) {
    if (stats) {
        *stats = g_exec_stats;
    }
}

// Reset execution statistics
void bitactor_reset_exec_stats(void) {
    memset(&g_exec_stats, 0, sizeof(g_exec_stats));
}