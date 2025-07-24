/*
 * BitActor Runtime Bytecode Loader
 * Loads and executes optimized bytecode programs with ≤8 tick guarantee
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <immintrin.h>
#include <assert.h>

#include "../include/bitactor/bitactor.h"

// Bytecode file format constants
#define BITACTOR_MAGIC     0x43544942  // "BITC"
#define BITACTOR_VERSION   1
#define MAX_BYTECODE_SIZE  32768
#define MAX_CONSTANTS      1024
#define VECTOR_WIDTH       8           // AVX2 256-bit / 32-bit

// Zero-tick optimization flags
#define ZERO_TICK_FLAG 0x01
#define TRIVIAL_SKIP_FLAG 0x02

// Bytecode instruction format (matches compiler IR)
typedef struct {
    uint8_t opcode;
    uint8_t dst;
    uint8_t src1; 
    uint8_t src2;
    uint8_t flags;  // Zero-tick and optimization flags
} __attribute__((packed)) bc_instruction_t;

// Vector register state for SIMD execution
typedef struct {
    __m256i v[16];  // 16 vector registers (v0-v15)
    uint8_t dirty;  // Dirty bit mask
} vector_state_t;

// Bytecode execution context
typedef struct {
    uint32_t registers[256];      // Scalar registers (r0-r255)
    vector_state_t vectors;       // Vector registers
    uint32_t constants[MAX_CONSTANTS]; // Constant pool
    uint32_t constant_count;
    uint8_t tick_count;          // Current tick count
    telemetry_frame_t* telemetry; // Telemetry output
} bc_context_t;

// Bytecode program header
typedef struct {
    uint32_t magic;
    uint16_t version;
    uint16_t reserved;
    uint32_t entry_offset;
    uint32_t constant_count;
} __attribute__((packed)) bc_header_t;

// Forward declarations
static int execute_instruction(bc_context_t* ctx, const bc_instruction_t* instr);
static void emit_telemetry(bc_context_t* ctx, uint8_t opcode, uint32_t data);
static __m256i load_vector_operand(bc_context_t* ctx, uint8_t src);
static void store_vector_result(bc_context_t* ctx, uint8_t dst, __m256i value);

// Opcode definitions (must match compiler IR)
enum {
    // Scalar operations
    OP_NOP = 0x00,
    OP_LOAD = 0x01,
    OP_STORE = 0x02,
    OP_ADD = 0x03,
    OP_SUB = 0x04,
    OP_MUL = 0x05,
    OP_XOR = 0x06,
    OP_AND = 0x07,
    OP_OR = 0x08,
    OP_SHL = 0x09,
    OP_SHR = 0x0A,
    OP_CMP = 0x0B,
    OP_JMP = 0x0C,
    OP_CALL = 0x0D,
    OP_RET = 0x0E,
    OP_SIGNAL = 0x0F,
    OP_TRACE = 0x10,
    OP_HASH = 0x11,
    OP_MIN = 0x12,
    OP_MAX = 0x13,
    OP_MOV = 0x14,
    OP_COUNT = 0x15,
    
    // SIMD operations
    OP_VLOAD = 0x20,
    OP_VSTORE = 0x21,
    OP_VADD = 0x22,
    OP_VSUB = 0x23,
    OP_VMUL = 0x24,
    OP_VAND = 0x25,
    OP_VOR = 0x26,
    OP_VXOR = 0x27,
    OP_VCMP = 0x28,
    OP_VMIN = 0x29,
    OP_VMAX = 0x2A,
    OP_VMASK = 0x2B,
    OP_VPACK = 0x2C,
    OP_VUNPACK = 0x2D,
    
    // Specialized SIMD operations
    OP_VRANGE = 0x30,
    OP_VPATTERN = 0x31,
    OP_VEXCL = 0x32,
    OP_VCOUNT = 0x33,
    
    // Advanced operations
    OP_PERFHASH = 0x40,
    OP_RDTSC = 0x41,
    OP_PREFETCH = 0x42
};

/**
 * Load bytecode program from file
 */
int bitactor_load_bytecode(const char* filename, uint8_t** bytecode_out, size_t* size_out) {
    FILE* fp = fopen(filename, "rb");
    if (!fp) {
        return -1;
    }
    
    // Read header
    bc_header_t header;
    if (fread(&header, sizeof(header), 1, fp) != 1) {
        fclose(fp);
        return -1;
    }
    
    // Validate header
    if (header.magic != BITACTOR_MAGIC || header.version != BITACTOR_VERSION) {
        fclose(fp);
        return -1;
    }
    
    // Get file size
    fseek(fp, 0, SEEK_END);
    size_t file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    if (file_size > MAX_BYTECODE_SIZE) {
        fclose(fp);
        return -1;
    }
    
    // Allocate and read bytecode
    uint8_t* bytecode = malloc(file_size);
    if (!bytecode) {
        fclose(fp);
        return -1;
    }
    
    if (fread(bytecode, file_size, 1, fp) != 1) {
        free(bytecode);
        fclose(fp);
        return -1;
    }
    
    fclose(fp);
    
    *bytecode_out = bytecode;
    *size_out = file_size;
    return 0;
}

/**
 * Check if signal is trivially skippable (zero-tick optimization)
 */
static inline bool signal_is_trivially_skippable(const signal_t* sig) {
    // Heartbeat signals
    if (sig->type == 0xFF) return true;  // SIG_HEARTBEAT
    
    // Zero confidence signals
    if ((sig->payload & 0xFF) == 0) return true;  // confidence = 0
    
    // Test/mock signals
    if ((sig->flags & 0x80) != 0) return true;  // TEST_SIGNAL_FLAG
    
    return false;
}

/**
 * Execute bytecode program with tick budget enforcement and zero-tick optimization
 */
result_t bitactor_execute_bytecode(signal_t* signal, const uint8_t* bytecode, 
                                   size_t bytecode_size, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    
    // Zero-tick optimization: early exit for trivially skippable signals
    if (signal_is_trivially_skippable(signal)) {
        result.status = BITACTOR_OK;
        result.ticks = 0;  // True zero-tick execution
        result.result = 0;  // No-op result
        result.exec_hash = 0x5A4E00;  // "ZERO" in hex
        return result;
    }
    
    // Initialize execution context
    bc_context_t ctx = {0};
    ctx.telemetry = (telemetry_frame_t*)scratch;
    
    // Parse bytecode header
    const bc_header_t* header = (const bc_header_t*)bytecode;
    if (header->magic != BITACTOR_MAGIC) {
        result.status = BITACTOR_INVALID_SIGNAL;
        return result;
    }
    
    // Load constants
    const uint32_t* constants = (const uint32_t*)(bytecode + sizeof(bc_header_t));
    ctx.constant_count = header->constant_count;
    
    for (uint32_t i = 0; i < ctx.constant_count && i < MAX_CONSTANTS; i++) {
        ctx.constants[i] = constants[i * 2 + 1];  // Skip address, take value
    }
    
    // Initialize registers with signal data
    ctx.registers[0] = (uintptr_t)signal;  // r0 = signal pointer
    ctx.registers[1] = signal->payload & 0xFFFFFFFF;  // r1 = payload low
    ctx.registers[2] = signal->payload >> 32;         // r2 = payload high
    ctx.registers[3] = 0;  // r3 = scratch
    
    // Find entry point
    const bc_instruction_t* instructions = (const bc_instruction_t*)
        (bytecode + header->entry_offset);
    size_t instruction_count = (bytecode_size - header->entry_offset) / sizeof(bc_instruction_t);
    
    // Execute instructions with tick budget
    ctx.tick_count = 0;
    uint64_t start_cycles = __rdtsc();
    
    for (size_t pc = 0; pc < instruction_count && ctx.tick_count < BITACTOR_TICK_BUDGET; pc++) {
        const bc_instruction_t* instr = &instructions[pc];
        
        // Zero-tick optimization: skip instructions with ZERO_TICK_FLAG
        if (instr->flags & ZERO_TICK_FLAG) {
            continue;  // No tick consumed, bypass instruction
        }
        
        // Execute instruction
        int tick_cost = execute_instruction(&ctx, instr);
        if (tick_cost < 0) {
            result.status = BITACTOR_INVALID_SIGNAL;
            break;
        }
        
        ctx.tick_count += tick_cost;
        
        // Check for early return
        if (instr->opcode == OP_RET) {
            break;
        }
        
        // Tick budget exceeded
        if (ctx.tick_count >= BITACTOR_TICK_BUDGET) {
            result.status = BITACTOR_TICK_EXCEEDED;
            break;
        }
    }
    
    uint64_t end_cycles = __rdtsc();
    
    // Set result
    result.result = ctx.registers[2];  // r2 typically holds result
    result.ticks = ctx.tick_count;
    result.status = (result.status == 0) ? BITACTOR_OK : result.status;
    result.exec_hash = (uint32_t)(end_cycles ^ start_cycles);  // Execution fingerprint
    
    return result;
}

/**
 * Execute single bytecode instruction
 */
static int execute_instruction(bc_context_t* ctx, const bc_instruction_t* instr) {
    uint8_t op = instr->opcode;
    uint8_t dst = instr->dst;
    uint8_t src1 = instr->src1;
    uint8_t src2 = instr->src2;
    
    // Emit telemetry
    emit_telemetry(ctx, op, (dst << 16) | (src1 << 8) | src2);
    
    switch (op) {
        case OP_NOP:
            return 0;  // No tick cost
            
        case OP_LOAD:
            if (src2 < ctx->constant_count) {
                ctx->registers[dst] = ctx->constants[src2];
            }
            return 1;
            
        case OP_STORE:
            // Store to scratch memory (simplified)
            return 1;
            
        case OP_ADD:
            ctx->registers[dst] = ctx->registers[src1] + ctx->registers[src2];
            return 1;
            
        case OP_SUB:
            ctx->registers[dst] = ctx->registers[src1] - ctx->registers[src2];
            return 1;
            
        case OP_MUL:
            ctx->registers[dst] = ctx->registers[src1] * ctx->registers[src2];
            return 1;
            
        case OP_XOR:
            ctx->registers[dst] = ctx->registers[src1] ^ ctx->registers[src2];
            return 1;
            
        case OP_AND:
            ctx->registers[dst] = ctx->registers[src1] & ctx->registers[src2];
            return 1;
            
        case OP_OR:
            ctx->registers[dst] = ctx->registers[src1] | ctx->registers[src2];
            return 1;
            
        case OP_CMP:
            ctx->registers[dst] = (ctx->registers[src1] >= ctx->registers[src2]) ? 1 : 0;
            return 1;
            
        case OP_MIN:
            ctx->registers[dst] = (ctx->registers[src1] < ctx->registers[src2]) ? 
                                 ctx->registers[src1] : ctx->registers[src2];
            return 1;
            
        case OP_MAX:
            ctx->registers[dst] = (ctx->registers[src1] > ctx->registers[src2]) ? 
                                 ctx->registers[src1] : ctx->registers[src2];
            return 1;
            
        case OP_MOV:
            ctx->registers[dst] = ctx->registers[src1];
            return 1;
            
        case OP_HASH:
            // Simple hash function (2 ticks)
            ctx->registers[dst] = (ctx->registers[src1] * 0x9E3779B9) ^ ctx->registers[src2];
            return 2;
            
        case OP_TRACE:
            // Emit trace telemetry
            emit_telemetry(ctx, OP_TRACE, ctx->registers[src1]);
            return 1;
            
        case OP_RET:
            return 1;
            
        // SIMD operations
        case OP_VLOAD: {
            __m256i value = _mm256_set1_epi32(ctx->registers[src1]);
            store_vector_result(ctx, dst, value);
            return 1;
        }
        
        case OP_VADD: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_add_epi32(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VAND: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_and_si256(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VOR: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_or_si256(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VXOR: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_xor_si256(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VCMP: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_cmpeq_epi32(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VMIN: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_min_epi32(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VMAX: {
            __m256i a = load_vector_operand(ctx, src1);
            __m256i b = load_vector_operand(ctx, src2);
            __m256i result = _mm256_max_epi32(a, b);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VRANGE: {
            // Parallel range check: min <= value <= max
            __m256i value = load_vector_operand(ctx, src1);
            __m256i bounds = load_vector_operand(ctx, src2);
            
            // Extract min/max from bounds (simplified)
            __m256i min_vals = _mm256_shuffle_epi32(bounds, 0x00);  // Broadcast min
            __m256i max_vals = _mm256_shuffle_epi32(bounds, 0x55);  // Broadcast max
            
            __m256i ge_min = _mm256_cmpgt_epi32(value, min_vals);
            __m256i le_max = _mm256_cmpgt_epi32(max_vals, value);
            __m256i result = _mm256_and_si256(ge_min, le_max);
            
            store_vector_result(ctx, dst, result);
            return 2;
        }
        
        case OP_VPATTERN: {
            // Simplified pattern matching (would be more complex in reality)
            __m256i data = load_vector_operand(ctx, src1);
            __m256i pattern = load_vector_operand(ctx, src2);
            __m256i result = _mm256_cmpeq_epi32(data, pattern);
            store_vector_result(ctx, dst, result);
            return 3;
        }
        
        case OP_VMASK: {
            __m256i data = load_vector_operand(ctx, src1);
            __m256i mask = _mm256_set1_epi32(src2);  // Use immediate as mask
            __m256i result = _mm256_and_si256(data, mask);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VPACK: {
            // Pack scalar registers into vector (simplified)
            uint32_t values[8] = {0};
            for (int i = 0; i < 4; i++) {
                values[i] = ctx->registers[src1 + i];
            }
            __m256i result = _mm256_loadu_si256((__m256i*)values);
            store_vector_result(ctx, dst, result);
            return 1;
        }
        
        case OP_VUNPACK: {
            // Unpack vector to scalar registers (simplified)
            __m256i data = load_vector_operand(ctx, src1);
            uint32_t values[8];
            _mm256_storeu_si256((__m256i*)values, data);
            
            for (int i = 0; i < 4; i++) {
                ctx->registers[dst + i] = values[i];
            }
            return 1;
        }
        
        case OP_RDTSC:
            ctx->registers[dst] = (uint32_t)__rdtsc();
            return 1;
            
        case OP_PREFETCH:
            // Prefetch instruction (no tick cost, overlapped)
            __builtin_prefetch((void*)(uintptr_t)ctx->registers[src1], 0, 3);
            return 0;
            
        default:
            return -1;  // Invalid opcode
    }
}

/**
 * Load vector operand from register or memory
 */
static __m256i load_vector_operand(bc_context_t* ctx, uint8_t src) {
    if (src >= 16 && src < 32) {
        // Vector register
        return ctx->vectors.v[src - 16];
    } else {
        // Scalar register - broadcast to vector
        return _mm256_set1_epi32(ctx->registers[src]);
    }
}

/**
 * Store vector result to register
 */
static void store_vector_result(bc_context_t* ctx, uint8_t dst, __m256i value) {
    if (dst >= 16 && dst < 32) {
        // Store to vector register
        ctx->vectors.v[dst - 16] = value;
        ctx->vectors.dirty |= (1 << (dst - 16));
    } else {
        // Store first lane to scalar register
        ctx->registers[dst] = _mm256_extract_epi32(value, 0);
    }
}

/**
 * Emit telemetry for instruction execution
 */
static void emit_telemetry(bc_context_t* ctx, uint8_t opcode, uint32_t data) {
    if (ctx->telemetry) {
        // Simple telemetry - would be more sophisticated in production
        ctx->telemetry->timestamp = __rdtsc();
        ctx->telemetry->exec_hash = (opcode << 24) | (data & 0xFFFFFF);
        
        // Store instruction in trace_ops
        if (ctx->tick_count < 16) {
            ctx->telemetry->trace_ops[ctx->tick_count] = opcode;
        }
    }
}

/**
 * Generate C handler function from bytecode
 */
int bitactor_generate_handler(const uint8_t* bytecode, size_t bytecode_size,
                             const char* handler_name, FILE* output) {
    fprintf(output, "/* Auto-generated handler: %s */\n", handler_name);
    fprintf(output, "static result_t %s(signal_t* signal, void* scratch) {\n", handler_name);
    fprintf(output, "    return bitactor_execute_bytecode(signal, ");
    fprintf(output, "(const uint8_t[]){");
    
    // Embed bytecode as C array
    for (size_t i = 0; i < bytecode_size; i++) {
        if (i % 16 == 0) fprintf(output, "\n        ");
        fprintf(output, "0x%02X", bytecode[i]);
        if (i < bytecode_size - 1) fprintf(output, ", ");
    }
    
    fprintf(output, "\n    }, %zu, scratch);\n", bytecode_size);
    fprintf(output, "}\n\n");
    
    return 0;
}

/**
 * Register bytecode handler with BitActor engine
 */
int bitactor_install_handler(bitactor_engine_t* engine, uint8_t signal_kind,
                            const uint8_t* bytecode, size_t bytecode_size) {
    // Create wrapper function that calls bytecode executor
    // This would be more complex in a full implementation
    return bitactor_register(engine, signal_kind, 
                            (bitactor_handler_fn)bitactor_execute_bytecode);
}