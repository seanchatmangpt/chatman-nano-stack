/*
 * BitActor Runtime Bytecode Loader Header
 * High-performance bytecode execution with SIMD acceleration
 */
#ifndef BITACTOR_BYTECODE_LOADER_H
#define BITACTOR_BYTECODE_LOADER_H

#include <stdint.h>
#include <stdio.h>
#include "../include/bitactor/bitactor.h"

#ifdef __cplusplus
extern "C" {
#endif

// Bytecode loader API
int bitactor_load_bytecode(const char* filename, uint8_t** bytecode_out, size_t* size_out);
result_t bitactor_execute_bytecode(signal_t* signal, const uint8_t* bytecode, 
                                   size_t bytecode_size, void* scratch);

// Handler generation
int bitactor_generate_handler(const uint8_t* bytecode, size_t bytecode_size,
                             const char* handler_name, FILE* output);
int bitactor_install_handler(bitactor_engine_t* engine, uint8_t signal_kind,
                            const uint8_t* bytecode, size_t bytecode_size);

// Bytecode validation and introspection
int bitactor_validate_bytecode(const uint8_t* bytecode, size_t bytecode_size);
int bitactor_analyze_tick_budget(const uint8_t* bytecode, size_t bytecode_size, 
                                uint8_t* max_ticks_out);
int bitactor_dump_bytecode(const uint8_t* bytecode, size_t bytecode_size, FILE* output);

#ifdef __cplusplus
}
#endif

#endif /* BITACTOR_BYTECODE_LOADER_H */