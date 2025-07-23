/*
 * BitActor CNS Pipeline Integration Header
 */
#ifndef CNS_BITACTOR_INTEGRATION_H
#define CNS_BITACTOR_INTEGRATION_H

#include <stdio.h>
#include "../include/bitactor/bitactor.h"
#include "../../src/cns/cns_pipeline.h"

#ifdef __cplusplus
extern "C" {
#endif

// Integration API
int cns_bitactor_init(void);
int cns_bitactor_load_ontology(const char* ttl_file, const char* shacl_file);
result_t cns_bitactor_process_signal(signal_t* signal);
int cns_bitactor_tick_integration(cns_pipeline_t* pipeline);
void cns_bitactor_performance_report(FILE* output);
void cns_bitactor_cleanup(void);

// Configuration
#define CNS_BITACTOR_MAX_HANDLERS    64
#define CNS_BITACTOR_CACHE_SIZE      (256 * 1024)
#define CNS_BITACTOR_VALIDATION_SAMPLES 1000

#ifdef __cplusplus
}
#endif

#endif /* CNS_BITACTOR_INTEGRATION_H */