#ifndef BITACTOR_NEWS_VALIDATION_INTEGRATION_H
#define BITACTOR_NEWS_VALIDATION_INTEGRATION_H

#include "bitactor.h"

#ifdef __cplusplus
extern "C" {
#endif

// News validation signal types
#define NEWS_VALIDATION_SIGNAL  0x1001
#define NEWS_CLAIM_SIGNAL      0x1002
#define NEWS_SOURCE_SIGNAL     0x1003

// News validation result flags
#define NEWS_VALIDATED     0x00000001
#define NEWS_DISPUTED      0x00000002
#define NEWS_FALSE         0x00000004
#define NEWS_UNVERIFIED    0x00000008
#define NEWS_REJECTED      0x80000000

// Initialize news validation integration
void bitactor_init_news_validation(bitactor_t* ba);

// Enhanced tick function with news validation optimization
void bitactor_tick_with_news_validation(bitactor_t* ba);

// Get news validation results
uint32_t bitactor_get_news_validation_result(bitactor_t* ba);

// Batch news validation for SIMD optimization
void bitactor_batch_validate_news(signal_t* signals, uint32_t count, uint32_t* results);

#ifdef __cplusplus
}
#endif

#endif // BITACTOR_NEWS_VALIDATION_INTEGRATION_H