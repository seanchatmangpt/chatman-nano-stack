#ifndef NEWS_VALIDATOR_H
#define NEWS_VALIDATOR_H

#include <stdint.h>
#include <stdbool.h>

// Public API for news validation

// Validate a news article in 8 CPU cycles
uint32_t validate_news_article(const claim_t* claims, uint32_t claim_count);

// Check source credibility in 1 cycle
uint32_t check_source_credibility(uint64_t source_id);

// Initialize fact database
void init_fact_database(const char* db_path);

// Process real-time fact updates
void process_fact_stream(const claim_t* new_facts, uint32_t count);

#endif