/*
 * Real News Validator Implementation BDD Tests
 * Testing the actual news validator with ontology validation
 * NO MOCKS - Real news validation logic
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../src/news/news_validator.h"
#include "../bitactor/include/bitactor.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>

// Real news article structure
typedef struct {
    char* title;
    char* content;
    char* source_url;
    char* author;
    time_t published_date;
    char* category;
    double sentiment_score;
    char** entities;
    int entity_count;
} news_article_t;

// Validation result details
typedef struct {
    bool is_valid;
    double credibility_score;
    char* validation_errors[10];
    int error_count;
    uint64_t validation_ticks;
} validation_result_t;

// Test helper to create realistic news articles
static news_article_t* create_test_article(const char* title, const char* content, 
                                          const char* source, const char* author) {
    news_article_t* article = (news_article_t*)malloc(sizeof(news_article_t));
    article->title = strdup(title);
    article->content = strdup(content);
    article->source_url = strdup(source);
    article->author = strdup(author);
    article->published_date = time(NULL);
    article->category = strdup("technology");
    article->sentiment_score = 0.7;
    article->entities = NULL;
    article->entity_count = 0;
    return article;
}

static void free_test_article(news_article_t* article) {
    if (!article) return;
    free(article->title);
    free(article->content);
    free(article->source_url);
    free(article->author);
    free(article->category);
    for (int i = 0; i < article->entity_count; i++) {
        free(article->entities[i]);
    }
    free(article->entities);
    free(article);
}

FEATURE(News_Validator_Real_Implementation) {
    
    SCENARIO("News validator initialization with ontology loading") {
        news_validator_t* validator = NULL;
        
        GIVEN("uninitialized news validator",
            validator = NULL;
        );
        
        WHEN("validator is initialized with ontologies",
            uint64_t start = rdtsc_portable();
            validator = news_validator_init("../ontologies/news_validator.ttl",
                                          "../ontologies/news_validator_shacl.ttl");
            uint64_t end = rdtsc_portable();
            uint64_t init_time = end - start;
        );
        
        THEN("validator initializes with loaded ontologies",
            EXPECT_NE(validator, NULL);
            EXPECT(validator->initialized);
            EXPECT_NE(validator->ontology, NULL);
            EXPECT_NE(validator->shacl_shapes, NULL);
            EXPECT_GT(validator->rule_count, 0);
            EXPECT_NE(validator->bitactor_engine, NULL);
            
            printf("       Validator init time: %llu ticks\n",
                   (unsigned long long)init_time);
            printf("       Validation rules loaded: %u\n", validator->rule_count);
        );
        
        AND("BitActor integration is configured",
            // Verify BitActor handlers registered
            EXPECT(validator->bitactor_engine->initialized);
            EXPECT_GT(validator->bitactor_engine->dispatch.handler_count, 0);
            
            // Check for news validation handlers
            bool has_content_validator = false;
            bool has_source_validator = false;
            bool has_entity_extractor = false;
            
            for (uint32_t i = 0; i < validator->bitactor_engine->dispatch.handler_count; i++) {
                uint32_t hash = validator->bitactor_engine->dispatch.handlers[i].exec_hash;
                if (hash == hash_string("validate_content")) has_content_validator = true;
                if (hash == hash_string("validate_source")) has_source_validator = true;
                if (hash == hash_string("extract_entities")) has_entity_extractor = true;
            }
            
            EXPECT(has_content_validator);
            EXPECT(has_source_validator);
            EXPECT(has_entity_extractor);
            
            news_validator_free(validator);
        );
    } END_SCENARIO
    
    SCENARIO("Article validation with SHACL shape compliance") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("validator ready with test article",
            EXPECT_NE(validator, NULL);
            
            news_article_t* article = create_test_article(
                "Breaking: New Technology Revolutionizes Computing",
                "Researchers at MIT have developed a quantum computing breakthrough that "
                "promises to solve complex problems 1000x faster than classical computers. "
                "The new approach uses topological qubits for error correction.",
                "https://news.mit.edu/2024/quantum-breakthrough",
                "Dr. Jane Smith"
            );
        );
        
        WHEN("article is validated against SHACL shapes",
            validation_result_t result = {0};
            
            uint64_t start = rdtsc_portable();
            result.is_valid = news_validator_validate_article(validator, article);
            uint64_t end = rdtsc_portable();
            result.validation_ticks = end - start;
            
            // Get detailed validation results
            result.credibility_score = news_validator_get_credibility_score(validator, article);
            result.error_count = news_validator_get_errors(validator, result.validation_errors, 10);
        );
        
        THEN("validation completes within 8-tick budget",
            EXPECT_LE(result.validation_ticks, 8);
            EXPECT(result.is_valid);
            EXPECT_GT(result.credibility_score, 0.0);
            EXPECT_LE(result.credibility_score, 1.0);
            EXPECT_EQ(result.error_count, 0);
            
            printf("       Article validation: %llu ticks\n",
                   (unsigned long long)result.validation_ticks);
            printf("       Credibility score: %.2f\n", result.credibility_score);
            
            free_test_article(article);
        );
    } END_SCENARIO
    
    SCENARIO("Invalid article detection with constraint violations") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("articles with various validation issues",
            news_article_t* articles[] = {
                // Missing required fields
                create_test_article("", "Content without title", "http://example.com", "Author"),
                
                // Invalid source URL
                create_test_article("Valid Title", "Valid content", "not-a-url", "Author"),
                
                // Content too short
                create_test_article("Title", "Short", "http://example.com", "Author"),
                
                // Missing author
                create_test_article("Title", "Valid content here", "http://example.com", ""),
                
                // Future publication date
                create_test_article("Future News", "This hasn't happened yet", "http://example.com", "Time Traveler")
            };
            
            // Set future date for last article
            articles[4]->published_date = time(NULL) + 86400; // Tomorrow
        );
        
        WHEN("invalid articles are validated",
            validation_result_t results[5];
            
            for (int i = 0; i < 5; i++) {
                uint64_t start = rdtsc_portable();
                results[i].is_valid = news_validator_validate_article(validator, articles[i]);
                uint64_t end = rdtsc_portable();
                results[i].validation_ticks = end - start;
                results[i].error_count = news_validator_get_errors(validator, 
                                                                  results[i].validation_errors, 10);
            }
        );
        
        THEN("validation correctly identifies all issues",
            // All should be invalid
            for (int i = 0; i < 5; i++) {
                EXPECT_FALSE(results[i].is_valid);
                EXPECT_GT(results[i].error_count, 0);
                EXPECT_LE(results[i].validation_ticks, 8);
                
                printf("       Article %d: %d errors in %llu ticks\n",
                       i, results[i].error_count,
                       (unsigned long long)results[i].validation_ticks);
                
                for (int j = 0; j < results[i].error_count; j++) {
                    printf("         - %s\n", results[i].validation_errors[j]);
                }
                
                free_test_article(articles[i]);
            }
        );
    } END_SCENARIO
    
    SCENARIO("Entity extraction and semantic analysis") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("article with multiple named entities",
            news_article_t* article = create_test_article(
                "Apple CEO Tim Cook Announces Partnership with Tesla",
                "In a surprising move, Apple Inc. and Tesla Motors announced a strategic "
                "partnership today in Cupertino, California. Tim Cook and Elon Musk appeared "
                "together to unveil plans for integrating Apple's software with Tesla vehicles. "
                "The collaboration will bring CarPlay to all Tesla models by Q3 2024.",
                "https://techcrunch.com/2024/apple-tesla-partnership",
                "Sarah Johnson"
            );
        );
        
        WHEN("entities are extracted and analyzed",
            uint64_t start = rdtsc_portable();
            int entity_count = news_validator_extract_entities(validator, article,
                                                             &article->entities);
            uint64_t end = rdtsc_portable();
            uint64_t extraction_time = end - start;
            
            article->entity_count = entity_count;
        );
        
        THEN("entity extraction completes within budget",
            EXPECT_LE(extraction_time, 8);
            EXPECT_GT(entity_count, 0);
            EXPECT_NE(article->entities, NULL);
            
            printf("       Entity extraction: %llu ticks\n",
                   (unsigned long long)extraction_time);
            printf("       Entities found: %d\n", entity_count);
            
            // Verify key entities found
            bool found_apple = false;
            bool found_tesla = false;
            bool found_tim_cook = false;
            bool found_elon_musk = false;
            
            for (int i = 0; i < entity_count; i++) {
                printf("         - %s\n", article->entities[i]);
                
                if (strstr(article->entities[i], "Apple")) found_apple = true;
                if (strstr(article->entities[i], "Tesla")) found_tesla = true;
                if (strstr(article->entities[i], "Tim Cook")) found_tim_cook = true;
                if (strstr(article->entities[i], "Elon Musk")) found_elon_musk = true;
            }
            
            EXPECT(found_apple);
            EXPECT(found_tesla);
            EXPECT(found_tim_cook);
            EXPECT(found_elon_musk);
            
            free_test_article(article);
        );
    } END_SCENARIO
    
    SCENARIO("Source credibility assessment") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("articles from various sources",
            struct {
                const char* url;
                double expected_min_score;
                double expected_max_score;
            } test_sources[] = {
                {"https://www.reuters.com/article/123", 0.8, 1.0},      // High credibility
                {"https://www.bbc.com/news/article", 0.8, 1.0},         // High credibility
                {"https://unknown-blog.xyz/post", 0.0, 0.3},            // Low credibility
                {"https://academic.journal.edu/paper", 0.7, 0.9},       // Academic source
                {"https://social-media.com/user/post", 0.1, 0.4}        // Social media
            };
        );
        
        WHEN("source credibility is assessed",
            double scores[5];
            uint64_t assessment_times[5];
            
            for (int i = 0; i < 5; i++) {
                news_article_t* article = create_test_article(
                    "Test Article", "Test content for credibility assessment",
                    test_sources[i].url, "Test Author"
                );
                
                uint64_t start = rdtsc_portable();
                scores[i] = news_validator_assess_source_credibility(validator, 
                                                                   article->source_url);
                uint64_t end = rdtsc_portable();
                assessment_times[i] = end - start;
                
                free_test_article(article);
            }
        );
        
        THEN("credibility scores reflect source reliability",
            for (int i = 0; i < 5; i++) {
                EXPECT_LE(assessment_times[i], 8);
                EXPECT_GE(scores[i], test_sources[i].expected_min_score);
                EXPECT_LE(scores[i], test_sources[i].expected_max_score);
                
                printf("       Source: %s\n", test_sources[i].url);
                printf("         Score: %.2f (expected %.2f-%.2f)\n",
                       scores[i], test_sources[i].expected_min_score,
                       test_sources[i].expected_max_score);
                printf("         Time: %llu ticks\n",
                       (unsigned long long)assessment_times[i]);
            }
        );
    } END_SCENARIO
    
    SCENARIO("Fact checking with knowledge graph integration") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("article with verifiable claims",
            news_article_t* article = create_test_article(
                "NASA Confirms Water on Mars",
                "NASA scientists have confirmed the presence of liquid water on Mars. "
                "The discovery was made using data from the Mars Reconnaissance Orbiter. "
                "Previous missions had found evidence of ancient water flows, but this is "
                "the first confirmation of present-day liquid water on the Red Planet.",
                "https://nasa.gov/press-release/water-on-mars",
                "NASA Communications"
            );
            
            // Add known facts to knowledge graph
            news_validator_add_fact(validator, "Mars", "hasFeature", "water_ice");
            news_validator_add_fact(validator, "Mars_Reconnaissance_Orbiter", "operatedBy", "NASA");
            news_validator_add_fact(validator, "NASA", "isA", "space_agency");
        );
        
        WHEN("article claims are fact-checked",
            fact_check_result_t* results = NULL;
            
            uint64_t start = rdtsc_portable();
            int claim_count = news_validator_fact_check(validator, article, &results);
            uint64_t end = rdtsc_portable();
            uint64_t fact_check_time = end - start;
        );
        
        THEN("fact checking validates claims efficiently",
            EXPECT_LE(fact_check_time, 8);
            EXPECT_GT(claim_count, 0);
            EXPECT_NE(results, NULL);
            
            printf("       Fact checking: %llu ticks\n",
                   (unsigned long long)fact_check_time);
            printf("       Claims checked: %d\n", claim_count);
            
            int verified_count = 0;
            int unverified_count = 0;
            
            for (int i = 0; i < claim_count; i++) {
                printf("         Claim %d: %s - %s\n",
                       i, results[i].claim,
                       results[i].verified ? "VERIFIED" : "UNVERIFIED");
                
                if (results[i].verified) verified_count++;
                else unverified_count++;
            }
            
            EXPECT_GT(verified_count, 0); // At least some claims verified
            
            free(results);
            free_test_article(article);
        );
    } END_SCENARIO
    
    SCENARIO("BitActor message integration for distributed validation") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        
        GIVEN("validator with BitActor messaging enabled",
            EXPECT_NE(validator->bitactor_engine, NULL);
            
            // Register validation result handler
            news_validator_register_result_handler(validator);
        );
        
        WHEN("validation triggers BitActor messages",
            news_article_t* article = create_test_article(
                "Distributed Validation Test",
                "This article will be validated across multiple BitActor nodes",
                "https://distributed.test/article",
                "Test Author"
            );
            
            uint64_t start = rdtsc_portable();
            
            // Validate with message generation
            bool valid = news_validator_validate_with_messages(validator, article);
            
            // Process generated messages
            bitactor_tick(validator->bitactor_engine);
            
            uint64_t end = rdtsc_portable();
            uint64_t total_time = end - start;
        );
        
        THEN("distributed validation completes within budget",
            EXPECT_LE(total_time, 8);
            
            // Check for validation messages in telemetry
            uint32_t validation_messages = 0;
            
            for (uint32_t i = 0; i < validator->bitactor_engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &validator->bitactor_engine->telemetry.frames[i];
                if (frame->signal.kind == SIGNAL_KIND_VALIDATION) {
                    validation_messages++;
                }
            }
            
            EXPECT_GT(validation_messages, 0);
            
            printf("       Distributed validation: %llu ticks\n",
                   (unsigned long long)total_time);
            printf("       Validation messages: %u\n", validation_messages);
            
            free_test_article(article);
        );
    } END_SCENARIO
    
    SCENARIO("Batch article validation performance") {
        news_validator_t* validator = news_validator_init("../ontologies/news_validator.ttl",
                                                        "../ontologies/news_validator_shacl.ttl");
        const int BATCH_SIZE = 100;
        
        GIVEN("batch of articles for validation",
            news_article_t* articles[BATCH_SIZE];
            
            for (int i = 0; i < BATCH_SIZE; i++) {
                char title[100], source[100];
                sprintf(title, "Article %d: Technology News Update", i);
                sprintf(source, "https://news%d.example.com/article", i % 10);
                
                articles[i] = create_test_article(
                    title,
                    "Standard article content with sufficient length for validation. "
                    "This includes various facts and claims that need verification.",
                    source,
                    "Batch Author"
                );
            }
        );
        
        WHEN("batch validation is performed",
            uint64_t start = rdtsc_portable();
            
            int valid_count = 0;
            for (int i = 0; i < BATCH_SIZE; i++) {
                if (news_validator_validate_article(validator, articles[i])) {
                    valid_count++;
                }
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t batch_time = end - start;
        );
        
        THEN("batch processing maintains per-article performance",
            uint64_t avg_time = batch_time / BATCH_SIZE;
            EXPECT_LE(avg_time, 8);
            
            printf("       Batch validation: %d articles in %llu ticks\n",
                   BATCH_SIZE, (unsigned long long)batch_time);
            printf("       Average per article: %llu ticks\n",
                   (unsigned long long)avg_time);
            printf("       Valid articles: %d/%d (%.1f%%)\n",
                   valid_count, BATCH_SIZE, (valid_count * 100.0) / BATCH_SIZE);
            
            // Cleanup
            for (int i = 0; i < BATCH_SIZE; i++) {
                free_test_article(articles[i]);
            }
            
            news_validator_free(validator);
        );
    } END_SCENARIO
}