#ifndef SPARQL_AST_H
#define SPARQL_AST_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#define MAX_PATTERNS 8
#define MAX_STRING_LEN 256

typedef enum {
    FILTER_GT,
    FILTER_LT,
    FILTER_EQ,
    FILTER_NOW_MINUS
} filter_type_t;

typedef struct {
    filter_type_t type;
    char operand[MAX_STRING_LEN];
    int64_t value;
} filter_t;

typedef struct {
    char subject[MAX_STRING_LEN];
    char predicate[MAX_STRING_LEN];
    char object[MAX_STRING_LEN];
    filter_t* filter;
} sparql_pattern_t;

typedef struct {
    sparql_pattern_t patterns[MAX_PATTERNS];
    uint8_t pattern_count;
    char query_name[MAX_STRING_LEN];
} sparql_ast_t;

typedef struct {
    void* current;
    void* end;
} arena_t;

void* arena_alloc(arena_t* arena, size_t size);
sparql_ast_t* parse_sparql_file(const char* filename, arena_t* arena);

#endif