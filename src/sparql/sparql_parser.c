#include "sparql_ast.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

void* arena_alloc(arena_t* arena, size_t size) {
    void* p = arena->current;
    arena->current = (char*)arena->current + ((size + 63) & ~63);
    return p;
}

static char* skip_whitespace(char* p) {
    while (*p && isspace(*p)) p++;
    return p;
}

static char* parse_token(char* p, char* out, size_t max) {
    p = skip_whitespace(p);
    
    if (*p == '<') {
        p++;
        char* start = p;
        while (*p && *p != '>') p++;
        size_t len = p - start;
        if (len >= max) len = max - 1;
        memcpy(out, start, len);
        out[len] = '\0';
        if (*p == '>') p++;
    } else if (*p == '?') {
        char* start = p;
        while (*p && (isalnum(*p) || *p == '_')) p++;
        size_t len = p - start;
        if (len >= max) len = max - 1;
        memcpy(out, start, len);
        out[len] = '\0';
    } else if (*p == ':') {
        char* start = p - 1;
        while (*(p-1) && isalnum(*(p-1))) start--;
        while (*p && (isalnum(*p) || *p == ':' || *p == '_')) p++;
        size_t len = p - start;
        if (len >= max) len = max - 1;
        memcpy(out, start, len);
        out[len] = '\0';
    } else {
        char* start = p;
        while (*p && !isspace(*p) && *p != '.' && *p != ';') p++;
        size_t len = p - start;
        if (len >= max) len = max - 1;
        memcpy(out, start, len);
        out[len] = '\0';
    }
    
    return p;
}

static bool parse_pattern(char* line, sparql_pattern_t* pattern) {
    char* p = line;
    
    p = parse_token(p, pattern->subject, MAX_STRING_LEN);
    if (!pattern->subject[0]) return false;
    
    p = parse_token(p, pattern->predicate, MAX_STRING_LEN);
    if (!pattern->predicate[0]) return false;
    
    p = parse_token(p, pattern->object, MAX_STRING_LEN);
    if (!pattern->object[0]) return false;
    
    pattern->filter = NULL;
    return true;
}

static bool parse_filter(char* line, filter_t* filter, arena_t* arena) {
    char* p = strstr(line, "FILTER");
    if (!p) return false;
    
    p += 6;
    p = skip_whitespace(p);
    
    if (*p == '(') p++;
    p = skip_whitespace(p);
    
    if (*p == '?') {
        char var[MAX_STRING_LEN];
        p = parse_token(p, var, MAX_STRING_LEN);
        
        p = skip_whitespace(p);
        
        if (*p == '>') {
            filter->type = FILTER_GT;
            p++;
        } else if (*p == '<') {
            filter->type = FILTER_LT;
            p++;
        } else if (*p == '=') {
            filter->type = FILTER_EQ;
            p += 2;
        }
        
        p = skip_whitespace(p);
        
        if (strstr(p, "NOW()")) {
            filter->type = FILTER_NOW_MINUS;
            strcpy(filter->operand, "NOW");
        } else {
            filter->value = strtol(p, NULL, 10);
            strcpy(filter->operand, var);
        }
        
        return true;
    }
    
    return false;
}

sparql_ast_t* parse_sparql_file(const char* filename, arena_t* arena) {
    FILE* file = fopen(filename, "r");
    if (!file) return NULL;
    
    sparql_ast_t* ast = arena_alloc(arena, sizeof(sparql_ast_t));
    memset(ast, 0, sizeof(sparql_ast_t));
    
    const char* basename = strrchr(filename, '/');
    if (!basename) basename = filename;
    else basename++;
    
    strncpy(ast->query_name, basename, MAX_STRING_LEN - 1);
    char* dot = strrchr(ast->query_name, '.');
    if (dot) *dot = '\0';
    
    char line[1024];
    bool in_where = false;
    
    while (fgets(line, sizeof(line), file)) {
        if (strstr(line, "WHERE")) {
            in_where = true;
            continue;
        }
        
        if (!in_where) continue;
        
        if (strchr(line, '}')) break;
        
        if (strstr(line, "FILTER")) {
            if (ast->pattern_count > 0) {
                filter_t* filter = arena_alloc(arena, sizeof(filter_t));
                if (parse_filter(line, filter, arena)) {
                    ast->patterns[ast->pattern_count - 1].filter = filter;
                }
            }
        } else if (strchr(line, '?') && strchr(line, '.')) {
            if (ast->pattern_count < MAX_PATTERNS) {
                if (parse_pattern(line, &ast->patterns[ast->pattern_count])) {
                    ast->pattern_count++;
                }
            }
        }
    }
    
    fclose(file);
    return ast;
}